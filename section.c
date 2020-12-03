#include "cc.h"
#include "elf.h"
#include "liberty.h"

struct section **sections;
int sections_used;
static int sections_alloc;

struct section *newsec(const char *name, int type, int flags)
{
    struct section *s;
    
    s = zmalloc(sizeof(struct section) + strlen(name));
    strcpy(s->name, name);
    s->sh_type = type;
    s->sh_flags = flags;
    switch (type) {
    case SHT_STRTAB:
        /* The first byte of string table must be zero. */
        sec_add_str(s, "");
        s->sh_addralign = 1;
        break;
    case SHT_PROGBITS:
    case SHT_NOBITS:
    case SHT_REL:
    case SHT_RELA:
    case SHT_HASH:
    case SHT_DYNAMIC:
    case SHT_SYMTAB:
    default:
        s->sh_addralign = 8;
        break;
    }
    
    s->shndx = sections_used;
    if (sections_used + 1 > sections_alloc) {
        sections_alloc = 2 * (sections_used + 1);
        sections = xrealloc(sections, sections_alloc * sizeof(*sections));
    }
    sections[sections_used++] = s;
    
    return s;
}

static void sec_data_grow(struct section *s, size_t new_size)
{
    size_t size = s->data_alloc;
    
    if (size == 0)
        size = 256;
    while (size < new_size)
        size = size * 2;
    if (s->sh_type != SHT_NOBITS)
        s->data = xrealloc(s->data, size);
    s->data_alloc = size;
}

void sec_set_data(struct section *s, const void *src, size_t len, size_t offset)
{
    size_t newsize = offset + len;
    if (newsize > s->data_alloc)
        sec_data_grow(s, newsize);
    if (newsize > s->data_len)
        s->data_len = newsize;
    if (s->data)
        memcpy(s->data + offset, src, len);
}

size_t sec_add_data(struct section *s, const void *src, size_t len)
{
    size_t offset = s->data_len;
    sec_set_data(s, src, len, offset);
    return offset;
}

size_t sec_add_str(struct section *s, const char *str)
{
    /* including terminating zero */
    return sec_add_data(s, str, strlen(str) + 1);
}

size_t sec_extend(struct section *s, size_t exsize)
{
    size_t oldsize = s->data_len;
    s->data_len += exsize;
    if (s->data_len > s->data_alloc)
        sec_data_grow(s, s->data_len);
    /* always zero bytes to prevent random bytes in bootstrap */
    if (s->data)
        memset(s->data + oldsize, 0, exsize);
    return oldsize;
}

void sec_align(struct section *s, int align)
{
    if (align > 1) {
        size_t size = ROUNDUP(s->data_len, align);
        if (size > s->data_len)
            sec_extend(s, size - s->data_len);
    }
}

int sec_find_sym(struct section *s, const char *name)
{
    Elf64_Sym *pstart, *plimit, *p;
    
    if (name == NULL)
        return 0;

    pstart = (Elf64_Sym *)s->data;
    plimit = (Elf64_Sym *)(s->data + s->data_len);

    for (p = pstart; p < plimit; p++) {
        const char *symname = s->link->data + p->st_name;
        if (!strcmp(symname, name))
            return p - pstart;
    }

    return 0;
}

int sec_add_sym(struct section *s, const char *name, int bind, int type, 
                int shndx, size_t value, size_t size)
{
    Elf64_Sym *sym;
    int index;

    index = (Elf64_Sym *)(s->data + s->data_len) - (Elf64_Sym *)s->data;
    sym = xmalloc(sizeof *sym);
    if (name)
        sym->st_name = sec_add_str(s->link, name);
    else
        sym->st_name = 0;
    sym->st_info = ELF64_ST_INFO(bind, type);
    sym->st_shndx = shndx;
    sym->st_other = 0;
    sym->st_value = value;
    sym->st_size = size;
    sec_add_data(s, sym, sizeof(*sym));
    
    return index;
}

struct section *newsymtab(const char *name, int type, int flags, 
                          const char *strtab, int entsize)
{
    struct section *symtab;
    
    symtab = newsec(name, type, flags);
    symtab->sh_entsize = entsize;
    symtab->link = newsec(strtab, SHT_STRTAB, 0);
    /* The first symbol table entry must be all zero. */
    sec_extend(symtab, entsize);
    return symtab;
}

/* generate a relocation entry */
void sec_add_rela(struct section *symtab, struct section *s, 
                  size_t offset, int sym, int type, long addend)
{
    Elf64_Rela *entry;

    if (s->reloc == NULL) {
        s->reloc = newsec(stringf(".rela%s", s->name), SHT_RELA, 0);
        s->reloc->link = symtab;
        s->reloc->sh_info = s->shndx;
        s->reloc->sh_entsize = sizeof(Elf64_Rela);
    }
    
    entry = xmalloc(sizeof(*entry));
    entry->r_offset = offset;
    entry->r_info = ELF64_R_INFO((long)sym, type);
    entry->r_addend = addend;
    sec_add_data(s->reloc, entry, sizeof(*entry));
}

/**
 * local symbols must be placed in front of global symbols.
 * `sh_info` of symbol table must be the index of first non-local symbol.
 * (i.e. number of local symbols)
 */
void sec_sort_syms(struct section *s)
{
    Elf64_Sym *pstart, *plimit, *p1, *p2, tmp;
    int *ndxmap, i;
    long type, symndx;
    
    pstart = (Elf64_Sym *)s->data;
    plimit = (Elf64_Sym *)(s->data + s->data_len);
    p1 = pstart + 1;    /* skip first empty entry */
    p2 = plimit - 1;

    ndxmap = xmalloc((plimit - pstart) * sizeof (int));
    for (i = 0; i < plimit - pstart; i++)
        ndxmap[i] = i;

    while (1) {
        while (p1 < plimit && ELF64_ST_BIND(p1->st_info) == STB_LOCAL)
            p1++;
        if (p1 == plimit)
            break;
        while (p2 > p1 && ELF64_ST_BIND(p2->st_info) != STB_LOCAL)
            p2--;
        if (p2 <= p1)
            break;

        i = p1 - pstart;
        ndxmap[p1 - pstart] = ndxmap[p2 - pstart];
        ndxmap[p2 - pstart] = i;

        tmp = *p1;
        *p1 = *p2;
        *p2 = tmp;
    }

    /* number of local symbols */
    s->sh_info = p1 - pstart;

    /* recaculate .rela/.rel symbol index */
    for (i = 0; i < sections_used; i++) {
        struct section *s1 = sections[i];
        if (s1->symndx)
            s1->symndx = ndxmap[s1->symndx];
        if (s1->sh_type == SHT_RELA && s1->link == s) {
            Elf64_Rela *rel, *limit;
            limit = (Elf64_Rela *)(s1->data + s1->data_len);
            for (rel = (Elf64_Rela *)s1->data; rel < limit; rel++) {
                type = ELF64_R_TYPE(rel->r_info);
                symndx = ELF64_R_SYM(rel->r_info);
                symndx = ndxmap[symndx];
                rel->r_info = ELF64_R_INFO(symndx, type);
            }
        }
        if (s1->sh_type == SHT_REL && s1->link == s) {
            Elf64_Rel *rel, *limit;
            limit = (Elf64_Rel *)(s1->data + s1->data_len);
            for (rel = (Elf64_Rel *)s1->data; rel < limit; rel++) {
                type = ELF64_R_TYPE(rel->r_info);
                symndx = ELF64_R_SYM(rel->r_info);
                symndx = ndxmap[symndx];
                rel->r_info = ELF64_R_INFO(symndx, type);
            }
        }
    }

    free(ndxmap);
}