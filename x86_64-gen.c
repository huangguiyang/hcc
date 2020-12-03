#include "config.h"
#include "compat.h"
#include "cc.h"
#include "x86_64.h"
#include "elf.h"

#define SZ_CHAR     1
#define SZ_SHORT    2
#define SZ_INT      4
#define SZ_LONG     8
#define SZ_LLONG    8
#define SZ_FLOAT    4
#define SZ_DOUBLE   8
#define SZ_PTR      8

char typsize[NTYPE] = {
    0,                          /* TXXX */
    SZ_CHAR,                    /* TBOOL */
    SZ_CHAR,                    /* TCHAR */
    SZ_CHAR,                    /* TUCHAR */
    SZ_SHORT,                   /* TSHORT */
    SZ_SHORT,                   /* TUSHORT */
    SZ_INT,                     /* TINT */
    SZ_INT,                     /* TUINT */
    SZ_LONG,                    /* TLONG */
    SZ_LONG,                    /* TULONG */
    SZ_LLONG,                   /* TLLONG */
    SZ_LLONG,                   /* TULLONG */
    SZ_FLOAT,                   /* TFLOAT */
    SZ_DOUBLE,                  /* TDOUBLE */
    SZ_PTR,                     /* TPTR */
    0,                          /* TVOID */
    0,                          /* TFUNC */
    0,                          /* TARRAY */
    0,                          /* TSTRUCT */
    0,                          /* TUNION */
    0,                          /* TENUM */
};

int ncast[NTYPE] = {
    0,                                          /* TXXX */
    BBOOL | BCHAR | BUCHAR,                     /* TBOOL */
    BBOOL | BCHAR | BUCHAR,                     /* TCHAR */
    BBOOL | BCHAR | BUCHAR,                     /* TUCHAR */
    BSHORT | BUSHORT,                           /* TSHORT */
    BSHORT | BUSHORT,                           /* TUSHORT */
    BINT | BUINT | BENUM,                       /* TINT */
    BINT | BUINT | BENUM,                       /* TUINT */
    BLONG | BULONG | BLLONG | BULLONG | BPTR,   /* TLONG */
    BLONG | BULONG | BLLONG | BULLONG | BPTR,   /* TULONG */
    BLONG | BULONG | BLLONG | BULLONG | BPTR,   /* TLLONG */
    BLONG | BULONG | BLLONG | BULLONG | BPTR,   /* TULLONG */
    BFLOAT,                                     /* TFLOAT */
    BDOUBLE,                                    /* TDOUBLE */
    BLONG | BULONG | BLLONG | BULLONG | BPTR,   /* TPTR */
    0,                                          /* TVOID */
    0,                                          /* TFUNC */
    0,                                          /* TARRAY */
    BSTRUCT,                                    /* TSTRUCT */
    BUNION,                                     /* TUNION */
    BINT | BUINT | BENUM,                       /* TENUM */
};

#define ARG_CLASS(a, b) (((a) << 16) + ((b) & 0xFF))

static int iaregs[] = {RDI, RSI, RDX, RCX, R8, R9};
static int regs[NREG];

/* builtin functions */
static void (*btfunc[NBUILTIN])(struct node *, struct node *);

struct jmp {
    struct jmp *link;
    struct jmp *jmps;
    long off;
    int lab;
};

static void tentative(struct symbol *, void *);
static void bitload(struct node *, struct node *);
static void bitstore(struct node *, struct node *);
static void gboolexpr(struct node *, struct node *);
static void gcpexpr(struct node *, struct node *);
static void gsuexpr(struct node *, struct node *);
static void gcallexpr(struct node *, struct node *);
static void gzeroexpr(struct node *, struct node *);
static void gmemcopy(struct node *, struct node *);
static void naddr(struct node *, struct node *);
static void gmove(struct node *, struct node *);
static void regialloc(struct node *, struct node *, struct node *);
static void regfixalloc(struct node *, struct node *, int);
static void gspill(struct node *, struct node *);
static void greload(struct node *, struct node *);
static void solveall(void);
static void gopcode(int, struct type *, struct node *, struct node *);
static void ginstr(int, struct node *, struct node *);
static void va_xxx_init(void);

static struct section *text_sec;
static struct section *data_sec;
static struct section *bss_sec;
static struct section *rodata_sec;
static struct section *symtab_sec;
static struct node regnode;
static struct node constnode;
static struct node autonode;
static int vafndx, vaindx;
static long reg_save_area;
static long redzone;
static long suretoff;
static struct jmp *jmplist, *freejmplist;
static int retlab;

void ginit(void)
{
    struct section *comment_sec;

    va_xxx_init();

    /* first empty section header */
    newsec("", SHT_NULL, 0);
    /* standard sections */
    text_sec = newsec(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
    data_sec = newsec(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE);
    bss_sec = newsec(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE);
    rodata_sec = newsec(".rodata", SHT_PROGBITS, SHF_ALLOC);
    
    /* version control info */
    comment_sec = newsec(".comment", SHT_PROGBITS, 0);
    sec_add_str(comment_sec, "");
    sec_add_str(comment_sec, "HCC: " CONFIG_VERSION);

    symtab_sec = newsymtab(".symtab", SHT_SYMTAB, 0, ".strtab", sizeof(Elf64_Sym));
    /* add source file entry */
    if (main_input_filename == NULL || main_input_filename[0] == 0)
        sec_add_sym(symtab_sec, "<stdin>",
                    STB_LOCAL, STT_FILE, SHN_ABS, 0, 0);
    else
        sec_add_sym(symtab_sec, file_basename(main_input_filename),
                    STB_LOCAL, STT_FILE, SHN_ABS, 0, 0);
    /* add section entry */
    bss_sec->symndx = sec_add_sym(symtab_sec, NULL, STB_LOCAL, STT_SECTION,
                                  bss_sec->shndx, 0, 0);
    rodata_sec->symndx = sec_add_sym(symtab_sec, NULL, STB_LOCAL, STT_SECTION,
                                     rodata_sec->shndx, 0, 0);
    
    regnode.op = OREGISTER;
    regnode.type = types[TLONG];

    constnode.op = OCONST;
    constnode.type = types[TINT];

    autonode.op = ONAME;
    autonode.type = types[TLONG];
    autonode.sclass = CAUTO;
    autonode.addable = 1;
    autonode.sym = lookup(".SPILL", OPT_CREATE);
    
    opc_init(text_sec);
}

void gfini(void)
{
    int i;
    FILE *fd;
    const char *path;
    Elf64_Ehdr elf_hdr;
    Elf64_Shdr shdr;
    size_t offset;
    struct section *shstrtab_sec, *s;

    foreach(tentative, NULL);

    shstrtab_sec = newsec(".shstrtab", SHT_STRTAB, 0);
    sec_sort_syms(symtab_sec);

    offset = sizeof(Elf64_Ehdr);
    for (i = 0; i < sections_used; i++) {
        s = sections[i];
        s->sh_name = sec_add_str(shstrtab_sec, s->name);
        s->sh_offset = offset;
        s->sh_size = s->data_len;
        offset += s->sh_size;
        if (s->sh_addralign > 1)
            offset = ROUNDUP(offset, s->sh_addralign);
    }
    offset = ROUNDUP(offset, 8);

    memset(&elf_hdr, 0, sizeof(elf_hdr));
    elf_hdr.e_ident[EI_MAG0] = 0x7f;
    elf_hdr.e_ident[EI_MAG1] = 'E';
    elf_hdr.e_ident[EI_MAG2] = 'L';
    elf_hdr.e_ident[EI_MAG3] = 'F';
    elf_hdr.e_ident[EI_CLASS] =  ELFCLASS64;
    elf_hdr.e_ident[EI_DATA] = ELFDATA2LSB;
    elf_hdr.e_ident[EI_VERSION] = EV_CURRENT;
    elf_hdr.e_ident[EI_OSABI] = ELFOSABI_SYSV;
    elf_hdr.e_ident[EI_ABIVERSION] = 0;
    elf_hdr.e_type = ET_REL;
    elf_hdr.e_machine = EM_X86_64;
    elf_hdr.e_version = EV_CURRENT;
    elf_hdr.e_ehsize = sizeof(Elf64_Ehdr);
    elf_hdr.e_shentsize = sizeof(Elf64_Shdr);
    elf_hdr.e_shstrndx = sections_used - 1;
    elf_hdr.e_shnum = sections_used;
    elf_hdr.e_shoff = offset;

    /* write to file */
    if (main_output_filename == NULL || main_output_filename[0] == 0)
        path = "a.out";
    else
        path = main_output_filename;
    fd = fopen(path, "w+b");
    if (fd == NULL)
        die_errno("can't open file '%s'", path);

    fwrite(&elf_hdr, 1, sizeof(elf_hdr), fd);
    
    /* write section content */
    offset = sizeof(elf_hdr);
    for (i = 0; i < sections_used; i++) {
        s = sections[i];
        if (s->sh_type == SHT_NOBITS || s->data_len == 0)
            continue;
        while (offset < s->sh_offset) {
            fputc(0, fd);
            offset++;
        }
        fwrite(s->data, 1, s->data_len, fd);
        offset += s->data_len;
    }
    
    /* write section headers */
    while (offset < elf_hdr.e_shoff) {
        fputc(0, fd);
        offset++;
    }

    for (i = 0; i < sections_used; i++) {
        s = sections[i];
        memset(&shdr, 0, sizeof(shdr));
        shdr.sh_name = s->sh_name;
        shdr.sh_type = s->sh_type;
        shdr.sh_offset = s->sh_offset;
        shdr.sh_size = s->sh_size;
        shdr.sh_info = s->sh_info;
        shdr.sh_flags = s->sh_flags;
        shdr.sh_addralign = s->sh_addralign;
        shdr.sh_entsize = s->sh_entsize;
        shdr.sh_addr = s->sh_addr;
        if (s->link)
            shdr.sh_link = s->link->shndx;
        fwrite(&shdr, 1, sizeof(shdr), fd);
    }

    fclose(fd);
}

long gstring(const char *str, size_t len)
{
    return sec_add_data(rodata_sec, str, len);
}

static long gfloat(struct type *ty, double val)
{
    float f;

    if (ty->etype == TFLOAT) {
        f = val;
        return sec_add_data(rodata_sec, &f, ty->size);
    } else if (ty->etype == TDOUBLE) {
        return sec_add_data(rodata_sec, &val, ty->size);
    }
    return 0;
}

static int put_elf_sym(struct section *symtab, struct symbol *s)
{
    int bind, type;

    if (s->sclass == CSTATIC)
        bind = STB_LOCAL;
    else
        bind = STB_GLOBAL;
    type = STT_NOTYPE;
    
    s->symndx = sec_add_sym(symtab, s->name, bind, type, SHN_UNDEF, 0, 0);
    return s->symndx;
}

static int find_symndx(struct symbol *sym)
{
    int ndx;

    if (sym == NULL) /* string or fconst */
        return rodata_sec->symndx;

    ndx = sym->symndx;
    if (ndx == 0)
        ndx = put_elf_sym(symtab_sec, sym);

    return ndx;
}

static void gdata1(struct node *p, long o)
{
    struct node *l, *r;
    void *addr;
    int ndx;
    /* GCC -O2 BUG: we declare `val`, `f` here to prevent this. */
    long val, oval, mask;
    float f;

    if (p == NULL)
        return;
    
    l = p->left;
    r = p->right;

    if (p->op == OLIST) {
        gdata1(l, o);
        gdata1(r, o);
        return;
    }
    if (p->op != OAS && p->op != OASI)
        return;

    o += l->offset;
    if (r->op == OCONST) {
        /* handle bit-field */
        if (l->type->nbits) {
            val = r->v.i;
            mask = MASK(l->type->nbits);
            if (val > mask)
                warn(p, "bit-field value overflow");
            oval = *(long *)(data_sec->data + o);
            val &= mask;
            val <<= l->type->bitoff;
            mask = ~(mask << l->type->bitoff);
            oval &= mask;
            val |= oval;
            addr = &val;
        } else if (p->type->etype == TFLOAT) {
            f = r->v.d;
            addr = &f;
        } else {
            addr = &r->v;
        }
        sec_set_data(data_sec, addr, p->type->size, o);
    }
        
    if (r->op == OADDR) {
        switch (r->left->op) {
        case ONAME:
        case OCOMPOUND:
            ndx = find_symndx(r->left->sym);
            sec_add_rela(symtab_sec, data_sec, o, ndx, R_X86_64_64, r->left->offset);
            break;
        }
    }
}

void gdata(struct symbol *s, struct type *ty, struct node *p)
{
    long offset;
    int bind;
    Elf64_Sym *elf_sym;

    if (ty->size < 1)
        return;

    sec_align(data_sec, ty->align);
    offset = sec_extend(data_sec, ty->size);
    
    if (!s->symndx)
        s->symndx = put_elf_sym(symtab_sec, s);
    elf_sym = &((Elf64_Sym *)symtab_sec->data)[s->symndx];
    bind = s->sclass == CSTATIC ? STB_LOCAL : STB_GLOBAL;
    elf_sym->st_info = ELF64_ST_INFO(bind, STT_OBJECT);
    elf_sym->st_shndx = data_sec->shndx;
    elf_sym->st_value = offset;
    elf_sym->st_size = ty->size;
    gdata1(p, offset);
}

/* handle tentative declaration */
static void tentative(struct symbol *s, void *v)
{
    Elf64_Sym *elf_sym;
    size_t offset, size;
    int shndx, bind, type;

    if (s->type == NULL)
        return;
    if (s->builtin_id && s->sclass != CSTATIC)
        return;
    if (s->type->size == 0 && s->type->etype != TFUNC)
        return;
    if (s->sclass != CGLOBAL && s->sclass != CSTATIC && s->sclass != CEXTERN)
        return;
    if (s->used == 0 && s->sclass != CGLOBAL)
        return;

    elf_sym = NULL;
    if (s->symndx) {
        elf_sym = &((Elf64_Sym *)symtab_sec->data)[s->symndx];
        if (elf_sym->st_size > 0)
            return;
    }

    switch (s->sclass) {
    case CGLOBAL:
        size = s->type->size;
        offset = size;
        bind = STB_GLOBAL;
        shndx = SHN_COMMON;
        break;

    case CSTATIC:
        sec_align(bss_sec, s->type->align);
        size = s->type->size;
        offset = sec_extend(bss_sec, size);
        bind = STB_LOCAL;
        shndx = bss_sec->shndx;
        break;

    case CEXTERN:
    default:
        offset = 0;
        size = 0;
        bind = STB_GLOBAL;
        shndx = SHN_UNDEF;
        break;
    }
    if (!s->symndx) {
        put_elf_sym(symtab_sec, s);
        elf_sym = &((Elf64_Sym *)symtab_sec->data)[s->symndx];
    }
    if (s->type->etype == TFUNC) {
        type = STT_NOTYPE;
        bind = STB_GLOBAL;
        shndx =  SHN_UNDEF;
    } else {
        type = STT_OBJECT;
    }
    elf_sym->st_info = ELF64_ST_INFO(bind, type);
    elf_sym->st_shndx = shndx;
    elf_sym->st_value = offset;
    elf_sym->st_size = size;
}

static int nodreg(struct node *dst, struct node *p, int reg)
{
    *dst = regnode;
    dst->reg = reg;
    if (p) {
        dst->type = p->type;
        dst->line = p->line;
    }
    if (regs[reg] == 0)
        return 0;
    if (p)
        if (p->op == OREGISTER && p->reg == reg)
            return 0;
    return 1;
}

static void regind(struct node *dst, struct type *ty)
{
    if (dst->op != OREGISTER) {
        error(dst, "bad parameter for regind");
        return;
    }
    dst->op = OINDREG;
    dst->type = ty;
}

static void nodconst(struct node *dst, struct type *ty, long val)
{
    *dst = constnode;
    dst->type = ty;
    dst->v.i = convltox(val, ty->etype);
}

static void nodfconst(struct node *dst, struct type *ty, double val)
{
    *dst = constnode;
    dst->type = ty;
    dst->v.d = val;
}

static void nodauto(struct node *dst, struct type *ty, long offset)
{
    *dst = autonode;
    dst->type = ty;
    dst->offset = offset;
}

static void praclass(struct node *params)
{
    print("=== ARG CLASS ===\n");
    for (struct node *p = params; p; p = p->right) {
        struct node *q = p->left;
        if (q->type->etype == TVOID)
            continue;
        print("%s:", q->sym ? q->sym->name : "");
        if (q->areg2)
            print("%d,%d", q->areg1, q->areg2);
        else if (q->areg1)
            print("%d", q->areg1);
        else
            print("memory");
        print("\n");
    }
}

static int classify(struct type *ty)
{
    int c, c1;
    long size;

loop:
    if (ty == NULL || ty->size < 1)
        return 0;

    size = ty->size;
    switch (ty->etype) {
    case TBOOL:
    case TCHAR:
    case TUCHAR:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TUINT:
    case TLONG:
    case TULONG:
    case TLLONG:
    case TULLONG:
    case TPTR:
    case TENUM:
        return TINT;

    case TFLOAT:
    case TDOUBLE:
        return TFLOAT;

    case TSTRUCT:
    case TUNION:
        if (size > 16)
            return TSTRUCT;
        c = c1 = 0;
        for (ty = ty->link; ty; ty = ty->next) {
            int t = classify(ty);
            if (ty->offset < 8) {
                if (c != TINT)
                    c = t;
            } else {
                if (c1 != TINT)
                    c1 = t;
            }
        }
        if (c == 0)
            c = TINT;
        if (c1 == 0 && size > 8)
            c1 = c;
        return ARG_CLASS(c1, c);

    case TARRAY:    /* array in structure */
        if (size > 16)
            return TSTRUCT;
        if (size <= 8) {
            ty = ty->link;
            goto loop;
        }
        c = classify(ty->link);
        return ARG_CLASS(c, c);

    default:
        return 0;
    }
}

/* return total size of arguments passed by registers */
static void classify_args(struct type *fty, struct node *a, long *rsz, long *msz)
{
    int i = 0, maxi = 6;
    int j = 0, maxj = 8;
    long size = 0;
    long aoffset = 0;

    /* implicit first argument passed by register */
    if (classify(fty->link) == TSTRUCT)
        i++;

    for (struct node *p = a; p; p = p->right) {
        struct node *q = p->left;
        switch (classify(q->type)) {
        case TINT:
            if (i < maxi) {
                q->areg1 = iaregs[i];
                i++;
                break;
            }
            goto memory;
        
        case TFLOAT:
            if (j < maxj) {
                q->areg1 = XMM0 + j;
                j++;
                break;
            }
            goto memory;
        
        case ARG_CLASS(TINT, TINT):
            if (i + 1 < maxi) {
                q->areg1 = iaregs[i];
                q->areg2 = iaregs[i + 1];
                i += 2;
                break;
            }
            goto memory;
        
        case ARG_CLASS(TINT, TFLOAT):
            if (i < maxi && j < maxj) {
                q->areg1 = XMM0 + j;
                q->areg2 = iaregs[i];
                i++, j++;
                break;
            }
            goto memory;
        
        case ARG_CLASS(TFLOAT, TINT):
            if (i < maxi && j < maxj) {
                q->areg1 = iaregs[i];
                q->areg2 = XMM0 + j;
                i++, j++;
                break;
            }
            goto memory;
        
        case ARG_CLASS(TFLOAT, TFLOAT):
            if (j + 1 < maxj) {
                q->areg1 = XMM0 + j;
                q->areg2 = XMM0 + j + 1;
                j += 2;
                break;
            }
            goto memory;

        case TSTRUCT:
        memory:
            /* pass by memory, must be 8-byte aligned */
            q->aoffset = aoffset;
            aoffset += ROUNDUP(q->type->size, 8);
            break;
        }

        /* argument will be promoted to at least 4 byte-aligned */
        if (q->areg1)
            size += ROUNDUP(q->type->size, 8);
    }

    if (options.debug)
        praclass(a);

    if (rsz)
        *rsz = size;
    if (msz)
        *msz = aoffset;
}

static void greg_save_area(long offset)
{
    struct node nod, nod1;
    int i, lab;

    nodreg(&nod1, NULL, RBP);
    regind(&nod1, types[TLONG]);
    offset += vaindx * 8;
    for (i = vaindx; i < 6; i++) {
        nodreg(&nod, &nod1, iaregs[i]);
        nod1.offset = offset;
        ginstr(AMOV, &nod, &nod1);
        offset += 8;
    }

    /* testb %al, %al */
    nodreg(&nod, NULL, RAX);
    nod.type = types[TCHAR];
    ginstr(ATEST, &nod, &nod);
    /* je $xxx */
    lab = genlab(1);
    gbranch(OEQ, lab, 0);

    offset += vafndx * 16;
    for (i = vafndx; i < 8; i++) {
        nodreg(&nod, &nod1, XMM0 + i);
        nod1.offset = offset;
        ginstr(AMOVAPS, &nod, &nod1);
        offset += 16;
    }
    glabel(lab);
}

static void gsave_reg_params(struct node *params, long offset)
{
    struct node *p, nod, nod1;
    long offset2 = 16;  /* saved rbp + return addr */

    nodreg(&nod1, NULL, RBP);
    regind(&nod1, types[TLONG]);
    for (p = params; p; p = p->right) {
        struct node *q = p->left;
        if (q->areg1) {
            q->offset = offset;
            q->sym->offset = offset;
            offset += ROUNDUP(q->type->size, 8);

            nodreg(&nod, &nod1, q->areg1);
            nod1.offset = q->offset;
            if (q->areg1 >= XMM0) {
                ginstr(AMOVQ, &nod, &nod1);
                vafndx++;
            } else {
                ginstr(AMOV, &nod, &nod1);
                vaindx++;
            }
            if (q->areg2) {
                nodreg(&nod, &nod1, q->areg2);
                nod1.offset = q->offset + 8;
                if (q->areg2 >= XMM0) {
                    ginstr(AMOVQ, &nod, &nod1);
                    vafndx++;
                } else {
                    ginstr(AMOV, &nod, &nod1);
                    vaindx++;
                }
            }
        } else if (q->type->size > 0) {
            q->offset = q->aoffset + offset2;
            q->sym->offset = q->offset;
        }
    }
}

static void symadjust(struct node *p)
{
    if (p == NULL)
        return;
    
    switch (p->op) {
    default:
        if (p->left)
            symadjust(p->left);
        if (p->right)
            symadjust(p->right);
        return;

    case ONAME:
        if (p->sclass == CPARAM)
            p->offset = p->sym->offset;
        return;

    case OCONST:
    case OSTRING:
        return;
    }
}

void gfunc(struct node *p, struct node *fn, struct node *params)
{
    Elf64_Sym *elf_sym;
    long codeoff, regoff, regsize;
    int dotdot, bind;
    struct node nod, nod1, nod2;
    
    codeoff = text_sec->data_len;
    redzone = 0;
    suretoff = 0;
    retlab = 0;
    vafndx = vaindx = 0;
    reg_save_area = 0;
    dotdot = variadic(fn->type);
    /* first implicit argument in RDI */
    if (classify(fn->type->link) == TSTRUCT) {
        stkoffset += 8;
        suretoff = -stkoffset;
    }
    classify_args(fn->type, params, &regsize, NULL);
    stkoffset += regsize;
    if (dotdot)
        stkoffset += 176;      /* 6*8 + 8*16 */
    /* must align to 16 bytes */
    stkoffset = ROUNDUP(stkoffset, 16);
    
    /* push %rbp */
    nodreg(&nod, NULL, RBP);
    ginstr(APUSH, NULL, &nod);
    /* mov %rsp, %rbp */
    nodreg(&nod1, NULL, RSP);
    ginstr(AMOV, &nod1, &nod);
    /* sub $stacksize, %rsp */
    if (stkoffset) {
        nodconst(&nod2, types[TLONG], stkoffset);
        ginstr(ASUB, &nod2, &nod1);
    }

    if (suretoff) {
        regind(&nod, types[TPTR]);
        nod.offset = suretoff;
        nodreg(&nod1, &nod, RDI);
        ginstr(AMOV, &nod1, &nod);
    }
    
    if (dotdot)
        regoff = stkoffset - 176;
    else
        regoff = stkoffset;
    gsave_reg_params(params, -regoff);
    if (dotdot) {
        reg_save_area = -stkoffset;
        greg_save_area(-stkoffset);
    }
    
    symadjust(p);
    codegen(p, fn, params);

    if (retlab)
        glabel(retlab);
    
    solveall();
    ginstr(ALEAVE, NULL, NULL);
    ginstr(ARET, NULL, NULL);

    if (!fn->sym->symndx)
        fn->sym->symndx = put_elf_sym(symtab_sec, fn->sym);
    elf_sym = &((Elf64_Sym *)symtab_sec->data)[fn->sym->symndx];
    bind = fn->sym->sclass == CSTATIC ? STB_LOCAL : STB_GLOBAL;
    elf_sym->st_info = ELF64_ST_INFO(bind, STT_FUNC);
    elf_sym->st_shndx = text_sec->shndx;
    elf_sym->st_value = codeoff;
    elf_sym->st_size = text_sec->data_len - codeoff;
}

void gconv(struct node *p)
{
}

static void gindir(struct node *p, struct node *nn, struct node *dst)
{
    struct node *l;
    long o;

    if (p->op != OINDIR)
        return;
    
    o = 0;
    l = p->left;
    if (l->op == OADD && l->right->op == OCONST) {
        o = l->right->v.i;
        l = l->left;
    }
    regalloc(dst, l, nn);
    gexpr(l, dst);
    regind(dst, p->type);
    dst->offset = o;
}

static void glexpr(struct node *p, struct node *dst)
{
    if (p->op == OBIT)
        p = p->left;

    switch (p->op) {
    case ONAME:
        naddr(dst, p);
        regs[dst->reg]++;
        break;

    case OINDIR:
        gindir(p, NULL, dst);
        break;

    default:
        abort();
    }
}

static void grexpr(struct node *p, struct node *nn, struct node *dst)
{
    switch (p->op) {
    case ONAME:
        naddr(dst, p);
        regs[dst->reg]++;
        break;

    case OCONST:
        if (typefd[p->type->etype])
            goto reg;
        if (p->v.i > INT32_MAX || p->v.i < INT32_MIN)
            goto reg;
        *dst = *p;
        break;

    default:
    reg:
        regalloc(dst, p, nn);
        gexpr(p, dst);
        break;
    }
}

static void nullwarn(struct node *l, struct node *r)
{
    if (l)
        gexpr(l, NULL);
    if (r)
        gexpr(r, NULL);
}

void gexpr(struct node *p, struct node *nn)
{
    struct node nod, nod1, nod2, nod3, nod4;
    struct node *l, *r;
    long saved_redzone;
    int op, v;

    if (p == NULL || p->type == NULL)
        return;

    if (typesu[p->type->etype]) {
        gsuexpr(p, nn);
        return;
    }

    l = p->left;
    r = p->right;
    op = p->op;
    saved_redzone = redzone;

    switch (op) {
    case OAS:
        glexpr(l, &nod1);
        if (r->op == OCONST) {
            grexpr(r, nn, &nod);
        } else {
            regalloc(&nod, r, nn);
            gexpr(r, &nod);
        }
        if (l->op == OBIT) {
            bitstore(&nod, &nod1);
            if (nn) {
                regalloc(&nod2, l, nn);
                bitload(&nod1, &nod2);
                gmove(&nod2, nn);
                regfree(&nod2);
            }
        } else {
            gmove(&nod, &nod1);
            if (nn)
                gmove(&nod, nn);
        }
        if (nod.op != OCONST)
            regfree(&nod);
        regfree(&nod1);
        break;

    case OASMUL:
    case OASUMUL:
    case OASADD:
    case OASSUB:
    case OASXOR:
    case OASAND:
    case OASOR:
    simpleasbop:
        glexpr(l, &nod1);
        regalloc(&nod, r, NULL);
        gexpr(r, &nod);
        regalloc(&nod2, l, nn);
        regalloc(&nod3, r, &nod2);
        if (l->op == OBIT) {
            bitload(&nod1, &nod2);
            gmove(&nod2, &nod3);
            gopcode(op, r->type, &nod, &nod3);
            gmove(&nod3, &nod2);
            bitstore(&nod2, &nod1);
            if (nn) {
                bitload(&nod1, &nod2);
                gmove(&nod2, nn);
            }
        } else {
            gmove(&nod1, &nod3);
            gopcode(op, r->type, &nod, &nod3);
            gmove(&nod3, &nod2);
            gmove(&nod2, &nod1);
            if (nn)
                gmove(&nod2, nn);
        }
        regfree(&nod);
        regfree(&nod1);
        regfree(&nod2);
        regfree(&nod3);
        break;

    case OASDIV:
    case OASUDIV:
    case OASMOD:
    case OASUMOD:
        if (typefd[p->type->etype])
            goto simpleasbop;
        /* must use DX:AX */
        if (nodreg(&nod1, nn, RDX)) {
            gspill(&nod1, &nod3);
            v = regs[RDX];
            regs[RDX] = 0;
            gexpr(p, nn);
            greload(&nod3, &nod1);
            regs[RDX] = v;
            break;
        }
        if (nodreg(&nod2, nn, RAX)) {
            gspill(&nod2, &nod3);
            v = regs[RAX];
            regs[RAX] = 0;
            gexpr(p, nn);
            greload(&nod3, &nod2);
            regs[RAX] = v;
            break;
        }
        regs[RAX]++;
        regs[RDX]++;
        nod1.type = r->type;
        nod2.type = r->type;
        glexpr(l, &nod3);
        regalloc(&nod, r, NULL);
        gexpr(r, &nod);
        if (l->op == OBIT) {
            regalloc(&nod4, l, &nod2);
            bitload(&nod3, &nod4);
            gmove(&nod4, &nod2);
            gopcode(op, p->type, NULL, &nod);
            if (op == OASMOD || op == OASUMOD)
                gmove(&nod1, &nod4);
            else
                gmove(&nod2, &nod4);
            bitstore(&nod4, &nod3);
            if (nn) {
                bitload(&nod3, &nod4);
                gmove(&nod4, nn);
            }
            regfree(&nod4);
        } else {
            gmove(&nod3, &nod2);
            gopcode(op, p->type, NULL, &nod);
            if (op == OASMOD || op == OASUMOD)
                gmove(&nod1, &nod3);
            else
                gmove(&nod2, &nod3);
            if (nn) {
                if (op == OASMOD || op == OASUMOD)
                    gmove(&nod1, nn);
                else
                    gmove(&nod2, nn);
            }
        }
        regfree(&nod);
        regfree(&nod1);
        regfree(&nod2);
        regfree(&nod3);
        break;

    case OASSHL:
    case OASSHR:
    case OASUSHR:
        if (r->op == OCONST) {
            glexpr(l, &nod1);
            regalloc(&nod2, l, nn);
            regalloc(&nod3, r, &nod2);
            if (l->op == OBIT) {
                bitload(&nod1, &nod2);
                gmove(&nod2, &nod3);
                gopcode(op, p->type, r, &nod3);
                gmove(&nod3, &nod2);
                bitstore(&nod2, &nod1);
                if (nn) {
                    bitload(&nod1, &nod2);
                    gmove(&nod2, nn);
                }
            } else {
                gmove(&nod1, &nod3);
                gopcode(op, p->type, r, &nod3);
                gmove(&nod3, &nod2);
                gmove(&nod2, &nod1);
                if (nn)
                    gmove(&nod2, nn);
            }
            regfree(&nod1);
            regfree(&nod2);
            regfree(&nod3);
            break;
        }
        /* must use CL */
        if (nodreg(&nod, nn, RCX)) {
            gspill(&nod, &nod1);
            v = regs[RCX];
            regs[RCX] = 0;
            gexpr(p, nn);
            greload(&nod1, &nod);
            regs[RCX] = v;
            break;
        }
        regs[RCX]++;
        nod.type = r->type;
        glexpr(l, &nod1);
        gexpr(r, &nod);
        if (nn == NULL || nn->reg == RCX)
            regalloc(&nod2, l, NULL);
        else
            regalloc(&nod2, l, nn);
        regalloc(&nod3, r, &nod2);
        if (l->op == OBIT) {
            bitload(&nod1, &nod2);
            gmove(&nod2, &nod3);
            gopcode(op, p->type, &nod, &nod3);
            gmove(&nod3, &nod2);
            bitstore(&nod2, &nod1);
            if (nn) {
                bitload(&nod1, &nod2);
                gmove(&nod2, nn);
            }
        } else {
            gmove(&nod1, &nod3);
            gopcode(op, p->type, &nod, &nod3);
            gmove(&nod3, &nod2);
            gmove(&nod2, &nod1);
            if (nn)
                gmove(&nod2, nn);
        }
        regfree(&nod);
        regfree(&nod1);
        regfree(&nod2);
        regfree(&nod3);
        break;

    case OPREINC:
    case OPREDEC:
    preinc:
        if (p->type->etype == TPTR)
            nodconst(&nod, p->type, p->type->link->size);
        else if (typefd[p->type->etype])
            nodfconst(&nod, p->type, 1);
        else
            nodconst(&nod, p->type, 1);
        glexpr(l, &nod1);
        if (l->op == OBIT) {
            regalloc(&nod2, l, nn);
            bitload(&nod1, &nod2);
            gopcode(op, p->type, &nod, &nod2);
            bitstore(&nod2, &nod1);
            if (nn) {
                bitload(&nod1, &nod2);
                gmove(&nod2, nn);
            }
            regfree(&nod2);
        } else {
            gopcode(op, p->type, &nod, &nod1);
            if (nn)
                gmove(&nod1, nn);
        }
        regfree(&nod1);
        break;

    case OPOSTINC:
    case OPOSTDEC:
        if (nn == NULL)
            goto preinc;
        if (p->type->etype == TPTR)
            nodconst(&nod, p->type, p->type->link->size);
        else if (typefd[p->type->etype])
            nodfconst(&nod, p->type, 1);
        else
            nodconst(&nod, p->type, 1);
        glexpr(l, &nod1);
        regalloc(&nod2, l, nn);
        if (l->op == OBIT) {
            bitload(&nod1, &nod2);
            regalloc(&nod3, l, NULL);
            gmove(&nod2, &nod3);
            gopcode(op, p->type, &nod, &nod3);
            bitstore(&nod3, &nod1);
            regfree(&nod3);
        } else {
            gmove(&nod1, &nod2);
            gopcode(op, p->type, &nod, &nod1);
        }
        gmove(&nod2, nn);
        regfree(&nod1);
        regfree(&nod2);
        break;
    
    case OCAST:
        if (nn == NULL || p->type->etype == TVOID) {
            nullwarn(l, NULL);
            break;
        }
        /* handle cast to bool */
        if (p->type->etype == TBOOL) {
            if (typefd[l->type->etype])
                nodfconst(&nod1, l->type, 0);
            else
                nodconst(&nod1, l->type, 0);

            nodnew(&nod, ONE, l, &nod1);
            nod.type = types[TINT];
            gexpr(&nod, nn);
            break;
        }
        if (l->op == ONAME) {
            gmove(l, nn);
            break;
        }
        regalloc(&nod1, l, nn);
        gexpr(l, &nod1);
        regalloc(&nod, p, nn);
        gmove(&nod1, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        regfree(&nod1);
        break;

    case OCOM:
    case ONEG:
        if (nn == NULL) {
            nullwarn(l, NULL);
            break;
        }
        regalloc(&nod, l, nn);
        gexpr(l, &nod);
        gopcode(op, p->type, NULL, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case OADD:
    case OSUB:
    case OMUL:
    case OUMUL:
    case OAND:
    case OOR:
    case OXOR:
    simplebop:
        if (nn == NULL) {
            nullwarn(l, r);
            break;
        }
        regalloc(&nod, l, nn);
        gexpr(l, &nod);
        grexpr(r, NULL, &nod1);
        gopcode(op, p->type, &nod1, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        if (nod1.op != OCONST)
            regfree(&nod1);
        break;

    case ODIV:
    case OMOD:
    case OUDIV:
    case OUMOD:
        if (typefd[p->type->etype])
            goto simplebop;
        if (nn == NULL) {
            nullwarn(l, r);
            break;
        }
        /* must use DX:AX */
        if (nodreg(&nod1, nn, RDX)) {
            gspill(&nod1, &nod3);
            v = regs[RDX];
            regs[RDX] = 0;
            gexpr(p, nn);
            greload(&nod3, &nod1);
            regs[RDX] = v;
            break;
        }
        if (nodreg(&nod2, nn, RAX)) {
            gspill(&nod2, &nod3);
            v = regs[RAX];
            regs[RAX] = 0;
            gexpr(p, nn);
            greload(&nod3, &nod2);
            regs[RAX] = v;
            break;
        }
        regs[RAX]++;
        regs[RDX]++;
        gexpr(l, &nod2);
        regalloc(&nod, r, NULL);
        gexpr(r, &nod);
        gopcode(op, p->type, NULL, &nod);
        if (op == OMOD || op == OUMOD)
            gmove(&nod1, nn);
        else
            gmove(&nod2, nn);
        regfree(&nod);
        regfree(&nod1);
        regfree(&nod2);
        break;

    case OSHL:
    case OSHR:
    case OUSHR:
        if (nn == NULL) {
            nullwarn(l, r);
            break;
        }
        if (r->op == OCONST) {
            regalloc(&nod, l, nn);
            gexpr(l, &nod);
            gopcode(op, p->type, r, &nod);
            gmove(&nod, nn);
            regfree(&nod);
            break;
        }
        /* must use CL */
        if (nodreg(&nod, nn, RCX)) {
            gspill(&nod, &nod1);
            v = regs[RCX];
            regs[RCX] = 0;
            gexpr(p, nn);
            greload(&nod1, &nod);
            regs[RCX] = v;
            break;
        }
        regs[RCX]++;
        if (nn->reg == RCX)
            regalloc(&nod1, l, NULL);
        else
            regalloc(&nod1, l, nn);
        gexpr(l, &nod1);
        gexpr(r, &nod);
        gopcode(op, p->type, &nod, &nod1);
        gmove(&nod1, nn);
        regfree(&nod);
        regfree(&nod1);
        break;

    case OLT:
    case OLE:
    case OGT:
    case OGE:
    case OEQ:
    case ONE:
    case ONOT:
    case OANDAND:
    case OOROR:
    case OCOND:
        gboolexpr(p, nn);
        break;

    case ODOT:
        if (r == NULL || r->op != OCONST) {
            error(r, "bad dot tree, missing constant");
            break;
        }
        if (nn == NULL) {
            gsuexpr(l, NULL);
            break;
        }
        regialloc(&nod, l, nn);
        gsuexpr(l, &nod);
        nod.offset += r->v.i;
        nod.type = p->type;
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case OBIT:
        if (nn == NULL) {
            nullwarn(l, NULL);
            break;
        }
        regalloc(&nod, l, nn);
        gexpr(l, &nod);
        bitload(NULL, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case OFUNC:
        gcallexpr(p, nn);
        break;

    case OZERO:
        gzeroexpr(p, nn);
        break;

    case OADDR:
        if (nn == NULL)
            break;
        regalloc(&nod, p, nn);
        naddr(&nod1, l);
        gopcode(op, p->type, &nod1, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case OINDIR:
        if (nn == NULL) {
            nullwarn(l, NULL);
            break;
        }
        gindir(p, nn, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case ONAME:
    case OCONST:
    gname:
        if (nn == NULL)
            break;
        gmove(p, nn);
        break;

    case OCOMPOUND:
        gcpexpr(l, NULL);
        p->op = ONAME;
        p->left = NULL;
        goto gname;

    case OCOMMA:
        gexpr(l, NULL);
        gexpr(r, nn);
        break;

    default:
        error(p, "unknown op(%O) in gexpr", p->op);
        break;
    }
    
    redzone = saved_redzone;
}

static void bitload(struct node *f, struct node *t)
{
    struct node nod;
    int total, nb;
    struct type *ty;

    if (f)
        ty = f->type;
    else
        ty = t->type;
    if (ty->nbits == 0 || t->op != OREGISTER)
        return;

    if (f)
        gmove(f, t);

    total = ty->size * 8;
    nb = total - ty->bitoff - ty->nbits;
    if (nb > 0) {
        nodconst(&nod, ty, nb);
        gopcode(OSHL, ty, &nod, t);
    }
    nb = total - ty->nbits;
    if (nb > 0) {
        nodconst(&nod, ty, nb);
        if (typeu[ty->etype])
            gopcode(OUSHR, ty, &nod, t);
        else
            gopcode(OSHR, ty, &nod, t);
    }
}

static void bitstore(struct node *f, struct node *t)
{
    struct node nod, nod1, nod2;
    long mask, off, val;
    struct type *ty;

    ty = t->type;
    if (ty->nbits == 0)
        return;

    mask = MASK(ty->nbits);
    off = ty->bitoff;
    switch (f->op) {
    case OCONST:
        val = f->v.i;
        val &= mask;
        val <<= off;
        nodconst(&nod, ty, val);
        goto bstore;

    case OREGISTER:
        regalloc(&nod, t, f);
        gmove(f, &nod);
        nodconst(&nod1, ty, mask);
        gopcode(OAND, ty, &nod1, &nod);
        nodconst(&nod1, ty, off);
        gopcode(OSHL, ty, &nod1, &nod);
    
    bstore:
        regalloc(&nod2, t, t);
        gmove(t, &nod2);
        mask = ~(mask << off);
        nodconst(&nod1, ty, mask);
        gopcode(OAND, ty, &nod1, &nod2);
        gopcode(OOR, ty, &nod, &nod2);
        gmove(&nod2, t);
        regfree(&nod2);
        if (nod.op == OREGISTER)
            regfree(&nod);
        break;

    default:
        error(f, "unknown op(%O) in bitstore", f->op);
        break;
    }
}

/* COMISS/COMISD using CF, the same as unsigned comparasion. */
static int signbr(struct type *ty)
{
    if (ty == NULL)
        return 0;
    
    return !typeu[ty->etype] && !typefd[ty->etype];
}

void gtest(struct node *p, int lab, int invert)
{
    struct node nod, nod1;
    struct node *l, *r;
    int acmp, lab1, op;

    if (p == NULL || p->type == NULL)
        return;

    l = p->left;
    r = p->right;
    switch (p->op) {
    case OLT:
        op = invert ? OGE : OLT;
        goto grel;
    
    case OLE:
        op = invert ? OGT : OLE;
        goto grel;

    case OGT:
        op = invert ? OLE : OGT;
        goto grel;

    case OGE:
        op = invert ? OLT : OGE;
        goto grel;

    case OEQ:
        op = invert ? ONE : OEQ;
        goto grel;

    case ONE:
        op = invert ? OEQ : ONE;
    grel:
        switch (l->type->etype) {
        case TDOUBLE: 
            acmp = ACOMISD;
            break;
        
        case TFLOAT: 
            acmp = ACOMISS;
            break;
        
        default: 
            acmp = ACMP;
            break;
        }
        if (l->op == OREGISTER) {
            regfixalloc(&nod, l, l->reg);
        } else {
            regalloc(&nod, l, NULL);
            gexpr(l, &nod);
        }
        grexpr(r, NULL, &nod1);
        ginstr(acmp, &nod1, &nod);
        regfree(&nod);
        if (nod1.op != OCONST)
            regfree(&nod1);
        if (lab)
            gbranch(op, lab, signbr(l->type));
        break;

    case ONOT:
        gtest(l, lab, !invert);
        break;

    case OANDAND:
        if (invert) {
            gtest(l, lab, 1);
            gtest(r, lab, 1);
        } else {
            lab1 = genlab(1);
            gtest(l, lab1, 1);
            gtest(r, lab, 0);
            glabel(lab1);
        }
        break;

    case OOROR:
        if (invert) {
            lab1 = genlab(1);
            gtest(l, lab1, 0);
            gtest(r, lab, 1);
            glabel(lab1);
        } else {
            gtest(l, lab, 0);
            gtest(r, lab, 0);
        }
        break;

    default:
        /* x ==> x != 0 */
        nodconst(&nod1, p->type, 0);
        nodnew(&nod, ONE, p, &nod1);
        nod.type = types[TINT];
        gtest(&nod, lab, invert);
        break;
    }
}

static void gboolexpr(struct node *p, struct node *nn)
{
    struct node nod, nod1;
    struct node *l, *r;
    int aset, sign, lab;

    l = p->left;
    r = p->right;
    sign = signbr(l->type);

    switch (p->op) {
    case OLT:
        aset = sign ? ASETL : ASETB;
        goto gsetcc;

    case OLE:
        aset = sign ? ASETLE : ASETBE;
        goto gsetcc;

    case OGT:
        aset = sign ? ASETG : ASETA;
        goto gsetcc;

    case OGE:
        aset = sign ? ASETGE : ASETAE;
        goto gsetcc;

    case OEQ:
        aset = ASETE;
        goto gsetcc;

    case ONE:
        aset = ASETNE;
        goto gsetcc;

    case ONOT:
        aset = ASETE;
    gsetcc:
        if (nn == NULL) {
            nullwarn(l, r);
            break;
        }
        gtest(p, 0, 0);
        regalloc(&nod, p, nn);
        nod.type = types[TUCHAR];
        ginstr(aset, NULL, &nod);
        gmove(&nod, nn);
        regfree(&nod);
        break;

    case OANDAND:
        regalloc(&nod, p, nn);
        lab = genlab(2);
        gtest(p, lab, 1);
        nodconst(&nod1, p->type, 1);
        gmove(&nod1, &nod);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        nodconst(&nod1, p->type, 0);
        gmove(&nod1, &nod);
        glabel(lab + 1);
        if (nn)
            gmove(&nod, nn);
        regfree(&nod);
        break;
    
    case OOROR:
        regalloc(&nod, p, nn);
        lab = genlab(2);
        gtest(p, lab, 0);
        nodconst(&nod1, p->type, 0);
        gmove(&nod1, &nod);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        nodconst(&nod1, p->type, 1);
        gmove(&nod1, &nod);
        glabel(lab + 1);
        if (nn)
            gmove(&nod, nn);
        regfree(&nod);
        break;

    case OCOND:
        lab = genlab(2);
        gtest(l, lab, 1);
        gexpr(r->left, nn);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        gexpr(r->right, nn);
        glabel(lab + 1);
        break;
    
    default:
        error(p, "unknown op(%O) in gboolexpr", p->op);
        break;
    }
}

static void gcpexpr(struct node *p, struct node *nn)
{
    switch (p->op) {
    case OLIST:
        gcpexpr(p->left, NULL);
        gcpexpr(p->right, nn);
        break;

    default:
        gexpr(p, nn);
        break;
    }
}

static void gsuexpr(struct node *p, struct node *nn)
{
    struct node nod, nod1;
    struct node *l, *r;
    int lab;

    if (p == NULL || p->type == NULL)
        return;

    l = p->left;
    r = p->right;

    switch (p->op) {
    case OAS:
        glexpr(l, &nod1);
        regialloc(&nod, r, nn);
        gexpr(r, &nod);
        gmemcopy(&nod, &nod1);
        if (nn)
            *nn = nod;
        regfree(&nod);
        regfree(&nod1);
        break;

    case OCOND:
        if (nn == NULL) {
            gboolexpr(p, NULL);
            break;
        }
        regialloc(&nod, p, nn);
        nod1 = nod;
        lab = genlab(2);
        gtest(l, lab, 1);
        gexpr(r->left, &nod);
        ginstr(ALEA, &nod, nn);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        gexpr(r->right, &nod1);
        ginstr(ALEA, &nod1, nn);
        glabel(lab + 1);
        regind(nn, p->type);
        regfree(&nod);
        break;

    case ODOT:
        gsuexpr(l, nn);
        if (nn == NULL)
            break;
        if (r == NULL || r->op != OCONST) {
            error(r, "bad dot tree, missing constant");
            break;
        }
        nn->offset += r->v.i;
        nn->type = p->type;
        break;

    case OFUNC:
        gcallexpr(p, nn);
        break;

    case OINDIR:
        if (nn == NULL) {
            nullwarn(l, NULL);
            break;
        }
        regialloc(&nod, l, nn);
        gexpr(l, &nod);
        gmove(&nod, nn);
        regind(nn, p->type);
        regfree(&nod);
        break;

    case ONAME:
    gname:
        if (nn == NULL)
            break;
        regialloc(&nod, p, nn);
        naddr(&nod1, p);
        gopcode(OADDR, p->type, &nod1, &nod);
        gmove(&nod, nn);
        regind(nn, p->type);
        regfree(&nod);
        break;

    case OCOMPOUND:
        gcpexpr(l, NULL);
        p->op = ONAME;
        p->left = NULL;
        goto gname;

    default:
        error(p, "unknown op(%O) in gsuexpr", p->op);
        break;
    }
}

/* compile struct tree to registers */
static void gsureg(struct node *p, struct node *r1, struct node *r2)
{
    struct node nod, nod1;
    long bytes;

    bytes = p->type->size;
    if (bytes <= 0 || bytes > 16) {
        error(p, "bad structure for registers");
        return;
    }

    if (p->op == ONAME) {
        naddr(&nod1, p);
        regs[nod1.reg]++;
    } else {
        regialloc(&nod1, p, NULL);
        gexpr(p, &nod1);
    }
    nod1.type = types[TLONG];
    nod = *r1;
    nod.type = types[TLONG];

    if (SSE_P(nod.reg))
        ginstr(AMOVQ, &nod1, &nod);
    else
        ginstr(AMOV, &nod1, &nod);

    if (bytes > 8) {
        nod1.offset += 8;
        nod = *r2;
        nod.type = types[TLONG];
        if (SSE_P(nod.reg))
            ginstr(AMOVQ, &nod1, &nod);
        else
            ginstr(AMOV, &nod1, &nod);
    }
    regfree(&nod1);
}

static void gcallret(struct node *p, struct node *nn, long o)
{
    struct node nod, nod1, nod2;
    struct type *ty;

    if (nn == NULL)
        return;

    ty = p->type->link;
    switch (ty->etype) {
    case TBOOL:
    case TCHAR:
    case TUCHAR:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TUINT:
    case TLONG:
    case TULONG:
    case TLLONG:
    case TULLONG:
    case TPTR:
    case TENUM:
        regfixalloc(&nod1, p, RAX);
        gmove(&nod1, nn);
        regfree(&nod1);
        break;

    case TFLOAT:
    case TDOUBLE:
        regfixalloc(&nod1, p, XMM0);
        gmove(&nod1, nn);
        regfree(&nod1);
        break;

    case TSTRUCT:
    case TUNION:
        nodauto(&nod2, types[TLONG], o);
        naddr(&nod1, &nod2);
        switch (classify(ty)) {
        case TINT:
            regfixalloc(&nod, p, RAX);
            ginstr(AMOV, &nod, &nod1);
            regfree(&nod);
            break;

        case TFLOAT:
            regfixalloc(&nod, p, XMM0);
            ginstr(AMOVQ, &nod, &nod1);
            regfree(&nod);
            break;

        case ARG_CLASS(TINT, TINT):
            regfixalloc(&nod, p, RAX);
            ginstr(AMOV, &nod, &nod1);
            regfree(&nod);
            regfixalloc(&nod, p, RDX);
            nod1.offset += 8;
            ginstr(AMOV, &nod, &nod1);
            regfree(&nod);
            break;

        case ARG_CLASS(TINT, TFLOAT):
            regfixalloc(&nod, p, XMM0);
            ginstr(AMOVQ, &nod, &nod1);
            regfree(&nod);
            regfixalloc(&nod, p, RAX);
            nod1.offset += 8;
            ginstr(AMOV, &nod, &nod1);
            regfree(&nod);
            break;

        case ARG_CLASS(TFLOAT, TINT):
            regfixalloc(&nod, p, RAX);
            ginstr(AMOV, &nod, &nod1);
            regfree(&nod);
            regfixalloc(&nod, p, XMM0);
            nod1.offset += 8;
            ginstr(AMOVQ, &nod, &nod1);
            regfree(&nod);
            break;

        case ARG_CLASS(TFLOAT, TFLOAT):
            regfixalloc(&nod, p, XMM0);
            ginstr(AMOVQ, &nod, &nod1);
            regfree(&nod);
            regfixalloc(&nod, p, XMM0 + 1);
            nod1.offset += 8;
            ginstr(AMOVQ, &nod, &nod1);
            regfree(&nod);
            break;
        }
        nod1.offset = o;
        ginstr(ALEA, &nod1, nn);
        regind(nn, ty);
        break;

    default:
        abort();
    }
}

static void gcallexpr(struct node *p, struct node *nn)
{
    struct node nods[NREG];
    struct node *l, *r, *a, *n;
    struct node nod, nod1, nod2, nod3;
    struct type *ty;
    int saved_regs[NREG];
    long saved_redzone, call_redzone;
    long aoffset, retoff;
    int i, nfd, vdc;

    l = p->left;
    /* handle builtin calls */
    if (l->op == ONAME && (i = l->sym->builtin_id)) {
        if (btfunc[i])
            btfunc[i](p, nn);
        return;
    }

    ty = l->type;
    saved_redzone = redzone;
    aoffset = 0;
    retoff = 0;
    nfd = 0;
    vdc = variadic(ty);

    /* spill all caller registers */
    for (i = 0; i < NREG; i++) {
        n = &nods[i];
        n->offset = 0;
        saved_regs[i] = regs[i];
        if (regs[i] <= 0)
            continue;
        if ((i < RAX || i > R11) && (i < XMM0 || i > XMM15))
            continue;
        if (nodreg(&nod, nn, i))
            gspill(&nod, n);
        regs[i] = 0;
    }

    /* gen left tree first */
    switch (l->op) {
    case ONAME:
        naddr(&nod, l);
        regs[nod.reg]++;
        break;

    case OINDIR:
        /* indirect call, using r11 simply */
        regfixalloc(&nod, l->left, R11);
        gexpr(l->left, &nod);
        break;

    default:
        abort();
    }

    classify_args(ty, p->right, NULL, &aoffset);
    if (typesu[ty->link->etype]) {
        retoff = aoffset;
        aoffset += ROUNDUP(ty->link->size, 8);
    }

    for (r = p->right; r; r = r->right) {
        a = r->left;
        if (a->areg1 == 0)
            continue;
        if (a->op == ONAME || a->op == OCONST || a->op == OADDR)
            continue;
        /* save it as tempory to prevent run out of registers */
        a->aoffset = aoffset;
        aoffset += a->type->size;
    }
    
    /* must be 16-byte aligned */
    redzone = call_redzone = ROUNDUP(redzone + aoffset, 16);
    aoffset = -(stkoffset + redzone);
    
    /* first pass: gen args */
    for (r = p->right; r; r = r->right) {
        a = r->left;
        if (a->type->size < 1)
            continue;
        if (a->areg1)
            if (a->op == ONAME || a->op == OCONST || a->op == OADDR)
                continue;
        nodauto(&nod1, a->type, a->aoffset + aoffset);
        nodnew(&nod2, OAS, &nod1, a);
        nod2.type = a->type;
        gexpr(&nod2, NULL);
    }

    if (classify(ty->link) == TSTRUCT) {
        regfixalloc(&nod1, p, RDI);
        nodauto(&nod2, ty->link, aoffset + retoff);
        naddr(&nod3, &nod2);
        ginstr(ALEA, &nod3, &nod1);
    }
    /* second pass: simply move to regs */
    for (r = p->right; r; r = r->right) {
        a = r->left;
        if (a->areg1 == 0)
            continue;
        if (vdc) {
            if (SSE_P(a->areg1))
                nfd++;
            if (SSE_P(a->areg2))
                nfd++;
        }
        if (a->op == OCONST || a->op == OADDR) {
            regfixalloc(&nod1, a, a->areg1);
            gexpr(a, &nod1);
            continue;
        }
        if (a->op == ONAME)
            nod1 = *a;
        else
            nodauto(&nod1, a->type, a->aoffset + aoffset);
        if (typesu[a->type->etype]) {
            regfixalloc(&nod2, a, a->areg1);
            if (a->areg2) {
                regfixalloc(&nod3, a, a->areg2);
                gsureg(&nod1, &nod2, &nod3);
            } else {
                gsureg(&nod1, &nod2, NULL);
            }
        } else {
            regfixalloc(&nod2, a, a->areg1);
            gmove(&nod1, &nod2);
        }
    }

    /* gen call */
    assert(redzone == call_redzone && "redzone corrupted");
    if (redzone) {
        nodconst(&nod1, types[TLONG], redzone);
        nodreg(&nod2, &nod1, RSP);
        ginstr(ASUB, &nod1, &nod2);
    }

    /* rax = number of float registers used */
    if (vdc) {
        regfixalloc(&nod1, p, RAX);
        nod1.type = types[TCHAR];
        nodconst(&nod2, types[TCHAR], nfd);
        ginstr(AMOV, &nod2, &nod1);
    }
    gopcode(p->op, ty, NULL, &nod);
    /* free above regiters */
    for (i = 0; i < NREG; i++)
        if ((i >= RAX && i <= R11) || (i >= XMM0 && i <= XMM15))
            if (nodreg(&nod, NULL, i))
                regfree(&nod);

    /* copy return value */
    if (nn)
        gcallret(l, nn, aoffset + retoff);

    if (redzone) {
        nodconst(&nod1, types[TLONG], redzone);
        nodreg(&nod2, &nod1, RSP);
        ginstr(AADD, &nod1, &nod2);
    }

    /* reload all caller registers */
    for (i = 0; i < NREG; i++) {
        n = &nods[i];
        if (n->offset) {
            nodreg(&nod, n, i);
            greload(n, &nod);
        }
        regs[i] = saved_regs[i];
    }
    
    redzone = saved_redzone;
}

void greturn(struct node *p)
{
    struct node nod, nod1;

    if (p == NULL || p->type == NULL)
        return;

    switch (p->type->etype) {
    case TBOOL:
    case TCHAR:
    case TUCHAR:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TUINT:
    case TLONG:
    case TULONG:
    case TLLONG:
    case TULLONG:
    case TPTR:
    case TENUM:
        regfixalloc(&nod, p, RAX);
        gexpr(p, &nod);
        regfree(&nod);
        break;

    case TFLOAT:
    case TDOUBLE:
        regfixalloc(&nod, p, XMM0);
        gexpr(p, &nod);
        regfree(&nod);
        break;

    case TSTRUCT:
    case TUNION:
        switch (classify(p->type)) {
        case TINT:
            regfixalloc(&nod, p, RAX);
            gsureg(p, &nod, NULL);
            regfree(&nod);
            break;

        case TFLOAT:
            regfixalloc(&nod, p, XMM0);
            gsureg(p, &nod, NULL);
            regfree(&nod);
            break;

        case ARG_CLASS(TINT, TINT):
            regfixalloc(&nod, p, RAX);
            regfixalloc(&nod1, p, RDX);
            gsureg(p, &nod, &nod1);
            regfree(&nod);
            regfree(&nod1);
            break;

        case ARG_CLASS(TINT, TFLOAT):
            regfixalloc(&nod, p, XMM0);
            regfixalloc(&nod1, p, RAX);
            gsureg(p, &nod, &nod1);
            regfree(&nod);
            regfree(&nod1);
            break;

        case ARG_CLASS(TFLOAT, TINT):
            regfixalloc(&nod, p, RAX);
            regfixalloc(&nod1, p, XMM0);
            gsureg(p, &nod, &nod1);
            regfree(&nod);
            regfree(&nod1);
            break;

        case ARG_CLASS(TFLOAT, TFLOAT):
            regfixalloc(&nod, p, XMM0);
            regfixalloc(&nod1, p, XMM0 + 1);
            gsureg(p, &nod, &nod1);
            regfree(&nod);
            regfree(&nod1);
            break;

        case TSTRUCT:
            /* first imlpicit argument in RDI, return it in RAX */
            regfixalloc(&nod, p, RAX);
            nodauto(&nod1, types[TPTR], suretoff);
            gmove(&nod1, &nod);
            regind(&nod, p->type);
            regialloc(&nod1, p, NULL);
            gexpr(p, &nod1);
            gmemcopy(&nod1, &nod);
            regfree(&nod);
            regfree(&nod1);
            break;

        default:
            abort();
        }
        break;

    default:
        error(p, "bad return type '%T'", p->type);
        break;
    }
}

static void gzeroexpr(struct node *p, struct node *nn)
{
    struct node nod, nod1, nod2, nod3, nod4;
    long bytes;

    bytes = p->right->v.i;
    if (bytes <= 0)
        return;

    naddr(&nod1, p->left);
    /* REP STOS: must use RCX, RAX, RDI */
    if (bytes >= 64)
        if (nodreg(&nod2, p, RDI) == 0 &&
            nodreg(&nod3, p, RAX) == 0 &&
            nodreg(&nod4, p, RCX) == 0) {
            nod2.type = types[TLONG];
            nod3.type = types[TLONG];
            nod4.type = types[TLONG];
            /* RAX ==> ES:(RDI), RCX counts */
            ginstr(ALEA, &nod1, &nod2);
            nodconst(&nod, types[TLONG], 0);
            ginstr(AMOV, &nod, &nod3);
            nodconst(&nod, types[TLONG], bytes/8);
            ginstr(AMOV, &nod, &nod4);
            nod1 = nod2;
            regind(&nod1, types[TLONG]);
            ginstr(AREPSTOS, NULL, &nod1);
            /* left bytes, fall thru */
            bytes %= 8;
        }
    
    nodconst(&nod, types[TLONG], 0);
    while (bytes >= 8) {
        nod.type = types[TLONG];
        nod1.type = types[TLONG];
        ginstr(AMOV, &nod, &nod1);
        bytes -= 8;
        nod1.offset += 8;
    }
    if (bytes >= 4) {
        nod.type = types[TINT];
        nod1.type = types[TINT];
        ginstr(AMOV, &nod, &nod1);
        bytes -= 4;
        nod1.offset += 4;
    }
    if (bytes >= 2) {
        nod.type = types[TSHORT];
        nod1.type = types[TSHORT];
        ginstr(AMOV, &nod, &nod1);
        bytes -= 2;
        nod1.offset += 2;
    }
    if (bytes == 1) {
        nod.type = types[TCHAR];
        nod1.type = types[TCHAR];
        ginstr(AMOV, &nod, &nod1);
    }
}

static void gmemcopy(struct node *f, struct node *t)
{
    struct node nod, nod1, nod2;
    long bytes;
    
    bytes = f->type->size;
    if (bytes <= 0)
        return;

    regialloc(&nod, f, NULL);
    nod1 = *f;
    nod2 = *t;
    while (bytes >= 8) {
        nod1.type = types[TLONG];
        nod2.type = types[TLONG];
        nod.type = types[TLONG];
        ginstr(AMOV, &nod1, &nod);
        ginstr(AMOV, &nod, &nod2);
        bytes -= 8;
        nod1.offset += 8;
        nod2.offset += 8;
    }
    if (bytes >= 4) {
        nod1.type = types[TINT];
        nod2.type = types[TINT];
        nod.type = types[TINT];
        ginstr(AMOV, &nod1, &nod);
        ginstr(AMOV, &nod, &nod2);
        bytes -= 4;
        nod1.offset += 4;
        nod2.offset += 4;
    }
    if (bytes >= 2) {
        nod1.type = types[TSHORT];
        nod2.type = types[TSHORT];
        nod.type = types[TSHORT];
        ginstr(AMOV, &nod1, &nod);
        ginstr(AMOV, &nod, &nod2);
        bytes -= 2;
        nod1.offset += 2;
        nod2.offset += 2;
    }
    if (bytes == 1) {
        nod1.type = types[TCHAR];
        nod2.type = types[TCHAR];
        nod.type = types[TCHAR];
        ginstr(AMOV, &nod1, &nod);
        ginstr(AMOV, &nod, &nod2);
    }
    regfree(&nod);
}

static void naddr(struct node *dst, struct node *p)
{
    switch (p->op) {
    case ONAME:
        if (p->sclass == CAUTO || p->sclass == CPARAM) {
            nodreg(dst, p, RBP);
            regind(dst, p->type);
            dst->offset = p->offset;
        } else {
            /* global name */
            nodreg(dst, p, RIP);
            regind(dst, p->type);
            dst->offset = p->offset;    /* string or constant has offset */
            dst->sym = p->sym;
        }
        break;

    case OINDREG:
        *dst = *p;
        break;

    default:
        abort();
    }
}

/* spill whole register to a node */
static void gspill(struct node *f, struct node *t)
{
    struct node nod = *f;
    redzone = ROUNDUP(redzone + typsize[TLONG], 8);
    if (SSE_P(nod.reg)) {
        nod.type = types[TDOUBLE];
        nodauto(t, types[TDOUBLE], -(stkoffset + redzone));
    } else {
        nod.type = types[TLLONG];
        nodauto(t, types[TLONG], -(stkoffset + redzone));
    }
    gmove(&nod, t);
    if (options.debug)
        print("spill register %d to %ld\n", nod.reg, t->offset);
}

/* reload whole register from a node */
static void greload(struct node *f, struct node *t)
{
    struct node nod = *t;

    nod.type = f->type;
    gmove(f, &nod);
    if (options.debug)
        print("reload register %d from %ld\n", nod.reg, f->offset);
}

void regfree(struct node *t)
{
    int i, j;

    if (t->op != OREGISTER && t->op != OINDREG)
        goto err;
    i = t->reg;
    j = 1;
again:
    if (i <= 0 || i >= NELEMS(regs))
        goto err;
    if (regs[i] <= 0)
        goto err;
    
    regs[i]--;
    if (j && t->op == OINDREG && t->index) {
        i = t->index;
        j = 0;
        goto again;
    }
    return;
err:
    error(t, "error in regfree: op=%O,%d", t->op);
}

void regalloc(struct node *t, struct node *p, struct node *nn)
{
    int i;

    switch (p->type->etype) {
    case TBOOL:
    case TCHAR:
    case TUCHAR:
    case TSHORT:
    case TUSHORT:
    case TINT:
    case TUINT:
    case TLONG:
    case TULONG:
    case TLLONG:
    case TULLONG:
    case TPTR:
    case TENUM:
        if (nn && nn->op == OREGISTER) {
            i = nn->reg;
            if (i >= RAX && i <= R11)
                goto out;
        }
        for (i = RAX; i <= R11; i++)
            if (regs[i] == 0)
                goto out;
        break;

    case TFLOAT:
    case TDOUBLE:
        if (nn && nn->op == OREGISTER) {
            i = nn->reg;
            if (i >= XMM0 && i <= XMM15)
                goto out;
        }
        for (i = XMM0; i <= XMM15; i++)
            if (regs[i] == 0)
                goto out;
        break;

    default:
        error(p, "invalid type '%T' in regalloc", p->type);
        goto err;
    }

    error(p, "registers run out");
err:
    i = 0;
out:
    if (i)
        regs[i]++;
    nodreg(t, p, i);
}

static void regialloc(struct node *t, struct node *p, struct node *nn)
{
    struct node nod;

    nod = *p;
    nod.type = types[TPTR];
    regalloc(t, &nod, nn);
}

static void regfixalloc(struct node *t, struct node *p, int reg)
{
    struct node nod;

    nod = *p;
    switch (p->type->etype) {
    case TSTRUCT:
    case TUNION:
        if (SSE_P(reg))
            nod.type = types[TDOUBLE];
        else
            nod.type = types[TLONG];
        break;
    
    case TFUNC:
    case TARRAY:
        nod.type = types[TPTR];
        break;
    }
    if (nodreg(t, &nod, reg))
        error(t, "register %d busy in fixed alloc", reg);
    regs[reg]++;
}

static int movself(struct node *f, struct node *t)
{
    return f->op == OREGISTER && t->op == OREGISTER && f->reg == t->reg;
}

static void gmove(struct node *f, struct node *t)
{
    struct node nod, nod1;
    int ft, tt, a, t64, nt;

    assert(f && t);

    ft = f->type->etype;
    if (ft == TENUM)
        ft = f->type->link->etype;
    if (ft == TBOOL)
        ft = TUCHAR;
    tt = t->type->etype;
    if (tt == TENUM)
        tt = t->type->link->etype;
    if (tt == TBOOL)
        tt = TUCHAR;
    t64 = tt == TLONG || tt == TULONG || tt == TLLONG || tt == TULLONG;

    /* SSE instructions do NOT support imm operand */
    if (f->op == OCONST)
        if (ft == TFLOAT || ft == TDOUBLE) {
            f->offset = gfloat(f->type, f->v.d);
            f->op = ONAME;
            f->sclass = CSTATIC;
        }

    /* load */
    if (f->op == ONAME || f->op == OINDREG)
        switch (ft) {
        case TCHAR:
        case TSHORT:
            a = AMOVSX;
            if (t64)
                nt = TLONG;
            else
                nt = TINT;
            goto load;

        case TUCHAR:
        case TUSHORT:
            a = AMOVZX;
            if (t64)
                nt = TLONG;
            else
                nt = TINT;
            goto load;

        case TINT:
            if (t64) {
                a = AMOVSX;
                nt = TLONG;
            } else {
                a = AMOV;
                nt = TINT;
            }
            goto load;

        case TUINT: /* u32 to int64 does nothing */
            a = AMOV;
            nt = TINT;
            goto load;

        case TLONG:
        case TLLONG:
        case TULONG:
        case TULLONG:
        case TPTR:
            a = AMOV;
            nt = TLONG;
            goto load;

        case TFLOAT:
            if (tt == TDOUBLE) {
                a = ACVTSS2SD;
                nt = TDOUBLE;
            } else {
                a = AMOVSS;
                nt = TFLOAT;
            }
            goto load;

        case TDOUBLE:
            if (tt == TFLOAT) {
                a = ACVTSD2SS;
                nt = TFLOAT;
            } else {
                a = AMOVSD;
                nt = TDOUBLE;
            }
        
        load:
            naddr(&nod1, f);
            regalloc(&nod, f, t);
            nod.type = types[nt];
            ginstr(a, &nod1, &nod);
            gmove(&nod, t);
            regfree(&nod);
            return;
        }

    /* store */
    if (t->op == ONAME || t->op == OINDREG)
        switch (tt) {
        case TCHAR:
        case TUCHAR:
        case TSHORT:
        case TUSHORT:
        case TINT:
        case TUINT:
        case TLONG:
        case TULONG:
        case TLLONG:
        case TULLONG:
        case TPTR:
            if (f->op == OCONST)
                if (f->v.i < INT32_MIN || f->v.i > INT32_MAX) {
                    regalloc(&nod, f, NULL);
                    ginstr(AMOV, f, &nod);
                    gmove(&nod, t);
                    regfree(&nod);
                    return;
                }
            a = AMOV;
            goto store;

        case TFLOAT:
            a = AMOVSS;
            goto store;

        case TDOUBLE:
            a = AMOVSD;

        store:
            naddr(&nod1, t);
            if (f->op == OCONST) {
                ginstr(a, f, &nod1);
                return;
            }
            regalloc(&nod, t, f);
            gmove(f, &nod);
            ginstr(a, &nod, &nod1);
            regfree(&nod);
            return;
        }

#define MOV(f, t)  (((f) << 16) | ((t) & 0xFF))

    /* move or cast */
    if (t->op == OREGISTER) {
        switch (MOV(ft, tt)) {
        case MOV(TCHAR, TINT):
        case MOV(TCHAR, TUINT):
        case MOV(TCHAR, TLONG):
        case MOV(TCHAR, TULONG):
        case MOV(TCHAR, TLLONG):
        case MOV(TCHAR, TULLONG):
        case MOV(TCHAR, TPTR):
        case MOV(TSHORT, TINT):
        case MOV(TSHORT, TUINT):
        case MOV(TSHORT, TLONG):
        case MOV(TSHORT, TULONG):
        case MOV(TSHORT, TLLONG):
        case MOV(TSHORT, TULLONG):
        case MOV(TSHORT, TPTR):
        case MOV(TINT, TLONG):
        case MOV(TINT, TULONG):
        case MOV(TINT, TLLONG):
        case MOV(TINT, TULLONG):
        case MOV(TINT, TPTR):
            a = AMOVSX;
            break;

        case MOV(TUCHAR, TINT):
        case MOV(TUCHAR, TUINT):
        case MOV(TUCHAR, TLONG):
        case MOV(TUCHAR, TULONG):
        case MOV(TUCHAR, TLLONG):
        case MOV(TUCHAR, TULLONG):
        case MOV(TUCHAR, TPTR):
        case MOV(TUSHORT, TINT):
        case MOV(TUSHORT, TUINT):
        case MOV(TUSHORT, TLONG):
        case MOV(TUSHORT, TULONG):
        case MOV(TUSHORT, TLLONG):
        case MOV(TUSHORT, TULLONG):
        case MOV(TUSHORT, TPTR):
            a = AMOVZX;
            break;

        case MOV(TINT, TFLOAT):
        case MOV(TUINT, TFLOAT):
        case MOV(TLONG, TFLOAT):
        case MOV(TULONG, TFLOAT):
        case MOV(TLLONG, TFLOAT):
        case MOV(TULLONG, TFLOAT):
            a = ACVTSI2SS;
            break;

        case MOV(TINT, TDOUBLE):
        case MOV(TUINT, TDOUBLE):
        case MOV(TLONG, TDOUBLE):
        case MOV(TULONG, TDOUBLE):
        case MOV(TLLONG, TDOUBLE):
        case MOV(TULLONG, TDOUBLE):
            a = ACVTSI2SD;
            break;

        case MOV(TFLOAT, TINT):
        case MOV(TFLOAT, TUINT):
        case MOV(TFLOAT, TLONG):
        case MOV(TFLOAT, TULONG):
        case MOV(TFLOAT, TLLONG):
        case MOV(TFLOAT, TULLONG):
            a = ACVTSS2SI;
            break;

        case MOV(TDOUBLE, TINT):
        case MOV(TDOUBLE, TUINT):
        case MOV(TDOUBLE, TLONG):
        case MOV(TDOUBLE, TULONG):
        case MOV(TDOUBLE, TLLONG):
        case MOV(TDOUBLE, TULLONG):
            a = ACVTSD2SI;
            break;

        case MOV(TFLOAT, TFLOAT):
            a = AMOVSS;
            break;

        case MOV(TFLOAT, TDOUBLE):
            a = ACVTSS2SD;
            break;

        case MOV(TDOUBLE, TFLOAT):
            a = ACVTSD2SS;
            break;

        case MOV(TDOUBLE, TDOUBLE):
            a = AMOVSD;
            break;

        /* misc */
        case MOV(TFLOAT, TCHAR):
        case MOV(TFLOAT, TUCHAR):
        case MOV(TFLOAT, TSHORT):
        case MOV(TFLOAT, TUSHORT):
        case MOV(TDOUBLE, TCHAR):
        case MOV(TDOUBLE, TUCHAR):
        case MOV(TDOUBLE, TSHORT):
        case MOV(TDOUBLE, TUSHORT):
            regalloc(&nod, t, t);
            nod.type = types[TINT];
            gmove(f, &nod);
            gmove(&nod, t);
            regfree(&nod);
            return;

        case MOV(TCHAR, TSHORT):
        case MOV(TCHAR, TUSHORT):
        case MOV(TUCHAR, TSHORT):
        case MOV(TUCHAR, TUSHORT):
        case MOV(TCHAR, TFLOAT):
        case MOV(TUCHAR, TFLOAT):
        case MOV(TSHORT, TFLOAT):
        case MOV(TUSHORT, TFLOAT):
        case MOV(TCHAR, TDOUBLE):
        case MOV(TUCHAR, TDOUBLE):
        case MOV(TSHORT, TDOUBLE):
        case MOV(TUSHORT, TDOUBLE):
            regalloc(&nod, f, f);
            nod.type = types[TINT];
            gmove(f, &nod);
            gmove(&nod, t);
            regfree(&nod);
            return;

        default:
            /* truncate or mov uint32 => int64 */
            a = AMOV;
            break;
        }
        if (a == AMOV || a == AMOVSS || a == AMOVSD)
            if (movself(f, t)) {
                if (typsize[ft] >= typsize[tt])
                    return;
                if (ft == TUINT && typsize[ft] < typsize[tt])
                    return;
            }
        if (a == AMOV) {
            struct type* saved_ty;
            /* special case: uint32 => int64 */
            if (ft == TUINT && typsize[ft] < typsize[tt]) {
                saved_ty = t->type;
                t->type = types[TUINT];
                ginstr(a, f, t);
                t->type = saved_ty;
                return;
            }
            /* truncate */
            if (typsize[ft] > typsize[tt]) {
                saved_ty = f->type;
                f->type = t->type;
                ginstr(a, f, t);
                f->type = saved_ty;
                return;
            }
        }
        ginstr(a, f, t);
        return;
    }

    error(f, "unknown mov(%O, %O)", f->op, t->op);
#undef MOV
}

static void freejmp(struct jmp *p)
{
    if (p) {
        p->link = freejmplist;
        freejmplist = p;
    }
}

static struct jmp *newjmp(int lab)
{
    struct jmp *p;

    if (freejmplist) {
        p = freejmplist;
        freejmplist = freejmplist->link;
    } else {
        p = xmalloc(sizeof *p);
    }

    p->lab = lab;
    p->off = 0;
    p->jmps = NULL;
    p->link = NULL;
    return p;
}

static struct jmp *findlab(int lab)
{
    struct jmp *p;

    for (p = jmplist; p; p = p->link)
        if (p->lab == lab)
            return p;

    p = newjmp(lab);
    p->link = jmplist;
    jmplist = p;
    return p;
}

static int findjmp(int lab, long reloc)
{
    struct jmp *p, *q;

    p = findlab(lab);
    if (p->off > 0)
        return p->off;

    q = newjmp(lab);
    q->off = reloc;
    q->link = p->jmps;
    p->jmps = q;
    return 0;
}

static void solvejmp(struct jmp *p)
{
    struct jmp *q, *next;

    for (q = p->jmps; q;) {
        next = q->link;
        *(int *)(text_sec->data + q->off) = p->off - q->off - 4;
        freejmp(q);
        q = next;
    }
    p->jmps = NULL;
}

static void solveall(void)
{
    struct jmp *p, *next;

    for (p = jmplist; p;) {
        next = p->link;
        if (p->off > 0)
            solvejmp(p);
        else
            error(0, "unsolved label %d", p->lab);
        freejmp(p);
        p = next;
    }
    jmplist = NULL;
}

void glabel(int lab)
{
    struct jmp *p;

    if (options.debug)
        print("L%d:\n", lab);
    p = findlab(lab);
    p->off = text_sec->data_len;
    solvejmp(p);
}

void gbranch(int op, int lab, int sign)
{
    struct node nod;
    int a;

    if (options.debug)
        print("%O L%d\n", op, lab);

    switch (op) {
    case OGOTO:
        a = AJMP;
        goto jcc;

    case OLT:
        a = sign ? AJL : AJB;
        goto jcc;

    case OLE:
        a = sign ? AJLE : AJBE;
        goto jcc;

    case OGT:
        a = sign ? AJG : AJA;
        goto jcc;

    case OGE:
        a = sign ? AJGE : AJAE;
        goto jcc;

    case OEQ:
        a = AJE;
        goto jcc;

    case ONE:
        a = AJNE;
        goto jcc;

    case ORETURN:
        a = AJMP;
        if (retlab == 0)
            retlab = genlab(1);
        lab = retlab;

    jcc:
        nodreg(&nod, NULL, RIP);
        regind(&nod, types[TINT]);
        nod.v.i = lab;
        ginstr(a, NULL, &nod);
        break;
    }
}

static void gvalist(struct node *p, struct node *dst)
{
    if (dst == NULL)
        return;

    if (p->op == OADDR) {
        naddr(dst, p->left);
        regs[dst->reg]++;
    } else {
        regalloc(dst, p, NULL);
        gexpr(p, dst);
        regind(dst, p->type->link);
    }
}

static void gva_start(struct node *p, struct node *nn)
{
    struct node nod, nod1, nod2;
    struct node *a;
    struct symbol *sym;
    struct type *ty, *flist;
    long offset, o;

    a = p->right->left;
    flist = a->type->link->link;
    gvalist(a, &nod);
    offset = nod.offset;
    /* ap->gp_offset = vaindx * 8; */
    sym = lookup("gp_offset", OPT_SEARCH);
    ty = find_field(sym, flist, &o);
    nod.offset = offset + o;
    nod.type = ty;
    nodconst(&nod1, ty, vaindx * 8);
    ginstr(AMOV, &nod1, &nod);    

    /* ap->fp_offset = 48 + vafndx * 16; */
    sym = lookup("fp_offset", OPT_SEARCH);
    ty = find_field(sym, flist, &o);
    nod.offset = offset + o;
    nod.type = ty;
    nodconst(&nod1, ty, 48 + vafndx * 16);
    ginstr(AMOV, &nod1, &nod);
    
    /* ap->overflow_arg_area = 16(%rbp); */
    sym = lookup("overflow_arg_area", OPT_SEARCH);
    ty = find_field(sym, flist, &o);
    nod.offset = offset + o;
    nod.type = ty;
    nodreg(&nod1, p, RBP);
    regind(&nod1, types[TCHAR]);
    nod1.offset = 16;
    regalloc(&nod2, &nod, NULL);
    ginstr(ALEA, &nod1, &nod2);
    ginstr(AMOV, &nod2, &nod);

    /* ap->reg_save_area = reg_save_area; */
    sym = lookup("reg_save_area", OPT_SEARCH);
    ty = find_field(sym, flist, &o);
    nod.offset = offset + o;
    nod.type = ty;
    nodreg(&nod1, p, RBP);
    regind(&nod1, types[TCHAR]);
    nod1.offset = reg_save_area;
    ginstr(ALEA, &nod1, &nod2);
    ginstr(AMOV, &nod2, &nod);

    regfree(&nod);
    regfree(&nod2);
}

static void gva_copy(struct node *p, struct node *nn)
{
    struct node nod, nod1, nod2;
    struct node *a1, *a2;

    /* va_copy(ap1, ap2) ==> ap1[0] = ap2[0] */
    a1 = p->right->left;
    a2 = p->right->right->left;
    nodnew(&nod1, OINDIR, a1, NULL);
    nod1.type = a1->type->link;
    nodnew(&nod2, OINDIR, a2, NULL);
    nod2.type = a2->type->link;
    nodnew(&nod, OAS, &nod1, &nod2);
    nod.type = a2->type->link;
    gexpr(&nod, NULL);
}

static void gva_arg(struct node *p, struct node *nn)
{
    struct node nod, nod1, nod2, nod3, nod4, nod5;
    struct node *a;
    struct symbol *sym;
    struct type *ty, *flist;
    long offset, o;
    int lab, c1, c2, c3;
    const char *name;

    a = p->right->left;
    flist = a->type->link->link;
    gvalist(a, &nod);
    offset = nod.offset;
    ty = p->type->link;
    switch (classify(ty)) {
    case TINT:
        name = "gp_offset";
        c1 = 8;
        c2 = 8;
        c3 = 48 - c1;       /* 48 - 1*8 */
        goto single;

    case TFLOAT:
        name = "fp_offset";
        c1 = 16;
        c2 = 8;
        c3 = 176 - c1;      /* 176 - 1*16 */
        goto single;

    case ARG_CLASS(TINT, TINT):
        name = "gp_offset";
        c1 = 16;
        c2 = 16;
        c3 = 48 - c1;       /* 48 - 2*8 */
        goto single;

    case ARG_CLASS(TFLOAT, TFLOAT):
        name = "fp_offset";
        c1 = 32;
        c2 = 16;
        c3 = 176 - c1;      /* 176 - 2*16 */
        /* 
         * if (ap->gp_offset <= c3) {
         *   addr = ap->reg_save_area + ap->gp_offset;
         *   ap->gp_offset += c1;
         * } else {
         *   addr = ap->overflow_arg_area;
         *   ap->overflow_arg_area += c2;
         * }
         */
    single:
        lab = genlab(2);
        sym = lookup(name, OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod.offset = offset + o;
        nod.type = ty;
        nodconst(&nod1, ty, c3);
        regalloc(&nod2, &nod, NULL);
        gmove(&nod, &nod2);
        ginstr(ACMP, &nod1, &nod2);
        gbranch(OGT, lab, signbr(ty));
        if (nn) {
            sym = lookup("reg_save_area", OPT_SEARCH);
            ty = find_field(sym, flist, &o);
            nod1 = nod;
            nod1.offset = offset + o;
            nod1.type = ty;
            regalloc(&nod3, &nod1, nn);
            gmove(&nod2, &nod3);
            ginstr(AADD, &nod1, &nod3);
            gmove(&nod3, nn);
            regfree(&nod3);
        }
        nodconst(&nod1, nod.type, c1);
        ginstr(AADD, &nod1, &nod);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        sym = lookup("overflow_arg_area", OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod.offset = offset + o;
        nod.type = ty;
        if (nn) {
            regalloc(&nod3, &nod, nn);
            gmove(&nod, &nod3);
            gmove(&nod3, nn);
            regfree(&nod3);
        }
        nodconst(&nod1, nod.type, c2);
        ginstr(AADD, &nod1, &nod);
        glabel(lab + 1);
        regfree(&nod2);
        break;

    case ARG_CLASS(TINT, TFLOAT):
        c1 = 8;
        c2 = 16;
        goto complex;

    case ARG_CLASS(TFLOAT, TINT):
        c1 = 16;
        c2 = 8;
        /*
         * if (ap->gp_offset <= 40 && ap->fp_offset <= 160) {
         *   qw1 = *(ap->reg_save_area + ap->gp_offset);
         *   qw2 = *(ap->reg_save_area + ap->fp_offset);
         *   tmp = {qw1, qw2};
         *   addr = &tmp;
         *   ap->gp_offset += 8;
         *   ap->fp_offset += 16;
         * } else {
         *   addr = ap->overflow_arg_area;
         *   ap->overflow_arg_area += 16;
         * }
         */
    complex:
        lab = genlab(2);
        sym = lookup("gp_offset", OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod.offset = offset + o;
        nod.type = ty;
        sym = lookup("fp_offset", OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod1 = nod;
        nod1.offset = offset + o;
        nod1.type = ty;
        nodconst(&nod2, nod.type, 40);
        regalloc(&nod3, &nod, NULL);
        gmove(&nod, &nod3);
        ginstr(ACMP, &nod2, &nod3);
        gbranch(OGT, lab, signbr(nod2.type));
        nodconst(&nod2, nod1.type, 160);
        regalloc(&nod4, &nod1, NULL);
        gmove(&nod1, &nod4);
        ginstr(ACMP, &nod2, &nod4);
        gbranch(OGT, lab, signbr(nod2.type));
        if (nn) {
            sym = lookup("reg_save_area", OPT_SEARCH);
            ty = find_field(sym, flist, &o);
            nod2 = nod;
            nod2.type = ty;
            nod2.offset = offset + o;
            nod3.type = ty;
            ginstr(AADD, &nod2, &nod3);
            nod4.type = ty;
            ginstr(AADD, &nod2, &nod4);
            /* INT */
            nod2 = nod3;
            regind(&nod2, types[TLONG]);
            gmove(&nod2, &nod3);
            nodreg(&nod5, &nod2, RBP);
            regind(&nod5, types[TLONG]);
            nod5.offset = -(stkoffset + redzone + c1);
            gmove(&nod3, &nod5);
            /* FLOAT */
            nod2 = nod4;
            regind(&nod2, types[TLONG]);
            gmove(&nod2, &nod4);
            nodreg(&nod5, &nod2, RBP);
            regind(&nod5, types[TLONG]);
            nod5.offset = -(stkoffset + redzone + c2);
            gmove(&nod4, &nod5);
            /* mov to nn */
            nod5.offset = -(stkoffset + redzone + 16);
            regalloc(&nod2, &nod3, nn);
            ginstr(ALEA, &nod5, &nod2);
            gmove(&nod2, nn);
            regfree(&nod2);
        }
        nodconst(&nod2, nod.type, 8);
        ginstr(AADD, &nod2, &nod);
        nodconst(&nod2, nod1.type, 16);
        ginstr(AADD, &nod2, &nod1);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        sym = lookup("overflow_arg_area", OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod.offset = offset + o;
        nod.type = ty;
        if (nn) {
            regalloc(&nod1, &nod, nn);
            gmove(&nod, &nod1);
            gmove(&nod1, nn);
            regfree(&nod1);
        }
        nodconst(&nod1, nod.type, 16);
        ginstr(AADD, &nod1, &nod);
        glabel(lab + 1);
        regfree(&nod3);
        regfree(&nod4);
        break;

    case TSTRUCT:
        /* 
         * addr = ap->overflow_arg_area;
         * ap->overflow_arg_area += ROUND(ty->size, 8);
         */
        sym = lookup("overflow_arg_area", OPT_SEARCH);
        ty = find_field(sym, flist, &o);
        nod.offset = offset + o;
        nod.type = ty;
        regalloc(&nod1, &nod, nn);
        gmove(&nod, &nod1);
        if (nn)
             gmove(&nod1, nn);
        regind(&nod1, types[TPTR]);
        nod1.offset = ROUNDUP(ty->size, 8);
        regalloc(&nod2, &nod, NULL);
        ginstr(ALEA, &nod1, &nod2);
        ginstr(AMOV, &nod2, &nod);
        regfree(&nod1);
        regfree(&nod2);
        break;
    }
    regfree(&nod);
}

/*
 *  struct __builtin_va_list_tag {
 *      unsigned int gp_offset;
 *      unsigned int fp_offset;
 *      void *overflow_arg_area;
 *      void *reg_save_area;
 *  };
 *  typedef struct __builtin_va_list_tag __builtin_va_list[1];
 *  void __builtin_va_start(__builtin_va_list, ...);
 *  void *__builtin_va_arg(__builtin_va_list, ...);
 *  void __builtin_va_copy(__builtin_va_list, __builtin_va_list);
 *  void __builtin_va_end(__builtin_va_list);
 */
static void va_xxx_init(void)
{
    struct symbol *s;
    struct type *ty, *va_list_typ;

    ty = mktag("__builtin_va_list_tag", TSTRUCT);
    mkfield(ty, types[TUINT], "gp_offset");
    mkfield(ty, types[TUINT], "fp_offset");
    mkfield(ty, ptrtyp(types[TVOID]), "overflow_arg_area");
    mkfield(ty, ptrtyp(types[TVOID]), "reg_save_area");
    sualign(ty);
    if (options.debug)
        prstruct(ty);

    s = lookup("__builtin_va_list", OPT_CREATE);
    va_list_typ = arraytyp(ty, 1);
    s->type = va_list_typ;
    s->block = 0;
    s->sclass = CTYPEDEF;
    s->offset = 0;
    if (options.debug)
        prdecl(s);

    s = lookup("__builtin_va_start", OPT_CREATE);
    ty = mkproto(va_list_typ, types[TVOID], NULL);
    s->type = functyp(types[TVOID], ty, 0);
    s->block = 0;
    s->sclass = CGLOBAL;
    s->offset = 0;
    s->defined = 1;
    if (options.debug)
        prdecl(s);

    s = lookup("__builtin_va_arg", OPT_CREATE);
    ty = mkproto(va_list_typ, types[TVOID], NULL);
    s->type = functyp(ptrtyp(types[TVOID]), ty, 0);
    s->block = 0;
    s->sclass = CGLOBAL;
    s->offset = 0;
    s->defined = 1;
    if (options.debug)
        prdecl(s);

    s = lookup("__builtin_va_copy", OPT_CREATE);
    ty = mkproto(va_list_typ, va_list_typ, NULL);
    s->type = functyp(types[TVOID], ty, 0);
    s->block = 0;
    s->sclass = CGLOBAL;
    s->offset = 0;
    s->defined = 1;
    if (options.debug)
        prdecl(s);

    s = lookup("__builtin_va_end", OPT_CREATE);
    ty = mkproto(va_list_typ, NULL);
    s->type = functyp(types[TVOID], ty, 0);
    s->block = 0;
    s->sclass = CGLOBAL;
    s->offset = 0;
    s->defined = 1;
    if (options.debug)
        prdecl(s);

    btfunc[BUILTIN_VA_START] = gva_start;
    btfunc[BUILTIN_VA_COPY] = gva_copy;
    btfunc[BUILTIN_VA_ARG] = gva_arg;
}

static void gopcode(int op, struct type *ty, struct node *f, struct node *t)
{
    struct node nod, nod1;
    int a;

    switch (op) {
    case OADD:
    case OASADD:
    case OPOSTINC:
    case OPREINC:
        if (ty->etype == TFLOAT)
            a = AADDSS;
        else if (ty->etype == TDOUBLE)
            a = AADDSD;
        else
            a = AADD;
        break;

    case OSUB:
    case OASSUB:
    case OPOSTDEC:
    case OPREDEC:
        if (ty->etype == TFLOAT)
            a = ASUBSS;
        else if (ty->etype == TDOUBLE)
            a = ASUBSD;
        else
            a = ASUB;
        break;
        break;

    case OMUL:
    case OUMUL:
    case OASMUL:
    case OASUMUL:
        if (ty->etype == TFLOAT)
            a = AMULSS;
        else if (ty->etype == TDOUBLE)
            a = AMULSD;
        else
            a = AMUL;
        break;

    case ODIV:
    case OASDIV:
        if (ty->etype == TFLOAT)
            a = ADIVSS;
        else if (ty->etype == TDOUBLE)
            a = ADIVSD;
        else
            a = ADIV;
        break;

    case OMOD:
    case OASMOD:
        a = ADIV;
        break;

    case OUDIV:
    case OUMOD:
    case OASUDIV:
    case OASUMOD:
        a = AUDIV;
        break;

    case OAND:
    case OASAND:
        a = AAND;
        break;

    case OOR:
    case OASOR:
        a = AOR;
        break;

    case OXOR:
    case OASXOR:
        a = AXOR;
        break;

    case OSHL:
    case OASSHL:
        a = ASAL;
        break;

    case OSHR:
    case OASSHR:
        a = ASAR;
        break;

    case OUSHR:
    case OASUSHR:
        a = ASHR;
        break;

    case ONEG:
        a = ANEG;
        break;

    case OCOM:
        a = ANOT;
        break;

    case OADDR:
        a = ALEA;
        break;

    case OFUNC:
        a = ACALL;
        break;

    default:
        error(f, "unknown op(%O) in gopcode", op);
        a = ANOP;
        break;
    }
    /* must clear RDX */
    if (a == ADIV || a == AUDIV) {
        nodreg(&nod, t, RDX);
        nod.type = types[TINT];
        nodconst(&nod1, types[TINT], 0);
        ginstr(AMOV, &nod1, &nod);
    }
    ginstr(a, f, t);
}

/* handle relocation */
static void ginstr(int op, struct node *f, struct node *t)
{
    long pc, reloc, addend;
    int len, ndx, type, c;
    struct node *p;

    pc = text_sec->data_len;
    reloc = 0;
    gopc(op, f, t, &reloc);
    len = text_sec->data_len - pc;

    if (reloc <= 0)
        return;

    if (f && f->op == OINDREG)
        p = f;
    else if (t && t->op == OINDREG)
        p = t;
    else
        p = NULL;

    if (p == NULL || p->reg != RIP)
        return;

    switch (op) {
    case AJMP:
    case AJA:
    case AJAE:
    case AJB:
    case AJBE:
    case AJG:
    case AJGE:
    case AJL:
    case AJLE:
    case AJE:
    case AJNE:
        c = findjmp(p->v.i, reloc);
        if (c > 0)
            *(int *)(text_sec->data + reloc) = c - reloc - 4;
        return;
    }

    if (p->type->etype == TFUNC)
        type = R_X86_64_PLT32;
    else
        type = R_X86_64_PC32;

    addend = p->offset - (pc + len - reloc);
    ndx = find_symndx(p->sym);
    sec_add_rela(symtab_sec, text_sec, reloc, ndx, type, addend);
}