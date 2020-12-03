#include "cc.h"

static struct symbol **symtab;
static unsigned int nslots;        /* number of slots */
static unsigned int nelements;     /* number of elements */
static unsigned int searches;
static unsigned int collisions;
static unsigned int expansions;

static void expand(void)
{
    unsigned int oldsize = nslots;
    struct symbol **oldtable = symtab;
    unsigned int sizemask;

    /* nslots must be 2^n */
    if (oldsize == 0) {
        nslots = 8 * 1024;
    } else {
        nslots = oldsize * 2;
        expansions++;
    }
    
    symtab = xcalloc(nslots, sizeof(struct symbol *));
    sizemask = nslots - 1;

    for (unsigned int i = 0; i < oldsize; i++) {
        struct symbol *entry = oldtable[i];
        while (entry) {
            struct symbol *next = entry->link;
            unsigned int index = entry->hash & sizemask;
            entry->link = symtab[index];
            symtab[index] = entry;
            entry = next;
        }
    }
    free(oldtable);
}

struct symbol *lookupn(const char *str, size_t len, int opt)
{
    unsigned int index, hash;
    struct symbol *p;
    char *dst;

    if (nelements * 4 >= nslots * 3)
        expand();

    searches++;
    hash = strnhash(str, len);
    index = hash & (nslots - 1);

    for (p = symtab[index]; p; p = p->link)
        if (p->len == len && !memcmp(str, p->name, len))
            return p;

    if (symtab[index])
        collisions++;
    if (opt == OPT_SEARCH)
        return NULL;

    nelements++;
    p = zmalloc(sizeof(struct symbol));
    p->len = len;
    p->hash = hash;
    dst = xmalloc(len + 1);
    memcpy(dst, str, len);
    dst[len] = '\0';
    p->name = dst;

    p->link = symtab[index];
    symtab[index] = p;

    return p;
}

struct symbol *lookup(const char *id, int opt)
{
    return lookupn(id, strlen(id), opt);
}

void foreach(void (*cb)(struct symbol *, void *), void *v)
{
    for (unsigned int i = 0; i < nslots; i++) {
        struct symbol *entry = symtab[i];
        while (entry) {
            struct symbol *next = entry->link;
            cb(entry, v);
            entry = next;
        }
    }
}

void dump_symtab(void)
{
    printf("symtab: %u elements, %u slots, %u searches, %u collisions, %u expansions.\n",
           nelements, nslots, searches, collisions, expansions);
}