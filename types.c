#include "cc.h"

struct type *types[NTYPE];

/* integer type? */
char typei[NTYPE];
static int typei_init[] = {
    TBOOL, TCHAR, TUCHAR, TSHORT, TUSHORT, TINT, TUINT,
    TLONG, TULONG, TLLONG, TULLONG, TENUM, -1
};

/* unsigned type? */
char typeu[NTYPE];
static int typeu_init[] = {
    TBOOL, TUCHAR, TUSHORT, TUINT, TULONG, TULLONG, TPTR, -1
};

/* float/double type? */
char typefd[NTYPE];
static int typefd_init[] = {
    TFLOAT, TDOUBLE, -1
};

/* struct/union type? */
char typesu[NTYPE];
static int typesu_init[] = {
    TSTRUCT, TUNION, -1
};

/* struct/union/enum type? */
char typesue[NTYPE];
static int typesue_init[] = {
    TSTRUCT, TUNION, TENUM, -1
};

/* array/function type? */
char typeaf[NTYPE];
static int typeaf_init[] = {
    TARRAY, TFUNC, -1
};

/* & */
int tand[NTYPE];
static struct init tand_init[] = {
    TBOOL,    BINTEGER,  0,
    TCHAR,    BINTEGER,  0,
    TUCHAR,   BINTEGER,  0,
    TSHORT,   BINTEGER,  0,
    TUSHORT,  BINTEGER,  0,
    TINT,     BINTEGER,  0,
    TUINT,    BINTEGER,  0,
    TLONG,    BINTEGER,  0,
    TULONG,   BINTEGER,  0,
    TLLONG,   BINTEGER,  0,
    TULLONG,  BINTEGER,  0,
    TENUM,    BINTEGER,  0,
    -1, 0, 0
};

/* ! */
int tnot[1] = {
    BNUMBER | BPTR
};

/* - */
int tneg[1] = {
    BNUMBER
};

/* >, >=, <, <=, ==, != */
int trel[NTYPE];
static struct init trel_init[] = {
    TBOOL,     BNUMBER,    0,
    TCHAR,     BNUMBER,    0,
    TUCHAR,    BNUMBER,    0,
    TSHORT,    BNUMBER,    0,
    TUSHORT,   BNUMBER,    0,
    TINT,      BNUMBER,    0,
    TUINT,     BNUMBER,    0,
    TLONG,     BNUMBER,    0,
    TULONG,    BNUMBER,    0,
    TLLONG,    BNUMBER,    0,
    TULLONG,   BNUMBER,    0,
    TFLOAT,    BNUMBER,    0,
    TDOUBLE,   BNUMBER,    0,
    TPTR,      BPTR,       0,
    TENUM,     BNUMBER,    0,
    -1, 0, 0
};

/* + */
int tadd[NTYPE];
static struct init tadd_init[] = {
    TBOOL,    BNUMBER | BPTR,  0,
    TCHAR,    BNUMBER | BPTR,  0,
    TUCHAR,   BNUMBER | BPTR,  0,
    TSHORT,   BNUMBER | BPTR,  0,
    TUSHORT,  BNUMBER | BPTR,  0,
    TINT,     BNUMBER | BPTR,  0,
    TUINT,    BNUMBER | BPTR,  0,
    TLONG,    BNUMBER | BPTR,  0,
    TULONG,   BNUMBER | BPTR,  0,
    TLLONG,   BNUMBER | BPTR,  0,
    TULLONG,  BNUMBER | BPTR,  0,
    TFLOAT,   BNUMBER,         0,
    TDOUBLE,  BNUMBER,         0,
    TPTR,     BINTEGER,        0,
    TENUM,    BNUMBER | BPTR,  0,
    -1, 0, 0
};

/* - */
int tsub[NTYPE];
static struct init tsub_init[] = {
    TBOOL,    BNUMBER,  0,
    TCHAR,    BNUMBER,  0,
    TUCHAR,   BNUMBER,  0,
    TSHORT,   BNUMBER,  0,
    TUSHORT,  BNUMBER,  0,
    TINT,     BNUMBER,  0,
    TUINT,    BNUMBER,  0,
    TLONG,    BNUMBER,  0,
    TULONG,   BNUMBER,  0,
    TLLONG,   BNUMBER,  0,
    TULLONG,  BNUMBER,  0,
    TFLOAT,   BNUMBER,  0,
    TDOUBLE,  BNUMBER,  0,
    TPTR,     BINTEGER | BPTR, 0,
    TENUM,    BNUMBER,  0,
    -1, 0, 0
};

/* * */
int tmul[NTYPE];
static struct init tmul_init[] = {
    TBOOL,    BNUMBER,  0,
    TCHAR,    BNUMBER,  0,
    TUCHAR,   BNUMBER,  0,
    TSHORT,   BNUMBER,  0,
    TUSHORT,  BNUMBER,  0,
    TINT,     BNUMBER,  0,
    TUINT,    BNUMBER,  0,
    TLONG,    BNUMBER,  0,
    TULONG,   BNUMBER,  0,
    TLLONG,   BNUMBER,  0,
    TULLONG,  BNUMBER,  0,
    TFLOAT,   BNUMBER,  0,
    TDOUBLE,  BNUMBER,  0,
    TENUM,    BNUMBER,  0,
    -1, 0, 0,
};

int tindir[1] = {
    BPTR
};

int tdot[1] = {
    BSTRUCT | BUNION
};

int tfunc[1] = {
    BFUNC
};

int targ[1] = {
    BNUMBER | BPTR | BSTRUCT | BUNION
};

int tcast[NTYPE];
static struct init tcast_init[] = {
    TBOOL,    BNUMBER | BPTR | BVOID,  0,
    TCHAR,    BNUMBER | BPTR | BVOID,  0,
    TUCHAR,   BNUMBER | BPTR | BVOID,  0,
    TSHORT,   BNUMBER | BPTR | BVOID,  0,
    TUSHORT,  BNUMBER | BPTR | BVOID,  0,
    TINT,     BNUMBER | BPTR | BVOID,  0,
    TUINT,    BNUMBER | BPTR | BVOID,  0,
    TLONG,    BNUMBER | BPTR | BVOID,  0,
    TULONG,   BNUMBER | BPTR | BVOID,  0,
    TLLONG,   BNUMBER | BPTR | BVOID,  0,
    TULLONG,  BNUMBER | BPTR | BVOID,  0,
    TFLOAT,   BNUMBER | BVOID,         0,
    TDOUBLE,  BNUMBER | BVOID,         0,
    TPTR,     BINTEGER | BPTR | BVOID, 0,
    TVOID,    BVOID,                   0,
    TSTRUCT,  BSTRUCT | BVOID,         0,
    TUNION,   BUNION  | BVOID,         0,
    TENUM,    BNUMBER | BPTR | BVOID,  0,
    -1, 0, 0
};

int tasgn[NTYPE];
static struct init tasgn_init[] = {
    TBOOL,    BNUMBER | BPTR,  0,
    TCHAR,    BNUMBER,  0,
    TUCHAR,   BNUMBER,  0,
    TSHORT,   BNUMBER,  0,
    TUSHORT,  BNUMBER,  0,
    TINT,     BNUMBER,  0,
    TUINT,    BNUMBER,  0,
    TLONG,    BNUMBER,  0,
    TULONG,   BNUMBER,  0,
    TLLONG,   BNUMBER,  0,
    TULLONG,  BNUMBER,  0,
    TFLOAT,   BNUMBER,  0,
    TDOUBLE,  BNUMBER,  0,
    TPTR,     BPTR,     0,
    TSTRUCT,  BSTRUCT,  0,
    TUNION,   BUNION,   0,
    TENUM,    BNUMBER,  0,
    -1, 0, 0
};

int tasadd[NTYPE];
static struct init tasadd_init[] = {
    TBOOL,    BNUMBER,  0,
    TCHAR,    BNUMBER,  0,
    TUCHAR,   BNUMBER,  0,
    TSHORT,   BNUMBER,  0,
    TUSHORT,  BNUMBER,  0,
    TINT,     BNUMBER,  0,
    TUINT,    BNUMBER,  0,
    TLONG,    BNUMBER,  0,
    TULONG,   BNUMBER,  0,
    TLLONG,   BNUMBER,  0,
    TULLONG,  BNUMBER,  0,
    TFLOAT,   BNUMBER,  0,
    TDOUBLE,  BNUMBER,  0,
    TPTR,     BINTEGER, 0,
    TENUM,    BNUMBER,  0,
    -1, 0, 0
};

char tab[NTYPE][NTYPE] = {
    /* TXXX */
    { 0, },
    /* TBOOL */
    { 0, TBOOL, TUCHAR, TUCHAR, TSHORT, TUSHORT, TINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TCHAR */
    { 0, TUCHAR, TCHAR, TUCHAR, TSHORT, TUSHORT, TINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TUCHAR */
    { 0, TUCHAR, TUCHAR, TUCHAR, TSHORT, TUSHORT, TINT, TUINT,
      TULONG, TULONG, TULLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TSHORT */
    { 0, TSHORT, TSHORT, TSHORT, TSHORT, TUSHORT, TINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TUSHORT */
    { 0, TUSHORT, TUSHORT, TUSHORT, TUSHORT, TUSHORT, TINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TINT */
    { 0, TINT, TINT, TINT, TINT, TINT, TINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TUINT */
    { 0, TUINT, TUINT, TUINT, TUINT, TUINT, TUINT, TUINT,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TLONG */
    { 0, TLONG, TLONG, TLONG, TLONG, TLONG, TLONG, TLONG,
      TLONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TULONG */
    { 0, TULONG, TULONG, TULONG, TULONG, TULONG, TULONG, TULONG,
      TULONG, TULONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
     /* TLLONG */
    { 0, TLLONG, TLLONG, TLLONG, TLLONG, TLLONG, TLLONG, TLLONG,
      TLLONG, TLLONG, TLLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TULLONG */
    { 0, TULLONG, TULLONG, TULLONG, TULLONG, TULLONG, TULLONG, TULLONG,
      TULLONG, TULLONG, TULLONG, TULLONG, TFLOAT, TDOUBLE, TPTR, },
    /* TFLOAT */
    { 0, TFLOAT, TFLOAT, TFLOAT, TFLOAT, TFLOAT, TFLOAT, TFLOAT,
      TFLOAT, TFLOAT, TFLOAT, TFLOAT, TFLOAT, TDOUBLE, TPTR, },
    /* TDOUBLE */
    { 0, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE,
      TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TDOUBLE, TPTR, },
    /* TPTR */
    { 0, TPTR, TPTR, TPTR, TPTR, TPTR, TPTR, TPTR,
      TPTR, TPTR, TPTR, TPTR, TPTR, TPTR, TPTR, },
};

static struct type *newtyp(int etype, struct type *link)
{
    struct type *p = zallocate(sizeof *p, PERM);
    p->etype = etype;
    p->link = link;
    p->qual = QXXX;
    p->size = typsize[etype];
    p->align = typsize[etype];
    if (p->align < 1)
        p->align = 1;
    return p;
}

struct type *typcpy(struct type *ty)
{
    struct type *p = newtyp(TXXX, NULL);
    *p = *ty;
    return p;
}

struct type *ptrtyp(struct type *ty)
{
    return newtyp(TPTR, ty);
}

struct type *arraytyp(struct type *ty, long n)
{
    struct type *p;
    
    if (ty->etype == TFUNC) {
        error(0, "array of function is invalid");
        ty = types[TINT];
    }
    if (ty->size < 1) {
        error(0, "array of incomplete type '%T'", ty);
        ty = types[TINT];
    }
    if (n < 0) {
        error(0, "array has negetive size '%ld'", n);
        n = 1;
    }

    p = newtyp(TARRAY, ty);
    p->align = ty->align;
    p->size = n * ty->size;
    return p;
}

struct type *functyp(struct type *ty, struct type *proto, int oldstyle)
{
    struct type *p;
    
    if (ty->etype == TARRAY || ty->etype == TFUNC) {
        error(0, "function can't return type '%T'", ty);
        ty = types[TINT];
    }

    p = newtyp(TFUNC, ty);
    p->next = proto;
    p->oldstyle = oldstyle;
    return p;
}

struct type *suetyp(int t)
{
    struct type *p;
    switch (t) {
    case TSTRUCT:
    case TUNION:
        p = newtyp(t, NULL);
        break;
    case TENUM:
        p = newtyp(t, types[TINT]);
        break;
    default:
        abort();
    }
    return p;
}

struct type *unqual(struct type *ty)
{
    if (ty->qual != QXXX) {
        ty = typcpy(ty);
        ty->qual = QXXX;
    }
    return ty;
}

struct type *qual(int q, struct type *ty)
{
    struct type *nty;
    
    q &= QMASK;
    if (q == QXXX)
        return unqual(ty);
    if (q != (ty->qual & q)) {
        nty = typcpy(ty);
        nty->qual |= q;
        return nty;
    }
    return ty;
}

int variadic(struct type *ty)
{
    struct type *ty1;

    if (ty->etype == TFUNC && ty->next)
        for (ty1 = ty->next; ty1; ty1 = ty1->next)
            if (ty1->etype == TVOID && ty1->next == NULL)
                return ty1 != ty->next;

    return 0;
}

/* 0-strict, 1-ignore qual */
int eqtype(struct type *ty1, struct type *ty2, int f)
{
    if (ty1 == ty2)
        return 1;
    if (ty1 == NULL || ty2 == NULL)
        return 0;
    if (ty1->etype != ty2->etype)
        return 0;
    if (ty1->qual != ty2->qual && f == 0)
        return 0;
    if (ty1->etype == TPTR)
        return eqtype(ty1->link, ty2->link, f);
    if (ty1->etype == TARRAY) {
        if (eqtype(ty1->link, ty2->link, f)) {
            if (ty1->size == ty2->size)
                return 1;
            if (ty1->size == 0 || ty2->size == 0)
                return 1;
        }
        return 0;
    }
    if (ty1->etype == TFUNC) {
        if (eqtype(ty1->link, ty2->link, f)) {
            struct type* p1 = ty1->next;
            struct type* p2 = ty2->next;

            if (p1 == p2)
                return 1;
            if (p1 && p2) {
                for (; p1 && p2; p1 = p1->next, p2 = p2->next)
                    if (!eqtype(p1, p2, f))
                        return 0;
                if (p1 == NULL && p2 == NULL)
                    return 1;
            } else {
                if (p1 == NULL)
                    p1 = p2;
                for (int i = 0; p1; p1 = p1->next, i++) {
                    if (p1->etype == TVOID && i)
                        return 0;
                    if (p1->etype == TENUM)
                        p1 = p1->link;
                    if (promote(p1)->etype != p1->etype)
                        return 0;
                }
                return 1;
            }
        }
        return 0;
    }
    if (typesue[ty1->etype])
        return ty1->tag == ty2->tag;
    return 1;
}

static struct type *tcompose(struct type *ty1, struct type *ty2)
{
    struct type *ty, *p1, *p2, *proto, **pp;
    
    if (ty1 == ty2)
        return ty1;
    if (ty1->etype != ty2->etype)
        return NULL;

    switch (ty1->etype) {
    case TPTR:
        ty = tcompose(ty1->link, ty2->link);
        if (!ty) return NULL;
        return qual(ty1->qual | ty2->qual, ptrtyp(ty));

    case TARRAY:
        ty = tcompose(ty1->link, ty2->link);
        if (!ty) return NULL;

        if (ty1->size && ((ty1->link->size && ty2->size == 0) ||
                          ty1->size == ty2->size))
            return arraytyp(ty, ty1->size / ty1->link->size);
        if (ty2->size && ty2->link->size && ty1->size == 0)
            return arraytyp(ty, ty2->size / ty2->link->size);

        return arraytyp(ty, 0);

    case TFUNC:
        ty = tcompose(ty1->link, ty2->link);
        if (!ty)
            return NULL;

        p1 = ty1->next;
        p2 = ty2->next;
        if (p1 == NULL && p2 == NULL)
            return functyp(ty, NULL, 1);
        if (p1 && p2 == NULL)
            return functyp(ty, p1, ty1->oldstyle);
        if (p2 && p1 == NULL)
            return functyp(ty, p2, ty2->oldstyle);

        proto = NULL, pp = &proto;
        for (; p1 && p2; p1 = p1->next, p2 = p2->next) {
            struct type *ty3 = tcompose(p1, p2);
            if (!ty3)
                return NULL;
            ty3 = typcpy(ty3);
            ty3->next = NULL;
            *pp = ty3;
            pp = &ty3->next;
        }
        if (p1 || p2)
            return NULL;

        return functyp(ty, proto, 0);

    case TSTRUCT:
    case TUNION:
    case TENUM:
        if (ty1->tag != ty2->tag)
            return NULL;
        /* fall thru */
    default:
        return qual(ty1->qual | ty2->qual, ty1);
    }
}

struct type *compose(struct type *ty1, struct type *ty2)
{
    struct type *ty = tcompose(ty1, ty2);
    if (ty == NULL)
        error(0, "compose incompatible types '%T' and '%T'", ty1, ty2);
    return ty;
}

struct type *decay(struct type *ty)
{
    switch (ty->etype) {
    case TFUNC:
        return ptrtyp(ty);
    case TARRAY:
        return ptrtyp(ty->link);
    default:
        return ty;
    }
}

struct type *promote(struct type *ty)
{
    if (ty->etype == TENUM)
        ty = ty->link;
    if (ty->etype == TFLOAT)
        return types[TDOUBLE];
    if (typei[ty->etype] && ty->size < types[TINT]->size)
        return types[TINT];
    return ty;
}

void sualign(struct type *sty)
{
    struct type **pp;
    int bits, align;
    long offset, size;

    bits = 0, offset = 0;
    align = 1, size = 1;
    pp = &sty->link;
    for (struct type *p = *pp; p; p = p->next) {
        int a, has_bit;

        /* flexible array as last field */
        if (p->size < 1 && (p->etype != TARRAY || p->next))
            error(0, "field '%s' has incomplete type", p->sym->name);

        has_bit = p->nbits > 0;
        if (has_bit)
            p->nbits -= 1;    
        a = MAX(p->align, 1);
        if (sty->etype == TUNION) {
            offset = 0;
            bits = 0;
        } else if (p->nbits == 0 || p->nbits + bits > ROUNDUP(bits, a * 8)) {
            offset += BYTES(bits);
            offset = ROUNDUP(offset, a);
            bits = 0;
        } else {
            int off = ROUNDUP(BYTES(bits), a) - p->size;
            offset += off;
            bits -= off * 8;
        }

        if (a > align && (!has_bit || (p->nbits > 0 && p->sym)))
            align = a;

        p->offset = offset;
        if (has_bit) {
            p->bitoff = bits;
            bits += p->nbits;
        } else {
            offset += p->size;
        }

        if (offset + BYTES(bits) > size)
            size = offset + BYTES(bits);

        /* only chain the named field or anonymous record */
        if (p->sym || typesu[p->etype]) {
            *pp = p;
            pp = &p->next;
        }
    }
    *pp = NULL;
    sty->align = align;
    sty->size = ROUNDUP(size, align);
}

struct type *find_field(struct symbol *s, struct type *flist, long *off)
{
    struct type *p, *q = NULL;

    for (p = flist; p; p = p->next)
        if (p->sym) {
            if (p->sym == s) {
                q = p;
                if (off) *off = p->offset;
                break;
            }
        } else if (typesu[p->etype]) {
            /* lookup in unnamed substructure */
            struct type *x = find_field(s, p->link, off);
            if (x) {
                q = x;
                if (off) *off += p->offset;
                break;
            }
        }

    return q;
}

void types_init(void)
{
    int *p;
    struct init *q;

    types[TXXX] = NULL;
    types[TBOOL] = newtyp(TBOOL, NULL);
    types[TCHAR] = newtyp(TCHAR, NULL);
    types[TUCHAR] = newtyp(TUCHAR, NULL);
    types[TSHORT] = newtyp(TSHORT, NULL);
    types[TUSHORT] = newtyp(TUSHORT, NULL);
    types[TINT] = newtyp(TINT, NULL);
    types[TUINT] = newtyp(TUINT, NULL);
    types[TLONG] = newtyp(TLONG, NULL);
    types[TULONG] = newtyp(TULONG, NULL);
    types[TLLONG] = newtyp(TLLONG, NULL);
    types[TULLONG] = newtyp(TULLONG, NULL);
    types[TFLOAT] = newtyp(TFLOAT, NULL);
    types[TDOUBLE] = newtyp(TDOUBLE, NULL);
    types[TVOID] = newtyp(TVOID, NULL);
    types[TPTR] = newtyp(TPTR, types[TVOID]);
    types[TFUNC] = newtyp(TFUNC, types[TINT]);

    for (p = typei_init; *p >= 0; p++)
        typei[*p] = 1;
    for (p = typeu_init; *p >= 0; p++)
        typeu[*p] = 1;
    for (p = typefd_init; *p >= 0; p++)
        typefd[*p] = 1;
    for (p = typesu_init; *p >= 0; p++)
        typesu[*p] = 1;
    for (p = typesue_init; *p >= 0; p++)
        typesue[*p] = 1;
    for (p = typeaf_init; *p >= 0; p++)
        typeaf[*p] = 1;

    for (q = tand_init; q->code >= 0; q++)
        tand[q->code] = q->value;
    for (q = trel_init; q->code >= 0; q++)
        trel[q->code] = q->value;
    for (q = tadd_init; q->code >= 0; q++)
        tadd[q->code] = q->value;
    for (q = tsub_init; q->code >= 0; q++)
        tsub[q->code] = q->value;
    for (q = tmul_init; q->code >= 0; q++)
        tmul[q->code] = q->value;
    for (q = tcast_init; q->code >= 0; q++)
        tcast[q->code] = q->value;
    for (q = tasgn_init; q->code >= 0; q++)
        tasgn[q->code] = q->value;
    for (q = tasadd_init; q->code >= 0; q++)
        tasadd[q->code] = q->value;
}