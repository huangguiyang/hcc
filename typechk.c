#include "cc.h"

enum {
    ADDROF = 1 << 0,
    ADDROP = 1 << 1,
};

static int vconst(struct node *p)
{
    int i;

    if (p == NULL)
        goto bad;
    if (p->op != OCONST)
        goto bad;
    if (p->type == NULL)
        goto bad;
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
        i = p->v.i;             /* downcast */
        if (i != p->v.i)
            goto bad;
        return i;
    
    case TFLOAT:
    case TDOUBLE:
        i = p->v.d;
        if (i != p->v.d)        /* round */
            goto bad;
        return i;
    }
bad:
    return -666;                /* any value except 0,1 */
}

int vbconst(struct node *p)
{
    if (p == NULL)
        goto bad;
    if (p->op == OADDR)
        return 1;
    if (p->op != OCONST)
        goto bad;
    if (typefd[p->type->etype])
        return p->v.d != 0;
    return p->v.i != 0;
bad:
    return -666;    /* any value except 0,1 */
}

static int nullptr_p(struct node *p)
{
    if (p == NULL || p->type == NULL)
        return 0;
    if (p->type->etype != TPTR && !typei[p->type->etype])
        return 0;
    while (p->op == OCAST)
        p = p->left;
    return vconst(p) == 0;
}

static int tlvalue(struct node *p)
{
    if (!p->addable) {
        error(p, "not an lvalue");
        return 1;
    }
    return 0;
}

static int stcompat(struct node *p, struct type *ty1, struct type *ty2, int ttab[])
{
    int i, b;

    i = 0;
    if (ty2)
        i = ty2->etype;
    b = 1 << i;
    i = 0;
    if (ty1)
        i = ty1->etype;
    if (b & ttab[i]) {
        if (ttab == tcast || ttab == tasgn)
            if (b == BSTRUCT || b == BUNION)
                if (!eqtype(ty1, ty2, 1))
                    return 1;
        if (p->op != OCAST)
            if (b == BPTR && i == TPTR)
                if (!eqtype(ty1, ty2, 1))
                    return 1;
        return 0;
    }
    return 1;
}

int tcompat(struct node *p, struct type *ty1, struct type *ty2, int ttab[])
{
    if (stcompat(p, ty1, ty2, ttab)) {
        if (ty1 == NULL)
            error(p, "incompatible type '%T' for op '%O'", ty2, p->op);
        else
            error(p, "incompatible types '%T' and '%T' for op '%O'", ty1, ty2, p->op);
        return 1;
    }
    return 0;
}

/* universal binary conversion */
static void arith(struct node *p)
{
    struct type *ty1, *ty2;
    struct node *q;
    int i, j, k;
    long size;

    ty1 = p->left->type;
    if (p->right)
        ty2 = p->right->type;
    else
        ty2 = ty1;
    if (ty1 && ty1->etype == TENUM)
        ty1 = ty1->link;
    if (ty2 && ty2->etype == TENUM)
        ty2 = ty2->link;
    i = TXXX;
    if (ty1)
        i = ty1->etype;
    j = TXXX;
    if (ty2)
        j = ty2->etype;
    k = tab[i][j];
    if (k == TPTR) {
        if (i == TPTR)
            p->type = ty1;
        else if (j == TPTR)
            p->type = ty2;
    } else {
        p->type = types[k];
        /* promote to int */
        if (typei[p->type->etype] && p->type->size < types[TINT]->size)
            p->type = types[TINT];
    }

    if (p->op == OSUB)
        if (i == TPTR && j == TPTR) {
            size = p->right->type->link->size;
            if (size < 1 || p->left->type->link->size < 1)
                goto bad;
            p->type = types[typsize[TPTR] <= typsize[TLONG] ? TLONG : TLLONG];
            if (typsize[TPTR] > typsize[TLONG]) {
                q = node(OXXX, NULL, NULL);
                *q = *p;
                p->op = OCAST;
                p->left = q;
                p->right = NULL;
                p->type = types[TLONG];
            }
            if (size > 1) {
                q = node(OXXX, NULL, NULL);
                *q = *p;
                p->op = ODIV;
                p->left = q;
                q = node1(OCONST, NULL, NULL);
                q->v.i = size;
                q->type = p->type;
                p->right = q;
                size = log2i(size);
                if (size >= 0) {
                    p->op = OSHR;
                    q->v.i = size;
                }
            }
            return;
        }

    if (!eqtype(p->type, p->left->type, 1)) {
        p->left = node(OCAST, p->left, NULL);
        p->left->type = p->type;
        if (p->type->etype == TPTR) {
            size = p->type->link->size;
            if (size < 1)
                goto bad;
            if (size > 1) {
                q = node1(OCONST, NULL, NULL);
                q->v.i = size;
                q->type = p->type;
                p->left = node(OMUL, p->left, q);
                p->left->type = p->type;
            }
        }
    }
    if (p->right)
        if (!eqtype(p->type, p->right->type, 1)) {
            p->right = node(OCAST, p->right, NULL);
            p->right->type = p->type;
            if (p->type->etype == TPTR) {
                size = p->type->link->size;
                if (size < 1)
                    goto bad;
                if (size > 1) {
                    q = node1(OCONST, NULL, NULL);
                    q->v.i = size;
                    q->type = p->type;
                    p->right = node(OMUL, p->right, q);
                    p->right->type = p->type;
                }
            }
        }
    return;
bad:
    error(p, "pointer addition to incomplete type: '%T'", p->type->link);
}

static void typext(struct type *ty, struct node *p, int f)
{
    struct node *q;

    if (ty->etype == TPTR && typei[p->type->etype])
        goto cast;
    if (f)
        if (ty->etype == TPTR && p->type->etype == TPTR)
            goto cast;
    
    return;
cast:
    q = node(OXXX, NULL, NULL);
    *q = *p;
    p->op = OCAST;
    p->type = ty;
    p->left = q;
    p->right = NULL;
    p->addable = 0;
}

static void makedot(struct node *p, struct type *ty, long o)
{
    struct node *p1, *p2;

    if (ty->nbits) {
        p1 = node(OXXX, NULL, NULL);
        *p1 = *p;
        p->op = OBIT;
        p->left = p1;
        p->right = NULL;
        p->type = ty;
        p->addable = p1->left->addable;
        p = p1;
    }
    p->addable = p->left->addable;
    if (p->addable == 0) {
        p1 = node1(OCONST, NULL, NULL);
        p1->v.i = o;
        p1->type = types[TLONG];
        p->right = p1;
        p->type = ty;
        return;
    }
    p->left->type = ty;
    if (o == 0) {
        *p = *p->left;
        return;
    }
    /* left.x/left->x => *(&left + offset) */
    p->type = ty;
    ty = ptrtyp(ty);

    p1 = node(OADDR, p->left, NULL);
    p1->type = ty;

    p2 = node1(OCONST, NULL, NULL);
    p2->v.i = o;
    p2->type = ty;

    p1 = node(OADD, p1, p2);
    p1->type = ty;

    p->op = OINDIR;
    p->left = p1;
    p->right = NULL;
}

/* convert dot */
static int tconvd(struct node *p)
{
    struct type *ty;
    long o;
    
    if (p->sym == NULL)
        return 1;
    o = 0;
    ty = find_field(p->sym, p->left->type->link, &o);
    if (ty == NULL) {
        error(p, "field '%s' not found", p->sym->name);
        return 1;
    }
    makedot(p, ty, o);
    return 0;
}

/* initialize bit-field */
static void makeidot(struct node *p)
{
    struct node *q;

    q = node(OXXX, NULL, NULL);
    *q = *p;
    p->op = OBIT;
    p->left = q;
    p->right = NULL;
    p->type = q->type;
    p->addable = q->addable;
}

/* convert arg */
static int tconva(struct node *l, struct node *a, struct type *ty, int f)
{
    int o;

    if (a == NULL) {
        if (ty != NULL && ty->etype != TVOID) {
            error(0, "not enough function arguments");
            return 1;
        }
        return 0;
    }
    if (a->op == OLIST) {
        o = tconva(l, a->left, ty, 0);
        if (ty) {
            ty = ty->next;
            if (ty == NULL)
                ty = types[TVOID];
            else if (ty->etype == TVOID)    /* variadic */
                ty = NULL;
        }
        return o | tconva(l, a->right, ty, 1);
    }
    if (f && ty)                            /* check last arg */
        tconva(l, NULL, ty->next, 0);
    if (a->op == OXXX)                      /* __builtin_va_arg */
        return 0;
    if (tconv(a) || tcompat(a, NULL, a->type, targ))
        return 1;
    if (eqtype(ty, types[TVOID], 1)) {
        error(a, "too many function arguments");
        return 1;
    }
    if (ty) {
        typext(ty, a, 1);
        if (stcompat(l, ty, a->type, tasgn)) {
            error(a, "argument prototype mismatch '%T' for '%T'", a->type, ty);
            return 1;
        }
        if (typei[ty->etype])
            ty = promote(ty);
    } else {
        /* variadic case */
        ty = promote(a->type);
    }
    if (!eqtype(ty, a->type, 1)) {
        l = node(OXXX, NULL, NULL);
        *l = *a;
        a->op = OCAST;
        a->type = ty;
        a->left = l;
        a->right = NULL;
        a->addable = 0;
    }
    return 0;
}

/* check assignment to `const' */
static void constas(struct node *p, struct type *ty1, struct type *ty2)
{
    struct type *l, *r;

    l = ty1;
    r = ty2;
    if (l == NULL)
        return;
    if (l->qual & QCONST) {
        warn(p, "assignment to a const type '%T'", ty2);
        return;
    }
    if (r == NULL)
        return;
    while (1) {
        if (l->etype != TPTR || r->etype != TPTR)
            break;
        l = l->link;
        r = r->link;
        if (l == NULL || r == NULL)
            break;
        if (r->qual & QCONST)
            if (!(l->qual & QCONST)) {
                warn(p, "type qualifiers discarded: '%T'", ty2);
                break;
            }
    }
}

/* builtin functions */
static int tconvb(struct node *p)
{
    struct node *l, *r;
    struct node *q;
    struct type *ty;

    l = p->left;
    if (l == NULL || l->op != ONAME || l->sym == NULL)
        return 0;

    r = p->right;
    if (l->sym->builtin_id == BUILTIN_VA_START) {
        if (thisfn == NULL || thisfn->etype != TFUNC) {
            error(p, "'va_start' misused");
            return 1;
        }
        if (!thisfn->oldstyle && !variadic(thisfn)) {
            error(p, "'va_start' used in function with fixed args");
            return 1;
        }
    }
    if (l->sym->builtin_id == BUILTIN_VA_ARG) {
        ty = r->right->type;
        if (ty->size < 1) {
            error(p, "'va_arg' with incomplete type '%T'", ty);
            return 1;
        }
        ty = promote(ty);
        ty = ptrtyp(ty);
        r->right = NULL;
        /* INDIR(CAST(p)) */
        q = node(OXXX, NULL, NULL);
        *q = *p;
        p->op = OCAST;
        p->left = q;
        p->right = NULL;
        p->type = ty;
        q = node(OXXX, NULL, NULL);
        *q = *p;
        p->op = OINDIR;
        p->left = q;
        p->right = NULL;
        p->type = q->type->link;
        p->addable = 1;
    }
    return 0;
}

/* compound literal */
static int tconvcp(struct node *p)
{
    if (p->op != OLIST)
        return tconv(p);
    return tconvcp(p->left) | tconvcp(p->right);
}

static int tconvo(struct node *p, int f)
{
    struct node *l, *r;
    struct type *ty;
    
    p->addable = 0;
    l = p->left;
    r = p->right;

    switch (p->op) {
    case OCOMMA:
        if (tconv(l) | tconv(r))
            goto bad;
        p->type = r->type;
        break;

    /* same as AS, but 
        1. no test for `const'
        2. handle bit-field initialization */
    case OASI:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tlvalue(l))
            goto bad;
        typext(l->type, r, 1);
        if (tcompat(p, l->type, r->type, tasgn))
            goto bad;
        if (l->type->nbits)
            makeidot(l);
        if (!eqtype(l->type, r->type, 1)) {
            r = node(OCAST, r, NULL);
            r->type = l->type;
            p->right = r;
        }
        p->type = l->type;
        p->op = OAS;
        break;

    case OAS:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tlvalue(l))
            goto bad;
        typext(l->type, r, 1);
        if (tcompat(p, l->type, r->type, tasgn))
            goto bad;
        constas(p, l->type, r->type);
        if (!eqtype(l->type, r->type, 1)) {
            r = node(OCAST, r, NULL);
            r->type = l->type;
            p->right = r;
        }
        p->type = l->type;
        break;
        
    case OASADD:
    case OASSUB:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tlvalue(l))
            goto bad;
        if (tcompat(p, l->type, r->type, tasadd))
            goto bad;
        constas(p, l->type, r->type);
        ty = l->type;
        /* perform arith conv and then remove cast added by arith */
        arith(p);
        while (p->left->op == OCAST)
            p->left = p->left->left;
        p->type = ty;
        break;
        
    case OASMUL:
    case OASUMUL:
    case OASDIV:
    case OASUDIV:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tlvalue(l))
            goto bad;
        if (tcompat(p, l->type, r->type, tmul))
            goto bad;
        constas(p, l->type, r->type);
        ty = l->type;
        arith(p);
        while (p->left->op == OCAST)
            p->left = p->left->left;
        p->type = ty;
        if (typeu[p->type->etype]) {
            if (p->op == OASDIV)
                p->op = OASUDIV;
            if (p->op == OASMUL)
                p->op = OASUMUL;
        }
        break;

    case OASMOD:
    case OASUMOD:
    case OASXOR:
    case OASAND:
    case OASOR:
    case OASSHL:
    case OASSHR:
    case OASUSHR:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tlvalue(l))
            goto bad;
        if (tcompat(p, l->type, r->type, tand))
            goto bad;
        constas(p, l->type, r->type);
        ty = l->type;
        arith(p);
        while (p->left->op == OCAST)
            p->left = p->left->left;
        p->type = ty;
        if (typeu[p->type->etype]) {
            if (p->op == OASMOD)
                p->op = OASUMOD;
            if (p->op == OASSHR)
                p->op = OASUSHR;
        }
        break;
        
    case OCOND:
        if (tconv(l) | tconv(r->left) | tconv(r->right))
            goto bad;
        if (tcompat(p, NULL, l->type, tnot))
            goto bad;
        if (r->left->type->etype == TPTR && nullptr_p(r->right))
            r->right->type = r->left->type;
        if (r->right->type->etype == TPTR && nullptr_p(r->left))
            r->left->type = r->right->type;
        if (eqtype(r->left->type, r->right->type, 0)) {
            r->type = r->right->type;
            p->type = r->type;
            break;
        }
        if (tcompat(r, r->left->type, r->right->type, trel))
            goto bad;
        arith(r);
        p->type = r->type;
        break;
        
    case OADD:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tcompat(p, l->type, r->type, tadd))
            goto bad;
        arith(p);
        break;
        
    case OSUB:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tcompat(p, l->type, r->type, tsub))
            goto bad;
        arith(p);
        break;
        
    case OMUL:
    case OUMUL:
    case ODIV:
    case OUDIV:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tcompat(p, l->type, r->type, tmul))
            goto bad;
        arith(p);
        if (typeu[p->type->etype]) {
            if (p->op == ODIV)
                p->op = OUDIV;
            if (p->op == OMUL)
                p->op = OUMUL;
        }
        break;

    case OMOD:
    case OUMOD:
    case OAND:
    case OOR:
    case OXOR:
    case OSHL:
    case OSHR:
    case OUSHR:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tcompat(p, l->type, r->type, tand))
            goto bad;
        arith(p);
        if (typeu[p->type->etype]) {
            if (p->op == OMOD)
                p->op = OUMOD;
            if (p->op == OSHR)
                p->op = OUSHR;
        }
        break;

    case OGT:
    case OGE:
    case OLT:
    case OLE:
        if (tconv(l) | tconv(r))
            goto bad;
        typext(l->type, r, 0);
        typext(r->type, l, 0);
        if (tcompat(p, l->type, r->type, trel))
            goto bad;
        arith(p);
        p->type = types[TINT];
        break;

    case OEQ:
    case ONE:
        if (tconv(l) | tconv(r))
            goto bad;
        if (l->type->etype == TPTR && r->type->etype == TPTR) {
            if (l->type->link->etype == TVOID)
                typext(l->type, r, 1);
            else if (r->type->link->etype == TVOID)
                typext(r->type, l, 1);
        }
        if (l->type->etype == TPTR && nullptr_p(r))
            r->type = l->type;
        if (r->type->etype == TPTR && nullptr_p(l))
            l->type = r->type;
        if (tcompat(p, l->type, r->type, trel))
            goto bad;
        arith(p);
        p->type = types[TINT];
        break;

    case OANDAND:
    case OOROR:
        if (tconv(l) | tconv(r))
            goto bad;
        if (tcompat(p, NULL, l->type, tnot) | tcompat(p, NULL, r->type, tnot))
            goto bad;
        p->type = types[TINT];
        break;

    case OCAST:
        if (tconv(l))
            goto bad;
        if (tcompat(p, l->type, p->type, tcast))
            goto bad;
        break;
        
    case OPOS:
        if (tconv(l))
            goto bad;
        if (tcompat(p, NULL, l->type, tneg))
            goto bad;
        arith(p);
        *p = *p->left;
        p->addable = 0;
        break;

    case ONEG:              /* -x = 0-x */
        if (tconv(l))
            goto bad;
        if (tcompat(p, NULL, l->type, tneg))
            goto bad;
        if (typefd[l->type->etype]) {
            r = l;
            l = node1(OCONST, NULL, NULL);
            l->type = types[TINT];
            l->v.i = 0;
            p->op = OSUB;
            p->left = l;
            p->right = r;
        }
        arith(p);
        break;
        
    case OCOM:
        if (tconv(l))
            goto bad;
        if (tcompat(p, l->type, types[TINT], tand))
            goto bad;
        arith(p);
        break;
        
    case ONOT:
        if (tconv(l))
            goto bad;
        if (tcompat(p, NULL, l->type, tnot))
            goto bad;
        arith(p);
        p->type = types[TINT];
        break;
        
    case OSIZEOF:
        if (l != NULL) {
            if (tconvo(l, 0))
                goto bad;
            if (p->op == OBIT) {
                error(p, "'sizeof' bitfield");
                goto bad;
            }
            p->type = l->type;
        }
        if (p->type == NULL)
            goto bad;
        if (p->type->etype == TFUNC) {
            error(p, "'sizeof' function");
            goto bad;
        }
        if (p->type->size < 1) {
            error(p, "'sizeof' incomplete type '%T'", p->type);
            goto bad;
        }
        p->op = OCONST;
        p->left = NULL;
        p->right = NULL;
        p->v.i = p->type->size;
        p->type = types[TULONG];
        break;

    case OADDR:
        if (tconvo(l, ADDROP))
            goto bad;
        if (tlvalue(l))
            goto bad;
        if (l->type->nbits) {
            error(p, "address of a bit-field");
            goto bad;
        }
        /* TODO: CREGISTER ignore... */
        if (l->op == ONAME && l->sclass == CREGISTER) {
            error(p, "address of a register");
            goto bad;
        }
        p->type = ptrtyp(l->type);
        break;
        
    case OINDIR:
        if (tconv(l))
            goto bad;
        if (tcompat(p, NULL, l->type, tindir))
            goto bad;
        p->type = l->type->link;
        p->addable = 1;
        break;

    case OPREINC:
    case OPREDEC:
    case OPOSTINC:
    case OPOSTDEC:
        if (tconv(l))
            goto bad;
        if (tlvalue(l))
            goto bad;
        if (tcompat(p, l->type, types[TINT], tadd))
            goto bad;
        p->type = l->type;
        if (p->type->etype == TPTR)
            if (p->type->link->size < 1) {
                error(p, "inc/dec of incomplete size");
                goto bad;
            }
        break;
        
    case ODOT:
        if (tconv(l))
            goto bad;
        if (tcompat(p, NULL, l->type, tdot))
            goto bad;
        if (tconvd(p))
            goto bad;
        break;
        
    case OFUNC:
        if (tconvo(l, 0))
            goto bad;
        if (l->type->etype == TPTR && l->type->link->etype == TFUNC) {
            l = node(OINDIR, l, NULL);
            l->type = l->left->type->link;
            p->left = l;
        }
        if (tcompat(p, NULL, l->type, tfunc))
            goto bad;
        if (tconva(l, r, l->type->next, 1))
            goto bad;
        p->type = l->type->link;
        if (tconvb(p))
            goto bad;
        if (p->type->etype != TVOID && p->type->size < 1) {
            error(p, "function with incomplete type '%T'", p->type);
            goto bad;
        }
        break;
        
    case ONAME:
        if (p->type == NULL) {
            error(p, "'%s' not declared", p->sym->name);
            goto bad;
        }
        if (p->sclass == CENUM) {
            p->op = OCONST;
            p->type = p->type->link;
            p->v.i = p->sym->vconst;
            break;
        }
        p->addable = 1;
        break;
        
    case OSTRING:
        p->offset = gstring(p->v.cstring, p->type->size);
        p->addable = 1;
        p->op = ONAME;
        break;

    case OCOMPOUND:
        if (tconvcp(l))
            goto bad;
        p->addable = 1;
        break;

    case OCONST:
        break;

    case OZERO:
        if (tconv(l) | tconv(r))
            goto bad;
        p->type = types[TVOID];
        break;

    case OPROTO:
        /* tconv has been called on subtree */
        *p = *p->left;
        if (p->type == NULL)
            goto bad;
        break;

    default:
        error(p, "unknown op(%O) in type conv", p->op);
        goto bad;
    }

    ty = p->type;
    if (ty == NULL)
        goto bad;
    if (typeaf[ty->etype]) {
        if (f & ADDROF)
            goto addrof;
    }
    return 0;

addrof:
    if (tlvalue(p))
        goto bad;
    l = node(OXXX, NULL, NULL);
    *l = *p;
    p->op = OADDR;
    if (l->type->etype == TARRAY)
        l->type = l->type->link;
    p->left = l;
    p->right = NULL;
    p->addable = 0;
    p->type = ptrtyp(l->type);
    return 0;
    
bad:
    p->type = NULL;
    return 1;
}

int tconv(struct node *p)
{
    return tconvo(p, ADDROF);
}

/* has side effect? return 0-no, 1-yes */
static int side(struct node *p)
{
loop:
    if (p)
        switch (p->op) {
        case OCAST:
        case OPOS:
        case ONEG:
        case OCOM:
        case ONOT:
        case OADDR:
        case OINDIR:
        case ODOT:
        case OBIT:
            p = p->left;
            goto loop;

        case OCOND:
            if (side(p->left))
                break;
            p = p->right;
            /* fall thru */
            
        case OCOMMA:
        case OADD:
        case OSUB:
        case OMUL:
        case OUMUL:
        case ODIV:
        case OUDIV:
        case OMOD:
        case OUMOD:
        case OAND:
        case OOR:
        case OSHL:
        case OSHR:
        case OUSHR:
        case OXOR:
        case OLT:
        case OLE:
        case OGT:
        case OGE:
        case OEQ:
        case ONE:
        case OANDAND:
        case OOROR:
            if (side(p->left))
                break;
            p = p->right;
            goto loop;

        case ONAME:
        case OCONST:
        case OSTRING:
        case OSIZEOF:
            return 0;
        }
    return 1;
}

static void evconst(struct node *p)
{
    struct node *l, *r;
    int et, isf, isbool;
    long i;
    double d;
    typedef unsigned long U;
    
    if (p == NULL || p->type == NULL)
        return;

    et = p->type->etype;
    isf = typefd[et];
    isbool = et == TBOOL;
    l = p->left;
    r = p->right;
    i = 0;
    d = 0;
    
    switch (p->op) {
    default:
        return;

    case ONEG:
        if (isf)
            d = -l->v.d;
        else
            i = -l->v.i;
        break;

    case OCOM:
        i = ~l->v.i;
        break;
        
    case ONOT:
        if (typefd[l->type->etype])
            i = !l->v.d;
        else
            i = !l->v.i;
        break;

    case OCAST:
        if (et == TVOID)
            return;
        et = l->type->etype;
        /* handle cast to bool */
        if (isbool) {
            if (typefd[et])
                i = l->v.d != 0;
            else
                i = l->v.i != 0;
        } else if (isf) {
            if (typefd[et])
                d = l->v.d;
            else
                d = l->v.i;
        } else {
            if (typefd[et])
                i = l->v.d;
            else
                i = convltox(l->v.i, p->type->etype);
        }
        break;

    case OADD:
        if (isf)
            d = l->v.d + r->v.d;
        else
            i = l->v.i + r->v.i;
        break;
        
    case OSUB:
        if (isf)
            d = l->v.d - r->v.d;
        else
            i = l->v.i - r->v.i;
        break;
        
    case OMUL:
        if (isf)
            d = l->v.d * r->v.d;
        else
            i = l->v.i * r->v.i;
        break;
        
    case OUMUL:
        i = (U)l->v.i * (U)r->v.i;
        break;
        
    case ODIV:
        if (vconst(r) == 0) {
            warn(p, "divide by zero");
            return;
        }
        if (isf)
            d = l->v.d / r->v.d;
        else
            i = l->v.i / r->v.i;
        break;
        
    case OUDIV:
        if (vconst(r) == 0) {
            warn(p, "divide by zero");
            return;
        }
        i = (U)l->v.i / (U)r->v.i;
        break;
        
    case OMOD:
        if (vconst(r) == 0) {
            warn(p, "modulo by zero");
            return;
        }
        i = l->v.i % r->v.i;
        break;
        
    case OUMOD:
        if (vconst(r) == 0) {
            warn(p, "modulo by zero");
            return;
        }
        i = (U)l->v.i % (U)r->v.i;
        break;
        
    case OAND:
        i = l->v.i & r->v.i;
        break;
        
    case OOR:
        i = l->v.i | r->v.i;
        break;
        
    case OSHL:
        i = l->v.i << r->v.i;
        break;
        
    case OSHR:
        i = l->v.i >> r->v.i;
        break;
        
    case OUSHR:
        i = (U)l->v.i >> r->v.i;
        break;
        
    case OXOR:
        i = l->v.i ^ r->v.i;
        break;
        
    case OLT:
        if (typefd[l->type->etype])
            i = l->v.d < r->v.d;
        else
            i = l->v.i < r->v.i;
        break;
        
    case OLE:
        if (typefd[l->type->etype])
            i = l->v.d <= r->v.d;
        else
            i = l->v.i <= r->v.i;
        break;
        
    case OGT:
        if (typefd[l->type->etype])
            i = l->v.d > r->v.d;
        else
            i = l->v.i > r->v.i;
        break;
        
    case OGE:
        if (typefd[l->type->etype])
            i = l->v.d >= r->v.d;
        else
            i = l->v.i >= r->v.i;
        break;
        
    case OEQ:
        if (typefd[l->type->etype])
            i = l->v.d == r->v.d;
        else
            i = l->v.i == r->v.i;
        break;
        
    case ONE:
        if (typefd[l->type->etype])
            i = l->v.d != r->v.d;
        else
            i = l->v.i != r->v.i;
        break;
        
    case OANDAND:
        if (typefd[l->type->etype]) {
            if (typefd[r->type->etype])
                i = l->v.d && r->v.d;
            else
                i = l->v.d && r->v.i;
        } else {
            if (typefd[r->type->etype])
                i = l->v.i && r->v.d;
            else
                i = l->v.i && r->v.i;
        }
        break;
        
    case OOROR:
        if (typefd[l->type->etype]) {
            if (typefd[r->type->etype])
                i = l->v.d || r->v.d;
            else
                i = l->v.d || r->v.i;
        } else {
            if (typefd[r->type->etype])
                i = l->v.i || r->v.d;
            else
                i = l->v.i || r->v.i;
        }
        break;
    }
    if (isf)
        p->v.d = d;
    else
        p->v.i = convltox(i, p->type->etype);
    p->op = OCONST;
}

/* a cast that generates no code? */
static int nocast(struct type *ty1, struct type *ty2)
{
    int i, b;
    
    if (ty1->nbits)
        return 0;
    i = 0;
    if (ty2)
        i = ty2->etype;
    b = 1 << i;
    i = 0;
    if (ty1)
        i = ty1->etype;
    if (b & ncast[i])
        return 1;
    return 0;
}

/*
 * INDIR(ADDR x) ==> x
 * ADDR(INDIR x) ==> x
 * ADD(ADDR x, CONST) ==> ADDR(x+CONST)
 * ADD(CONST, ADDR x) ==> ADDR(x+CONST)
 * SUB(ADDR x, CONST) ==> ADDR(x-CONST)
 * fold constants
 */
static void cconv(struct node *p)
{
    struct node *l, *r;
    int c;

loop:
    if (p == NULL)
        return;
    l = p->left;
    r = p->right;
    
    switch (p->op) {
    case OCONST:
    case ONAME:
        break;

    case OAS:
    case OASMUL:
    case OASUMUL:
    case OASADD:
    case OASSUB:
    case OASDIV:
    case OASUDIV:
    case OASMOD:
    case OASUMOD:
    case OASXOR:
    case OASAND:
    case OASOR:
    case OASSHL:
    case OASSHR:
    case OASUSHR:
        cconv(l);
        cconv(r);
        if (p->op == OASMOD || p->op == OASUMOD || p->op == OASDIV || p->op == OASUDIV)
            if (vconst(r) == 0) {
                if (p->op == OASMOD || p->op == OASUMOD)
                    warn(p, "modulo by zero");
                if (p->op == OASDIV || p->op == OASUDIV)
                    warn(p, "divide by zero");
            }
        break;

    case OADDR:
        cconv(l);
        if (l->op == OINDIR) {
            l->left->type = p->type;
            *p = *l->left;
            break;
        }
        goto common;
        
    case OINDIR:
        cconv(l);
        if (l->op == OADDR) {
            l->left->type = p->type;
            *p = *l->left;
            break;
        }
        goto common;

    case OCOND:
        cconv(l);
        cconv(r);
        if (l->op == OCONST) {
            if (vconst(l) == 0)
                *p = *r->right;
            else
                *p = *r->left;
        }
        break;
        
    case OCAST:
        cconv(l);
        if (l->op == OCONST) {
            evconst(p);
            if (p->op == OCONST)
                break;
        }
        /* handle cast to bool */
        if (p->type->etype == TBOOL)
            if (l->type->etype != TBOOL)
                break;
        if (nocast(l->type, p->type)) {
            l->type = p->type;
            *p = *l;
        }
        break;

    case OMUL:
    case OUMUL:
        cconv(l);
        c = vconst(l);
        if (c == 0 && !side(r)) {
            *p = *l;
            break;
        }
        if (c == 1) {
            *p = *r;
            goto loop;
        }
        cconv(r);
        c = vconst(r);
        if (c == 0 && !side(l)) {
            *p = *r;
            break;
        }
        if (c == 1) {
            *p = *l;
            break;
        }
        goto commute;
        
    case OSHL:
    case OSHR:
    case OUSHR:
        cconv(l);
        if (vconst(l) == 0 && !side(r)) {
            *p = *l;
            break;
        }
        cconv(r);
        if (vconst(r) == 0) {
            *p = *l;
            break;
        }
        if (r->op == OCONST) {
            c = p->type->size * 8;
            if (r->v.i >= c || r->v.i <= -c)
                warn(p, "shift count overflow: %ld", r->v.i);
        }
        goto common;

    case ODIV:
    case OUDIV:
        cconv(l);
        if (vconst(l) == 0 && !side(r)) {
            *p = *l;
            break;
        }
        cconv(r);
        c = vconst(r);
        if (c == 1) {
            *p = *l;
            break;
        }
        if (c == 0) {
            warn(p, "divide by zero");
            break;
        }
        goto common;
        
    case OSUB:
        cconv(r);
        if (r->op == OCONST) {
            if (typefd[r->type->etype]) {
                p->op = OADD;
                r->v.d = -r->v.d;
                goto loop;
            } else {
                p->op = OADD;
                r->v.i = -r->v.i;
                goto loop;
            }
        }
        cconv(l);
        goto common;

    case OOR:
    case OXOR:
    case OADD:
        cconv(l);
        if (vconst(l) == 0) {
            *p = *r;
            goto loop;
        }
        cconv(r);
        if (vconst(r) == 0) {
            *p = *l;
            break;
        }
        if (l->op == OADDR && r->op == OCONST) {
            l->type = p->type;
            l->left->offset += r->v.i;
            *p = *l;
            break;
        }
        if (r->op == OADDR && l->op == OCONST) {
            r->type = p->type;
            r->left->offset += l->v.i;
            *p = *r;
            break;
        }
        goto commute;

    case OAND:
        cconv(l);
        cconv(r);
        if (vconst(l) == 0 && !side(r)) {
            *p = *l;
            break;
        }
        if (vconst(r) == 0 && !side(l)) {
            *p = *r;
            break;
        }
        /* fall thru */
        
    commute:
        if (r->op == OCONST) {
            if (l->op == p->op) {
                if (l->left->op == OCONST) {
                    p->right = l->right;
                    l->right = r;
                    goto loop;
                }
                if (l->right->op == OCONST) {
                    p->right = l->left;
                    l->left = r;
                    goto loop;
                }
            }
        }
        if (l->op == OCONST) {
            if (r->op == p->op) {
                if (r->left->op == OCONST) {
                    p->left = r->right;
                    r->right = l;
                    goto loop;
                }
                if (r->right->op == OCONST) {
                    p->left = r->left;
                    r->left = l;
                    goto loop;
                }
            }
        }
        goto common;

    case OANDAND:
        cconv(l);
        if (vbconst(l) == 0) {
            p->op = OCONST;
            p->v.i = 0;
            p->left = NULL;
            p->right = NULL;
            break;
        }
        cconv(r);
        goto common;

    case OOROR:
        cconv(l);
        if (vbconst(l) == 1) {
            p->op = OCONST;
            p->v.i = 1;
            p->left = NULL;
            p->right = NULL;
            break;
        }
        cconv(r);
        goto common;

    case OCOMMA:
        cconv(l);
        if (!side(l)) {
            *p = *r;
            goto loop;
        }
        cconv(r);
        goto common;

    default:
        if (l)
            cconv(l);
        if (r)
            cconv(r);
    common:
        if (l && l->op != OCONST)
            break;
        if (r && r->op != OCONST)
            break;
        evconst(p);
    }
}

/*
 * fold constants in commute
 * ADD(CONST, x) ==> ADD(x, CONST)
 * MUL(CONST, x) ==> MUL(x, CONST)
 * AND(CONST, x) ==> AND(x, CONST)
 * OR(CONST, x) ==> OR(x, CONST)
 * XOR(CONST, x) ==> XOR(x, CONST)
 */
static void aconv(struct node *p)
{
    struct node *l, *r, *q;

loop:
     if (p == NULL)
        return;

    l = p->left;
    r = p->right;
    
    switch (p->op) {
    case OADD:
    case OMUL:
    case OUMUL:
    case OAND:
    case OOR:
    case OXOR:
        aconv(l);
        aconv(r);
        if (r->op == p->op) {
            q = r;
            l = r->left;
            r = r->right;
            q->left = p->left;
            q->right = l;
            p->left = q;
            p->right = r;
            goto loop;
        }
        if (l->op == p->op) {
            if (l->right->op == OCONST) {
                if (r->op == OCONST) {
                    p->left = l->left;
                    l->left = r;
                    p->right = l;
                    evconst(l);
                } else {
                    p->right = l->right;
                    l->right = r;
                }
            }
        } else if (l->op == OCONST) {
            p->left = r;
            p->right = l;
        }
        break;

    default:
        if (l)
            aconv(l);
        if (r)
            aconv(r);
        break;
    }
}

void conv(struct node *p)
{
    if (p == NULL)
        return;
    nearln = p->line;
    if (options.debug)
        prtree(p, "pre conv");
    if (tconv(p))
        return;
    if (options.debug)
        prtree(p, "tconv");
    cconv(p);
    if (options.debug)
        prtree(p, "cconv");
    aconv(p);
    if (options.debug)
        prtree(p, "aconv");
    gconv(p);
    if (options.debug)
        prtree(p, "gconv");
}