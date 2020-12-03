#include "cc.h"

static struct node *parse_initializer(void);
static struct node *parse_expr(void);
static struct node *parse_assign_expr(void);
static struct node *parse_cond_expr(void);
static struct node *parse_stmt(void);
static struct type *parse_typename(void);
static struct type *parse_tag(int);
static struct type *parse_declarator(struct type *, struct symbol **, 
                                     struct node **, int);
static struct node *parse_decls(struct node *(*)(struct symbol *, struct type *, int));
static struct node *pdecl(struct symbol *, struct type *, int);
static struct node *gdecl(struct symbol *, struct type *, int);
static struct node *ldecl(struct symbol *, struct type *, int);
static struct node *init0(struct symbol *, struct type *, long, struct node *);
static struct node *doinit(struct symbol *, struct type *, struct node *);
static struct node *dostring(struct token *, int, int);
static int parse_int(int);

static struct token tokens[2];
static struct token *tok = &tokens[0]; /* current token */
static char peeked;
static int autobn, blockno;
static struct decl *dclstack, *freedcls, *firstdcl;
struct type *thisfn;            /* current function type */
static const char *funcname;    /* current function name */
long stkoffset;                 /* stack offset */
static struct node *initlist;
static long initoffset;
static char predeftab[NBUILTIN];
int nearln;

static void get_token(struct token *result)
{
    static char seen_eof;

    if (seen_eof) {
        result->id = TOK_EOF;
        return;
    }

    cpp_get_token(result);
    if (result->id == TOK_EOF)
        seen_eof = 1;

    if (result->id == TOK_NAME && result->val.sym->keyword_id)
        result->id = result->val.sym->keyword_id;
}

static struct token *peektok(void)
{
    if (peeked == 0) {
        get_token(&tokens[1]);
        peeked = 1;
    }
    return &tokens[1];
}

static int gettok(void)
{
    if (peeked) {
        tokens[0] = tokens[1];
        peeked = 0;
    } else {
        get_token(&tokens[0]);
    }
    return tokens[0].id;
}

static void skipto(int (*first)(struct token *))
{
    while (1) {
        if (tok->id == TOK_EOF)
            break;
        if (first(tok))
            break;
        gettok();
    }
}

/* [], {}, () */
static void skip_balance(int l, int r)
{
    int nests = 0;

    while (1) {
        if (tok->id == TOK_EOF)
            break;
        if (tok->id == r) {
            if (nests-- == 0)
                break;
        } else if (tok->id == l) {
            nests++;
        }
        gettok();
    }

    if (tok->id == r)
        gettok();
}

static void match1(int id, int balance)
{
    if (id == tok->id) {
        gettok();
    } else {
        if (id != TOK_EOF)
            error(0, "expect token '%s' before '%s'", lexmes[id], tok2s(tok));
        else
            error(0, "expect token '%s'", lexmes[id]);

        if (balance) {
            if (id == ')') skip_balance('(', ')');
            if (id == '}') skip_balance('{', '}');
            if (id == ']') skip_balance('[', ']');
        }
    }
}

static void expect(int id)
{
    match1(id, 0);
}

static void match(int id)
{
    match1(id, 1);
}

static int first_typename(struct token *p)
{
    return kinds[p->id] == TOK_INT ||
        kinds[p->id] == TOK_CONST ||
        (p->id == TOK_NAME && p->val.sym->sclass == CTYPEDEF);
}

static int first_decl(struct token *p)
{
    return kinds[p->id] == TOK_STATIC || first_typename(p);
}

static int first_expr(struct token *p)
{
    return kinds[p->id] == TOK_NAME;
}

static int first_stmt(struct token *p)
{
    return kinds[p->id] == TOK_IF || first_expr(p);
}

static struct decl *push_decl(void)
{
    struct decl *d;

    if (freedcls) {
        d = freedcls;
        freedcls = freedcls->link;
    } else {
        d = allocate(sizeof(struct decl), PERM);
    }

    d->link = dclstack;
    dclstack = d;
    return d;
}

static struct decl *push_sym(struct symbol *s)
{
    struct decl *d;

    d = push_decl();
    d->kind = DAUTO;
    d->sym = s;
    d->type = s->type;
    d->sclass = s->sclass;
    d->offset = s->offset;
    d->block = s->block;
    d->vconst = s->vconst;
    d->line = s->line;
    d->defined = s->defined;
    d->used = s->used;
    
    return d;
}

static void enterscope(void)
{
    struct decl *d;

    blockno++;
    d = push_decl();
    d->kind = DMARK;
    d->block = autobn;
    autobn = blockno;
}

static void exitscope(void)
{
    struct decl *d;
    struct symbol *s;

    while (1) {
        d = dclstack;
        if (d == NULL) {
            error(0, "pop nil dcl stack");
            break;
        }

        dclstack = d->link;
        d->link = freedcls;
        freedcls = d;
        s = d->sym;
        switch (d->kind) {
        case DMARK:
            autobn = d->block;
            return;

        case DAUTO:
            if (s->used == 0) {
                nearln = s->line;
                if (s->sclass == CAUTO && !s->anonymous)
                    warn(0, "variable '%s' not used", s->name);
            }
            s->type = d->type;
            s->sclass = d->sclass;
            s->offset = d->offset;
            s->block = d->block;
            s->vconst = d->vconst;
            s->line = d->line;
            s->defined = d->defined;
            s->used = d->used;
            break;
            
        case DSUE:
            s->suetype = d->type;
            s->sueblock = d->block;
            break;

        case DLABEL:
            if (s->label && s->label->addable == 0)
                warn(s->label, "label declared and not used: %s", s->name);
            s->label = NULL;
            break;

        default:
            abort();
        }
    }
}

/* save the element count to `size` field temporarily  */
static void doarray(struct type *ty, struct node *p)
{
    conv(p);
    if (!p || !p->type || !typei[p->type->etype] || p->op != OCONST) {
        error(p, "expect integer constant");
        goto bad;
    }
    if (p->v.i < 0) {
        error(p, "array has negative size '%ld'", p->v.i);
        goto bad;
    }
    ty->size = p->v.i;
    return;
bad:
    ty->size = 1;
}

static struct node *doproto(struct node *params)
{
    int i;
    struct node *q;

    if (params == NULL)
        return NULL;

    for (i = 0, q = params; q; q = q->right, i++) {
        struct node *p = q->left;
        if (p->type->etype != TVOID || p->complex)
            continue;            
        if (i > 0) {
            error(p, "'void' must be the first and only parameter");
            p->type = types[TINT];
        } else if (p->sym) {
            error(p, "argument may not have 'void' type");
            p->type = types[TINT];
        } else if (p->type->qual != QXXX) {
            error(p, "'void' as parameter can't have type qualifier");
            p->type->qual = QXXX;
        }
    }

    if (i > 1 && params->left->type->etype == TVOID) {
        error(params->left, "'void' must be the first and only parameter");
        params->right = NULL;
    }

    return params;
}

static struct type *dotag(struct symbol *s, int et, int bn)
{
    if (bn != 0 && bn != s->sueblock) {
        struct decl *d = push_decl();
        d->kind = DSUE;
        d->sym = s;
        d->type = s->suetype;
        d->block = s->sueblock;
        s->suetype = NULL;
    }
    if (s->suetype == NULL) {
        s->suetype = suetyp(et);
        s->sueblock = autobn;
    }
    if (s->suetype->etype != et)
        error(0, "tag '%s' used for more than one type", s->name);
    if (s->suetype->tag == NULL)
        s->suetype->tag = s;

    return s->suetype;
}

static void doenum(struct symbol *s, int val, struct type *sty)
{
    if (autobn == s->block && s->type)
        error(0, "'%s' redeclared", s->name);

    if (dclstack)
        push_sym(s);
    s->type = typcpy(sty);
    s->type->next = NULL;
    s->type->align = s->type->link->align;
    s->type->size = s->type->link->size;
    s->sclass = CENUM;
    s->vconst = val;
    s->block = autobn;
    if (options.debug)
        prdecl(s);
}

static void test_indir(struct type *flist, struct type *ty)
{
    struct type *p;

    for (p = ty->link; p; p = p->next)
        if (p->sym) {
            if (find_field(p->sym, flist, NULL))
                error(0, "field '%s' redeclared", p->sym->name);
        } else if (typesu[p->etype]) {
            test_indir(flist, p);
        }
}

static struct type *dofield(struct symbol *s, struct type *ty, int nbits, 
                            int has_bit, struct type *flist)
{
    if (has_bit) {
        if (!typei[ty->etype]) {
            error(0, "bit-field type must be integer");
            ty = types[TINT];
        }
        if (nbits < 0) {
            error(0, "bit-field has negative width '%d'", nbits);
            nbits = 1;
        }
        if (nbits == 0 && s) {
            error(0, "named bit-field '%s' has zero width", s->name);
            nbits = 1;
        }
        if (nbits > 8 * ty->size) {
            error(0, "bit-field size exceeds");
            nbits = 8 * ty->size;
        }
        nbits += 1;     /* mark has_bit */
    }

    if (!s && !has_bit) {
        if (!typesu[ty->etype] || !ty->tag->anonymous) {
            warn(0, "declare nothing");
            return NULL;
        }
        test_indir(flist, ty);        
    }

    if (ty->etype == TFUNC) {
        error(0, "field has function type");
        ty = ptrtyp(ty);
    }

    if (s && find_field(s, flist, NULL))
        error(0, "field '%s' redeclared", s->name);

    ty = typcpy(ty);
    ty->next = NULL;
    ty->sym = s;
    ty->nbits = nbits;
    return ty;
}

static void exitparams(struct node *params)
{
    assert(params);
    if (params->left && !params->left->type)
        error(params->left, "extraneous old-style parameter list");
    exitscope();
}

/*
 * merge test for global symbol 's' with type 'ty' and sclass 'c'
 *
 * c\s        XXX    TYPEDEF    STATIC    EXTERN
 * XXX        ok      -           -        ok
 * TYPEDEF    -       ok          -        -
 * STATIC     -       -           ok       -
 * EXTERN     ok      -           ok       ok
 */
static int tmerge(struct symbol *s, struct type *ty, int c)
{
    if (c == s->sclass) {
        if (c == CTYPEDEF)
            if (!eqtype(s->type, ty, 0) || ty->oldstyle != s->type->oldstyle)
                goto bad;

        if (!eqtype(s->type, ty, 0))
            goto bad;

        return 0;
    }

    if ((c == CEXTERN && s->sclass != CTYPEDEF) ||
        (c == CGLOBAL && s->sclass == CEXTERN))
        if (eqtype(s->type, ty, 0))
            return 0;

bad:
    error(0, "'%s' redeclared", s->name);
    return 1;
}

/* complete old-style parameter's type */
static void walkoparams(struct node *params)
{
    struct decl *d;
    struct node *p;
    struct symbol *s;
    
    for (d = dclstack; d->kind != DMARK; d = d->link) {
        s = d->sym;
        nearln = s->line;
        if (s->type->etype == TVOID) {
            error(0, "argument may not have 'void' type");
            s->type = types[TINT];
        }
        for (p = params; p; p = p->right)
            if (p->left->sym == s) {
                if (p->left->type)
                    error(0, "redefinition of '%s'", s->name);
                p->left->type = s->type;
                p->left->sclass = s->sclass;
                break;
            }
        if (p == NULL)
            error(0, "parameter name '%s' is missing", s->name);
    }
    for (p = params; p; p = p->right)
        if (p->left->type == NULL)
            p->left->type = types[TINT];
}

static void dodecl(struct node *p, int f)
{
    if (p->sclass == CPARAM || p->sclass == CTYPEDEF)
        return;
    if (p->sclass == CAUTO && p->type->size < 1)
        error(p, "variable '%s' has incomplete type '%T'", p->sym->name, p->type);
    if (p->sym->block == 0) {
        if (p->sym->defined && f)
            error(p, "redefinition of '%s'", p->sym->name);
        if (f)
            p->sym->defined = 1;
    }
}

static struct node *pdecl(struct symbol *s, struct type *ty, int c)
{
    struct node *p;
    
    if (c != CXXX && c != CREGISTER)
        error(0, "illegal storage class '%s' for parameter", cnames[c]);
    
    ty = decay(ty);
    if (s) {
        push_sym(s);            /* for walkoparams */
        s->type = ty;
        s->sclass = CPARAM;
        s->line = lineno;
    }

    p = node(OPROTO, NULL, NULL);
    p->sym = s;
    p->type = ty;
    p->sclass = CPARAM;
    return p;
}

static struct node *gdecl(struct symbol *s, struct type *ty, int c)
{
    struct node *p;
    assert(autobn == 0);

    if (c == CAUTO || c == CREGISTER) {
        error(0, "illegal storage class '%s' in file scope", cnames[c]);
        c = CGLOBAL;
    }
    if (c == CXXX)
        c = CGLOBAL;

    if (s->type)
        if (tmerge(s, ty, c) == 0) {
            ty = compose(ty, s->type);
            if (c == CEXTERN)
                c = s->sclass;
        }

    s->type = ty;
    s->block = autobn;
    s->sclass = c;
    s->offset = 0;
    s->line = nearln;

    p = node(ONAME, NULL, NULL);
    p->sym = s;
    p->type = s->type;
    p->sclass = s->sclass;
    p->offset = s->offset;
    p->line = s->line;
    if (options.debug)
        prdecl(s);
    return p;
}

static struct node *ldecl(struct symbol *s, struct type *ty, int c)
{
    struct node *p;
    
    if (ty->etype == TFUNC) {
        if (c != CXXX && c != CEXTERN)
            error(0, "illegal storage class '%s' for function", cnames[c]);
        c = CEXTERN;
    }
    if (c == CXXX || c == CREGISTER)
        c = CAUTO;

    if (s->block == autobn) {
        if (s->sclass == CTYPEDEF && c == CTYPEDEF) {
            if (!eqtype(s->type, ty, 0) || ty->oldstyle != s->type->oldstyle)
                error(0, "'%s' declared with more than one type", s->name);
        } else if (s->sclass == CEXTERN && c == CEXTERN) {
            if (!eqtype(s->type, ty, 0))
                error(0, "'%s' declared with more than one type", s->name);
        } else {
            error(0, "'%s' redeclared", s->name);
        }
    }

    push_sym(s);
    s->type = ty;
    s->block = autobn;
    s->sclass = c;
    s->offset = 0;
    s->used = 0;
    s->line = nearln;

    if (c == CAUTO) {
        stkoffset = ROUNDUP(stkoffset, MAX(ty->size, 1));
        stkoffset += ty->size;
        s->offset = -stkoffset;
    }
    if (c == CSTATIC)
        s = mkstatic(s);

    p = node(ONAME, NULL, NULL);
    p->sym = s;
    p->type = s->type;
    p->sclass = s->sclass;
    p->offset = s->offset;
    p->line = s->line;
    if (options.debug)
        prdecl(s);
    return p;
}

/*
 * Predefined identifier: __func__
 * The identifier `__func__' is implicitly declared by C99
 * implementations as if the following declaration appeared
 * after the opening brace of each function definition:
 *
 * static const char __func__[] = "this-function-name";
 */
static void markpredef(void)
{
    struct symbol *s;

    s = lookup("__func__", OPT_CREATE);
    push_sym(s);

    s->type = arraytyp(qual(QCONST, types[TCHAR]), 0);
    s->sclass = CSTATIC;
    s->block = autobn;
    s->offset = 0;
    s->used = 0;
    s->line = lineno;
}

static void predefine(struct symbol *s)
{
    if (thisfn == NULL)
        return;
    if (predeftab[s->builtin_id])
        return;

    predeftab[s->builtin_id] = 1;

    if (s->builtin_id == BUILTIN_FUNC) {
        struct token t;
        struct node *init;

        t.id = TOK_SCON;
        t.val.str = stringf("\"%s\"", funcname);
        init = dostring(&t, 1, 0);
        doinit(s, s->type, init);
        if (options.debug)
            prdecl(s);
    }
}

static void fdefn(struct node *fn, struct node *params)
{
    struct node *p;

    thisfn = fn->type;
    funcname = fn->sym->name;
    stkoffset = 0;
    memset(predeftab, 0, sizeof(predeftab));

    for (p = params; p; p = p->right) {
        struct node *q = p->left;
        if (q->type->etype == TVOID)
            continue;
        if (q->sym) {
            nearln = q->line;
            q = ldecl(q->sym, q->type, q->sclass);
            q->sclass = CAUTO;
            dodecl(q, 1);
            q->sclass = CPARAM;
            p->left = q;
        } else {
            error(q, "parameter name missing");
        }
    }

    /* set proto only if non-empty */
    if (fn->type->oldstyle && params) {
        struct type *proto = NULL;
        struct type **pp = &proto;
        for (p = params; p; p = p->right) {
            struct type *ty = typcpy(p->left->type);
            *pp = ty;
            pp = &ty->next;
        }
        fn->type->next = proto;
    }
}

static void symadjust(struct symbol *s, struct node *p, long del)
{
    switch (p->op) {
    default:
        if (p->left)
            symadjust(s, p->left, del);
        if (p->right)
            symadjust(s, p->right, del);
        return;

    case ONAME:
        if (p->sym == s)
            p->offset -= del;
        return;

    case OCONST:
    case OSTRING:
        return;
    }
}

/* handle zero length array */
static void stkadjust(struct symbol *s, struct node *p, long v)
{
    long w;

    if (p == NULL)
        return;

    w = s->type->size;
    if (w != v) {
        if (v != 0)
            error(p, "adjust non-zero length array '%s'", s->name);
        v = s->offset;
        stkoffset = ROUNDUP(stkoffset, MAX(s->type->size, 1));
        stkoffset += s->type->size;
        s->offset = -stkoffset;
        symadjust(s, p, v - s->offset);
        if (options.debug)
            prtree(p, "adjust value");
    }
}

static struct node *peekinit(void)
{
    struct node *p;

    p = initlist;
loop:
    if (p == NULL)
        return p;
    if (p->op == OLIST) {
        p = p->left;
        goto loop;
    }
    return p;
}

static struct node *nextinit(void)
{
    struct node *p, *n, *q;

    p = initlist;
    n = NULL;
    if (p == NULL)
        return p;
    if (p->op == OLIST) {
        n = p->right;
        p = p->left;
    }
    if (p->op == OBREAK) {
        p = p->left;
        q = node(OCONST, NULL, NULL);
        q->type = p->type->link;
        if (q->type->etype == TCHAR) {
            q->v.i = convltox(*p->v.cstring, TCHAR);
            p->v.cstring++;
        }
        if (q->type->etype == TINT) {
            q->v.i = convltox(*p->v.wstring, TINT);
            p->v.wstring++;
        }
        p->type->size -= q->type->size;
        if (p->type->size <= 0)
            initlist = n;
        return q;
    }
    initlist = n;

    return p;
}

static void skipinit(void)
{
    struct node *p;

loop:
    p = peekinit();
    if (p == NULL)
        return;
    if (p->op == OELEM || p->op == OARRAY) {
        nextinit();
        goto loop;
    }
    nextinit();
}

static int isstruct(struct type *ty, struct node *p)
{
    struct node *q;

    switch (p->op) {
    case OPROTO:
        q = p->left;
        if (q && q->type && eqtype(q->type, ty, 1))
            return 1;

    case OCOMPOUND:
        return eqtype(p->type, ty, 1);

    case OCONST:
    case OSTRING:
    case OELEM:
    case OARRAY:
    case OINIT:
        return 0;
    }

    q = node(OXXX, NULL, NULL);
    *q = *p;

    /* mark as OPROTO to prevent performing tconv twice */
    p->op = OPROTO;
    p->left = q;
    p->right = NULL;

    if (tconv(q))
        return 0;

    if (eqtype(q->type, ty, 1))
        return 1;
    
    return 0;
}

static struct node *initz(struct symbol *s, long o, long size)
{
    struct node *l, *r;

    if (size <= 0)
        return NULL;
    
    l = node(ONAME, NULL, NULL);
    l->sym = s;
    l->offset = s->offset + o;
    l->type = types[TCHAR];
    l->sclass = s->sclass;
    r = cnstnode(types[TLONG], size);
    return node(OZERO, l, r);
}

/* global data may generate relocations, so don't copy data simply. */
static struct node *initcp(struct node *l, struct node *r)
{
    struct node *p;

    if (r == NULL)
        return NULL;
    if (r->op == OLIST)
        return newlist(initcp(l, r->left), initcp(l, r->right));
    if (r->op == OAS || r->op == OASI) {
        p = node(OXXX, NULL, NULL);
        *p = *l;
        p->offset += r->left->offset;
        p->type = r->left->type;
        l = p;
        r = r->right;
    }
    if (eqtype(l->type, r->type, 1)) {
        p = node(OASI, l, r);
        p->type = l->type;
        return p;
    }
    return NULL;
}

static struct node *init1(struct symbol *s, struct type *ty, long o, int flag)
{
    struct node *p, *l, *r;
    struct type *ty1;
    long i, w, maxi;

    p = peekinit();
    if (p == NULL)
        return p;
    
    if (options.debug) {
        print("O=%ld, T=%T, s=%s, flag=%d\n", o, ty, s->name, flag);
        prtree(p, "init1 value");
    }

    if (flag && p->op == OINIT)
        return init0(s, ty, o, nextinit());
    
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
    case TFLOAT:
    case TDOUBLE:
    case TPTR:
    case TENUM:
    single:
        if (p->op == OELEM || p->op == OARRAY)
            return NULL;
        p = nextinit();
        if (p == NULL)
            return p;

        l = node(ONAME, NULL, NULL);
        l->sym = s;
        l->offset = s->offset + o;
        l->type = ty;
        l->sclass = s->sclass;
        l->line = p->line;

        if (s->sclass == CAUTO) {
            /* padding zero */
            r = NULL;
            /* handle bit-field hole */
            if (ty->nbits) {
                if (o + ty->size > initoffset)
                    r = initz(s, initoffset, o + ty->size - initoffset);
            } else {
                if (o > initoffset)
                    r = initz(s, initoffset, o - initoffset);
            }
            if (o + ty->size > initoffset)
                initoffset = o + ty->size;

            if (p->op == OCOMPOUND)
                return newlist(r, initcp(l, p->left));

            return newlist(r, node(OASI, l, p));
        }

        /* static extent */
        if (typesu[ty->etype] || ty->etype  == TARRAY) {
            if (p->op != OCOMPOUND)
                goto bad;
            return initcp(l, p->left);
        }

        p = node(OASI, l, p);
        conv(p);
        if (p->type == NULL)
            return NULL;
        if (ty->etype == TPTR) {
            if (p->right->op == OCONST || p->right->op == OADDR)
                return p;
            goto bad;
        }
        if (p->right->op == OCONST)
            return p;
    bad:
        error(p, "initializer is not a constant");
        return NULL;

    case TARRAY:
        w = ty->link->size;
        if (p->op == OSTRING)
            if (typei[ty->link->etype] && p->type->link->size == w) {
                p = nextinit();
                /* handle terminating zero warning */
                i = ty->size / w;
                maxi = p->type->size / p->type->link->size;
                if (i && maxi > i)
                    p->type->size -= p->type->link->size;
                /* treated as brace initlist */
                p = node(OBREAK, p, NULL);
                return init0(s, ty, o, p);
            }

        if (p->op == OCOMPOUND)
            if (eqtype(ty, p->type, 1)) {
                if (ty->size < 1)
                    ty->size = p->type->size;
                if (ty->size < 1)
                    error(p, "initialize with incomplete array '%T'", p->type);
                goto single;
            }

        if (p->op == OELEM) {
            error(p, "field designator for array type");
            skipinit();
        }

        l = NULL;
        i = maxi = 0;
        while (1) {
            p = peekinit();
            if (p == NULL)
                break;
            if (p->op == OELEM)
                break;
            if (p->op == OARRAY) {
                if (i && flag)
                    break;      /* belong to upper */
                nextinit();
                i = p->v.i;
                if (i < 0 || (ty->size > 0 && i * w >= ty->size)) {
                    error(p, "index '%ld' out of range", i);
                    skipinit();
                    continue;
                }
            }
            if (i > maxi)
                maxi = i;
            if (ty->size > 0)
                if (maxi * w >= ty->size)
                    break;
            r = init1(s, ty->link, o + i * w, 1);
            l = newlist(l, r);
            i++;
        }
        if (ty->size < 1)
            ty->size = (maxi + 1) * w;
        return l;

    case TSTRUCT:
    case TUNION:
        if (isstruct(ty, p))
            goto single;
        if (ty->size < 1) {
            error(p, "initialize incomplete strcuture");
            return NULL;
        }
        if (p->op == OARRAY) {
            error(p, "array designator for structure type");
            skipinit();
            p = peekinit();
            if (p == NULL)
                return p;
        }

        l = NULL;
    again:
        for (ty1 = ty->link; ty1; ty1 = ty1->next) {
            if (p->op == OARRAY)
                break;
            if (p->op == OELEM) {
                if (p->sym != ty1->sym)
                    continue;
                nextinit();
            }
            r = init1(s, ty1, o + ty1->offset, 1);
            l = newlist(l, r);
            if (ty->etype == TUNION)
                return l;
            p = peekinit();
            if (p == NULL)
                break;
            if (p->op == OELEM) {
                if (flag)
                    break;
                goto again;
            }
        }
        if (flag == 0 && p && p->op == OELEM)
            error(p, "field '%s' not found", p->sym->name);
        return l;

    default:
        error(p, "initialization to type '%T'", ty);
        return NULL;
    }
}

static struct node *init0(struct symbol *s, struct type *ty, long o, struct node *p)
{
    if (p == NULL)
        return p;

    if (options.debug) {
        print("O=%ld, T=%T, s=%s\n", o, ty, s->name);
        prtree(p, "init0 value");
    }

    struct node *saved_initlist = initlist;
    if (p->op == OINIT)
        p = p->left;
    initlist = p;
    
    p = init1(s, ty, o, 0);
    if (s->sclass == CAUTO && ty->size > initoffset)
        p = newlist(p, initz(s, initoffset, ty->size - initoffset));
    if (initlist != NULL)
        error(p, "excess elements in initializer");
    initlist = saved_initlist;

    return p;    
}

static struct node *doinit(struct symbol *s, struct type *ty, struct node *p)
{
    long v;

    if (s->sclass == CTYPEDEF) {
        error(p, "'typedef' has an initializer");
        return NULL;
    }
    if (s->sclass == CPARAM) {
        error(p, "parameter has an initializer");
        return NULL;
    }
    if (s->sclass == CEXTERN && s->block != 0) {
        error(p, "'extern' has an initializer");
        return NULL;
    }

    /* TODO: check top-level initializer */

    initoffset = 0;
    v = s->type->size;
    p = init0(s, ty, 0, p);
    if (options.debug)
        prtree(p, "doinit value");
    if (s->sclass == CAUTO) {
        stkadjust(s, p, v);
        return p;
    }
        
    gdata(s, ty, p);
    return NULL;
}

static struct node *dcllabel(struct symbol *s, int creat)
{
    struct decl *d, d1;
    struct node *n;
    
    if (s == NULL)
        return NULL;
    n = s->label;
    if (n) {
        if (creat) {
            if (n->complex)
                error(n, "label redeclared: %s", s->name);
            n->complex = 1;     /* declared */
        } else {
            n->addable = 1;     /* used */
        }
        return n;
    }

    d = push_decl();
    d->kind = DLABEL;
    d->sym = s;
    dclstack = d->link;         /* unlink from dclstack */

    d1 = *firstdcl;
    *firstdcl = *d;
    *d = d1;

    firstdcl->link = d;
    firstdcl = d;

    n = node(OXXX, NULL, NULL);
    n->sym = s;
    n->complex = creat;
    n->addable = !creat;
    s->label = n;
    return n;
}

static struct node *doname(struct symbol *s)
{
    struct node *p;
    struct type *ty;

    if (s->type == NULL && peektok()->id == '(') {
        ty = functyp(types[TINT], NULL, 1);
        if (autobn == 0)
            p = gdecl(s, ty, CEXTERN);
        else
            p = ldecl(s, ty, CEXTERN);
        s = p->sym;
        warn(p, "implicit declaration of function '%s'", s->name);
    }
    if (s->sclass == CSTATIC)
        s = mkstatic(s);
    s->used = 1;
    p = node(ONAME, NULL, NULL);
    p->sym = s;
    p->type = s->type;
    p->sclass = s->sclass;
    p->offset = s->offset;
    return p;
}

static struct node *donumber(struct token *t)
{
    static int _ll10[] = {TLLONG, -1};
    static int _l10[] = {TLLONG, TLONG, -1};
    static int _i10[] = {TLLONG, TLONG, TINT, -1};

    static int _ll[] = {TULLONG, TLLONG, -1};
    static int _l[] = {TULLONG, TLLONG, TULONG, TLONG, -1};
    static int _i[] = {TULLONG, TLLONG, TULONG, TLONG, TUINT, TINT, -1};

    static int _ull[] = {TULLONG, -1};
    static int _ul[] = {TULLONG, TULONG, -1};
    static int _u[] = {TULLONG, TULONG, TUINT, -1};
    
    struct svalue value;
    eval_number(t, &value);

    if (value.id == TOK_FCON) {
        switch (value.suffix) {
        case BFLOAT:
            return cnstnode(types[TFLOAT], value.d);
        case BLONG | BDOUBLE:
        default:
            return cnstnode(types[TDOUBLE], value.d);
        }
    }

    assert(value.id == TOK_ICON);
    unsigned long n = value.i;
    int *p = NULL;
    int t0 = TINT;

    switch (value.suffix) {
    case 0: default:
        if (value.base == 10)
            p = _i10;
        else
            p = _i;
        break;
    
    case BLLONG | BLONG:
        if (value.base == 10)
            p = _ll10;
        else
            p = _ll;
        break;
    
    case BLONG:
        if (value.base == 10)
            p = _l10;
        else
            p = _l;
        break;
    
    case BUNSIGNED | BLLONG | BLONG:
        p = _ull;
        break;
    
    case BUNSIGNED | BLONG:
        p = _ul;
        break;
    
    case BUNSIGNED:
        p = _u;
        break;
    }

    for (; *p >= 0; p++) {
        unsigned long max;
        int t1 = p[1];

        t0 = *p;
        if (t1 < 0)
            break;
        if (typeu[t1])
            max = ONES(typsize[t1]);
        else
            max = ONES(typsize[t1]) >> 1;
        if (n > max)
            break;
    }

    return cnstnode(types[t0], n);
}

static struct node *dostring(struct token *tokens, int n, int wide)
{
    void *bytes, *buf;
    size_t total, alloc, count;
    struct type *ty;
    struct node *p;
    struct token *t;

    assert(n > 0);
    bytes = NULL;
    ty = wide ? types[TINT] : types[TCHAR];
    total = alloc = 0;
    
    for (int i = 0; i < n; i++) {
        t = tokens + i;
        buf = eval_string(t, wide, &count);
        assert(count > 0);
        if (total + count > alloc) {
            alloc = 2 * (total + count) + 10;
            bytes = xrealloc(bytes, alloc * ty->size);
        }
        memcpy((char *)bytes + total * ty->size, buf, count * ty->size);
        total += count - 1;
    }

    p = node(OSTRING, NULL, NULL);
    p->type = arraytyp(ty, total + 1);
    p->v.p = bytes;
    p->sclass = CSTATIC;
    return p;
}

/* Initializers */

static struct node *parse_initializer_list(void)
{
    struct node *list = NULL;
    struct node *p;
    
    expect('{');
    while (tok->id != '}') {
        if (tok->id == '.' || tok->id == '[') {
            do {
                if (tok->id == '.') {
                    p = node(OELEM, NULL, NULL);
                    gettok();
                    if (tok->id == TOK_NAME)
                        p->sym = tok->val.sym;
                    else
                        error(p, "expect field name");
                    gettok();
                } else {
                    p = node(OARRAY, NULL, NULL);
                    gettok();
                    p->v.i = parse_int(0);
                    match(']');
                }

                list = newlist(list, p);
            } while (tok->id == '.' || tok->id == '[');
            expect('=');
        }
        
        p = parse_initializer();
        list = newlist(list, p);
        
        if (tok->id != ',')
            break;
        gettok();
    }
    match('}');

    return node(OINIT, invert(list), NULL);
}

static struct node *parse_initializer(void)
{
    if (tok->id == '{')
        return parse_initializer_list();
    else
        return parse_assign_expr();
}

/* Expressions */

static struct type *parse_cast_type(void)
{
    struct type *ty;

    expect('(');
    ty = parse_typename();
    match(')');

    return ty;
}

static struct node *parse_argument_expr_list(void)
{
    struct node *list = NULL;
    struct node **pp, *q;

    if (tok->id != ')') {
        pp = &list;
        while (1) {
            q = parse_assign_expr();
            q = node(OLIST, q, NULL);
            *pp = q;
            pp = &q->right;
            if (tok->id != ',')
                break;
            gettok();
        }
    }

    return list;
}

static struct node *parse_compound_literal(struct type *ty)
{
    struct node *p, *q, *init;
    struct symbol *s;

    nearln = lineno;
    init = parse_initializer_list();
    s = anonymous();
    if (autobn == 0)
        q = gdecl(s, ty, CSTATIC);
    else
        q = ldecl(s, ty, CAUTO);
    
    init = doinit(q->sym, q->sym->type, init);
    p = node(OXXX, NULL, NULL);
    *p = *q;
    p->op = OCOMPOUND;
    p->left = init;
    return p;
}

/* __builtin_va_arg ( assignment-expression, type-name ) */
static struct node *parse_builtin_va_arg(struct node *lhs)
{
    struct node *p1, *p2;
    struct type *ty;

    expect('(');
    p1 = parse_assign_expr();
    expect(',');
    ty = parse_typename();
    match(')');

    p2 = node(OXXX, NULL, NULL);
    p2->type = ty;

    return node(OFUNC, lhs, node(OLIST, p1, p2));
}

static struct node *parse_primary_expr(void)
{
    struct node *p;
    struct symbol *s;
    struct token *tokens;
    int n, wide, alloc;

    switch (tok->id) {
    case TOK_NAME:
        s = tok->val.sym;
        if (s->builtin_id == BUILTIN_FUNC)
            predefine(s);
        p = doname(s);
        gettok();
        if (s->builtin_id == BUILTIN_VA_ARG)
            p =  parse_builtin_va_arg(p);
        break;
        
    case TOK_PP_NUMBER:
    case TOK_PP_CHAR:
    case TOK_PP_WCHAR:
        p = donumber(tok);
        gettok();
        break;

    case TOK_SCON:
    case TOK_WSCON:
        /* handle string concatenation */
        nearln = lineno;
        tokens = NULL;
        n = wide = alloc = 0;
        do {
            if (alloc <= n) {
                alloc = alloc * 2 + 8;
                tokens = xrealloc(tokens, alloc * sizeof(struct token));
            }
            tokens[n++] = *tok;
            if (tok->id == TOK_WSCON)
                wide = 1;
            gettok();
        } while (tok->id == TOK_SCON || tok->id == TOK_WSCON);
        p = dostring(tokens, n, wide);
        p->line = nearln;
        free(tokens);
        break;

    default:
        error(0, "illegal expression");
        p = cnstnode(types[TINT], 0L);
        gettok();
        break;
    }
    return p;
}

static struct node *parse_postfix_expr(struct node *p)
{
    struct node *q;
    
    while (1) {
        switch (tok->id) {
        case '[':
            gettok();
            q = parse_expr();
            match(']');
            p = node(OINDIR, node(OADD, p, q), NULL);
            break;

        case '(':
            gettok();            
            q = parse_argument_expr_list();
            match(')');
            p = node(OFUNC, p, q);
            break;

        case '.':
        case TOK_DEREF:
            if (tok->id == '.')
                p = node(ODOT, p, NULL);
            else
                p = node(ODOT, node(OINDIR, p, NULL), NULL);
            gettok();
            if (tok->id == TOK_NAME)
                p->sym = tok->val.sym;
            expect(TOK_NAME);
            break;

        case TOK_INCR:
            gettok();
            p = node(OPOSTINC, p, NULL);
            break;

        case TOK_DECR:
            gettok();
            p = node(OPOSTDEC, p, NULL);
            break;

        default:
            return p;
        }
    }
}

static struct node *parse_unary_expr(void)
{
    struct node *p, *q;
    struct type *ty;
    int t = tok->id;

    switch (t) {
    case TOK_INCR:
    case TOK_DECR:
        gettok();
        return node(uops[t], parse_unary_expr(), NULL);

    case '+':
    case '-':
    case '~':
    case '!':
    case '&':
    case '*':
        gettok();
        return node(uops[t], parse_unary_expr(), NULL);

    case TOK_SIZEOF:
        ty = NULL;
        q = NULL;
        gettok();
        if (tok->id == '(' && first_typename(peektok())) {
            ty = parse_cast_type();
            if (tok->id == '{') {
                /* it's a postfix expression. */
                q = parse_compound_literal(ty);
                q = parse_postfix_expr(q);
            }
        } else {
            q = parse_unary_expr();
        }
        p = node(OSIZEOF, q, NULL);
        p->type = ty;
        return p;

    case '(':
        if (first_typename(peektok())) {
            ty = parse_cast_type();
            if (tok->id == '{') {
                /* it's a postfix expression. */
                p = parse_compound_literal(ty);
                return parse_postfix_expr(p);
            }
            p = node(OCAST, parse_unary_expr(), NULL);
            p->type = ty;
            return p;
        }
        gettok();
        p = parse_expr();
        match(')');
        return parse_postfix_expr(p);

    default:
        return parse_postfix_expr(parse_primary_expr());
    }
}

static struct node *parse_binary_expr(void)
{
    struct {
        struct node *expr;
        int prec, op;
    } stack[NPREC];
    int sp;                     /* stack pointer */
    int prec, t;

    /* pop stack[sp] and stack[sp-1] */
#define POP()                                           \
    do {                                                \
        stack[sp-1].expr = node(stack[sp].op,           \
                                stack[sp-1].expr,       \
                                stack[sp].expr);        \
        sp--;                                           \
    } while (0)

    /* init stack */
    stack[0].expr = parse_unary_expr();
    stack[0].prec = 0;
    sp = 0;

    while (1) {
        prec = precs[tok->id];
        if (prec == 0)
            break;

        while (prec <= stack[sp].prec)
            POP();

        t = tok->id;
        gettok();
        /* push */
        sp++;
        stack[sp].expr = parse_unary_expr();
        stack[sp].prec = prec;
        stack[sp].op = bops[t];
    }

    while (sp > 0)
        POP();

#undef POP
    return stack[0].expr;
}

static struct node *parse_cond_expr_tail(struct node *cond)
{
    struct node *then, *els;
    
    expect('?');
    then = parse_expr();
    expect(':');
    els = parse_cond_expr();
    return node(OCOND, cond, node(OLIST, then, els));
}

static struct node *parse_cond_expr(void)
{
    struct node *p = parse_binary_expr();
    if (tok->id == '?')
        return parse_cond_expr_tail(p);
    return p;
}

static struct node *parse_assign_expr(void)
{
    struct node *p = parse_binary_expr();
    if (tok->id == '?')
        return parse_cond_expr_tail(p);
    if (kinds[tok->id] == '=') {
        int op = bops[tok->id];
        gettok();
        return node(op, p, parse_assign_expr());
    }
    return p;
}

static struct node *parse_expr(void)
{
    struct node *p = parse_assign_expr();
    while (tok->id == ',') {
        gettok();
        p = node(OCOMMA, p, parse_assign_expr());
    }
    return p;
}

/* Statements */

static struct node *parse_if_stmt(void)
{
    struct node *cond, *then, *els = NULL;
    
    gettok();
    expect('(');
    cond = parse_expr();
    match(')');
    then = parse_stmt();

    if (tok->id == TOK_ELSE) {
        gettok();
        els = parse_stmt();
    }

    return node(OIF, cond, node(OLIST, then, els));
}

static struct node *parse_while_stmt(void)
{
    struct node *cond, *body;
    
    gettok();
    expect('(');
    cond = parse_expr();
    match(')');
    body = parse_stmt();

    return node(OWHILE, cond, body);
}

static struct node *parse_do_while_stmt(void)
{
    struct node *cond, *body;

    gettok();
    body = parse_stmt();
    expect(TOK_WHILE);
    expect('(');
    cond = parse_expr();
    match(')');
    expect(';');

    return node(ODOWHILE, cond, body);
}

static struct node *parse_for_stmt(void)
{
    struct node *p1 = NULL;
    struct node *p2 = NULL;
    struct node *p3 = NULL;
    struct node *body = NULL;
    
    enterscope();
    gettok();
    expect('(');

    if (tok->id == ';') {
        gettok();
    } else if (first_decl(tok)) {
        p1 = parse_decls(ldecl);
    } else {
        p1 = parse_expr();
        expect(';');
    }

    if (tok->id != ';')
        p2 = parse_expr();
    expect(';');

    if (tok->id != ')')
        p3 = parse_expr();
    match(')');

    body = parse_stmt();
    exitscope();

    return node(OFOR, node(OLIST, p1, node(OLIST, p2, p3)), body);
}

static struct node *parse_switch_stmt(void)
{
    struct node *cond, *body;
    
    gettok();
    expect('(');
    cond = parse_expr();
    match(')');
    body = parse_stmt();

    return node(OSWITCH, cond, body);
}

static struct node *parse_case_stmt(void)
{
    struct node *cond, *body;
    
    gettok();
    cond = parse_expr();
    expect(':');
    body = parse_stmt();

    return node(OCASE, cond, body);
}

static struct node *parse_default_stmt(void)
{
    struct node *body;
    
    gettok();
    expect(':');
    body = parse_stmt();

    return node(OCASE, NULL, body);
}

static struct node *parse_label_stmt(void)
{
    struct symbol *s = tok->val.sym;
    struct node *body;

    gettok();
    expect(':');
    body = parse_stmt();

    return node(OLABEL, dcllabel(s, 1), body);
}

static struct node *parse_goto_stmt(void)
{
    struct symbol *s = NULL;
    
    gettok();
    if (tok->id == TOK_NAME)
        s = tok->val.sym;
    else
        error(0, "expect identifier after 'goto'");
    gettok();
    expect(';');

    return node(OGOTO, dcllabel(s, 0), NULL);
}

static struct node *parse_return_stmt(void)
{
    struct node *p;
    struct node *e = NULL;
    
    gettok();
    if (tok->id != ';')
        e = parse_expr();
    expect(';');

    p = node(ORETURN, e, NULL);
    p->type = thisfn->link;
    return p;
}

static struct node *parse_block(void)
{
    struct node *list = NULL;

    expect('{');
    while (1) {
        if (first_decl(tok))
            list = newlist(list, parse_decls(ldecl));
        else if (first_expr(tok) || first_stmt(tok))
            list = newlist(list, parse_stmt());
        else
            break;
    }
    match('}');
    
    return invert(list);
}

static struct node *parse_stmt(void)
{
    struct node *p;
    
    switch (tok->id) {
    case '{':
        enterscope();
        p = parse_block();
        exitscope();
        return p;
    case TOK_IF:
        return parse_if_stmt();
    case TOK_SWITCH:
        return parse_switch_stmt();
    case TOK_WHILE:
        return parse_while_stmt();
    case TOK_DO:
        return parse_do_while_stmt();
    case TOK_FOR:
        return parse_for_stmt();
    case TOK_GOTO:
        return parse_goto_stmt();
    case TOK_CONTINUE:
        p = node(OCONTINUE, NULL, NULL);
        gettok();
        expect(';');
        return p;
    case TOK_BREAK:
        p = node(OBREAK, NULL, NULL);
        gettok();
        expect(';');
        return p;
    case TOK_RETURN:
        return parse_return_stmt();
    case TOK_CASE:
        return parse_case_stmt();
    case TOK_DEFAULT:
        return parse_default_stmt();
    case TOK_NAME:
        if (peektok()->id == ':')
            return parse_label_stmt();
        /* fall thru */
    default:
        p = NULL;
        if (tok->id != ';')
            p = parse_expr();
        expect(';');
        return p;
    }
}

/* Declarations */

static struct type *parse_specifiers(int *sclass)
{
    int b = 0, b1;
    struct type *basety = NULL;

    while (1) {
        switch (tok->id) {
        case TOK_BOOL:     b1 = BBOOL;     gettok(); break;
        case TOK_CHAR:     b1 = BCHAR;     gettok(); break;
        case TOK_INT:      b1 = BINT;      gettok(); break;
        case TOK_FLOAT:    b1 = BFLOAT;    gettok(); break;
        case TOK_DOUBLE:   b1 = BDOUBLE;   gettok(); break;
        case TOK_VOID:     b1 = BVOID;     gettok(); break;
        case TOK_SHORT:    b1 = BSHORT;    gettok(); break;
        case TOK_LONG:     b1 = BLONG;     gettok(); break;
        case TOK_SIGNED:   b1 = BSIGNED;   gettok(); break;
        case TOK_UNSIGNED: b1 = BUNSIGNED; gettok(); break;

        case TOK_AUTO:     b1 = BAUTO;     gettok(); break;
        case TOK_EXTERN:   b1 = BEXTERN;   gettok(); break;
        case TOK_REGISTER: b1 = BREGISTER; gettok(); break;
        case TOK_STATIC:   b1 = BSTATIC;   gettok(); break;
        case TOK_TYPEDEF:  b1 = BTYPEDEF;  gettok(); break;

        case TOK_CONST:    b1 = BCONST;    gettok(); break;
        case TOK_VOLATILE: b1 = BVOLATILE; gettok(); break;
        case TOK_RESTRICT: b1 = 0;         gettok(); break; /* garbage */
        case TOK_INLINE:   b1 = 0;         gettok(); break; /* garbage */

        case TOK_ENUM:     b1 = BENUM;   basety = parse_tag(TENUM);   break;
        case TOK_STRUCT:   b1 = BSTRUCT; basety = parse_tag(TSTRUCT); break;
        case TOK_UNION:    b1 = BUNION;  basety = parse_tag(TUNION);  break;

        case TOK_NAME:
            if (tok->val.sym->sclass == CTYPEDEF && (b & ~BCLASS & ~BQUAL) == 0) {
                b1 = BTYPENAME;
                basety = tok->val.sym->type;
                gettok();
            } else {
                b1 = -1;
            }
            break;

        default:
            b1 = -1;
            break;
        }

        if (b1 == -1)
            break;

        if (b1 & BCLASS && !sclass) {
            error(0, "storage class not allowed");
            b1 &= ~BCLASS;
        }

        b = typebitor(b, b1);
    }

    if (sclass)
        *sclass = simplec(b);

    return qual(simpleq(b), simplet(b, basety));
}

static struct type *parse_typename(void)
{
    struct type *ty;

    ty = parse_specifiers(NULL);
    if (tok->id == '*' || tok->id == '(' || tok->id == '[')
        ty = parse_declarator(ty, NULL, NULL, 1);
    return ty;
}

static int parse_int(int r)
{
    struct node *p = parse_cond_expr();
    conv(p);
    if (!p || !typei[p->type->etype] || p->op != OCONST) {
        error(p, "expect integer constant");
        return r;
    }
    return p->v.i;
}

static void parse_enum_body(int et, struct type *sty)
{
    int val;
    struct symbol *s;

    if (tok->id == TOK_NAME) {
        val = 0;
        do {
            s = tok->val.sym;
            gettok();
            if (tok->id == '=') {
                gettok();
                val = parse_int(val);
            }
            doenum(s, val++, sty);
            if (tok->id != ',')
                break;
            gettok();
        } while (tok->id == TOK_NAME);
    } else if (tok->id == '}') {
        warn(0, "empty %s", tnames[et]);
    } else {
        error(0, "expect identifier");
        gettok();
    }

    if (sty->etype == et) {
        if (sty->size > 0)
            error(0, "'%s %s' redeclared", tnames[et], sty->tag->name);
        sty->align = sty->link->align;
        sty->size = sty->link->size;
    }
}

static void parse_struct_body(int et, struct type *sty)
{
    struct type *flist = NULL;
    struct type **pp = &flist;
    struct type *basety;
    struct type *field, *ty;
    int nbits, has_bit;
    struct symbol *sym;

    if (tok->id == '}')
        warn(0, "empty %s", tnames[et]);
    
    while (first_decl(tok)) {
        basety = parse_specifiers(NULL);

        while (1) {
            nbits = has_bit = 0;
            sym = NULL;
            if (tok->id == ':') {
                gettok();
                has_bit = 1;
                nbits = parse_int(0);
                ty = basety;
            } else if (tok->id == ';') {
                /* [C11] anonymous struct/union */
                ty = basety;
            } else {
                ty = parse_declarator(basety, &sym, NULL, 0);
                if (tok->id == ':') {
                    gettok();
                    has_bit = 1;
                    nbits = parse_int(1);
                }
            }

            field = dofield(sym, ty, nbits, has_bit, flist);
            if (field) {
                *pp = field;
                pp = &field->next;
            }

            if (tok->id != ',')
                break;
            gettok();
        }
        expect(';');
    }

    if (sty->etype == et) {
        if (sty->size > 0)
            error(0, "'%s %s' redeclared", tnames[sty->etype], sty->tag->name);
        sty->link = flist;
        sualign(sty);
        if (options.debug)
            prstruct(sty);
    }
}

static struct type *parse_tag(int et)
{
    struct type *sty;
    struct symbol *sym = NULL;

    gettok();
    if (tok->id == TOK_NAME) {
        sym = tok->val.sym;
        gettok();
    }
    if (tok->id == '{') {
        gettok();
        sty = dotag(sym ? sym : anonymous(), et, autobn);
        if (et == TENUM)
            parse_enum_body(et, sty);
        else
            parse_struct_body(et, sty);
        match('}');
    } else {
        if (sym == NULL)
            error(0, "expect identifier or '{'");
        sty = dotag(sym ? sym : anonymous(), et, 0);
    }

    return sty;
}

static int parse_type_qualifiers(void)
{
    int b = 0;
    int t;

    while (kinds[tok->id] == TOK_CONST) {
        switch (tok->id) {
        case TOK_CONST:    t = BCONST;    break;
        case TOK_VOLATILE: t = BVOLATILE; break;
        case TOK_RESTRICT: t = 0;         break; /* garbage */
        default: abort();
        }

        b = typebitor(b, t);
        gettok();
    }

    return btot(b);
}

static void parse_array(struct type *ty, int abstract)
{
    /* NOTE: '*' is in `first_expr' */
    switch (abstract) {
    case 1:
        if (tok->id == '*' && peektok()->id == ']')
            gettok();
        else if (first_expr(tok))
            doarray(ty, parse_assign_expr());
        break;

    case 0:
    case 2:
        if (tok->id == TOK_STATIC) {
            gettok();
            if (kinds[tok->id] == TOK_CONST)
                parse_type_qualifiers();
            doarray(ty, parse_assign_expr());
        } else {
            if (kinds[tok->id] == TOK_CONST)
                parse_type_qualifiers();
            if (tok->id == TOK_STATIC) {
                gettok();
                doarray(ty, parse_assign_expr());
            } else if (tok->id == '*' && peektok()->id == ']') {
                gettok();
            } else if (first_expr(tok)) {
                doarray(ty, parse_assign_expr());
            }
        }
        break;

    default:
        abort();
    }
}

static struct node *parse_prototype(struct type *fty)
{
    struct node *params = NULL;
    struct node **pp = &params;
    int sclass;
    struct type *ty;
    struct symbol *sym;
    struct node *p;

    while (1) {
        sym = NULL;
        ty = parse_specifiers(&sclass);
        ty = parse_declarator(ty, &sym, NULL, 2);
        p = pdecl(sym, ty, sclass);
        p = node(OLIST, p, NULL);
        *pp = p;
        pp = &p->right;

        if (tok->id != ',')
            break;

        gettok();
        if (tok->id == TOK_ELLIPSIS) {
            p = node(OPROTO, NULL, NULL);
            p->type = types[TVOID];
            p->complex = 1;     /* mark for variadic test in `doproto' */
            p = node(OLIST, p, NULL);
            *pp = p;
            gettok();
            break;
        }
    }

    return doproto(params);
}

static struct node *parse_oldstyle(struct type *fty)
{
    struct node *params = NULL;
    struct node **pp = &params;
    struct node *p;

    while (1) {
        if (tok->id == TOK_NAME) {
            p = pdecl(tok->val.sym, types[TINT], CXXX);
            p->type = NULL;     /* mark as undefined */
            p = node(OLIST, p, NULL);
            *pp = p;
            pp = &p->right;
        }
        expect(TOK_NAME);
        if (tok->id != ',')
            break;
        gettok();
    }

    return params;
}

static struct node *parse_parameters(struct type *fty)
{
    struct node *params;

    if (first_decl(tok)) {
        /* prototype */
        struct type *proto = NULL;
        struct type **next = &proto;

        params = parse_prototype(fty);
        for (struct node *p = params; p; p = p->right) {
            struct type *ty = typcpy(p->left->type);
            *next = ty;
            next = &ty->next;
        }
        fty->next = proto;
        fty->oldstyle = 0;
    } else if (tok->id == TOK_NAME) {
        /* oldstyle */
        params = parse_oldstyle(fty);
        fty->next = NULL;
        fty->oldstyle = 1;
    } else {
        /* empty or error */
        params = NULL;
        fty->next = NULL;
        fty->oldstyle = 1;
        if (tok->id != ')') {
            if (tok->id == TOK_ELLIPSIS)
                error(0, "require a named parameter before '...'");
            else
                error(0, "expect parameter declarator at 's'", tok2s(tok));
            gettok();
        }
    }

    return params ? params : node(OLIST, NULL, NULL);
}

static struct type *tnode(int et, struct type *ty)
{
    struct type *nty = zallocate(sizeof(struct type), FUNC);
    nty->etype = et;
    nty->link = ty;
    return nty;
}

/*
 * This function returns a temporary type list which is in the revserse order
 * to the result type. It's rebuilt and type-checked in `parse_declarator'.
 */
static struct type *parse_declarator1(struct symbol **sym,
                                      struct node **params, int abstract)
{
    struct type *ty = NULL;
    struct node *args;

    switch (tok->id) {
    case '*':
        gettok();
        if (kinds[tok->id] == TOK_CONST) {
            ty = tnode(parse_type_qualifiers(), NULL);
            ty->link = parse_declarator1(sym, params, abstract);
        } else {
            ty = parse_declarator1(sym, params, abstract);
        }
        return tnode(TPTR, ty);

    case TOK_NAME:
        if (sym)
            *sym = tok->val.sym;
        else
            error(0, "extraneous identifier '%s'", tok2s(tok));
        nearln = lineno;
        gettok();
        break;

    case '(':
        gettok();
        if (abstract && (tok->id == ')' || first_decl(tok))) {
            ty = tnode(TFUNC, ty);
            enterscope();
            args = parse_parameters(ty);
            exitparams(args);
        } else {
            ty = parse_declarator1(sym, params, abstract);
        }
        match(')');
        break;

    case '[':
        break;

    default:
        return ty;
    }

    /* function or array */
    while (tok->id == '(' || tok->id == '[') {
        if (tok->id == '[') {
            gettok();
            ty = tnode(TARRAY, ty);
            parse_array(ty, abstract);
            match(']');
        } else {
            gettok();
            ty = tnode(TFUNC, ty);
            enterscope();
            args = parse_parameters(ty);
            if (params && *params == NULL)
                *params = args;
            else
                exitparams(args);
            match(')');
        }
    }

    return ty;
}

/*
 * parse a direct or abstract declarator.
 * abstract: 0-direct, 1-abstract, 2-any
 */
static struct type *parse_declarator(struct type *basety, struct symbol **sym, 
                                     struct node **params, int abstract)
{
    struct type *ty = parse_declarator1(sym, params, abstract);

    for (; ty; ty = ty->link) {
        switch (ty->etype) {
        case TPTR:
            basety = ptrtyp(basety);
            break;

        case TFUNC:
            basety = functyp(basety, ty->next, ty->oldstyle);
            break;

        case TARRAY:
            basety = arraytyp(basety, ty->size);
            break;

        case TCONST:
        case TVOLATILE:
        case TCONST + TVOLATILE:
            basety = qual(ttoq(ty->etype), basety);
            break;

        case 0:     /* restrict */
            break;

        default:
            abort();
        }
    }
    
    return basety;
}

static struct node *parse_decls(struct node *(*f)(struct symbol *, struct type *, int))
{
    struct type *basety, *ty;
    int sclass;
    struct node *p, *q, *params;
    struct symbol *sym;
    struct node *list = NULL;

    nearln = lineno;
    basety = parse_specifiers(&sclass);
    if (tok->id == TOK_NAME || tok->id == '*' || tok->id == '(') {
        sym = NULL;
        if (autobn == 0) {               /* global scope */
            params = NULL;               /* for functioness */

            ty = parse_declarator(basety, &sym, &params, 0);
            if (params && sym && ty->etype == TFUNC &&
                (tok->id == '{' || (first_decl(tok) && ty->oldstyle))) {
                
                exitscope();
                if (sclass == CEXTERN)
                    sclass = CGLOBAL;
                p = gdecl(sym, ty, sclass);
                dodecl(p, 1);

                if (params->left == NULL)
                    params = NULL;
                if (ty->oldstyle) {
                    /* start with a new table */
                    enterscope();
                    while (first_decl(tok))
                        parse_decls(pdecl);
                    walkoparams(params);
                    exitscope();
                }

                enterscope();
                firstdcl = dclstack;
                fdefn(p, params);
                if (tok->id == '{') {
                    markpredef();
                    q = parse_block();
                    gfunc(q, p, params);
                } else {
                    assert(ty->oldstyle);
                    error(0, "expect function body");
                }
                thisfn = NULL;      /* mark as not in a function */
                exitscope();
                return NULL;
            } else if (params) {
                exitparams(params);
            }
        } else {
            ty = parse_declarator(basety, &sym, NULL, 0);
        }

        while (1) {
            if (sym) {
                p = (*f)(sym, ty, sclass);
                q = NULL;
                if (tok->id == '=') {
                    gettok();
                    q = parse_initializer();
                    q = doinit(p->sym, p->type, q);
                    list = newlist(list, q);
                }
                dodecl(p, q != NULL);
            } else {
                error(0, "missing identifier");
            }

            if (tok->id != ',')
                break;

            gettok();
            sym = NULL;
            ty = parse_declarator(basety, &sym, NULL, 0);
        }
    } else if (typesue[basety->etype]) {
        if (sclass != CXXX)
            warn(0, "storage class ignored: %s", cnames[sclass]);
        if (basety->qual != QXXX)
            warn(0, "type qualifier ignored: %Q", bitwiseq(basety->qual));
    } else {
        error(0, "invalid token %s in declaration", tok2s(tok));
    }
    expect(';');
    return list;
}

static void parse_translation_unit(void)
{
    for (gettok(); tok->id != TOK_EOF;) {
        if (kinds[tok->id] == TOK_STATIC || kinds[tok->id] == TOK_INT ||
            kinds[tok->id] == TOK_CONST || tok->id == TOK_NAME) {
            assert(autobn == 0);
            parse_decls(gdecl);
            deallocate(FUNC);
        } else if (tok->id == ';') {
            gettok();           /* empty declaration */
        } else {
            error(0, "expect declaration");
            skipto(first_decl);
        }
    }
}

void parse(void)
{
    parse_translation_unit();
}

void mkfield(struct type *sty, struct type *fty, const char *name)
{
    struct type **pp;

    fty = typcpy(fty);
    fty->next = NULL;
    fty->sym = lookup(name, OPT_CREATE);

    for (pp = &sty->link; *pp;)
        pp = &(*pp)->next;

    *pp = fty;
}

struct type *mkproto(struct type *ty, ...)
{
    struct type *p, *proto, **next;
    va_list ap;

    proto = NULL;
    next = &proto;
    va_start(ap, ty);
    do {
        p = typcpy(ty);
        p = decay(p);
        p->next = NULL;
        *next = p;
        next = &p->next;
    } while ((ty = va_arg(ap, struct type *)));
    va_end(ap);

    return proto;
}

struct type *mktag(const char *name, int etype)
{
    struct symbol *s;

    s = lookup(name, OPT_CREATE);
    return dotag(s, etype, 0);
}