#include "cc.h"
#include <errno.h>
#include <limits.h>

#define MAX_ERRORS 6
#define STRINGSZ 512

enum { WRN = 1, ERR, FTL };
int errors, warnings;
static const char *prefix[] = {
    "null", "warning", "error", "fatal error"
};

static void vdiag(struct node *n, const char *fmt, va_list ap, int level)
{
    if (level == 0)
        return;
    if (level == WRN && options.warn_error)
        level = ERR;
    if (level == WRN && options.warn_none)
        return;

    fprint(stderr, "%L: %s: ", n ? n->line : nearln, prefix[level]);
    vfprint(stderr, fmt, ap);
    fprint(stderr, "\n");

    if (level == ERR)
        errors++;
    else if (level == WRN)
        warnings++;
    else if (level == FTL)
        exit(EXIT_FAILURE);

    if (errors >= MAX_ERRORS) {
        fprint(stderr, "Too many errors.\n");
        exit(EXIT_FAILURE);
    }
}

void error(struct node *n, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag(n, fmt, ap, ERR);
    va_end(ap);
}

void warn(struct node *n, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag(n, fmt, ap, WRN);
    va_end(ap);
}

void fatal(struct node *n, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vdiag(n, fmt, ap, FTL);
    va_end(ap);
}

static int Oconv(struct fmt *fp)
{
    int o = va_arg(fp->args, int);
    if (o < OXXX || o > NOTYPE)
        return fmtprint(fp, "***badO %d***", o);

    return fmtstrcpy(fp, onames[o]);
}

static int Tconv(struct fmt *fp)
{
    char str[STRINGSZ+20], s[STRINGSZ+20];
    struct type *ty, *ty1;
    int et;
    size_t n;

    str[0] = 0;
    for (ty = va_arg(fp->args, struct type *); ty; ty = ty->link) {
        et = ty->etype;
        if (str[0])
            strcat(str, " ");
        if (ty->qual != QXXX) {
            snprint(s, STRINGSZ, "%Q ", bitwiseq(ty->qual));
            if (strlen(str) + strlen(s) < STRINGSZ)
                strcat(str, s);
        }
        snprint(s, STRINGSZ, "%s", tnames[et]);
        if (strlen(str) + strlen(s) < STRINGSZ)
            strcat(str, s);
        if (et == TFUNC && (ty1 = ty->next)) {
            snprint(s, STRINGSZ, "(%T", ty1);
            if (strlen(str) + strlen(s) < STRINGSZ)
                strcat(str, s);
            while ((ty1 = ty1->next)) {
                snprint(s, STRINGSZ, ", %T", ty1);
                if (strlen(str) + strlen(s) < STRINGSZ)
                    strcat(str, s);
            }
            if (strlen(str) + strlen(s) < STRINGSZ)
                strcat(str, ")");
        }
        if (et == TARRAY) {
            n = ty->size;
            if (ty->link && ty->link->size)
                n /= ty->link->size;
            snprint(s, STRINGSZ, "[%lu]", n);
            if (strlen(str) + strlen(s) < STRINGSZ)
                strcat(str, s);
        }
        if (ty->nbits) {
            snprint(s, STRINGSZ, " :%d", ty->nbits);
            if (strlen(str) + strlen(s) < STRINGSZ)
                strcat(str, s);
        }
        if (typesue[et]) {
            if (ty->tag) {
                strcat(str, " ");
                if (strlen(str) + strlen(ty->tag->name) < STRINGSZ)
                    strcat(str, ty->tag->name);
            } else {
                strcat(str, " {}");
            }
            break;
        }
    }
    return fmtstrcpy(fp, str);
}

static int Qconv(struct fmt *fp)
{
    char str[STRINGSZ+20];
    const char *s;
    int b;

    str[0] = 0;
    for (b = va_arg(fp->args, int); b;) {
        int i = bitno(b);
        if (str[0])
            strcat(str, " ");
        s = tnames[i];
        if (strlen(str) + strlen(s) >= STRINGSZ)
            break;
        strcat(str, s);
        b &= ~(1 << i);
    }
    return fmtstrcpy(fp, str);
}

/* print source location */
static int Lconv(struct fmt *fp)
{
    char str[STRINGSZ];
    int line, oline;
    const char *oname;

    line = va_arg(fp->args, int);
    cpp_resolve_location(line, &oname, &oline);
    snprintf(str, STRINGSZ, "%s:%d", oname, oline);
    return fmtstrcpy(fp, str);
}

void fmtinit(void)
{
    fmtinstall('T', Tconv);     /* type */
    fmtinstall('Q', Qconv);     /* qualifier */
    fmtinstall('L', Lconv);     /* location */
    fmtinstall('O', Oconv);     /* operator */
}

static int invalid_ucn(int c)
{
    return (c < 0xA0 && (c != 0x24 && c != 0x40 && c != 0x60)) ||
        (c & 0x80000000) || (c >= 0xD800 && c <= 0xDFFF);
}

static const char *float_suffix(const char *p, int *suffix)
{
    assert(suffix);
    
    if (*p == 'f' || *p == 'F') {
        p++;
        *suffix = BFLOAT;
    } else if (*p == 'l' || *p == 'L') {
        p++;
        *suffix = BLONG | BDOUBLE;
    } else {
        *suffix = 0;
    }
    
    return p;
}

static void eval_float(struct token *t, struct svalue *v)
{
    const char *p = t->val.str;
    int suffix;

    if (p[0] == '.') {
        assert(ISDIGIT(p[1]));
        goto dotted;
    } else if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        p += 2;
        if (*p == '.') {
            if (!ISXDIGIT(p[1]))
                error(0, "hex floating constants require a significand");
            goto dotted_hex;
        } else {
            assert(ISXDIGIT(*p));

            while (ISXDIGIT(*p))
                p++;
        dotted_hex:
            if (*p == '.') {
                p++;
                while (ISXDIGIT(*p))
                    p++;
            }
            if (*p == 'p' || *p == 'P') {
                p++;
                if (*p == '+' || *p == '-')
                    p++;
                if (ISDIGIT(*p)) {
                    do
                        p++;
                    while (ISDIGIT(*p));
                } else {
                    error(0, "exponent has no digits");
                }
            } else {
                error(0, "hex floating constants require an exponent");
            }
        }
    } else {
        assert(ISDIGIT(*p));

        while (ISDIGIT(*p))
            p++;
    dotted:
        if (*p == '.') {
            p++;
            while (ISDIGIT(*p))
                p++;
        }
        if (*p == 'e' || *p == 'E') {
            p++;
            if (*p == '+' || *p == '-')
                p++;
            if (ISDIGIT(*p)) {
                do
                    p++;
                while (ISDIGIT(*p));
            } else {
                error(0, "exponent used with no following digits");
            }
        }
    }

    p = float_suffix(p, &suffix);

    if (*p != '\0')
        error(0, "illegal float constant '%s'", tok2s(t));

    if (v) {
        errno = 0;
        switch (suffix) {
        case BFLOAT:
            v->d = strtof(t->val.str, NULL);
            break;
        case BLONG | BDOUBLE:
            v->d = strtold(t->val.str, NULL);
            break;
        default:
            v->d = strtod(t->val.str, NULL);;
            break;
        }

        if (errno == ERANGE)
            error(0, "float constant overflow: '%s'", tok2s(t));

        v->suffix = suffix;
        v->id = TOK_FCON;
    }
}

static const char *integer_suffix(const char *p, int *suffix)
{
    assert(suffix);

    if ((p[0] == 'u' || p[0] == 'U') &&
        ((p[1] == 'l' && p[2] == 'l') || (p[1] == 'L' && p[2] == 'L'))) {
        p += 3;
        *suffix = BUNSIGNED | BLLONG | BLONG;
    } else if (((p[0] == 'l' && p[1] == 'l') || (p[0] == 'L' && p[1] == 'L')) &&
                (p[2] == 'u' || p[2] == 'U')) {
        p += 3;
        *suffix = BUNSIGNED | BLLONG | BLONG;
    } else if ((p[0] == 'l' && p[1] == 'l') ||
                (p[0] == 'L' && p[1] == 'L')) {
        p += 2;
        *suffix = BLLONG | BLONG;
    } else if ((p[0] == 'l' || p[0] == 'L') &&
                (p[1] == 'u' || p[1] == 'U')) {
        p += 2;
        *suffix = BUNSIGNED | BLONG;
    } else if ((p[0] == 'u' || p[0] == 'U') &&
                (p[1] == 'l' || p[1] == 'L')) {
        p += 2;
        *suffix = BUNSIGNED | BLONG;
    } else if (p[0] == 'l' || p[0] == 'L') {
        p += 1;
        *suffix = BLONG;
    } else if (p[0] == 'u' || p[0] == 'U') {
        p += 1;
        *suffix = BUNSIGNED;
    } else {
        *suffix = 0;
    }

    return p;
}

static void eval_int(struct token *t, struct svalue *v)
{
    const char *p = t->val.str;
    int overflow = 0;
    unsigned long n = 0;
    int suffix;
    int base;

    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        base = 16;
        p += 2;
        for (; ISXDIGIT(*p); p++) {
            int d;

            if (n & ~(~0UL >> 4))
                overflow = 1;

            if (ISXALPHA(*p))
                d = (*p & 0x5f) - 'A' + 10;
            else
                d = *p - '0';

            n = (n << 4) + d;
        }
    } else if (p[0] == '0') {
        base = 8;
        int err = 0;
        for (; ISDIGIT(*p); p++) {
            if (*p == '8' || *p == '9')
                err = 1;

            if (n & ~(~0UL >> 3))
                overflow = 1;

            n = (n << 3) + (*p - '0');
        }
        if (err)
            error(0, "illegal digit in octal constant");
    } else {
        base = 10;
        for (; ISDIGIT(*p); p++) {
            int d = *p - '0';
            if (n > (~0UL - d) / 10)
                overflow = 1;

            n = n * 10 + d;
        }
    }

    p = integer_suffix(p, &suffix);

    if (overflow)
        error(0, "integer constant overflow: '%s'", tok2s(t));
    if (p[0] != '\0')
        error(0, "illegal integer constant: '%s'", tok2s(t));

    if (v) {
        v->i = n;
        v->suffix = suffix;
        v->base = base;
        v->id = TOK_ICON;
    }
}

static const char *read_escape_char(const char *p, int wide, int *result, int *ucn)
{
    int c = 0, i, n;

    assert(*p == '\\');
    if (ucn) *ucn = 0;
    p++;
    switch (*p++) {
    case 'a':
        c = 7;
        break;
    case 'b':
        c = '\b';
        break;
    case 'e':
        c = 033;
        break;
    case 'f':
        c = '\f';
        break;
    case 'n':
        c = '\n';
        break;
    case 'r':
        c = '\r';
        break;
    case 't':
        c = '\t';
        break;
    case 'v':
        c = '\v';
        break;
    case '\'':
    case '"':
    case '\\':
    case '\?':
        c = p[-1];
        break;
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
        c = p[-1] - '0';
        if (*p >= '0' && *p <= '7') {
            c = (c << 3) + (*p++) - '0';
            if (*p >= '0' && *p <= '7')
                c = (c << 3) + (*p++) - '0';
        }
        break;
    case 'x':
        n = wide ? 2 * sizeof(wchar_t) : 2 * sizeof(char);
        for (i = 0; ISXDIGIT(*p); p++, i++) {
            if (i >= n)
                error(0, "hex escape sequence out of range");

            if (ISDIGIT(*p))
                c = (c << 4) + *p - '0';
            else
                c = (c << 4) + (*p & 0x5f) - 'A' + 10;
        }
        if (i == 0)
            error(0, "\\x used with no following hex digits");
        break;
        /*
         * Univeral Character Name:
         * The C99 standard permits $, @ and ` to be specified as UCNs.
         */
    case 'u':
        n = 4;
        goto ucn;
    case 'U':
        n = 8;
    ucn:
        if (ucn) *ucn = 1;
        for (i = 0; ISXDIGIT(*p); i++, p++) {
            if (i == n)
                break;
            if (ISDIGIT(*p))
                c = (c << 4) + *p - '0';
            else
                c = (c << 4) + (*p & 0x5f) - 'A' + 10;
        }
        if (i < n)
            error(0, "incomplete universal character name");
        else if (invalid_ucn(c))
            error(0, "invalid universal character");
        break;
    default:
        c = p[-1];
        warn(0, "unknown escape sequence '\\x%X'", c);
        break;
    }

    if (result)
        *result = c;
    return p;
}

void *eval_string(struct token *t, int wide, size_t *len)
{
    const char *s = t->val.str;
    size_t i = 0;
    size_t alloc = strlen(s);
    char *d = xmalloc(alloc);

    assert(s[0] == '"' || s[0] == 'L');

    if (s[0] == 'L')
        s += 2;
    else
        s += 1;

    while (*s != '"' && *s != 0) {
        int c, ucn;
        const char *p;

        if (*s != '\\') {
            d[i++] = *s++;
            continue;
        }
        p = read_escape_char(s, wide, &c, &ucn);
        if (ucn || (s[1] == 'x' && wide)) {
            int mblen;
            char mbs[MB_LEN_MAX];

            mblen = wc2mb(mbs, c);
            if (i + mblen > alloc) {
                alloc = i + mblen + 16;
                d = xrealloc(d, alloc);
            }
            for (int k = 0; k < mblen; k++)
                d[i++] = mbs[k];
        } else {
            d[i++] = c;
        }
        s = p;
    }
    d[i++] = 0;

    if (wide) {
        wchar_t *pwcs = xmalloc(i * sizeof(wchar_t));
        size_t count = mbs2wcs(pwcs, d, i-1);
        assert(count < i);
        pwcs[count++] = 0;
        if (len) *len = count;
        free(d);
        return pwcs;
    }

    if (len) *len = i;
    return d;
}

static void eval_char(struct token *t, struct svalue *v)
{
    const char *p = t->val.str;
    int n = 0;
    int bytes = 0;

    p++;                        /* skip separator */
    while (*p != 0 && *p != '\'') {
        int c, ucn;
        
        if (bytes >= sizeof(int)) {
            error(0, "character constant too large");
            break;
        }
        
        if (*p == '\\') {
            p = read_escape_char(p, 0, &c, &ucn);
            if (ucn) {
                n = c;
                bytes += sizeof(int);
            } else {
                goto non_ucn;
            }
        } else {
            c = *p++;
        non_ucn:
            n = (n << 8) + c;
            bytes++;
        }
    }

    if (bytes > 1)
        warn(0, "multi-character character constant");

    if (v) {
        v->i = n;
        v->suffix = 0;
        v->base = 10;
        v->id = TOK_ICON;
    }
}

static void eval_wchar(struct token *t, struct svalue *v)
{
    const char *p = t->val.str;
    wchar_t n = 0;

    p += 2;                     /* skip 'L' and separator */
    if (*p != 0 && *p != '\'') {
        int c, ucn, r;

        if (*p == '\\') {
            p = read_escape_char(p, 1, &c, &ucn);
            if (ucn) {
                n = c;
            } else {
                char ch = c;
                errno = 0;
                r = mb2wc(&n, &ch, 1);
                if (r < 0)
                    error(0, "invalid multibyte sequence");
            }
        } else {
            errno = 0;
            r = mb2wc(&n, p, strlen(p) - 1);
            if (r < 0)
                error(0, "invalid multibyte sequence");
            else
                p += r;
        }
    }

    if (*p != 0 && *p != '\'')
        error(0, "character constant too large");

    if (v) {
        v->i = n;
        v->suffix = 0;
        v->base = 10;
        v->id = TOK_ICON;
    }
}

/* 0-9 or .[0-9] */
static int classify_number(struct token *t)
{
    const char *p = t->val.str;

    if (p[0] == '.')
        return TOK_FCON;

    if (p[0] == '0' && (p[1] == 'x' || p[1] == 'X')) {
        /* Hex */
        p += 2;
        if (*p == '.')
            return TOK_FCON;

        while (ISXDIGIT(*p))
            p++;
        if (*p == '.' || *p == 'p' || *p == 'P')
            return TOK_FCON;
        else
            return TOK_ICON;
    } else {
        /* Oct/Dec */
        assert(ISDIGIT(*p));

        while (ISDIGIT(*p))
            p++;
        if (*p == '.' || *p == 'e' || *p == 'E')
            return TOK_FCON;
        else
            return TOK_ICON;
    }
}

void eval_number(struct token *t, struct svalue *v)
{
    switch (t->id) {
    case TOK_PP_NUMBER:
        switch (classify_number(t)) {
        case TOK_ICON:
            eval_int(t, v);
            break;
        case TOK_FCON:
            eval_float(t, v);
            break;
        default:
            abort();
        }
        break;
    case TOK_PP_CHAR:
        eval_char(t, v);
        break;
    case TOK_PP_WCHAR:
        eval_wchar(t, v);
        break;
    default:
        abort();
    }
}

struct node *node(int op, struct node *l, struct node *r)
{
    struct node *p = zallocate(sizeof(struct node), FUNC);
    p->op = op;
    p->left = l;
    p->right = r;
    if (l)
        p->line = l->line;
    else if (r)
        p->line = r->line;
    else
        p->line = lineno;
    return p;
}

struct node *node1(int op, struct node *l, struct node *r)
{
    struct node *p = node(op, l, r);
    p->line = nearln;
    return p;
}

void nodnew(struct node *dst, int op, struct node *l, struct node *r)
{
    dst->op = op;
    dst->left = l;
    dst->right = r;
    if (l)
        dst->line = l->line;
    else if (r)
        dst->line = r->line;
    else
        dst->line = lineno;
}

struct node *invert(struct node *p)
{
    if (p == NULL || p->op != OLIST)
        return p;
    for (struct node *q = p->left; q; q = q->left) {
        if (q->op != OLIST)
            break;
        p->left = q->right;
        q->right = p;
        p = q;
    }
    return p;
}

struct node *newlist(struct node *l, struct node *r)
{
    if (r == NULL)
        return l;
    if (l == NULL)
        return r;
    return node(OLIST, l, r);
}

int bitno(int b)
{
    for (int i = 0; i < 32; i++)
        if (b & (1 << i))
            return i;
    error(0, "bad bitno");
    return 0;
}

int bitwiseq(int q)
{
    q &= QMASK;
    switch (q) {
    case QCONST:
        return BCONST;
    case QVOLATILE:
        return BVOLATILE;
    case QCONST | QVOLATILE:
        return BCONST | BVOLATILE;
    }
    return 0;
}

int simpleq(int b)
{
    b &= BQUAL;
    switch (b) {
    case BCONST:
        return QCONST;
    case BVOLATILE:
        return QVOLATILE;
    case BCONST | BVOLATILE:
        return QCONST | QVOLATILE;
    }
    return QXXX;
}

int btot(int b)
{
    b &= BQUAL;
    switch (b) {
    case BCONST:
        return TCONST;
    case BVOLATILE:
        return TVOLATILE;
    case BCONST | BVOLATILE:
        return TCONST + TVOLATILE;
    }
    return TXXX;
}

int ttoq(int t)
{
    switch (t) {
    case TCONST:
        return QCONST;
    case TVOLATILE:
        return QVOLATILE;
    case TCONST + TVOLATILE:
        return QCONST | QVOLATILE;
    }
    return QXXX;
}

int simplec(int b)
{
    b &= BCLASS;
    switch (b) {
    case 0:
        return CXXX;
    case BAUTO:
        return CAUTO;
    case BEXTERN:
        return CEXTERN;
    case BSTATIC:
        return CSTATIC;
    case BTYPEDEF:
        return CTYPEDEF;
    case BREGISTER:
        return CREGISTER;
    }
    error(0, "illegal combination of storage classes %Q", b);
    return CXXX;
}

struct type *simplet(int b, struct type *ty)
{
    b &= ~BCLASS & ~BQUAL;
    switch (b) {
    case BBOOL:
        return types[TBOOL];

    case BCHAR:
    case BCHAR | BSIGNED:
        return types[TCHAR];

    case BCHAR | BUNSIGNED:
        return types[TUCHAR];

    case BSHORT:
    case BSHORT | BINT:
    case BSHORT | BSIGNED:
    case BSHORT | BINT | BSIGNED:
        return types[TSHORT];

    case BSHORT | BUNSIGNED:
    case BSHORT | BUNSIGNED | BINT:
        return types[TUSHORT];

    case 0:
        warn(0, "type specifier missing, defaults to int");
    case BINT:
    case BINT | BSIGNED:
    case BSIGNED:
        return types[TINT];

    case BUNSIGNED:
    case BUNSIGNED | BINT:
        return types[TUINT];

    case BLONG:
    case BLONG | BINT:
    case BLONG | BSIGNED:
    case BLONG | BINT | BSIGNED:
        return types[TLONG];

    case BLONG | BUNSIGNED:
    case BLONG | BUNSIGNED | BINT:
        return types[TULONG];

    case BLLONG | BLONG:
    case BLLONG | BLONG | BINT:
    case BLLONG | BLONG | BSIGNED:
    case BLLONG | BLONG | BINT | BSIGNED:
        return types[TLLONG];

    case BLLONG | BLONG | BUNSIGNED:
    case BLLONG | BLONG | BUNSIGNED | BINT:
        return types[TULLONG];
        
    case BFLOAT:
        return types[TFLOAT];
        
    case BDOUBLE:
    case BDOUBLE | BLONG:
        return types[TDOUBLE];
        
    case BVOID:
        return types[TVOID];

    case BENUM:
    case BSTRUCT:
    case BUNION:
    case BTYPENAME:
        return ty;
    }

    error(0, "illegal combination of types %Q", b);
    return types[TINT];
}

int typebitor(int a, int b)
{
    int c = a | b;
    if (a & b) {
        if (a & b & BQUAL)
            warn(0, "duplicate type qualifier %Q", a & b);
        else if (a & b & BCLASS)
            error(0, "duplicate storage class %Q", a & b);
        else if ((a & b) == BLONG && (c & BLLONG) == 0)
            c |= BLLONG;        /* long long */
        else
            error(0, "duplicate type specifier %Q", a & b);
    }
    return c;
}

struct symbol *anonymous(void)
{
    static int i = 1;
    struct symbol *s;
    char str[30];
    
    snprintf(str, sizeof str, ".L%d", i++);
    s = lookup(str, OPT_CREATE);
    s->anonymous = 1;
    return s;
}

/* local static variable */
struct symbol *mkstatic(struct symbol *s)
{
    struct symbol *s1;

    if (s->block == 0 || s->sclass != CSTATIC)
        return s;

    s1 = lookup(stringf("%s$%d", s->name, s->block), OPT_CREATE);
    if (s1->sclass != CSTATIC) {
        s1->type = s->type;
        s1->offset = s->offset;
        s1->block = s->block;
        s1->line = s->line;
        s1->sclass = CSTATIC;
    }
    return s1;
}

struct node *cnstnode(struct type *ty, ...)
{
    struct node *p;
    va_list ap;

    p = node(OCONST, NULL, NULL);
    va_start(ap, ty);
    if (typei[ty->etype])
        p->v.i = convltox(va_arg(ap, long), ty->etype);
    else if (typefd[ty->etype])
        p->v.d = va_arg(ap, double);
    else if (ty->etype == TPTR)
        p->v.p = va_arg(ap, void *);
    else
        abort();
    va_end(ap);
    p->type = ty;

    return p;
}

long convltox(long v, int et)
{
    int n;

    n = 8 * typsize[et];
    v &= MASK(n);
    if (!typeu[et])
        if (v & SIGN(n))
            v |= ~MASK(n);
    return v;
}

int log2i(size_t i)
{
    int r;

    if (i == 0)
        return -1;
    r = 0;
    while ((i & 0x01) == 0) {
        r++;
        i >>= 1;
    }
    return i >> 1 ? -1 : r;
}

void prstruct(struct type *ty)
{
    if (typesu[ty->etype]) {
        print("%s \"%s\": S=%ld A=%d\n",
              tnames[ty->etype],
              ty->tag->anonymous ? "{}" : ty->tag->name,
              ty->size, ty->align);
        for (struct type *q = ty->link; q; q = q->next)
            print("   \"%s\": O=%ld S=%ld BO=%d BS=%d\n",
                  q->sym ? q->sym->name : "",
                  q->offset, q->size, q->bitoff, q->nbits);
    }
}

void prdecl(struct symbol *s)
{
    print("decl \"%s\": C=%s B=%d O=%ld T=%T\n",
          s->name, cnames[s->sclass], s->block, s->offset, s->type);
}

static void prtree1(struct node *p, int lev, int f)
{
    static int dumplist = 0;
    int i;

    if (f)
        for (i = 0; i < lev; i++)
            print("  ");

    if (p == NULL) {
        print("Z\n");
        return;
    }
    if (p->op == OLIST && dumplist == 0) {
        prtree1(p->left, lev, 0);
        prtree1(p->right, lev, 1);
        return;
    }
    lev++;
    print("%O", p->op);
    i = 3;
    switch (p->op) {
    case OELEM:
        i = 0;
        if (p->sym)
            print(" \".%s\"", p->sym->name);
        break;

    case OARRAY:
        i = 0;
        print(" [%ld]", p->v.i);
        break;
        
    case ONAME:
        i = 0;
        if (p->sym)
            print(" \"%s\"", p->sym->name);
        print(" %ld", p->offset);
        break;

    case OCOMPOUND:
        print(" \"%s\" %ld", p->sym->name, p->offset);
        break;

    case OCONST:
        i = 0;
        if (typefd[p->type->etype])
            print(" \"%f\"", p->v.d);
        else
            print(" \"%ld\"", p->v.i);
        break;

    case OSTRING:
        i = 0;
        print(" \"%s\" %ld", p->v.cstring, p->offset);
        break;

    case OREGISTER:
        i = 0;
        print(" \"%d\"", p->reg);
        break;

    case OINDREG:
        print(" \"%d\" (%ld)", p->reg, p->offset);
        break;
    }
    if (p->type)
        print(" %T", p->type);
    print(" %L\n", p->line);
    if (i & 1)
        prtree1(p->left, lev, 1);
    if (i & 2)
        prtree1(p->right, lev, 1);
}

void prtree(struct node *p, const char *s)
{
    print("=== %s ===\n",s );
    prtree1(p, 0, 0);
    print("\n");
}
