#ifndef CC_H
#define CC_H

#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "liberty.h"

#define ISWHITESPACE(c)  (map[c] & BLANK)
#define ISNEWLINE(c)     (map[c] & NEWLINE)
#define ISDIGITLETTER(c) (map[c] & (DIGIT|LETTER))
#define ISXALPHA(c)      (map[c] & HEX)
#define ISDIGIT(c)       (map[c] & DIGIT)
#define ISXDIGIT(c)      (map[c] & (DIGIT|HEX))
#define ISLETTER(c)      (map[c] & LETTER)

#define MACRO_DEFINED_P(sym) ((sym)->kind == SYM_MACRO)
#define Z 0

/* decoded command-line options */
struct options {
    int preprocess_only;
    int leading_underscore;
    int warn_unsupported;
    int warn_error;
    int warn_none;
    int pic;
    int verbose;
    int optimize;
    int debug;
};

struct init {
    int code;
    unsigned long value;
    const char *s;
};

enum {
    BLANK = 01, NEWLINE = 02, LETTER = 04, DIGIT = 010, HEX = 020
};

/* binary op precedence */
enum {
    PREC_XXX,
    PREC_OROR,
    PREC_ANDAND,
    PREC_OR,
    PREC_XOR,
    PREC_AND,
    PREC_EQ,
    PREC_REL,
    PREC_SHIFT,
    PREC_ADD,
    PREC_MUL,
    NPREC
};

/* reuse ascii value as token id. */
enum {
    TOK_XXX,
    TOK_AUTO,                   /* 1 */
    TOK_EXTERN,
    TOK_REGISTER,
    TOK_STATIC,
    TOK_TYPEDEF,
    TOK_BREAK,
    TOK_CASE,
    TOK_CONTINUE,
    TOK_DEFAULT,
    TOK_DO,
    TOK_ELSE,
    TOK_FOR,
    TOK_GOTO,
    TOK_IF,
    TOK_RETURN,
    TOK_SWITCH,
    TOK_WHILE,
    TOK_SIZEOF,
    TOK_VOID,           
    TOK_BOOL,
    TOK_CHAR,           
    TOK_SHORT,
    TOK_ENUM,           
    TOK_SIGNED,
    TOK_INT,            
    TOK_UNSIGNED,
    TOK_LONG,           
    TOK_FLOAT,
    TOK_DOUBLE,         
    TOK_STRUCT,
    TOK_UNION,
    TOK_INLINE,                 /* 32, non-used: 34,39 */
    TOK_CONST = 48,
    TOK_VOLATILE,
    TOK_RESTRICT,
    TOK_MULEQ,
    TOK_ADDEQ,
    TOK_SUBEQ,
    TOK_DIVEQ,
    TOK_MODEQ,                  /* 55, non-used: 56,57 */
    TOK_XOREQ = 65,
    TOK_ANDEQ,
    TOK_OREQ,
    TOK_SHLEQ,
    TOK_SHREQ,
    TOK_SHL,
    TOK_SHR,
    TOK_GEQ,
    TOK_LEQ,
    TOK_EQ,
    TOK_NEQ,
    TOK_INCR,
    TOK_DECR,
    TOK_DEREF,
    TOK_ANDAND,
    TOK_OROR,
    TOK_ELLIPSIS,
    TOK_ICON,
    TOK_FCON,
    TOK_SCON,
    TOK_WSCON,
    TOK_NAME,                   /* 86, non-used: 87-90,92,95-115 */
    TOK_INVALID = 116,
    TOK_PASTE,
    TOK_PP_NUMBER,
    TOK_PP_CHAR,
    TOK_PP_WCHAR,
    TOK_PP_HEADER_NAME,
    TOK_PP_MACRO_ARG,           /* 122 */
    TOK_EOF = 127,
    NTOKEN
};

enum {
    TOK_FLAG_BOL           = 1 << 0, /* beginning of line */
    TOK_FLAG_PREV_WHITE    = 1 << 1, /* leading whitespace */
    TOK_FLAG_STRINGIFY_ARG = 1 << 2, /* stringify argument */
    TOK_FLAG_PASTE_LEFT    = 1 << 3, /* appear on the left side of ## */
    TOK_FLAG_BOF           = 1 << 4, /* beginning of file */
    TOK_FLAG_NO_EXPAND     = 1 << 5, /* do NOT macro-expand this token */
};

struct token {
    short id;
    short flags;
    union {
        struct symbol *sym;       /* identifier */
        const char *str;          /* string or number */
        int macro_arg_i;          /* macro arg index */
    } val;
};

/* symbol kind */
enum { SYM_MACRO = 1 };

enum {
    SYM_FLAG_DISABLED = 1 << 0,  /* disabled macro */
    SYM_FLAG_MACRO_ARG = 1 << 2, /* macro argument */
};

/* symtab lookup options */
enum { OPT_SEARCH = 0, OPT_CREATE };

struct symbol {
    struct symbol *link;          /* next symbol in symtab */
    const char *name;
    unsigned int hash;
    unsigned int len;

    /* used by cpp */
    struct macro *macro;
    int arg_i;
    short flags;
    short keyword_id;
    char kind;
    char dir_no;
    char directive_p;
    char builtin_id;

    /* used by parser */
    struct type *type;
    struct type *suetype;       /* struct/union/enum type */
    struct node *label;         /* label in function */
    long offset;
    int block;
    int sueblock;               /* struct/union/enum blockno */
    int vconst;
    int line;
    char sclass;
    char anonymous;
    char defined;
    char used;

    /* used by backend */
    int symndx;                 /* symtab index */
};

struct svalue {
    long i;
    double d;
    short id;
    short base;
    int suffix;
};

struct cpp_dir {
    const char **include;
    int ninclude, maxinclude;
};

struct cached_include {
    const char *filename;
    struct symbol *sym;
    struct cached_include *next;
};

struct cpp_state {
    int skipping:1;
    int in_directive:1;
    int bracket_header:1;       /* parsing bracket header name */
    int parsing_args:1;         /* parsing arguments? */
    int prevent_expansion:8;
    int standalone:1;
    int seen_eof:1;             /* seen EOF */
};

/* file change reason */
enum { FC_ENTER, FC_LEAVE, FC_RENAME };

/* source location history */
struct hist {
    struct hist *link;
    const char *name;       /* file name */
    int line;               /* beginning lineno */
    int offset;             /* #line directive */
    int reason;             /* file change reason */
};

struct cpp_callbacks {
    void (*file_change)(int, int);
    void (*line_change)(int, struct token *);
};

/* #if stack */
struct ifstack {
    int type:8;
    int was_skipping:1;
    int skip_else:1;
    struct ifstack *link;
};

/* macro arguments */
struct macro_arg {
    struct token *token, *exp;
    int ntoken, nexp;
    struct token *stringified;
    int token_alloc, arg_alloc;
};

/* macro type */
enum { MACRO_OBJ, MACRO_FUNC, MACRO_SPECIAL };

struct macro {
    int type:8;
    int variadic:1;
    int nparam, maxparam;
    int nbody, maxbody;
    struct symbol **param;
    struct token *body;
    const char * (*handler)(struct token *);
};

enum {
    TOKENS_KIND_INDIRECT,
    TOKENS_KIND_DIRECT
};

/* macro expansion context */
struct cpp_context {
    struct cpp_context *prev;
    union {
        struct token *token;
        struct token **ptoken;
    } first, end;
    struct symbol *sym;
    void *buff;
    int tokens_kind;
};

struct directive {
    const char *name;
    void (*handler)(void);
    int flags;
};

struct line_note {
    const unsigned char *pos;
    int type;
};

/* A buffer represents a file's content. */
struct buffer {
    const char *path;               /* file full path */
    const char *name;               /* buffer name */
    int bol:1;                      /* beginning of line? */
    int bof:1;                      /* first non-whitespace? */
    int endif:1;                    /* #endif at the end of file seen */
    int need_line:1;                /* need a nextline? */
    const unsigned char *cur;       /* current position */
    const unsigned char *limit;     /* end position */
    const unsigned char *line_base; /* current line base */
    const unsigned char *next_line; /* next line beginning */
    struct line_note *notes;        /* array of notes */
    unsigned int cur_note;          /* cuurent note index */
    unsigned int notes_used;        /* number of notes */
    unsigned int notes_alloc;       /* number of notes allocated */
    struct ifstack *ifstack;        /* top of 'if' stack */
    struct ifstack *ifndef_stack;   /* #ifndef stack at beginning of file */
    struct symbol *ifndef_sym;      /* #ifndef ident at beginning of file */
    struct buffer *prev;            /* previous buffer */
    unsigned char *buf;             /* content of this buffer */
};

/* storage class */
enum {
    CXXX,
    CAUTO,
    CEXTERN,
    CSTATIC,
    CTYPEDEF,
    CREGISTER,
    CGLOBAL,
    CENUM,
    CPARAM,
    NCTYPE
};

/* type qaulifiers */
enum {
    QXXX       = 0,
    QCONST     = 1 << 0,
    QVOLATILE  = 1 << 1,
    QMASK      = QCONST | QVOLATILE
};

enum {
    TXXX,
    TBOOL,
    TCHAR,
    TUCHAR,
    TSHORT,
    TUSHORT,
    TINT,
    TUINT,
    TLONG,
    TULONG,
    TLLONG,
    TULLONG,
    TFLOAT,
    TDOUBLE,
    TPTR,
    TVOID,
    TFUNC,
    TARRAY,
    TSTRUCT,
    TUNION,
    TENUM,
    NTYPE,

    TCONST = NTYPE,
    TVOLATILE,
    TUNSIGNED,
    TSIGNED,
    TAUTO,
    TEXTERN,
    TSTATIC,
    TTYPEDEF,
    TREGISTER,
    TTYPENAME,
    NALLTYPE
};

enum {
    BBOOL      = 1 << TBOOL,
    BCHAR      = 1 << TCHAR,
    BUCHAR     = 1 << TUCHAR,
    BSHORT     = 1 << TSHORT,
    BUSHORT    = 1 << TUSHORT,
    BINT       = 1 << TINT,
    BUINT      = 1 << TUINT,
    BLONG      = 1 << TLONG,
    BULONG     = 1 << TULONG,
    BLLONG     = 1 << TLLONG,
    BULLONG    = 1 << TULLONG,
    BFLOAT     = 1 << TFLOAT,
    BDOUBLE    = 1 << TDOUBLE,
    BVOID      = 1 << TVOID,
    BPTR       = 1 << TPTR,
    BFUNC      = 1 << TFUNC,
    BARRAY     = 1 << TARRAY,
    BSTRUCT    = 1 << TSTRUCT,
    BUNION     = 1 << TUNION,
    BENUM      = 1 << TENUM,
    BCONST     = 1 << TCONST,
    BVOLATILE  = 1 << TVOLATILE,
    BSIGNED    = 1 << TSIGNED,
    BUNSIGNED  = 1 << TUNSIGNED,
    BAUTO      = 1 << TAUTO,
    BEXTERN    = 1 << TEXTERN,
    BSTATIC    = 1 << TSTATIC,
    BTYPEDEF   = 1 << TTYPEDEF,
    BREGISTER  = 1 << TREGISTER,
    BTYPENAME  = 1 << TTYPENAME,

    BINTEGER   = BBOOL | BCHAR | BUCHAR | BSHORT | BUSHORT | BINT | BUINT |
                 BLONG | BULONG | BLLONG | BULLONG | BENUM,
    BNUMBER    = BINTEGER | BFLOAT | BDOUBLE,

    BCLASS     = BAUTO | BEXTERN | BSTATIC | BTYPEDEF | BREGISTER,
    BQUAL      = BCONST | BVOLATILE,
};

struct type {
    struct type *link;          /* array,ptr,function return,struct fields */
    struct type *next;          /* field/proto list */
    struct symbol *sym;         /* field's name */
    struct symbol *tag;         /* sue type' name */
    long size;
    long offset;
    char etype;
    char align;
    char qual;
    char oldstyle;
    char nbits;
    char bitoff;
};

/* decl kind */
enum {
    DMARK,
    DAUTO,
    DSUE,
    DLABEL
};

struct decl {
    struct decl *link;
    struct symbol *sym;
    struct type *type;
    long offset;
    int block;
    int vconst;
    int line;
    char sclass;
    char defined;
    char used;
    char kind;
};

/* node op */
enum {
    OXXX,
    OLIST,
    OCOMMA,
    OAS,                        /* assign */
    OASI,
    OASMUL,
    OASUMUL,
    OASADD,
    OASSUB,
    OASDIV,
    OASUDIV,
    OASMOD,
    OASUMOD,
    OASXOR,
    OASAND,
    OASOR,
    OASSHL,
    OASSHR,
    OASUSHR,
    OCOND,                      /* conditional */
    OADD,                       /* binary */
    OSUB,
    OMUL,
    OUMUL,
    ODIV,
    OUDIV,
    OMOD,
    OUMOD,
    OAND,
    OOR,
    OSHL,
    OSHR,
    OUSHR,
    OXOR,
    OLT,
    OLE,
    OGT,
    OGE,
    OEQ,
    ONE,
    OANDAND,
    OOROR,
    OCAST,                      /* cast */
    OPREINC,                    /* unary */
    OPREDEC,
    OPOS,
    ONEG,
    OCOM,
    ONOT,
    OADDR,
    OINDIR,
    OSIZEOF,
    OPOSTINC,                   /* postfix */
    OPOSTDEC,
    ODOT,
    OFUNC,
    ONAME,                      /* primary */
    OCONST,
    OSTRING,
    OCOMPOUND,
    OINIT,                      /* init */
    OELEM,
    OARRAY,
    OPROTO,                     /* misc */
    OBIT,
    OZERO,
    OREGISTER,
    OINDREG,
    OIF,                        /* statement */
    OWHILE,
    ODOWHILE,
    OFOR,
    OSWITCH,
    OCASE,
    OLABEL,
    OGOTO,
    OBREAK,
    OCONTINUE,
    ORETURN,
    NOTYPE
};

union value {
    long i;
    double d;
    void *p;
    char *cstring;
    int *wstring;
};

struct node {
    struct node *left;
    struct node *right;
    struct type *type;
    struct symbol *sym;
    union value v;
    long offset;
    int line;
    char op;
    char sclass;
    char addable;
    char complex;
    
    /* used by backend */
    char reg;
    char index;
    char scale;
    char areg1;
    char areg2;
    int label;
    long aoffset;
};

struct fmt {
    va_list args;
    char *start;
    char *stop;
    char *to;
    FILE *stream;
    int c;                      /* the char */
    int nfmt;
};

/* builtin symbols */
enum {
    BUILTIN_XXX,
    BUILTIN_FUNC,
    BUILTIN_VA_START,
    BUILTIN_VA_ARG,
    BUILTIN_VA_COPY,
    BUILTIN_VA_END,
    BUILTIN_VA_LIST,
    NBUILTIN
};

struct section {
    int sh_name;                /* elf section name */
    int sh_type;                /* elf section type */
    int sh_flags;               /* elf section flags */
    int sh_info;                /* elf misc innformation */
    int sh_addralign;           /* elf address alignment boundary */
    int sh_entsize;             /* elf size of entries, if section has table */
    unsigned long sh_addr;      /* elf virtual address in memory */
    unsigned long sh_offset;    /* elf offset in file */
    unsigned long sh_size;      /* elf size of section */
    struct section *link;       /* link section */
    struct section *reloc;      /* relocation section */
    char *data;                 /* section data */
    size_t data_len;
    size_t data_alloc;
    int shndx;                  /* section table index */
    int symndx;                 /* section symtab index */
    char name[1];               /* section name (MUST BE THE LAST FIELD) */
};

/* cpp.c */
extern void cpp_init(const char *);
extern void cpp_get_token(struct token *);
extern void cpp_add_include(const char *, int);
extern void cpp_define(const char *);
extern void cpp_undef(const char *);
extern int cpp_macro_defined_p(const char *);
extern const char *tok2s(struct token *);
extern void cpp_resolve_location(int, const char **, int *);

extern struct cpp_callbacks cpp_callbacks;
extern struct hist *hist;
extern int lineno;

/* parse.c */
extern void parse(void);
extern void mkfield(struct type *, struct type *, const char *);
extern struct type *mkproto(struct type *, ...);
extern struct type *mktag(const char *name, int);
extern struct type *thisfn;
extern long stkoffset;
extern int nearln;

/* sub.c */
extern void fmtinit(void);
extern void error(struct node *, const char *, ...);
extern void warn(struct node *, const char *, ...);
extern void fatal(struct node *, const char *, ...);
extern int errors, warnings;

extern void *eval_string(struct token *, int, size_t *);
extern void eval_number(struct token *, struct svalue *);
extern struct node *node(int, struct node *, struct node *);
extern struct node *node1(int, struct node *, struct node *);
extern void nodnew(struct node *, int, struct node *, struct node *);
extern struct node *invert(struct node *);
extern struct node *newlist(struct node *, struct node *);
extern int bitno(int);
extern int bitwiseq(int);
extern int simpleq(int);
extern int btot(int);
extern int ttoq(int);
extern int simplec(int);
extern struct type *simplet(int, struct type *);
extern int typebitor(int, int);
extern struct symbol *anonymous(void);
extern struct symbol *mkstatic(struct symbol *);
extern struct node *cnstnode(struct type *, ...);
extern long convltox(long, int);
extern int log2i(size_t);
extern void prstruct(struct type *);
extern void prdecl(struct symbol *);
extern void prtree(struct node *, const char *);

/* typechk.c */
extern int tcompat(struct node *, struct type *, struct type *, int []);
extern int tconv(struct node *);
extern void conv(struct node *);
extern int vbconst(struct node *);

/* types.c */
extern void types_init(void);
extern struct type *typcpy(struct type *);
extern struct type *ptrtyp(struct type *);
extern struct type *arraytyp(struct type *, long);
extern struct type *functyp(struct type *, struct type *, int);
extern struct type *suetyp(int);
extern struct type *unqual(struct type *);
extern struct type *qual(int, struct type *);
extern int variadic(struct type *);
extern int eqtype(struct type *, struct type *, int);
extern struct type *compose(struct type *, struct type *);
extern struct type *decay(struct type *);
extern struct type *promote(struct type * );
extern void sualign(struct type *);
extern struct type *find_field(struct symbol *, struct type *, long *);

extern struct type *types[NTYPE];
extern char typei[NTYPE];
extern char typeu[NTYPE];
extern char typefd[NTYPE];
extern char typesu[NTYPE];
extern char typesue[NTYPE];
extern char typeaf[NTYPE];
extern int tand[];
extern int tnot[];
extern int tneg[];
extern int trel[];
extern int tadd[];
extern int tsub[];
extern int tmul[];
extern int tindir[];
extern int tdot[];
extern int tfunc[];
extern int targ[];
extern int tcast[];
extern int tasgn[];
extern int tasadd[];
extern char tab[NTYPE][NTYPE];

/* tokens.c */
extern void tokens_init(void);
extern char map[256];
extern int kinds[NTOKEN];
extern char precs[NTOKEN];
extern const char *lexmes[NTOKEN];
extern int bops[NTOKEN];
extern int uops[NTOKEN];
extern const char *tnames[NALLTYPE];
extern const char *cnames[NCTYPE];
extern const char *onames[NOTYPE];

/* symtab.c */
extern struct symbol *lookupn(const char *, size_t, int);
extern struct symbol *lookup(const char *, int);
extern void foreach(void (*)(struct symbol *, void *), void *);
extern void dump_symtab(void);

/* fmt.c */
extern int fmtinstall(int, int (*)(struct fmt *));
extern int print(const char *, ...);
extern int vfprint(FILE *, const char *, va_list);
extern int fprint(FILE *, const char *, ...);
extern int vsnprint(char *, size_t, const char *, va_list);
extern int snprint(char *, size_t, const char *, ...);
extern int fmtstrcpy(struct fmt *, const char *);
extern int fmtprint(struct fmt *, const char *, ...);

/* gen.c */
extern void codegen(struct node *, struct node *, struct node *);
extern int genlab(int);

/* cc.c */
extern int cc1(const char *, const char *, char **);

extern const char *main_input_filename;
extern const char *main_output_filename;

/* machine dependent */
extern void ginit(void);
extern void gfini(void);
extern long gstring(const char *, size_t);
extern void gdata(struct symbol *, struct type *, struct node *);
extern void gconv(struct node *);
extern void gfunc(struct node *, struct node *, struct node *);
extern void gexpr(struct node *, struct node *);
extern void greturn(struct node *);
extern void glabel(int);
extern void gbranch(int, int, int);
extern void gtest(struct node *, int, int);
extern void regalloc(struct node *, struct node *, struct node *);
extern void regfree(struct node *);

extern char typsize[NTYPE];
extern int ncast[NTYPE];

/* main.c */
extern struct options options;

/* section.c */
extern struct section *newsec(const char *, int, int);
extern void sec_set_data(struct section *, const void *, size_t, size_t);
extern size_t sec_add_data(struct section *, const void *, size_t);
extern size_t sec_add_str(struct section *, const char *);
extern size_t sec_extend(struct section *, size_t);
extern void sec_align(struct section *, int);
extern int sec_find_sym(struct section *, const char *);
extern int sec_add_sym(struct section *, const char *, int, int, int, size_t, size_t);
extern struct section *newsymtab(const char *, int, int, const char *, int);
extern void sec_add_rela(struct section *, struct section *, size_t, int, int, long);
extern void sec_sort_syms(struct section *);

extern struct section **sections;
extern int sections_used;

#endif  /* CC_H */
