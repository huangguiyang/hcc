#include "compat.h"
#include "cc.h"
#include <limits.h>
#include <time.h>
#include <errno.h>

static void handle_directive(int);
static void pop_context(void);
static void expand(struct token *);
static long parse_expr(void);
static long parse_cond(void);
static int parse_int(void);
static void cpp_error(const char *, ...);
static void cpp_warn(const char *, ...);

#define ASSIGN_TOKEN_P(t) (kinds[(t)->id] == '=')
#define CACHED_INCLUDE_SLOTS 512

static struct cpp_state _state;
static struct buffer *_buffer;
static struct cpp_context _base_context;
static struct cpp_context *_context = &_base_context;
static struct cpp_dir *_bracket_include;
static struct cpp_dir *_quote_include;
static struct directive *_directive;
static struct symbol *_va_args, *_defined;
static struct cpp_context *_free_contexts;
static struct ifstack *_free_ifstacks;
static struct cached_include *_cached_includes[CACHED_INCLUDE_SLOTS];
static struct token _backup[4];
static int _lookaheads, _cur_token;
static struct token _token;

struct cpp_callbacks cpp_callbacks;
struct hist *hist;
int lineno;

#define DIR_FLAG_COND (1 << 0)           /* conditional directive */
#define DIR_FLAG_BOF  (1 << 1)           /* beginning of file */
#define DIR_FLAG_INL  (1 << 2)           /* include directive */

enum {
    D_DEFINE,
    D_UNDEF,
    D_INCLUDE,
    D_IF,
    D_IFDEF,
    D_IFNDEF,
    D_ELIF,
    D_ELSE,
    D_ENDIF,
    D_LINE,
    D_ERROR,
    D_WARNING,
    D_PRAGMA,
    NDIRECTIVE
};

static void do_define(void);
static void do_include(void);
static void do_if(void);
static void do_ifdef(void);
static void do_elif(void);
static void do_else(void);
static void do_endif(void);
static void do_undef(void);
static void do_line(void);
static void do_error(void);
static void do_pragma(void);

static struct directive _dirtab[] = {
    "define",   do_define,   0,
    "undef",    do_undef,    0,
    "include",  do_include,  DIR_FLAG_INL,
    "if",       do_if,       DIR_FLAG_COND,
    "ifdef",    do_ifdef,    DIR_FLAG_COND,
    "ifndef",   do_ifdef,    DIR_FLAG_COND,
    "elif",     do_elif,     DIR_FLAG_COND,
    "else",     do_else,     DIR_FLAG_COND,
    "endif",    do_endif,    DIR_FLAG_COND,
    "line",     do_line,     0,
    "error",    do_error,    0,
    "warning",  do_error,    0,
    "pragma",   do_pragma,   0,
};

const char *tok2s(struct token *t)
{
    switch (t->id) {
    case TOK_NAME:
        return t->val.sym->name;
    case TOK_ICON:
    case TOK_FCON:
    case TOK_PP_NUMBER:
    case TOK_PP_HEADER_NAME:
    case TOK_SCON:
    case TOK_WSCON:
    case TOK_PP_CHAR:
    case TOK_PP_WCHAR:
    case TOK_INVALID:
        return t->val.str;
    default:
        return lexmes[t->id];
    }
}

void cpp_resolve_location(int line, const char **pname, int *pline)
{
    struct hist *p, *q;
    int c, r, l, rel;
    const char *oname;

    if (hist == NULL)
        return;
    if (line == 0)
        line = lineno;
    if (line <= 0)
        return;

    p = hist;
    while (line < p->line)
        p = p->link;

    switch (p->reason) {
    case FC_ENTER:
        if (pname)
            *pname = p->name;
        if (pline)
            *pline = line - p->line + 1;
        break;

    case FC_LEAVE:
        c = 0;
        rel = 0;
        l = line + 1;
        for (q = p; q; q = q->link) {
            r = q->reason;
            if (c == 0) {
                rel += l - q->line;
                if (r == FC_ENTER) {
                    oname = q->name;
                    break;
                }
                if (r == FC_RENAME) {
                    cpp_resolve_location(q->line, &oname, NULL);
                    rel += q->offset - 1;
                    break;
                }
            }
            if (r == FC_LEAVE)
                c += 1;
            if (r == FC_ENTER)
                c -= 1;
            l = q->line;
        }
        if (q == NULL)
            die("can't resolve line %d", line);
        if (pname)
            *pname = oname;
        if (pline)
            *pline = rel;
        break;

    case FC_RENAME:
        if (p->name) {
            oname = p->name;
        } else {
            c = 0;
            for (q = p; q; q = q->link) {
                r = q->reason;
                if (c == 0)
                    if (q->name)
                        break;
                if (r == FC_LEAVE)
                    c += 1;
                if (r == FC_ENTER)
                    c -= 1;
            }
            if (q == NULL)
                die("can't resolve line %d", line);
            oname = q->name;
        }
        if (pname)
            *pname = oname;
        if (pline)
            *pline = p->offset + line - p->line;
        break;

    default:
        abort();        
    }
}

static void new_hist(const char *name, int offset, int reason)
{
    struct hist *p;

    p = xmalloc(sizeof *p);
    p->name = name;
    p->line = lineno;
    p->offset = offset;
    p->reason = reason;
    p->link = hist;
    hist = p;

    if (cpp_callbacks.file_change)
        cpp_callbacks.file_change(lineno, reason);
}

static struct buffer *new_buffer(void)
{
    struct buffer *buffer;
    
    buffer = zmalloc(sizeof(struct buffer));
    buffer->bol = 1;
    buffer->bof = 1;
    buffer->need_line = 1;
    return buffer;
}

static void free_buffer(struct buffer *buffer)
{
    /* free ifstack */
    struct ifstack *ifs = buffer->ifstack;
    while (ifs) {
        struct ifstack *next = ifs->link;
        ifs->link = _free_ifstacks;
        _free_ifstacks = ifs;
        ifs = next;
    }
    free(buffer->notes);
    free(buffer->buf);
    free(buffer);
}

static void set_stdin_to_binary_mode(void)
{
    /* TODO: set stdin to binary mode */
}

static struct buffer *buffer_with_file(const char *path)
{
    FILE *fd;
    size_t size, total, count;
    unsigned char *buf;
    struct buffer *buffer;
    const char *name;

    if (path == NULL || path[0] == '\0') {
        fd = stdin;
        set_stdin_to_binary_mode();
        size = 8 * 1024;
        name = "<stdin>";
    } else {
        fd = fopen(path, "rb");
        if (fd == NULL)
            die_errno("can't open file '%s'", path);
        size = file_size(path);
        name = path;
    }

    /* +1 to place the end-of-buffer character */
    buf = xmalloc(size + 1);
    total = 0;
    while ((count = fread(buf + total, 1, size - total, fd)) > 0) {
        total += count;
        if (total == size) {
            size *= 2;
            buf = xrealloc(buf, size + 1);
        }
    }

    if (ferror(fd))
        die_errno("read file error");
    if (fd != stdin)
        fclose(fd);

    buf[total] = '\n';
    buffer = new_buffer();
    buffer->buf = buf;
    buffer->cur = buffer->line_base = buffer->next_line = buf;
    buffer->limit = &buf[total];
    buffer->path = path;
    buffer->name = name;
    return buffer;
}

static struct buffer *buffer_with_string(const char *str, size_t len)
{
    struct buffer *buffer;

    buffer = new_buffer();
    buffer->buf = xmalloc(len + 1);
    memcpy(buffer->buf, str, len);
    buffer->buf[len] = '\n';
    buffer->cur = buffer->line_base = buffer->next_line = buffer->buf;
    buffer->limit = &buffer->buf[len];
    return buffer;
}

static void push_buffer(struct buffer *buffer)
{
    struct buffer *old = _buffer;

    buffer->prev = old;
    _buffer = buffer;
    if (buffer->name)
        new_hist(buffer->name, 0, FC_ENTER);
}

static void pop_buffer(void)
{
    struct buffer *old = _buffer;
    struct ifstack *ifs;

    for (ifs = old->ifstack; ifs; ifs = ifs->link)
        cpp_error("unterminated conditional directive");

    /* in case of missing #endif */
    _state.skipping = 0;
    _buffer = old->prev;

    /* if this is the last buffer, dont add a history entry */
    if (old->name && _buffer) {
        lineno++;
        new_hist(NULL, 0, FC_LEAVE);
    }

    free_buffer(old);
}

static void push_cond(int skip, int type)
{
    struct ifstack *ifs;

    if (_free_ifstacks) {
        ifs = _free_ifstacks;
        _free_ifstacks = _free_ifstacks->link;
    } else {
        ifs = xmalloc(sizeof(struct ifstack));
    }

    ifs->was_skipping = _state.skipping;
    ifs->skip_else = _state.skipping || !skip;
    ifs->type = type;
    ifs->link = _buffer->ifstack;
    _buffer->ifstack = ifs;
    _state.skipping = skip;
}

static void pop_cond(void)
{
    struct ifstack *ifs = _buffer->ifstack;
    _buffer->ifstack = ifs->link;
    _state.skipping = ifs->was_skipping;
    /* free list */
    ifs->link = _free_ifstacks;
    _free_ifstacks = ifs;
}

static inline unsigned int hash_include_filename(const char *name)
{
    unsigned int h;

    h = strhash(name);
    h &= (CACHED_INCLUDE_SLOTS - 1);
    return h;
}

static struct cached_include *find_cached_include(const char *name)
{
    unsigned int h;
    struct cached_include *ci;

    h = hash_include_filename(name);
    ci = _cached_includes[h];
    for (; ci; ci = ci->next)
        if (!strcmp(ci->filename, name))
            return ci;

    return NULL;
}

static void add_cached_include(const char *filename, struct symbol *sym)
{
    unsigned int h;
    struct cached_include *ci;
    
    if (find_cached_include(filename))
        return;

    assert(sym);
    h = hash_include_filename(filename);
    ci = xmalloc(sizeof(struct cached_include));
    ci->filename = filename;
    ci->sym = sym;
    ci->next = _cached_includes[h];
    _cached_includes[h] = ci;
}

#define INCLINE(buffer)         \
    do {                        \
        if ((buffer)->name)     \
            lineno++;           \
    } while (0)

static void add_line_note(struct buffer *buffer, const unsigned char *pos, int type)
{
    if (buffer->notes_used == buffer->notes_alloc) {
        buffer->notes_alloc = buffer->notes_alloc * 2 + 20;
        buffer->notes = xrealloc(buffer->notes,
                                 buffer->notes_alloc * sizeof(struct line_note));
    }
    buffer->notes[buffer->notes_used].pos = pos;
    buffer->notes[buffer->notes_used].type = type;
    buffer->notes_used++;
}

static void process_line_notes(struct buffer *buffer)
{
    while (1) {
        struct line_note *note = &buffer->notes[buffer->cur_note];

        if (note->pos > buffer->cur)
            break;

        buffer->cur_note++;
        
        if (note->type == '\\') {
            buffer->line_base = note->pos;
            INCLINE(buffer);
        } else {
            assert(0);
        }
    }
}

/* return an unescaped logical line */
static void next_clean_line(struct buffer *buffer)
{
    const unsigned char *s, *pbackslash;
    unsigned char *d, c;

    pbackslash = NULL;
    buffer->cur_note = buffer->notes_used = 0;
    buffer->cur = buffer->line_base = buffer->next_line;
    buffer->need_line = 0;
    s = buffer->next_line;

    /* search '\n', '\r', '\\' */
    while (1) {
        c = *s;
        if (c == '\n' || c == '\r')
            break;
        if (c == '\\')
            pbackslash = s;
        s++;
    }

    /* d must be '\n' or '\r' */
    d = (unsigned char *)s;
    if (d == buffer->limit)
        goto done;
    if (c == '\r' && s[1] == '\n') {
        s++;
        if (s == buffer->limit)
            goto done;
    }
    if (pbackslash == NULL)
        goto done;
    if (d-1 != pbackslash)
        goto done;

    /* have an escaped newline '\[\r]\n' */
    add_line_note(buffer, d-1, '\\');
    d -= 2;

    while (1) {
        c = *++s;
        *++d = c;
        if (c == '\n' || c == '\r') {
            if (c == '\r' && s != buffer->limit && s[1] == '\n')
                s++;
            if (s == buffer->limit)
                break;
            if (d[-1] != '\\')
                break;

            add_line_note(buffer, d-1, '\\');
            d -= 2;
        }
    }

done:
    *d = '\n';
    /* a sentinel note that should never be processed */
    add_line_note(buffer, d+1, '\n');
    buffer->next_line = s + 1;
}

static void line_comment(struct buffer *buffer)
{
    while (!ISNEWLINE(*buffer->cur))
        buffer->cur++;
    process_line_notes(buffer);
}

static void block_comment(struct buffer *buffer)
{
    const unsigned char *cur = buffer->cur;
    int c;

    cur++;
    while (1) {
        c = *cur++;
        if (c == '/' && cur[-2] == '*')
            break;
        if (ISNEWLINE(c)) {
            buffer->cur = cur - 1;
            process_line_notes(buffer);
            if (buffer->next_line >= buffer->limit) {
                cpp_error("unterminated comment");
                return;
            }
            next_clean_line(buffer);
            INCLINE(buffer);
            cur = buffer->cur;
        }
    }

    buffer->cur = cur;
    process_line_notes(buffer);
}

static void sequence(struct buffer *buffer, struct token *result,
                     const unsigned char *p)
{
    unsigned char terminator;
    const unsigned char *cur;
    char *buf;
    int c;

    cur = p;
    terminator = *cur++;
    if (terminator == 'L')
        terminator = *cur++;
    else if (terminator == '<')
        terminator = '>';

    while (1) {
        c = *cur++;
        if (c == terminator)
            break;
        if (ISNEWLINE(c)) {
            cur--;
            break;
        }
        if (c == '\\')
            cur++;
    }

    buf = strndup((const char *)p, cur - p);
    if (c != terminator) {
        cpp_error("unterminated sequence");
        result->id = TOK_INVALID;
    }

    result->val.str = buf;
    buffer->cur = cur;
}

#define IF_NEXT_IS(CHAR, THEN_ID, ELSE_ID)      \
    do {                                        \
        if (*buffer->cur == CHAR) {             \
            buffer->cur++;                      \
            result->id = THEN_ID;               \
        } else {                                \
            result->id = ELSE_ID;               \
        }                                       \
    } while (0)

static void lex_direct(struct token *result)
{
    int c;
    const unsigned char *p;
    struct buffer *buffer = _buffer;

fresh_line:
    if (buffer->need_line)
        next_clean_line(buffer);
    
    result->flags = 0;
    if (buffer->bof)
        result->flags |= TOK_FLAG_BOF;
    if (buffer->bol)
        result->flags |= TOK_FLAG_BOL;
fresh_column:
    if (buffer->cur >= buffer->notes[buffer->cur_note].pos)
        process_line_notes(buffer);
start:
    c = *buffer->cur++;
    switch (c) {
    case '\n':
        if (buffer->cur > buffer->limit) {
            buffer->cur--;
            result->id = TOK_EOF;
            return;
        }
        INCLINE(buffer);
        buffer->bol = 1;
        buffer->need_line = 1;
        if (_state.in_directive) {
            /* return EOF if in directive. */
            result->id = TOK_EOF;
            return;
        }
        goto fresh_line;

    case ' ': case '\t': case '\v': case '\f': case '\r':
        while (ISWHITESPACE(*buffer->cur))
            buffer->cur++;
        result->flags |= TOK_FLAG_PREV_WHITE;
        goto fresh_column;

    case 'L':
        if (*buffer->cur == '\'' || *buffer->cur == '"') {
            result->id = *buffer->cur == '\'' ? TOK_PP_WCHAR : TOK_WSCON;
            sequence(buffer, result, buffer->cur - 1);
            break;
        }
        /* fall through */
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
    case '_':
        p = buffer->cur - 1;
        while (ISDIGITLETTER(*buffer->cur))
            buffer->cur++;
        result->val.sym = lookupn((const char *)p, buffer->cur - p, OPT_CREATE);
        result->id = TOK_NAME;
        break;

    case '+':
        if (*buffer->cur == '+') {
            buffer->cur++;
            result->id = TOK_INCR;
        } else if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_ADDEQ;
        } else {
            result->id = '+';
        }
        break;

    case '-':
        if (*buffer->cur == '-') {
            buffer->cur++;
            result->id = TOK_DECR;
        } else if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_SUBEQ;
        } else if (*buffer->cur == '>') {
            buffer->cur++;
            result->id = TOK_DEREF;
        } else {
            result->id = '-';
        }
        break;

    case '*': IF_NEXT_IS('=', TOK_MULEQ, '*'); break;
    case '=': IF_NEXT_IS('=', TOK_EQ, '='); break;
    case '!': IF_NEXT_IS('=', TOK_NEQ, '!'); break;
    case '^': IF_NEXT_IS('=', TOK_XOREQ, '^'); break;
    case '#': IF_NEXT_IS('#', TOK_PASTE, '#'); break;
    case '%': IF_NEXT_IS('=', TOK_MODEQ, '%'); break;

    case '&':
        if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_ANDEQ;
        } else if (*buffer->cur == '&') {
            buffer->cur++;
            result->id = TOK_ANDAND;
        } else {
            result->id = '&';
        }
        break;

    case '|':
        if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_OREQ;
        } else if (*buffer->cur == '|') {
            buffer->cur++;
            result->id = TOK_OROR;
        } else {
            result->id = '|';
        }
        break;

    case '<':
        if (_state.bracket_header) {
            result->id = TOK_PP_HEADER_NAME;
            sequence(buffer, result, buffer->cur - 1);
            break;
        }
        if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_LEQ;
        } else if (*buffer->cur == '<') {
            buffer->cur++;
            IF_NEXT_IS('=', TOK_SHLEQ, TOK_SHL);
        } else {
            result->id = '<';
        }
        break;

    case '>':
        if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_GEQ;
        } else if (*buffer->cur == '>') {
            buffer->cur++;
            IF_NEXT_IS('=', TOK_SHREQ, TOK_SHR);
        } else {
            result->id = '>';
        }
        break;

    case '(': case ')': case '{': case '}': case '[': case ']':
    case ',': case ';': case '~': case '?': case ':':
        result->id = c;
        break;

    case '/':
        if (*buffer->cur == '/') {
            line_comment(buffer);
            result->flags |= TOK_FLAG_PREV_WHITE;
            goto start;
        } else if (*buffer->cur == '*') {
            block_comment(buffer);
            result->flags |= TOK_FLAG_PREV_WHITE;
            goto fresh_column;
        } else if (*buffer->cur == '=') {
            buffer->cur++;
            result->id = TOK_DIVEQ;
        } else {
            result->id = '/';
        }
        break;

    case '\'':
    case '"':
        result->id = c == '\'' ? TOK_PP_CHAR : TOK_SCON;
        sequence(buffer, result, buffer->cur - 1);
        break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    number:
        p = buffer->cur - 1;
        while (ISDIGIT(*buffer->cur) || ISLETTER(*buffer->cur) ||
               *buffer->cur == '.' ||
               ((*buffer->cur == '+' || *buffer->cur == '-') &&
                (buffer->cur[-1] == 'e' || buffer->cur[-1] == 'E' ||
                 buffer->cur[-1] == 'p' || buffer->cur[-1] == 'P')))
            buffer->cur++;
        result->val.str = strndup((const char *)p, buffer->cur - p);
        result->id = TOK_PP_NUMBER;
        break;

    case '.':
        if (ISDIGIT(*buffer->cur)) {
            goto number;
        } else if (*buffer->cur == '.' && buffer->cur[1] == '.') {
            buffer->cur += 2;
            result->id = TOK_ELLIPSIS;
        } else {
            result->id = '.';
        }
        break;
        
    default:
        result->val.str = stringf("%c", c);
        result->id = TOK_INVALID;
        break;
    }

    buffer->bof = 0;
    buffer->bol = 0;
    buffer->endif = 0;
}

static void lex(struct token *result)
{
    while (1) {
        if (_lookaheads) {
            *result = _backup[_cur_token];
            _cur_token = (_cur_token + 1) % NELEMS(_backup);
            _lookaheads--;
        } else {
            lex_direct(result);
        }

        if (result->flags & TOK_FLAG_BOL) {
            if (result->id == '#') {
                handle_directive(result->flags & TOK_FLAG_BOF);
                continue;
            }

            if (cpp_callbacks.line_change)
                if (!_state.skipping && !_state.parsing_args && _buffer->name)
                    cpp_callbacks.line_change(lineno, result);
        }

        if (_state.in_directive)
            break;

        if (result->id == TOK_EOF) {
            if (_buffer->endif) {
                _buffer->endif = 0;
                add_cached_include(_buffer->path, _buffer->ifndef_sym);
            }
            pop_buffer();
            if (_buffer)
                continue;
            else
                break;
        }

        if (!_state.skipping)
            break;
    }

    _state.seen_eof = result->id == TOK_EOF;
}

/* unget tokens obtained from the lexer. */
static void lex_undo(struct token *t)
{
    int i;

    if (_lookaheads >= NELEMS(_backup))
        abort();

    i = (_cur_token + _lookaheads) % NELEMS(_backup);
    _backup[i] = *t;
    _lookaheads++;
}

static struct token *make_temp_token(void)
{
    return zmalloc(sizeof(struct token));
}

#define SEEN_EOL() (_state.seen_eof)

static void skip_rest_of_line(void)
{
    /* discard all macro contexts. */
    while (_context->prev)
        pop_context();
    
    if (!SEEN_EOL()) {
        struct token t;
        do
            lex(&t);
        while (t.id != TOK_EOF);
    }
}

static void check_eol(int expand)
{
    if (!SEEN_EOL()) {
        struct token t;
        expand ? cpp_get_token(&t) : lex(&t);
        if (t.id != TOK_EOF)
            cpp_error("extraneous token at the end of #%s", _directive->name);
    }
}

static void do_if(void)
{
    int skip = 1;

    if (!_state.skipping)
        skip = !parse_int();

    push_cond(skip, D_IF);
}

static void do_elif(void)
{
    struct ifstack *ifs = _buffer->ifstack;

    if (ifs) {
        if (ifs->type == D_ELSE)
            cpp_error("#elif after #else");

        ifs->type = D_ELIF;
        if (ifs->skip_else) {
            _state.skipping = 1;
        } else {
            _state.skipping = !parse_int();
            ifs->skip_else = !_state.skipping;
        }
    } else {
        cpp_error("#elif without #if");
    }
}

static void do_else(void)
{
    struct ifstack *ifs = _buffer->ifstack;

    if (ifs) {
        if (ifs->type == D_ELSE)
            cpp_error("#else after #else");

        ifs->type = D_ELSE;
        _state.skipping = ifs->skip_else;
        /* skip any future (erroneous) #else or #elif directives */
        ifs->skip_else = 1;
        check_eol(0);
    } else {
        cpp_error("#else without #if");
    }
}

static void do_endif(void)
{
    if (_buffer->ifstack) {
        /* detect #endif at the end of file */
        if (_buffer->ifndef_stack == _buffer->ifstack) {
            /*
             * clear the stack pointer, because it will be
             * freed and reused, we will then use `ifndef_sym' instead.
             */
            _buffer->ifndef_stack = NULL;
            _buffer->endif = 1;
        }
        check_eol(0);
        pop_cond();
    } else {
        cpp_error("#endif without #if");
    }
}

static void do_ifdef(void)
{
    int skip = 1;
    struct symbol *sym = NULL;
    int type = !strcmp(_directive->name, "ifdef") ? D_IFDEF : D_IFNDEF;

    if (!_state.skipping) {
        struct token t;

        lex(&t);
        if (t.id == TOK_NAME) {
            int b = MACRO_DEFINED_P(t.val.sym);
            skip = (type == D_IFDEF) ? !b : b;
            sym = t.val.sym; /* save for below use */

            check_eol(0);
        } else {
            cpp_error("expect identifier after '%s'", _directive->name);
        }
    }

    push_cond(skip, type);

    /* detect #ifndef at beginning of file */
    if ((_directive->flags & DIR_FLAG_BOF) && type == D_IFNDEF && sym) {
        _buffer->ifndef_sym = sym;
        _buffer->ifndef_stack = _buffer->ifstack;
    }
}

static const char *find_file_in_dirs(const char *name, struct cpp_dir *dir)
{
    const char *path;

    for (int i = 0; i < dir->ninclude; i++) {
        path = joinpath(dir->include[i], name);
        path = fullpath(path);
        if (file_exist(path))
            return path;
    }
    return NULL;
}

static const char *find_file(const char *name, int bracket)
{
    const char *path, *cur_dir;
    struct buffer *p;

    /* try current path */
    if (!bracket) {
        for (p = _buffer; p; p = p->prev)
            if (p->path)
                break;
        if (p == NULL)
            cur_dir = ".";
        else
            cur_dir = file_dirname(p->path);
        path = joinpath(cur_dir, name);
        path = fullpath(path);
        if (file_exist(path))
            return path;
    }

    if ((path = find_file_in_dirs(name, _quote_include)))
        return path;

    /* try system path last */
    if ((path = find_file_in_dirs(name, _bracket_include)))
        return path;

    return NULL;
}

static int include_file(const char *name, int bracket)
{
    const char *path;
    struct cached_include *ci;

    path = find_file(name, bracket);
    if (path == NULL)
        return -1;

    /* find cached include */
    ci = find_cached_include(path);
    if (ci && MACRO_DEFINED_P(ci->sym))
        return 0;

    push_buffer(buffer_with_file(path));
    return 0;
}

static char *lex_header_name(int *bracket)
{
    char *fname;
    struct token t;

    /* allows macro expansion */
    cpp_get_token(&t);
    if (t.id == TOK_SCON || t.id == TOK_PP_HEADER_NAME) {
        size_t len = strlen(t.val.str) - 2;
        fname = xmalloc(len + 1);
        memcpy(fname, t.val.str + 1, len);
        fname[len] = '\0';
        *bracket = t.id == TOK_PP_HEADER_NAME;
    } else {
        cpp_error("expect \"FILENAME\" or <FILENAME>");
        return NULL;
    }

    check_eol(0);
    return fname;
}

static void do_include(void)
{
    int bracket;
    char *fname;

    fname = lex_header_name(&bracket);
    if (!fname)
        return;
    if (fname[0] == '\0') {
        cpp_error("empty filename");
        free(fname);
        return;
    }

    /*
     * To make SEEN_EOL() to be true in `end_directive'.
     *
     * After `do_include' succeded, a new buffer is pushed
     * and we don't want the skipping in `end_directive' later
     * to skip tokens in the new buffer wrongly.
     */
    skip_rest_of_line();

    if (include_file(fname, bracket) < 0)
        cpp_error("'%s' file not found", fname);

    free(fname);
}

static void add_macro(struct symbol *sym, struct macro *m)
{
    sym->kind = SYM_MACRO;
    sym->macro = m;
}

static void remove_macro(struct symbol *sym)
{
    if (sym->kind == SYM_MACRO) {
        sym->kind = 0;
        sym->macro = NULL;
    }
}

static void save_parameter(struct macro *m, int i, struct symbol *sym)
{
    if (m->nparam >= m->maxparam) {
        m->maxparam = 2 * m->maxparam + 12;
        m->param = xrealloc(m->param, m->maxparam * sizeof(struct symbol *));
    }
    
    sym->flags |= SYM_FLAG_MACRO_ARG;
    sym->arg_i = i;
    m->param[m->nparam++] = sym;
}

static int read_parameters(struct macro *m)
{
    struct token t;

    lex(&t);
    if (t.id == TOK_NAME) {
        /* (a,b) or (a,b,...) */
        int i = 0;
        while (1) {
            if (t.id == TOK_NAME) {
                if (t.val.sym->flags & SYM_FLAG_MACRO_ARG) {
                    cpp_error("duplicate macro parameter name '%s'", tok2s(&t));
                    goto failure;
                }
                save_parameter(m, i++, t.val.sym);
            } else if (t.id == TOK_ELLIPSIS) {
                m->variadic = 1;
                save_parameter(m, i++, _va_args);
                lex(&t);
                break;
            } else {
                cpp_error("expect identifier or ...");
                goto failure;
            }
            lex(&t);
            if (t.id != ',')
                break;
            lex(&t);
        }
        if (t.id != ')') {
            cpp_error("unterminated macro parameter list");
            goto failure;
        }
    } else if (t.id == ')') {
        /* () */
    } else if (t.id == TOK_ELLIPSIS) {
        m->variadic = 1;
        save_parameter(m, 0, _va_args);
        lex(&t);
        if (t.id != ')') {
            cpp_error("missing ')' in macro parameter list");
            goto failure;
        }
    } else {
        cpp_error("missing ')' in macro parameter list");
    failure:
        skip_rest_of_line();
        return 0;
    }

    return 1;
}

static void save_replacement(struct macro *m, struct token *t)
{
    if (m->nbody >= m->maxbody) {
        m->maxbody = 2 * m->maxbody + 20;
        m->body = xrealloc(m->body, m->maxbody * sizeof(struct token));
    }

    m->body[m->nbody++] = *t;
}

static int read_replacements(struct macro *m, struct token *t)
{
    int seen_paste = 0;
    
    while (1) {
        struct token *p;

        if (t->id == TOK_EOF) {
            if (seen_paste) {
                cpp_error("'##' cannot appear at the end");
                return 0;
            }
            break;
        }
        save_replacement(m, t);
        p = &m->body[m->nbody-1];

        /* check __VA_ARGS__ */
        if (p->id == TOK_NAME && p->val.sym == _va_args &&
            (m->type != MACRO_FUNC || !m->variadic))
            cpp_warn("__VA_ARGS__ can only appear in a variadic macro");

        if (p->id == TOK_NAME && (p->val.sym->flags & SYM_FLAG_MACRO_ARG)) {
            p->id = TOK_PP_MACRO_ARG;
            p->val.macro_arg_i = p->val.sym->arg_i;
        }

        if (m->nbody > 1 && p[-1].id == '#' && m->type == MACRO_FUNC) {
            if (p->id == TOK_PP_MACRO_ARG) {
                p->flags &= ~TOK_FLAG_PREV_WHITE;
                p->flags |= p[-1].flags & TOK_FLAG_PREV_WHITE;
                p->flags |= TOK_FLAG_STRINGIFY_ARG;
                p[-1] = p[0];
                m->nbody--;
            } else {
                cpp_error("'#' is not followed by a macro parameter");
                return 0;
            }
        }

        if (p->id == TOK_PASTE) {
            if (m->nbody == 1) {
                cpp_error("'##' cannot appear at the beginning");
                return 0;
            }

            if (p[-1].flags & TOK_FLAG_PASTE_LEFT) {
                /* TODO: extra tokens */
            } else {
                p[-1].flags |= TOK_FLAG_PASTE_LEFT;
                m->nbody--;
            }
        }

        seen_paste = p->id == TOK_PASTE;
        lex(t);
    }

    return 1;
}

static int macro_redefined_p(struct symbol *sym, struct macro *m)
{
    struct macro *old;
    
    if (!MACRO_DEFINED_P(sym))
        return 0;

    old = sym->macro;

    /* fast path */
    if (m->type == MACRO_OBJ && m->nbody == 0 &&
        old->type == MACRO_OBJ && old->nbody == 0)
        return 0;

    /* slow path */
    if (m->type != old->type ||
        m->nparam != old->nparam ||
        m->nbody != old->nbody ||
        m->variadic != old->variadic)
        return 1;
    
    for (int i = 0; i < m->nparam; i++)
        if (m->param[i] != old->param[i])
            return 1;

    /* compare replacement list spellings */
    for (int i = 0; i < m->nbody; i++)
        if (strcmp(tok2s(&m->body[i]), tok2s(&old->body[i])))
            return 1;
    
    return 0;
}

static int create_macro(struct symbol *sym)
{
    int ok = 1;
    struct token t;
    struct macro *m = zmalloc(sizeof(struct macro));

    lex(&t);
    if (t.id == '(' && !(t.flags & TOK_FLAG_PREV_WHITE)) {
        m->type = MACRO_FUNC;
        ok = read_parameters(m);
        if (!ok)
            goto out;
        /* read the first token of the replacement list */
        lex(&t);
    }
    /* else: default is MACRO_OBJ (== 0) */
    ok = read_replacements(m, &t);
    if (!ok)
        goto out;
    /* redefinition */
    if (macro_redefined_p(sym, m))
        cpp_warn("'%s' macro redefined", sym->name);
    /* check and clear the first one */
    if (m->nbody) {
        struct token *first = &m->body[0];
        if (m->type == MACRO_OBJ && !(first->flags & TOK_FLAG_PREV_WHITE))
            cpp_warn("missing whitespace after the macro name");
        first->flags &= ~TOK_FLAG_PREV_WHITE;
    }

    add_macro(sym, m);
    
    /* clear parameter flag and values */
out:
    for (int i = 0; i < m->nparam; i++) {
        struct symbol *sym = m->param[i];
        sym->flags &= ~SYM_FLAG_MACRO_ARG;
        sym->arg_i = 0;
    }
    return ok;
}

static int lex_macro_name(struct token *t)
{
    lex(t);
    if (t->id != TOK_NAME) {
        cpp_error("macro name must be an identifier");
        return -1;
    }
    if (t->val.sym == _defined) {
        cpp_error("'defined' cannot be used as a macro name");
        return -1;
    }
    return 0;
}

static void do_define(void)
{
    struct token t;

    if (lex_macro_name(&t) == 0) {
        int ok = create_macro(t.val.sym);
        if (ok)
            check_eol(0);
    }
}

static void do_undef(void)
{
    struct token t;
    
    if (lex_macro_name(&t) == 0) {
        remove_macro(t.val.sym);
        check_eol(0);
    }
}

/*
 * standard format:
 *
 *  # line digit-sequence new-line
 *  # line digit-sequence "s-char-sequence(opt)" new-line
 *  # line pp-tokens new-line
 *
 * extended format:
 *
 * # digit-sequence new-line
 * # digit-sequence "s-char-sequence(opt)" new-line
 */
static void do_line(void)
{
    struct svalue value;
    const char *name;
    struct token t;

    cpp_get_token(&t);
    if (t.id != TOK_PP_NUMBER) {
        cpp_error("expect a line number");
        return;
    }

    eval_number(&t, &value);
    if (value.id != TOK_ICON) {
        cpp_error("expect integer");
        return;
    }
    if (value.i > INT_MAX || value.i < 0) {
        cpp_error("line number overflow");
        return;
    }
    
    cpp_get_token(&t);
    if (t.id == TOK_SCON) {
        name = strndup(t.val.str + 1, strlen(t.val.str) - 2);
        check_eol(1);
    } else {
        if (t.id != TOK_EOF)
            cpp_error("'%s': not a valid file name", tok2s(&t));
        name = NULL;
    }
    skip_rest_of_line();
    
    new_hist(name, value.i, FC_RENAME);
}

static void do_error(void)
{
    struct strbuf sb = STRBUF_INIT;
    struct token t;

    lex(&t);
    if (t.id == TOK_EOF)
        goto out;

    /* ignore the first token's leading whitespace */
    strbuf_cats(&sb, tok2s(&t));
    
    while (1) {
        lex(&t);
        if (t.id == TOK_EOF)
            break;
        if (t.flags & TOK_FLAG_PREV_WHITE)
            strbuf_catc(&sb, ' ');
        strbuf_cats(&sb, tok2s(&t));
    }
out:
    strbuf_catc(&sb, '\0');
    if (_directive->name[0] == 'w')
        cpp_warn("%s", sb.str);
    else
        cpp_error("%s", sb.str);
    strbuf_free(&sb);
}

static void do_pragma(void)
{
    cpp_warn("pragma directive not supported yet");
}

static void start_directive(void)
{
    _state.in_directive = 1;
}

static void end_directive(int skip)
{
    if (skip)
        skip_rest_of_line();

    _state.in_directive = 0;
    _state.bracket_header = 0;
    _directive = NULL;
}

static void handle_directive(int bof)
{
    struct directive *dir = NULL;
    struct token t;
    int skip = 1;

    start_directive();

    lex(&t);
    if (t.id == TOK_NAME) {
        if (t.val.sym->directive_p)
            dir = &_dirtab[t.val.sym->dir_no];
        else
            cpp_error("unknown directive #%s", tok2s(&t));
    } else if (t.id == TOK_PP_NUMBER) {
        dir = &_dirtab[D_LINE];
        lex_undo(&t);
    } else if (t.id == TOK_EOF) {
        /* OK: end of line/file */
    } else {
        if (!_state.skipping)
            cpp_error("invalid directive #%s", tok2s(&t));
    }

    /* check skipping state */
    if (dir) {
        _state.bracket_header = (dir->flags & DIR_FLAG_INL) != 0;
        if (_state.skipping && !(dir->flags & DIR_FLAG_COND))
            dir = NULL;
    }
    
    _directive = dir;
    if (dir) {
        if (bof)
            dir->flags |= DIR_FLAG_BOF;
        _directive->handler();
        if (bof)
            dir->flags &= ~DIR_FLAG_BOF;
    }

    end_directive(skip);
}

static void run_directive(int dir_no, const char *buf, size_t len)
{
    push_buffer(buffer_with_string(buf, len));
    start_directive();

    _directive = &_dirtab[dir_no];
    _directive->handler();
    
    end_directive(1);
    pop_buffer();
}

/* used by user command line */
void cpp_define(const char *str)
{
    size_t len;
    char *buf;
    const char *p;

    if (!str)
        return;

    len = strlen(str);
    buf = xmalloc(len + 3);
    memcpy(buf, str, len);

    /*
     * replace the first '=' with space,
     * append '1' to the end if there is none.
     */

    p = strchr(str, '=');
    if (p) {
        buf[p - str] = ' ';
    } else {
        buf[len++] = ' ';
        buf[len++] = '1';
    }
    buf[len] = '\n';

    run_directive(D_DEFINE, buf, len);
    free(buf);
}

void cpp_undef(const char *str)
{
    size_t len;
    char *buf;

    if (!str)
        return;

    len = strlen(str);
    buf = xmalloc(len + 1);
    memcpy(buf, str, len);
    buf[len] = '\n';
    
    run_directive(D_UNDEF, buf, len);
    free(buf);
}

static void define_special(const char *name, const char * (*h)(struct token *))
{
    struct symbol *sym;
    struct macro *m = zmalloc(sizeof(struct macro));
    m->type = MACRO_SPECIAL;
    m->handler = h;
    sym = lookup(name, OPT_CREATE);
    add_macro(sym, m);
}

static void define_builtin(const char *str)
{
    run_directive(D_DEFINE, str, strlen(str));
}

int cpp_macro_defined_p(const char *name)
{
    struct symbol *sym = lookup(name, OPT_SEARCH);
    return sym ? MACRO_DEFINED_P(sym) : 0;
}

static struct cpp_context *new_context(void)
{
    struct cpp_context *result;

    if (_free_contexts) {
        result = _free_contexts;
        _free_contexts = result->prev;
    } else {
        result = xmalloc(sizeof(struct cpp_context));
    }

    result->prev = _context;
    _context = result;
    return result;
}

static void push_token_context(struct symbol *sym, struct token *first,
                               int count, void *buff)
{
    struct cpp_context *context;

    context = new_context();
    context->tokens_kind = TOKENS_KIND_DIRECT;
    context->sym = sym;
    context->buff = buff;
    context->first.token = first;
    context->end.token = first + count;
}

static void push_ptoken_context(struct symbol *sym, struct token **first, 
                                int count, void *buff)
{
    struct cpp_context *context;

    context = new_context();
    context->tokens_kind = TOKENS_KIND_INDIRECT;
    context->sym = sym;
    context->buff = buff;
    context->first.ptoken = first;
    context->end.ptoken = first + count;
}

static void pop_context(void)
{
    struct cpp_context *context = _context;

    if (context == &_base_context)
        abort();

    if (context->sym)
        context->sym->flags &= ~SYM_FLAG_DISABLED;
    if (context->buff)
        free(context->buff);
    
    _context = context->prev;
    /* add to free list */
    context->prev = _free_contexts;
    _free_contexts = context;
}

static inline int reached_end_of_context(struct cpp_context *context)
{
    if (context->tokens_kind == TOKENS_KIND_DIRECT)
        return context->first.token == context->end.token;
    else if (context->tokens_kind == TOKENS_KIND_INDIRECT)
        return context->first.ptoken == context->end.ptoken;
    else
        abort();
}

static inline void consume_token_of_context(struct cpp_context *context,
                                            struct token *result)
{
    if (context->tokens_kind == TOKENS_KIND_DIRECT)
        *result = *context->first.token++;
    else if (context->tokens_kind == TOKENS_KIND_INDIRECT)
        *result = **context->first.ptoken++;
    else
        abort();
}

static struct macro_arg *make_macro_args(int count)
{
    struct macro_arg *args = zmalloc(count * sizeof(struct macro_arg));    
    for (int i = 0; i < count; i++) {
        struct macro_arg *arg = &args[i];
        arg->arg_alloc = count;
        arg->token_alloc = 50;
        arg->token = xmalloc(arg->token_alloc * sizeof(struct token));
    }
    return args;
}

static void free_macro_args(struct macro_arg *args)
{
    for (int i = 0; i < args->arg_alloc; i++) {
        struct macro_arg *arg = &args[i];
        free(arg->exp);
        free(arg->token);
        free(arg->stringified);
    }
    free(args);
}

static int args_ok(struct macro *m, int argc)
{
    if (argc == m->nparam)
        return 1;
    
    if (argc < m->nparam) {
        if (argc + 1 == m->nparam && m->variadic) {
            cpp_warn("requires at least one argument");
            return 1;
        }

        cpp_error("too few arguments");
    } else {
        cpp_error("too many arguments");
    }

    return 0;
}

/*
 * we allow arguments to be empty, so if a func-like macro has
 * only one parameter, an empty argument is valid.
 */
static struct macro_arg *collect_args(struct macro *m, int *pargc)
{
    int count = MAX(m->nparam, 1);
    int argc = 0;
    struct macro_arg *args, *arg;
    struct token t;

    args = arg = make_macro_args(count);
    
    do {
        int paren_depth = 0;
        int ntoken = 0;

        argc++;
        while (1) {
            /* extend buffer if needed (+2: including next token and EOF) */
            if (ntoken + 2 > arg->token_alloc) {
                arg->token_alloc += 1000;
                arg->token = xrealloc(arg->token, 
                                      arg->token_alloc * sizeof(struct token));
            }
            
            expand(&t);
            if (t.id == TOK_EOF)
                break;
            if (t.id == ')' && paren_depth == 0)
                break;
            if (t.id == ',' && paren_depth == 0 &&
                !(m->variadic && argc == m->nparam))
                break;
            if (t.id == '(')
                paren_depth++;
            else if (t.id == ')')
                paren_depth--;

            arg->token[ntoken++] = t;
        }

        arg->ntoken = ntoken;
        /* append EOF token (flag must be initialized to zero) */
        arg->token[ntoken] = (struct token){.id = TOK_EOF, .flags = 0};

        if (argc <= m->nparam) {
            if (argc != m->nparam)
                arg++;
        }
    } while (t.id != ')' && t.id != TOK_EOF);

    if (t.id == TOK_EOF) {
        cpp_error("unterminated function-like macro invocation");
    } else {
        /* empty argument */
        if (argc == 1 && m->nparam == 0 && args[0].ntoken == 0)
            argc = 0;
    
        /* check parameters and arguments */
        if (args_ok(m, argc)) {
            if (pargc)
                *pargc = argc;
            return args;
        }
    }

    /* free */
    free_macro_args(args);
    return NULL;
}

static void backslash(const char *str, struct strbuf *sb)
{
    char c;

    for (; (c = *str); str++) {
        if (c == '"' || c == '\\')
            strbuf_catc(sb, '\\');
        strbuf_catc(sb, c);
    }
}

static void stringify_arg(struct macro_arg *arg)
{
    struct strbuf sb = STRBUF_INIT;
    struct token *result = make_temp_token();

    strbuf_catc(&sb, '"');
    for (int i = 0; i < arg->ntoken; i++) {
        struct token *t = &arg->token[i];

        if (t->flags & TOK_FLAG_PREV_WHITE)
            strbuf_catc(&sb, ' ');

        switch (t->id) {
        case TOK_SCON:
        case TOK_WSCON:
        case TOK_PP_CHAR:
        case TOK_PP_WCHAR:
        case TOK_INVALID:
            backslash(t->val.str, &sb);
            break;
        default:
            strbuf_cats(&sb, tok2s(t));
            break;
        }
    }
    strbuf_catc(&sb, '"');
    strbuf_catc(&sb, '\0');

    result->id = TOK_SCON;
    result->val.str = sb.str;
    arg->stringified = result;
}

static void expand_arg(struct macro_arg *arg)
{
    struct token *buff;
    int count, alloc;
    
    if (arg->ntoken == 0)
        return;

    buff = NULL;
    count = alloc = 0;
    push_token_context(NULL, arg->token, arg->ntoken + 1, NULL);
    while (1) {
        struct token t;

        expand(&t);
        if (t.id == TOK_EOF)
            break;

        if (count == alloc) {
            if (count == 0)
                alloc = 12;
            while (alloc < count + 1)
                alloc *= 2;
            buff = xrealloc(buff, alloc * sizeof(struct token));
        }
        buff[count++] = t;
    }
    pop_context();

    arg->exp = buff;
    arg->nexp = count;
}

/*
 * expand each argument before replacement:
 * C99.6.10.3.1p1: a parameter in the replacement list, unless preceded
 * by a # or ## preprocessing token or followed by a ## preprocessing
 * token, is replaced by the corresponding argument after all macros
 * contained therein have been expanded.
 */
static void subst_args(struct symbol *sym, struct macro *m,
                       struct token *dname, struct macro_arg *args)
{
    int count = 0;
    struct token *buff;

    for (int i = 0; i < m->nbody; i++) {
        struct token *t = &m->body[i];

        count++;
        if (t->id == TOK_PP_MACRO_ARG) {
            struct macro_arg *arg = &args[t->val.macro_arg_i];

            if (t->flags & TOK_FLAG_STRINGIFY_ARG) {
                if (!arg->stringified)
                    stringify_arg(arg);
            } else if ((t->flags & TOK_FLAG_PASTE_LEFT) ||
                       (i > 0 && (t[-1].flags & TOK_FLAG_PASTE_LEFT))) {
                /* `param ##` or `## param` */
                count += arg->ntoken - 1;
            } else {
                if (!arg->exp)
                    expand_arg(arg);
                count += arg->nexp - 1;
            }
        }
    }

    buff = (struct token *)xmalloc(count * sizeof(struct token));
    count = 0;
    
    for (int i = 0; i < m->nbody; i++) {
        struct token *t = &m->body[i];

        if (t->id != TOK_PP_MACRO_ARG) {
            buff[count++] = *t;
            continue;
        }

        struct macro_arg *arg = &args[t->val.macro_arg_i];
        struct token *from, *paste_flag = NULL;
        int n;

        if (t->flags & TOK_FLAG_STRINGIFY_ARG) {
            n = 1;
            from = arg->stringified;
        } else if (t->flags & TOK_FLAG_PASTE_LEFT) {
            /* param ## */
            n = arg->ntoken;
            from = arg->token;
        } else if (i > 0 && (t[-1].flags & TOK_FLAG_PASTE_LEFT)) {
            /* ## param */
            n = arg->ntoken;
            from = arg->token;
            if (count && n == 0)
                paste_flag = &buff[count-1];
        } else {
            n = arg->nexp;
            from = arg->exp;
        }

        if (n) {
            for (int i = 0; i < n; i++)
                buff[count+i] = from[i];

            if (t->flags & TOK_FLAG_PASTE_LEFT)
                paste_flag = &buff[count+n-1];
        }

        /* add a new paste flag or remove an unwanted one */
        if (paste_flag) {
            if (t->flags & TOK_FLAG_PASTE_LEFT)
                paste_flag->flags |= TOK_FLAG_PASTE_LEFT;
            else
                paste_flag->flags &= ~TOK_FLAG_PASTE_LEFT;
        }
        
        /* if the arg replacement is NOT empty, update the PREV_WHITE flag */
        if (n && (t->flags ^ buff[count].flags) & TOK_FLAG_PREV_WHITE) {
            buff[count].flags &= ~TOK_FLAG_PREV_WHITE;
            buff[count].flags |= t->flags & TOK_FLAG_PREV_WHITE;
        }

        count += n;
    }

    if (count && (dname->flags ^ buff[0].flags) & TOK_FLAG_PREV_WHITE) {
        buff[0].flags &= ~TOK_FLAG_PREV_WHITE;
        buff[0].flags |= dname->flags & TOK_FLAG_PREV_WHITE;
    }

    push_token_context(sym, buff, count, buff);
}

static void cpp_unget(struct token *t)
{
    if (_context->prev == NULL) {
        lex_undo(t);
    } else {
        if (_context->tokens_kind == TOKENS_KIND_DIRECT)
            _context->first.token--;
        else if (_context->tokens_kind == TOKENS_KIND_INDIRECT)
            _context->first.ptoken--;
        else
            abort();
    }
}

static void push_macro_body(struct symbol *sym, struct macro *m, struct token *t)
{
    if (m->nbody && (t->flags ^ m->body[0].flags) & TOK_FLAG_PREV_WHITE) {
        struct token **buff = xmalloc(m->nbody * sizeof(struct token *) +
                                      sizeof(struct token));
        for (int i = 0; i < m->nbody; i++)
            buff[i] = &m->body[i];
        buff[0] = (struct token *)&buff[m->nbody];
        *buff[0] = m->body[0];
        buff[0]->flags &= ~TOK_FLAG_PREV_WHITE;
        buff[0]->flags |= t->flags & TOK_FLAG_PREV_WHITE;
        push_ptoken_context(sym, buff, m->nbody, buff);
    } else {
        push_token_context(sym, m->body, m->nbody, NULL);
    }
}

static int enter_macro_context(struct token *dname, struct symbol *sym)
{
    struct macro *m = sym->macro;

    switch (m->type) {
    case MACRO_FUNC:
        {
            struct macro_arg *args;
            struct token result;

            _state.prevent_expansion++;
            _state.parsing_args = 1;
            expand(&result);
            if (result.id == '(') {
                args = collect_args(m, NULL);
            } else {
                cpp_unget(&result);
                args = NULL;
            }
            _state.parsing_args = 0;
            _state.prevent_expansion--;

            if (args == NULL) {
                return 0;
            }

            if (m->nparam > 0)
                subst_args(sym, m, dname, args);
            else
                push_macro_body(sym, m, dname);

            free_macro_args(args);
        }
        break;
        
    case MACRO_OBJ:
        push_macro_body(sym, m, dname);
        break;

    case MACRO_SPECIAL:
        {
            const char *text;
            struct token *result;

            text = m->handler(dname);
            push_buffer(buffer_with_string(text, strlen(text)));
            result = make_temp_token();
            lex_direct(result);
            pop_buffer();
            result->flags |= dname->flags & TOK_FLAG_PREV_WHITE;
            push_token_context(sym, result, 1, result);
        }
        break;

    default:
        abort();
    }

    /* disable the macro within its expansion */
    sym->flags |= SYM_FLAG_DISABLED;
    return 1;
}

/* paste two tokens into one (return 0:failure, 1:success) */
static int paste_tokens(struct token *lhs, struct token *rhs)
{
    struct strbuf sb = STRBUF_INIT;
    struct token t;
    int success = 1;

    strbuf_cats(&sb, tok2s(lhs));
    strbuf_cats(&sb, tok2s(rhs));

    push_buffer(buffer_with_string(sb.str, sb.len));
    lex_direct(&t);
    if (_buffer->cur != _buffer->limit) {
        strbuf_catc(&sb, '\0'); /* for below printing */
        cpp_error("pasting formed an invalid token '%s'", sb.str);
        success = 0;
    }
    pop_buffer();
    strbuf_free(&sb);
    t.flags = lhs->flags;
    t.flags &= ~TOK_FLAG_PASTE_LEFT; /* must clear PASTE_LEFT flag */
    *lhs = t;
    return success;
}

static void paste_all_tokens(struct token *lhs)
{
    struct token rhs;

    do {
        /*
         * take the token directly from the current context, we can do
         * this, because we are in the replacement list of either an
         * object-like macro, or a function-like macro with arguments
         * inserted, in either case, the constraints to #define
         * guarantee we have at least one more token.
         */
        consume_token_of_context(_context, &rhs);
        if (!paste_tokens(lhs, &rhs))
            break;
    } while (rhs.flags & TOK_FLAG_PASTE_LEFT);
}

static void expand(struct token *result)
{
    while (1) {
        struct symbol *sym;
        struct cpp_context *context = _context;
        int ok;

        if (!context->prev) {
            lex(result);
        } else if (!reached_end_of_context(context)) {
            consume_token_of_context(context, result);
            if (result->flags & TOK_FLAG_PASTE_LEFT)
                paste_all_tokens(result);
        } else {
            pop_context();
            continue;
        }
        
        if (result->id != TOK_NAME)
            break;

        sym = result->val.sym;

        if (sym->kind != SYM_MACRO || (result->flags & TOK_FLAG_NO_EXPAND))
            break;

        if (!(sym->flags & SYM_FLAG_DISABLED)) {
            if (_state.prevent_expansion)
                break;

            ok = enter_macro_context(result, sym);
            if (!ok)
                break;
        } else {
            /* flag the token as always unexpandable. */
            result->flags |= TOK_FLAG_NO_EXPAND;
            break;
        }
    }
}

static const char *file_handler(struct token *t)
{
    const char *name = "";
    cpp_resolve_location(lineno, &name, NULL);
    return stringf("\"%s\"", name);
}

static const char *line_handler(struct token *t)
{
    int line = 0;
    cpp_resolve_location(lineno, NULL, &line);
    return stringf("%d", line);
}

static const char *get_date_time_once(int ndx)
{
    static const char *months[] = {
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    };
    static char *datestr, *timestr;
    
    /*
     * init date and time only once.
     * we don't use setlocale() and strftime() because they are slow,
     * and don't generate them at init time because time() and localtime()
     * are very slow on some systems.
     */
    if (datestr == NULL) {
        time_t t = time(NULL);
        struct tm *now = localtime(&t);

        datestr = xmalloc(sizeof("qJan 01 1970q"));
        sprintf(datestr, "\"%s %2d %4d\"",
                months[now->tm_mon], now->tm_mday, now->tm_year + 1900);

        timestr = xmalloc(sizeof("q15:00:00q"));
        sprintf(timestr, "\"%02d:%02d:%02d\"",
                now->tm_hour, now->tm_min, now->tm_sec);
    }

    return ndx == 'd' ? datestr : timestr;
}

static const char *date_handler(struct token *t)
{
    return get_date_time_once('d');
}

static const char *time_handler(struct token *t)
{
    return get_date_time_once('t');
}

/*
 * get an expanded token.
 * all macro expansions and directives are handled transparently,
 * external callers see only one TOK_EOF token (really reached the end),
 * internal callers may see more TOK_EOF.
 */
void cpp_get_token(struct token *result)
{
    expand(result);
}

void cpp_add_include(const char *name, int bracket)
{
    int i;
    struct cpp_dir *dir = bracket ? _bracket_include : _quote_include;

    for (i = 0; i < dir->ninclude; i++)
        if (!strcmp(dir->include[i], name))
            break;

    if (i >= dir->ninclude) {
        if (i >= dir->maxinclude) {
            dir->maxinclude += 20;
            dir->include = xrealloc(dir->include,
                                    dir->maxinclude * sizeof(*dir->include));
        }
        dir->include[dir->ninclude++] = name;
    }
}

void cpp_init(const char *filename)
{
    struct symbol *sym;
    const char *name;

    lineno = 1;
    _bracket_include = zmalloc(sizeof(struct cpp_dir));
    _quote_include = zmalloc(sizeof(struct cpp_dir));
    _va_args = lookup("__VA_ARGS__", OPT_CREATE);
    _defined = lookup("defined", OPT_CREATE);
    /* init directives */
    for (int i = 0; i < NELEMS(_dirtab); i++) {
        name = _dirtab[i].name;
        sym = lookup(name, OPT_CREATE);
        sym->directive_p = 1;
        sym->dir_no = i;
    }

    /* VLA is ugly, it's not worth the effort. */
    define_builtin("__STDC_NO_VLA__ 1");
    define_builtin("__STDC_VERSION__ 199901L");
    define_builtin("__STDC__ 1");
    define_builtin("__STDC_HOSTED__ 1");
    define_builtin("__PTRDIFF_TYPE__ long");
    define_builtin("__SIZE_TYPE__ unsigned long");
    define_builtin("__WCHAR_TYPE__ int");
    define_builtin("__FUNCTION__ __func__");
    define_special("__FILE__", file_handler);
    define_special("__LINE__", line_handler);
    define_special("__DATE__", date_handler);
    define_special("__TIME__", time_handler);

    push_buffer(buffer_with_file(filename));
}

static void next(void)
{
    cpp_get_token(&_token);
}

static void expect(int id)
{
    if (_token.id == id)
        next();
    else
        cpp_error("expect token '%s'", lexmes[id]);
}

static void skip_line(void)
{
    while (_token.id != TOK_EOF)
        next();
}

static void skip_pair(int p1, int p2)
{
    int nests = 0;

    expect(p1);
    for (;;) {
        if (_token.id == p1) {
            nests++;
        } else if (_token.id == p2) {
            if (nests == 0)
                break;
            nests--;
        }
        next();
    }
    expect(p2);
}

static void parse_typename()
{
    skip_pair('(', ')');
}

static void parse_initializer_list(void)
{
    skip_pair('{', '}');
}

static void parse_args_list(void)
{
    skip_pair('(', ')');
}

static long parse_defined(void)
{
    long result = 0;
    struct token t;

    /* Don't expand macros here. */
    _state.prevent_expansion++;
    cpp_get_token(&t);
    if (t.id == TOK_NAME) {
        result = MACRO_DEFINED_P(t.val.sym);
    } else if (t.id == '(') {
        cpp_get_token(&t);
        if (t.id == TOK_NAME) {
            result = MACRO_DEFINED_P(t.val.sym);
            cpp_get_token(&t);
            if (t.id != ')') {
                cpp_error("expect ')");
                skip_line();
            }
        } else {
            cpp_error("expect identifier");
            skip_pair('(', ')');
        }
    } else {
        cpp_error("expect identifier or ( after defined operator");
        skip_line();
    }
    _state.prevent_expansion--;
    return result;
}

static long parse_primary(void)
{
    long num;
    struct svalue value;

    switch (_token.id) {
    case TOK_PP_NUMBER:
    case TOK_PP_CHAR:
    case TOK_PP_WCHAR:
        eval_number(&_token, &value);
        if (value.id == TOK_FCON)
            cpp_error("floating constant not allowed");
        num = value.i;
        next();
        break;

    case TOK_NAME:
        /*
         * C99 6.10.1.3 says that remaining identifiers
         * should be replaced with pp-number 0.
         */
        if (_token.val.sym == _defined)
            num = parse_defined() ? 1 : 0;
        else
            num = 0;
        next();
        break;

    case TOK_SCON:
    case TOK_WSCON:
    default:
        cpp_error("invalid token '%s'", tok2s(&_token));
        num = 1;
        next();
        break;
    }
    
    return num;
}

static long parse_postfix(long num)
{
    int fail = 0;

    while (1) {
        switch (_token.id) {
        case '[':
            fail = 1;
            next();
            parse_expr();
            expect(']');
            break;

        case '(':
            fail = 1;
            next();
            parse_args_list();
            expect(')');
            break;

        case '.':
        case TOK_DEREF:
            fail = 1;
            next();
            expect(TOK_NAME);
            break;

        case TOK_INCR:
        case TOK_DECR:
            fail = 1;
            next();
            break;

        default:
            if (fail)
                cpp_error("invalid postfix expression");
            return num;
        }
    }
}

static long parse_unary(void)
{
    long num;
    int id = _token.id;

    switch (id) {
    case TOK_INCR:
    case TOK_DECR:
        cpp_error("invalid preprocessor expression");
        next();
        num = parse_unary();
        return id == TOK_INCR ? (num + 1) : (num - 1);
    
    case '+':
        next();
        return parse_unary();
    
    case '-':
        next();
        return -parse_unary();
    
    case '~':
        next();
        return ~parse_unary();
    
    case '!':
        next();
        return !parse_unary();
    
    case '&':
    case '*':
        cpp_error("invalid preprocessor expression");
        next();
        return parse_unary();
    
    case TOK_SIZEOF:
        cpp_error("invalid preprocessor expression");
        next();
        if (_token.id == '(') {
            parse_typename();
            if (_token.id == '{') {
                parse_initializer_list();
                parse_postfix(0);
            }
        } else {
            parse_unary();
        }
        return 0;
    
    case '(':
        next();
        num = parse_expr();
        expect(')');
        return parse_postfix(num);

    default:
        return parse_postfix(parse_primary());
    }
}

static long evbop(int op, long lhs, long rhs)
{
    switch (op) {
    case TOK_OROR:
        return lhs || rhs;
    
    case TOK_ANDAND:
        return lhs && rhs;
    
    case '+':
        return lhs + rhs;
    
    case '-':
        return lhs - rhs;
    
    case '*':
        return lhs * rhs;
    
    case TOK_SHL:
        return lhs << rhs;
    
    case TOK_SHR:
        return lhs >> rhs;
    
    case '>':
        return lhs > rhs;
    
    case '<':
        return lhs < rhs;
    
    case TOK_LEQ:
        return lhs <= rhs;
    
    case TOK_GEQ:
        return lhs >= rhs;
    
    case TOK_EQ:
        return lhs == rhs;
    
    case TOK_NEQ:
        return lhs != rhs;
    
    case '&':
        return lhs & rhs;
    
    case '^':
        return lhs ^ rhs;
    
    case '|':
        return lhs | rhs;
    
    case '/':
        if (rhs) {
            return lhs / rhs;
        } else {
            cpp_error("division by zero");
            return 0;
        }
    
    case '%':
        if (rhs) {
            return lhs % rhs;
        } else {
            cpp_error("division by zero");
            return 0;
        }
    
    default:
        cpp_error("unknown binary operator(%d)", op);
        return 0;
    }
}

static long parse_binary(void)
{
    /* parse a binary expression using precedence. */
    struct {
        long num;
        int prec;
        int op;
    } stack[NPREC];
    int sp;                     /* stack pointer */
    int prec, op;

    /* pop stack[sp] and stack[sp-1] */
#define POP()                                                    \
    do {                                                         \
        stack[sp-1].num = evbop(stack[sp].op,                    \
                                stack[sp-1].num, stack[sp].num); \
        sp--;                                                    \
    } while (0)

    /* init stack */
    stack[0].num = parse_unary();
    stack[0].prec = 0;
    sp = 0;

    while (1) {
        prec = precs[_token.id];
        if (prec == 0)
            break;

        while (prec <= stack[sp].prec)
            POP();

        op = _token.id;
        next();
        /* push */
        sp++;
        stack[sp].num = parse_unary();
        stack[sp].prec = prec;
        stack[sp].op = op;
    }

    while (sp > 0)
        POP();

#undef POP
    return stack[0].num;
}

static long parse_cond_tail(long num)
{
    long then, els;

    expect('?');
    then = parse_expr();
    expect(':');
    els = parse_cond();

    return num ? then : els;
}

static long parse_cond(void)
{
    long num;

    num = parse_binary();
    if (_token.id == '?')
        return parse_cond_tail(num);

    return num;
}

static long parse_assign(void)
{
    long num;

    num = parse_binary();
    if (_token.id == '?')
        return parse_cond_tail(num);
    if (ASSIGN_TOKEN_P(&_token)) {
        cpp_error("invalid preprocessor expression");
        next();
        num = parse_assign();
    }

    return num;
}

static long parse_expr(void)
{
    long num;

    num = parse_assign();
    while (_token.id == ',') {
        next();
        num = parse_assign();
    }

    return num;
}

/* SEEN '#if' or '#elif' */
static int parse_int(void)
{
    int val;

    next();
    val = parse_cond();
    if (_token.id != TOK_EOF)
        cpp_error("extraneous token");

    return val;
}

static void cpp_vdiag(int level, const char *fmt, va_list ap)
{
    const char *oname = "<noname>";
    int oline = 1;
    int line = lineno;

    if (_state.in_directive)
        if (SEEN_EOL())
            line--;
    cpp_resolve_location(line, &oname, &oline);
    fprintf(stderr, "%s:%d: ", oname, oline);
    if (level)
        fputs("error: ", stderr);
    else
        fputs("warning: ", stderr);
    vfprintf(stderr, fmt, ap);
    fputs("\n", stderr);
    if (level)
        exit(EXIT_FAILURE);
}

static void cpp_error(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    cpp_vdiag(1, fmt, ap);
    va_end(ap);
}

static void cpp_warn(const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    cpp_vdiag(0, fmt, ap);
    va_end(ap);
}