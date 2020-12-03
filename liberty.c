#include "compat.h"
#include "liberty.h"
#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <ctype.h>

__noreturn void die_errno(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fputs("error: ", stderr);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, ": %s\n", strerror(errno));
    va_end(ap);
    exit(EXIT_FAILURE);
}

__noreturn void die(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fputs("error: ", stderr);
    vfprintf(stderr, fmt, ap);
    fputs("\n", stderr);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void *xmalloc(size_t size)
{
    void *p = malloc(size);
    if (!p)
        die("memory exhausted");
    return p;
}

void *zmalloc(size_t size)
{
    return memset(xmalloc(size), 0, size);
}

void *xcalloc(size_t count, size_t size)
{
    void *p = calloc(count, size);
    if (!p)
        die("memory exhausted");
    return p;
}

void *xrealloc(void *ptr, size_t size)
{
    void *p = realloc(ptr, size);
    if (!p)
        die("memory exhausted");
    return p;
}

/* alloc */

union align {
    long l;
    double d;
    void (*f)(void);
};

struct bucket {
    struct bucket *next;
    char *cur;
    char *limit;
};

union header {
    struct bucket b;
    union align a;
};

static struct bucket first[] = { { NULL }, { NULL } };
static struct bucket *area[] = { &first[0], &first[1] };
static struct bucket *freebuckets;

void *allocate(size_t n, enum allocate_area a)
{
    struct bucket *p;
    assert(a >= 0 && a < NELEMS(area));

    p = area[a];
    n = ROUNDUP(n, sizeof(union align));
    while (n > p->limit - p->cur) {
        if ((p->next = freebuckets) != NULL) {
            freebuckets = freebuckets->next;
            p = p->next;
        } else {
            size_t m = sizeof(union header) + n +
                       ROUNDUP(10 * 1024, sizeof(union align));
            p->next = xmalloc(m);
            p = p->next;
            p->limit = (char *)p + m;
        }

        p->cur = (char *)((union header *)p + 1);
        p->next = NULL;
        area[a] = p;
    }

    p->cur += n;
    return p->cur - n;
}

void *zallocate(size_t n, enum allocate_area a)
{
    return memset(allocate(n, a), 0, n);
}

void deallocate(enum allocate_area a)
{
    assert(a >= 0 && a < NELEMS(area));
    area[a]->next = freebuckets;
    freebuckets = first[a].next;
    first[a].next = NULL;
    area[a] = &first[a];
}

int file_exist(const char *path)
{
    struct stat st;
    return stat(path, &st) == 0;
}

size_t file_size(const char *path)
{
    struct stat st;
    if (stat(path, &st) == 0)
        return st.st_size;
    return 0;
}

/*
 * return extension of a file (".c", ".s" etc.)
 * (if no extension, return pointer to end-of-string)
 */
char *file_ext(const char *file)
{    
    char *base = file_basename(file);
    char *dot = strrchr(base, '.');
    return dot ? dot : strchr(base, 0);
}

/*
 * change extension of a file to `ext`
 * (if no extension, add `ext` to filename)
 */
char *ch_file_ext(const char *file, const char *ext)
{
    char *path, *dot, *part;

    path = xmalloc(strlen(file) + strlen(ext) + 2);
    dot = file_ext(file);
    if (dot[0]) {
        part = strrchr(file, '.');
        strncpy(path, file, part - file + 1);
        strcpy(path + (part - file + 1), ext);
    } else {
        strcpy(path, file);
        strcat(path, ".");
        strcat(path, ext);
    }
    return path;
}

char *fullpath(const char *path)
{
    if (path == NULL)
        return NULL;

    if (path[0] == '~' && path[1] == '/')
        return joinpath(getenv("HOME"), path + 2);
    else
        return strdup(path);
}

char *joinpath(const char *dir, const char *name)
{
    char *path;
    size_t len;

    if (dir == NULL || dir[0] == 0)
        return name ? strdup(name) : NULL;
    if (name == NULL || name[0] == 0)
        return dir ? strdup(dir) : NULL;

    if (name[0] == '/')
        return strdup(name);

    len = strlen(dir);
    path = xmalloc(len + strlen(name) + 2);
    strcpy(path, dir);
    if (path[len - 1] != '/')
        path[len++] = '/';
    strcpy(path + len, name);
    return path;
}

char *file_basename(const char *name)
{
    char *p = strchr(name, 0);
    while (p > name && !IS_DIRSEP(p[-1]))
        p--;
    return p;
}

char *file_dirname(const char *p)
{
    return dirname(strdup(p));
}

char *mktempfile(const char *suffix)
{
    char buf[32];
    int fd;
    
    if (suffix)
        snprintf(buf, sizeof(buf), "/tmp/XXXXXX.%s", suffix);
    else
        snprintf(buf, sizeof(buf), "/tmp/XXXXXX");
    fd = mkstemps(buf, strlen(buf) - strlen("/tmp/XXXXXX"));
    if (fd < 0 || close(fd) < 0)
        die_errno("can't make temporary filename");
    return strdup(buf);
}

#define STR_HASH_INIT 5381
#define STR_HASH_STEP(h, c) (((h) << 5) + (h) + (c))

unsigned int strhash(const char *s)
{
    unsigned int hash = STR_HASH_INIT;
    int c;

    while ((c = *s++))
        hash = STR_HASH_STEP(hash, c);

    return hash;
}

unsigned int strnhash(const char *s, size_t len)
{
    unsigned int hash = STR_HASH_INIT;

    for (const char *d = s + len; s < d; s++)
        hash = STR_HASH_STEP(hash, *s);

    return hash;
}

char *vstringf(const char *fmt, va_list ap)
{
    char buf[BUFSIZ];
    va_list ap2;
    int total;
    char *d;

    va_copy(ap2, ap);
    total = vsnprintf(buf, NELEMS(buf), fmt, ap);
    if (total >= NELEMS(buf)) {
        d = xmalloc(total + 8);
        if (vsnprintf(d, total + 8, fmt, ap2) > total)
            die("vstringf: how can this happend");
        return d;
    }
    return strdup(buf);
}

char *stringf(const char *fmt, ...)
{
    char *str;
    va_list ap;

    va_start(ap, fmt);
    str = vstringf(fmt, ap);
    va_end(ap);
    return str;
}

int strstart(const char *prefix, char **pstr)
{
    char *str;
    size_t n;

    if (!prefix || !pstr || !(str = *pstr))
        return 0;
    n = strlen(prefix);
    if (strncmp(str, prefix, n) != 0)
        return 0;
    *pstr = str + n;
    return 1;
}

/* for multi byte and wide char conversion. */
static void locale_init_once(void)
{
    static int once;
    if (!once) {
        once = 1;
        setlocale(LC_ALL, "");
    }
}

int mb2wc(wchar_t *wc, const char *s, size_t n)
{
    locale_init_once();
    return mbtowc(wc, s, n);
}

size_t mbs2wcs(wchar_t *pwcs, const char *s, size_t n)
{
    locale_init_once();
    return mbstowcs(pwcs, s, n);
}

int wc2mb(char *s, wchar_t wc)
{
    locale_init_once();
    return wctomb(s, wc);
}

size_t wcs2mbs(char *s, const wchar_t *pwcs, size_t n)
{
    locale_init_once();
    return wcstombs(s, pwcs, n);
}

/* strbuf */

static void strbuf_grow(struct strbuf *sb, size_t new_size)
{
    size_t size = sb->alloc;
    
    if (size == 0)
        size = 12;              /* maybe a small string */
    while (size < new_size)
        size = size * 2;
    sb->str = xrealloc(sb->str, size);
    sb->alloc = size;
}

void strbuf_new(struct strbuf *sb)
{
    memset(sb, 0, sizeof(struct strbuf));
}

void strbuf_free(struct strbuf *sb)
{
    free(sb->str);
    strbuf_new(sb);
}

void strbuf_reset(struct strbuf *sb)
{
    sb->len = 0;
}

void strbuf_catc(struct strbuf *sb, int ch)
{
    size_t size;

    size = sb->len + 1;
    if (size >= sb->alloc)
        strbuf_grow(sb, size);

    sb->str[sb->len] = ch;
    sb->len++;
}

void strbuf_catn(struct strbuf *sb, const char *str, size_t len)
{
    while (len--) {
        int c = *str;
        strbuf_catc(sb, c);
        str++;
    }
}

void strbuf_cats(struct strbuf *sb, const char *str)
{
    while (1) {
        int c = *str;
        if (c == '\0')
            break;
        strbuf_catc(sb, c);
        str++;
    }
}

void strbuf_catf(struct strbuf *sb, const char *fmt, ...)
{
    char *str;
    va_list ap;

    va_start(ap, fmt);
    str = vstringf(fmt, ap);
    va_end(ap);
    strbuf_cats(sb, str);
    free(str);
}

/* list */
static struct list *freelists;

void append(struct list **list, void *x)
{
    struct list *new;
    struct list *old = *list;

    if ((new = freelists) != NULL)
        freelists = freelists->link;
    else
        new = xmalloc(sizeof(*list));

    if (old) {
        new->link = old->link;
        old->link = new;
    } else {
        new->link = new;
    }
    new->x = x;
    *list = new;
}

size_t nlist(struct list *list)
{
    size_t n = 0;

    if (list) {
        struct list *p = list;
        do
            n++;
        while ((p = p->link) != list);
    }

    return n;
}

void *ltoa(struct list **list)
{
    int i = 0;
    void **array = xmalloc(sizeof(array[0]) * (nlist(*list) + 1));
    
    if (*list) {
        struct list *p = *list;
        do {
            p = p->link;
            array[i++] = p->x;
        } while (p != *list);

        p = (*list)->link;
        (*list)->link = freelists;
        freelists = p;
    }

    *list = NULL;
    array[i] = NULL;
    return array;
}

size_t length(void *array)
{
    size_t i;
    void **a;
    
    if (array == NULL)
        return 0;
    i = 0;
    a = (void **)array;
    while (a[i])
        i++;
    return i;
}