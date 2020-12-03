#ifndef LIBERTY_H
#define LIBERTY_H

#include <string.h>
#include <stdarg.h>
#include <stddef.h>
#include <wchar.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#if __STDC_VERSION__ >= 201112L
#define __noreturn _Noreturn
#elif __GNUC__ >= 2
#define __noreturn __attribute__((noreturn))
#else
#define __noreturn
#endif

#define NELEMS(array) (sizeof(array) / sizeof((array)[0]))
#define FIELD_SIZEOF(st, f) (sizeof(((st*)0)->f))

#define ALIGN_SIZE (sizeof (long))
#define ROUNDUP(x, align) (((x)+((align)-1))&(~((align)-1)))

#define BYTES(bits) ((ROUNDUP(bits, 8)) / 8)

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))

#define SIGN(bits) (1UL << (bits-1))
#define MASK(bits) (SIGN(bits)|(SIGN(bits) - 1))
#define ONES(bytes) MASK(8*(bytes))

#define IS_DIRSEP(c)    ((c) == '/')

extern __noreturn void die_errno(const char *, ...);
extern __noreturn void die(const char *, ...);

extern void *xmalloc(size_t size);
extern void *zmalloc(size_t size);
extern void *xcalloc(size_t count, size_t size);
extern void *xrealloc(void *p, size_t size);

/* alloc */
enum allocate_area { PERM, FUNC };
extern void *allocate(size_t, enum allocate_area);
extern void *zallocate(size_t, enum allocate_area);
extern void deallocate(enum allocate_area);

extern char *fullpath(const char *);
extern char *joinpath(const char *, const char *);
extern int file_exist(const char *);
extern size_t file_size(const char *);
extern char *file_ext(const char *);
extern char *ch_file_ext(const char *, const char *);
extern char *file_basename(const char *);
extern char *file_dirname(const char *);
extern char *mktempfile(const char *);

extern unsigned int strhash(const char *);
extern unsigned int strnhash(const char *, size_t);
extern char *vstringf(const char *, va_list);
extern char *stringf(const char *, ...);
extern int strstart(const char *, char **);

extern int mb2wc(wchar_t *wc, const char *s, size_t n);
extern size_t mbs2wcs(wchar_t *pwcs, const char *s, size_t n);
extern int wc2mb(char *s, wchar_t wc);
extern size_t wcs2mbs(char *s, const wchar_t *pwcs, size_t n);

/* strbuf */
#define STRBUF_INIT  {NULL, 0, 0}
struct strbuf {
    char *str;
    size_t len;
    size_t alloc;
};

extern void strbuf_new(struct strbuf *);
extern void strbuf_free(struct strbuf *);
extern void strbuf_reset(struct strbuf *);
extern void strbuf_cats(struct strbuf *, const char *);
extern void strbuf_catn(struct strbuf *, const char *, size_t);
extern void strbuf_catc(struct strbuf *, int);
extern void strbuf_catf(struct strbuf *, const char *, ...);

/* list */
struct list {
    void *x;
    struct list *link;
};

extern void append(struct list **, void *);
extern size_t nlist(struct list *);
extern void *ltoa(struct list **);

extern size_t length(void *);

#endif  /* LIBERTY_H */