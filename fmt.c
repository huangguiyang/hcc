#include "cc.h"

#define MAX_FMT 64
static int badconv(struct fmt *);

struct convfmt {
    int c;
    int (*fmt)(struct fmt *);
};
static struct {
    int nfmt;
    struct convfmt fmt[MAX_FMT];
} fmtalloc;

int fmtinstall(int c, int (*fp)(struct fmt *))
{
    struct convfmt *p, *q;
    
    if (c <= 0 || c >= 256)
        return -1;
    if (!fp)
        fp = badconv;

    q = &fmtalloc.fmt[fmtalloc.nfmt];
    for (p = fmtalloc.fmt; p < q; p++)
        if (p->c == c)
            break;

    if (p == &fmtalloc.fmt[MAX_FMT])
        return -1;

    p->fmt = fp;
    if (p == q) {
        fmtalloc.nfmt++;
        p->c = c;
    }

    return 0;
}

static int fmtflush(struct fmt *fp)
{
    if (fp->stream) {
        for (char *p = fp->start; p < fp->to; p++)
            fputc(*p, fp->stream);
        
        fp->nfmt += fp->to - fp->start;
        fp->to = fp->start;
    }
    return 0;
}

static int fmtputc(struct fmt *fp, int c)
{
    if (fp->to < fp->stop) {
        *fp->to = c;
        ++fp->to;
    }
    if (fp->to == fp->stop)
        return fmtflush(fp);
    return 0;
}

static int fmtputs(struct fmt *fp, const char *s)
{
    if (!s)
        return -1;
    for (int c; (c = *s); s++) {
        if (fp->to < fp->stop) {
            *fp->to = c;
            ++fp->to;
        }
        if (fp->to == fp->stop)
            fmtflush(fp);
    }
    return 0;
}

static int badconv(struct fmt *fp)
{
    return fmtputc(fp, fp->c);
}

static int dofmt(struct fmt *fp, const char *fmt)
{
    struct convfmt *p, *q;
    char buf[256], *str;
    int nfmt = fp->nfmt;

    q = &fmtalloc.fmt[fmtalloc.nfmt];    
    for (; *fmt; fmt++) {
        if (*fmt != '%') {
            fmtputc(fp, *fmt);
            continue;
        }

        int c = *++fmt;
        for (p = fmtalloc.fmt; p < q; p++)
            if (p->c == c)
                break;

        if (p < q) {
            fp->c = c;
            p->fmt(fp);
            continue;
        }
        
        switch (c) {
        case 'c':
            snprintf(buf, sizeof(buf), "%c", va_arg(fp->args, int));
            break;
        case 'd':
        case 'i':
            snprintf(buf, sizeof(buf), "%d", va_arg(fp->args, int));
            break;
        case 'u':
            snprintf(buf, sizeof(buf), "%u", va_arg(fp->args, unsigned int));
            break;
        case 'x':
            snprintf(buf, sizeof(buf), "%x", va_arg(fp->args, int));
            break;
        case 'X':
            snprintf(buf, sizeof(buf), "%X", va_arg(fp->args, int));
            break;
        case 'o':
            snprintf(buf, sizeof(buf), "%o", va_arg(fp->args, int));
            break;
        case 's':
            str = va_arg(fp->args, char *);
            fmtputs(fp, str);
            buf[0] = '\0';
            break;
        case 'p':
            snprintf(buf, sizeof(buf), "%p", va_arg(fp->args, void *));
            break;
        case 'f':
            snprintf(buf, sizeof(buf), "%f", va_arg(fp->args, double));
            break;
        case 'l':
            if (fmt[1] == 'd') {
                fmt++;
                snprintf(buf, sizeof(buf), "%ld", va_arg(fp->args, long));
            } else if (fmt[1] == 'u') {
                fmt++;
                snprintf(buf, sizeof(buf), "%lu", va_arg(fp->args, unsigned long));
            } else if (fmt[1] == 'x') {
                fmt++;
                snprintf(buf, sizeof(buf), "%lx", va_arg(fp->args, long));
            } else if (fmt[1] == 'X') {
                fmt++;
                snprintf(buf, sizeof(buf), "%lX", va_arg(fp->args, long));
            } else if (fmt[1] == 'l' && fmt[2] == 'd') {
                fmt += 2;
                snprintf(buf, sizeof(buf), "%lld", va_arg(fp->args, long long));
            } else if (fmt[1] == 'l' && fmt[2] == 'u') {
                fmt += 2;
                snprintf(buf, sizeof(buf), "%llu", va_arg(fp->args, unsigned long long));
            } else {
                snprintf(buf, sizeof(buf), "%c", *fmt);
            }
            break;
        case 'L':
            if (fmt[1] == 'f') {
                fmt++;
                snprintf(buf, sizeof(buf), "%Lf", va_arg(fp->args, long double));
            } else {
                snprintf(buf, sizeof(buf), "%c", *fmt);
            }
            break;
        default:
            snprintf(buf, sizeof(buf), "%c", *fmt);
            break;
        }
        fmtputs(fp, buf);
    }
    fmtflush(fp);
    return fp->nfmt - nfmt;
}

int print(const char *fmt, ...)
{
    va_list ap;
    int n;

    va_start(ap, fmt);
    n = vfprint(stdout, fmt, ap);
    va_end(ap);
    return n;
}

int vfprint(FILE *stream, const char *fmt, va_list ap)
{
    struct fmt fp;
    char buf[256];

    va_copy(fp.args, ap);
    fp.start = buf;
    fp.stop = buf + sizeof(buf);
    fp.to = buf;
    fp.stream = stream;
    fp.nfmt = 0;
    return dofmt(&fp, fmt);
}

int fprint(FILE *stream, const char *fmt, ...)
{
    int n;
    va_list ap;

    va_start(ap, fmt);
    n = vfprint(stream, fmt, ap);
    va_end(ap);
    return n;
}

int vsnprint(char *buf, size_t size, const char *fmt, va_list ap)
{
    struct fmt fp;

    if (size <= 0)
        return -1;
    va_copy(fp.args, ap);
    fp.start = buf;
    fp.stop = buf + size - 1;
    fp.to = buf;
    fp.stream = NULL;
    fp.nfmt = 0;
    dofmt(&fp, fmt);
    *fp.to = '\0';
    return fp.to - buf;
}

int snprint(char *s, size_t size, const char *fmt, ...)
{
    int n;
    va_list ap;

    va_start(ap, fmt);
    n = vsnprint(s, size, fmt, ap);
    va_end(ap);
    return n;
}

int fmtstrcpy(struct fmt *fp, const char *s)
{
    int n;
    
    n = fmtputs(fp, s);
    if (n >= 0)
        return 0;
    return n;
}

int fmtprint(struct fmt *fp, const char *fmt, ...)
{
    int n;
    va_list ap;

    va_copy(ap, fp->args);
    va_start(fp->args, fmt);
    n = dofmt(fp, fmt);
    va_end(fp->args);
    va_copy(fp->args, ap);
    if (n >= 0)
        return 0;
    return n;
}
