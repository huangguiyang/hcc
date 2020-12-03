#ifndef _STDARG_H
#define _STDARG_H

#ifndef _VA_LIST
typedef __builtin_va_list va_list;
#define _VA_LIST
#endif

#define va_start(ap, param)   __builtin_va_start(ap, param)
#define va_end(ap)            __builtin_va_end(ap)
#define va_arg(ap, type)      __builtin_va_arg(ap, type)
#define va_copy(dest, src)    __builtin_va_copy(dest, src)

// GNU
#define __va_copy(d, s)       __builtin_va_copy(d, s)

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST 1
#endif
typedef __builtin_va_list __gnuc_va_list;

#endif
