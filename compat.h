/* This file imports the system dependent routines. */
#ifndef COMPAT_H
#define COMPAT_H

#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

/* strdup, strndup */
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

/* strndup */
#define _POSIX_C_SOURCE  200809L

#if defined(__APPLE__)
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE
#endif  /* _DARWIN_C_SOURCE */
#endif  /* __APPLE__ */

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
/*
 * NOTE!!!
 * The 'dirname()' manual page says:
 * Both dirname() and basename() may modify
 * the contents of path, so it may be desirable
 * to pass a copy when calling one of these functions.
 */
/* dirname, basename */
#include <libgen.h>
/* uname */
#include <sys/utsname.h>
/* file control */
#include <fcntl.h>

/* PATH_MAX */
#ifndef PATH_MAX
#define PATH_MAX  4096
#endif

/* strcasecmp */
#include <strings.h>

#endif  /* COMPAT_H */
