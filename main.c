/* driver of the c compiler */
#include "config.h"
#include "compat.h"
#include "cc.h"
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stddef.h>

/* cmd option */
struct option {
    const char *name;
    unsigned short index;
    unsigned short flags;
};

enum {
    OPTION_c,
    OPTION_d,
    OPTION_D,
    OPTION_E,
    OPTION_f,
    OPTION_g,
    OPTION_help,
    OPTION_I,
    OPTION_l,
    OPTION_L,
    OPTION_nostdlib,
    OPTION_o,
    OPTION_O,
    OPTION_pthread,
    OPTION_rdynamic,
    OPTION_r,
    OPTION_shared,
    OPTION_soname,
    OPTION_static,
    OPTION_U,
    OPTION_version,
    OPTION_v,
    OPTION_w,
    OPTION_Wl,
    OPTION_W,
};

/* flags */
#define OPTION_HAS_ARG (1 << 0)
#define OPTION_NO_SPC  (1 << 1)

static const struct option cmd_options[] = {
    { "c",            OPTION_c,         0 },
    { "d",            OPTION_d,         0 },
    { "dumpversion",  OPTION_version,   0 },
    { "D",            OPTION_D,         OPTION_HAS_ARG },
    { "E",            OPTION_E,         0 },
    { "f",            OPTION_f,         OPTION_HAS_ARG | OPTION_NO_SPC },
    { "g",            OPTION_g,         0 },
    { "h",            OPTION_help,      0 },
    { "-help",        OPTION_help,      0 },
    { "I",            OPTION_I,         OPTION_HAS_ARG },
    { "l",            OPTION_l,         OPTION_HAS_ARG | OPTION_NO_SPC },
    { "L",            OPTION_L,         OPTION_HAS_ARG },
    { "nostdlib",     OPTION_nostdlib,  0 },
    { "o",            OPTION_o,         OPTION_HAS_ARG },
    { "O",            OPTION_O,         OPTION_HAS_ARG },
    { "pthread",      OPTION_pthread,   0 },
    { "rdynamic",     OPTION_rdynamic,  0 },
    { "r",            OPTION_r,         0 },
    { "shared",       OPTION_shared,    0 },
    { "static",       OPTION_static,    0 },
    { "soname",       OPTION_soname,    OPTION_HAS_ARG },
    { "U",            OPTION_U,         OPTION_HAS_ARG },
    { "-version",     OPTION_version,   0 },
    { "v",            OPTION_v,         0 },
    { "w",            OPTION_w,         0 },
    { "Wl,",          OPTION_Wl,        OPTION_HAS_ARG | OPTION_NO_SPC },
    { "W",            OPTION_W,         OPTION_HAS_ARG | OPTION_NO_SPC },
    { NULL, 0, 0 },
};

struct flag {
    const char *name;
    unsigned short offset;
    unsigned short flags;
};

#define WD_ALL    (1 << 0)      /* activated when -Wall */
#define FD_INVERT (1 << 1)      /* invert value before set */

static const struct flag fflags[] = {
    { "leading-underscore", offsetof(struct options, leading_underscore), 0 },
    { "PIC", offsetof(struct options, pic), 0 },
};

static const struct flag wflags[] = {
    { "error", offsetof(struct options, warn_error), 0 },
    { "unsupported", offsetof(struct options, warn_unsupported), 0 },
};

enum {
    OUT_NONE,
    OUT_PP,     /* preprocess */
    OUT_OBJ,    /* object */
    OUT_DLL,    /* dynamic library */
    OUT_EXE,    /* executable */
};

struct options options;
static int outputtype;
static char *output;
static struct list *inlist;
static struct list *cclist;
static struct list *ldlist;
static struct list *pplist;
static int show_help;
static int show_version;
static int nostdlib;
static int static_link;

static void help(void)
{
    static char *usage[] = {
"General options:\n",
"  -c              Compile only\n",
"  -f<flag>        Set or reset (with 'no-' prefix) <flag>\n",
"  -h, --help      Display available options\n",
"  -o <file>       Write output to <file>\n",
"  -v              To be verbose\n",
"  --version       \n",
"  -dumpversion    Display version\n",
"  -w              Disable all warnings\n",
"  -W<warning>     Set or reset (with 'no-' prefix) <warning>\n",
"  -               Use stdin as input file\n",
"\n",
"Preprocessor options:\n",
"  -Dname[=value]  Define 'name' with value 'value'\n",
"  -E              Only run the preprocessor\n",
"  -Idir           Add 'dir' to include search path\n",
"  -Uname          Undefine 'name'\n",
"\n",
"Linker options:\n",
"  -L<dir>         Add <dir> to library search path\n",
"  -l<x>           Search for library <x>\n",
"  -nostdlib       Do not link with standard crt and libraries\n",
"  -pthread        link with -lpthread and -D_REENTRANT\n",
"  -rdynamic       Pass the flag '--export-dynamic' to the linker\n",
"  -r              Generate relocatable output\n",
"  -shared         Create a shared library\n",
"  -soname         Set internal name of shared library\n",
"  -static         Static linking\n",
"  -Wl,<arg>       Pass comma separated arguments in <arg> to linker\n",
"\n", 0
    };
    fputs("Usage: hcc [options] <files>\n\n", stdout);
    for (char **p = usage; *p; p++)
        fputs(*p, stdout);
}

static int exec(const char *file, char *argv[])
{
    int r = EXIT_SUCCESS;
    pid_t pid = vfork();
    if (pid == 0) {
        execvp(file, argv);     /* child process */
    } else if (pid > 0) {
        int status, n;
        while ((n = waitpid(pid, &status, 0)) != pid ||
               (n == -1 && errno == EINTR))
            ;              /* may be EINTR by a signal, so loop it. */
        if (n != pid || !WIFEXITED(status) || WEXITSTATUS(status) != 0)
            r = EXIT_FAILURE;
        /* print signal message */
        if (n == pid && !WIFEXITED(status) && WIFSIGNALED(status))
            fprintf(stderr, "PID %d: %s\n", pid, strsignal(WTERMSIG(status)));
    } else {
        perror("can't fork");
        r = EXIT_FAILURE;
    }
    return r;
}

/* set/reset a flag */
static int set_flag(struct options *opts, const struct flag *flags, int n, const char *name, int value)
{
    const struct flag *p, *q;

    /* no- */
    if (name[0] == 'n' && name[1] == 'o' && name[2] == '-') {
        name += 3;
        value = !value;
    }

    for (p = flags, q = p + n; p < q; p++)
        if (!strcasecmp(name, p->name)) {
            if (p->flags & FD_INVERT)
                value = !value;
            *(int *)((unsigned char *)opts + p->offset) = value;
            return 0;
        }

    return -1;
}

static int set_w_flag(struct options *opts, const char *name, int value)
{
    if (!strcasecmp(name, "all")) {
        const struct flag *p, *q;
        for (p = wflags, q = p + NELEMS(wflags); p < q; p++)
            if (p->flags & WD_ALL)
                *(int *)((unsigned char *)opts + p->offset) = 1;

        return 0;
    } else {
        return set_flag(opts, wflags, NELEMS(wflags), name, value);
    }
}

static int set_f_flag(struct options *opts, const char *name, int value)
{
    return set_flag(opts, fflags, NELEMS(fflags), name, value);
}

static void parse_opts(int argc, char *argv[])
{
    int optind = 0;
    char *arg, *optarg, *beg, *cur;
    const struct option *popt;
    struct options *popts = &options;

    while (optind < argc) {
        arg = argv[optind++];

        if (arg[0] != '-' || arg[1] == '\0') {
            append(&inlist, arg);
            continue;
        }

        for (popt = cmd_options; ; popt++) {
            const char *name = popt->name;
            char *arg1 = arg + 1;
            if (name == NULL)
                die("invalid option -- '%s'", arg);
            if (!strstart(name, &arg1))
                continue;
            optarg = arg1;
            if (popt->flags & OPTION_HAS_ARG) {
                if (*arg1 == '\0' && !(popt->flags & OPTION_NO_SPC)) {
                    if (optind >= argc)
                        die("argument to '%s' is missing", arg);
                    optarg = argv[optind++];
                }
            } else if (*arg1 != '\0') {
                continue;
            }
            break;
        }

        switch (popt->index) {
        case OPTION_c:
            outputtype = OUT_OBJ;
            break;
        case OPTION_d:
            popts->debug = 1;
            append(&cclist, arg);
            break;
        case OPTION_D:
            arg = stringf("-D%s", optarg);
            append(&pplist, arg);
            append(&cclist, arg);
            break;
        case OPTION_E:
            outputtype = OUT_PP;
            break;
        case OPTION_f:
            if (set_f_flag(popts, optarg, 1) < 0 && popts->warn_unsupported)
                goto unsupported_option;
            append(&cclist, arg);
            break;
        case OPTION_g:
            if (popts->warn_unsupported)
                goto unsupported_option;
            append(&cclist, arg);
            break;
        case OPTION_help:
            show_help = 1;
            break;
        case OPTION_I:
            arg = stringf("-I%s", optarg);
            append(&cclist, arg);
            append(&pplist, arg);
            break;
        case OPTION_L:
            append(&ldlist, stringf("-L%s", optarg));
            break;
        case OPTION_l:
            append(&ldlist, arg);
            break;
        case OPTION_nostdlib:
            nostdlib = 1;
            break;
        case OPTION_o:
            output = optarg;
            break;
        case OPTION_O:
            popts->optimize = *optarg - '0';
            append(&cclist, stringf("-O%s", optarg));
            break;
        case OPTION_pthread:
            append(&ldlist, "-lpthread");
            append(&pplist, "-D_REENTRANT");
            break;
        case OPTION_rdynamic:
            append(&ldlist, "--export-dynamic");
            break;
        case OPTION_r:
            append(&ldlist, "-r");
            break;
        case OPTION_shared:
            outputtype = OUT_DLL;
            append(&ldlist, "-shared");
            break;
        case OPTION_soname:
            append(&ldlist, stringf("-soname %s", optarg));
            break;
        case OPTION_static:
            static_link = 1;
            append(&ldlist, "-static");
            break;
        case OPTION_U:
            arg = stringf("-U%s", optarg);
            append(&pplist, arg);
            append(&cclist, arg);
            break;
        case OPTION_v:
            popts->verbose = 1;
            append(&cclist, arg);
            break;
        case OPTION_version:
            show_version = 1;
            break;
        case OPTION_w:
            popts->warn_none = 1;
            append(&cclist, arg);
            break;
        case OPTION_W:
            if (set_w_flag(popts, optarg, 1) < 0 && popts->warn_unsupported)
                goto unsupported_option;
            append(&cclist, arg);
            break;
        case OPTION_Wl:
            for (beg = cur = optarg; *cur; cur++)
                if (*cur == ',') {
                    append(&ldlist, strndup(beg, cur - beg));
                    beg = cur + 1;
                }
            append(&ldlist, strndup(beg, cur - beg));
            break;
        default:
            if (popts->warn_unsupported) {
            unsupported_option:
                fprintf(stderr, "warning: unsupported option -- '%s'\n", arg);
            }
            break;
        }
    }

    if (outputtype == 0)
        outputtype = OUT_EXE;
    if (outputtype == OUT_PP)
        popts->preprocess_only = 1;
}

/* $0: output file, $1: input files, $2: additional options */
static char **subst(char **argv, char **ifiles, char *ofile, char **options)
{
    int argc = length(argv);
    int nifiles = length(ifiles);
    int noptions = length(options);
    int ac = argc + nifiles + noptions;
    char **av = xmalloc(ac * sizeof(char *));
    int j = 0;
    for (int i = 0; i < argc; i++) {
        char *arg = argv[i];
        if (arg[0] == '$' && isdigit(arg[1])) {
            int k = arg[1] - '0';
            assert(k >= 0 && k <= 2);
            if (k == 0) {
                av[j++] = ofile ? ofile : "a.out";
            } else if (k == 1) {
                for (int i = 0; i < nifiles; i++)
                    av[j++] = ifiles[i];
            } else {
                for (int i = 0; i < noptions; i++)
                    av[j++] = options[i];
            }
        } else {
            av[j++] = arg;
        }
    }
    assert(j < ac);
    av[j] = NULL;
    return av;
}

static int do_ld(char **ifiles, char *ofile, char **options)
{
#ifdef CONFIG_TARGET_LINUX
    static char *ld[] = {
        "ld",
#if defined CONFIG_TARGET_X86_64
        "-A", "x86_64",
#endif
        "-o", "$0",
        "-dynamic-linker", CONFIG_DYNAMIC_LINKER,
        "-L" CONFIG_LIBC_PREFIX,
        "$2", "$1", NULL
    };
    /*
     * The ouput must be linked in such order:
     * crt1.o, crti.o, <ifiles>, <libgcc, libgcc_eh, libc>, crtn.o
     */
    int n = length(ifiles);
    char **av = xmalloc((n + 20) * sizeof(char *));
    int i = 0;

    if (nostdlib) {
        memcpy(&av[i], ifiles, n * sizeof(char *));
        i += n;
    } else {
        if (outputtype != OUT_DLL)
            av[i++] = CONFIG_CRT_PREFIX "/crt1.o";
        av[i++] = CONFIG_CRT_PREFIX "/crti.o";
        memcpy(&av[i], ifiles, n * sizeof(char *));
        i += n;
        if (static_link)
            av[i++] = "--start-group";
        av[i++] = "-lc";
        if (static_link)
            av[i++] = "--end-group";
        av[i++] = CONFIG_CRT_PREFIX "/crtn.o";
    }
    av[i] = NULL;

    return exec(ld[0], subst(ld, av, ofile, options));
#elif defined CONFIG_TARGET_DARWIN
    char *ld[] = {
        "ld",
        "-o", "$0",
#if defined CONFIG_TARGET_X86_64
        "-arch", "x86_64",
#endif
        "$1", "$2",
        NULL
    };

    int n = length(ifiles);
    char **av = xmalloc((n + 20) * sizeof(char *));
    int i = 0;

    if (nostdlib) {
        memcpy(&av[i], ifiles, n * sizeof(char *));
        i += n;
    } else {
        memcpy(&av[i], ifiles, n * sizeof(char *));
        i += n;
        av[i++] = "-lc";
        av[i++] = "-lm";
    }
    av[i] = NULL;

    return exec(ld[0], subst(ld, av, ofile, options));
#endif
}

static void do_sys_opts(void)
{
    int a, b, c;
    char buf[64];
    
    sscanf(CONFIG_VERSION, "%d.%d.%d", &a, &b, &c);
    snprintf(buf, sizeof(buf), "-D__HCC__=%d", a * 10000 + b * 100 + c);
    append(&pplist, strdup(buf));

    /* for passing some gnu configure test */
    append(&pplist, "-D__TINYC__");

#ifdef CONFIG_TARGET_X86_64
    append(&pplist, "-D__x86_64__");
    append(&pplist, "-D__LP64__");
#endif

#ifdef CONFIG_TARGET_LINUX
    append(&pplist, "-D__linux__");
    append(&pplist, "-D__linux");
    append(&pplist, "-D__unix__");
    append(&pplist, "-D__unix");
    append(&pplist, "-Dunix");
#elif defined CONFIG_TARGET_DARWIN
    append(&pplist, "-D__APPLE__");
#endif

    /* include */
    append(&pplist, "-I" CONFIG_CC_INCLUDE_DIR);
    append(&pplist, "-I/usr/include");
    /* linux */
#ifdef CONFIG_TARGET_LINUX
    append(&pplist, "-I/usr/include/linux");
    append(&pplist, "-I/usr/include/x86_64-linux-gnu");
#elif defined CONFIG_TARGET_DARWIN
    append(&pplist, "-I" CONFIG_MACOS_INCLUDE_DIR);
#endif
}

static int c_source_file_p(const char *filename)
{
    char *ext;

    /* standard input */
    if (!strcmp(filename, "-"))
        return 1;

    ext = file_ext(filename);
    return !strcasecmp(ext, ".c") || !strcasecmp(ext, ".h");
}

int main(int argc, char *argv[])
{
    int ninputs, c;
    char **inputs, **cmd, **opt;
    char *ifile, *ofile, *base;
    struct list *list;

    parse_opts(argc - 1, argv + 1);
    /* sys includes follow user defined includes */
    do_sys_opts();
    
    if (show_help) {
        help();
        return 0;
    }
    if (show_version) {
        fprintf(stdout, "%s\n", CONFIG_VERSION);
        return 0;
    }
    
    inputs = ltoa(&inlist);
    ninputs = length(inputs);
    if (ninputs == 0)
        die("no input files");
    if (output && ninputs > 1)
        if (outputtype != OUT_EXE && outputtype != OUT_DLL)
            die("can't specify -o when generating multiple output files");

    c = 0;
    if (ninputs == 1) {
        ifile = inputs[0];
        ofile = output;
        /* c source file, header file */
        if (c_source_file_p(ifile)) {
            switch (outputtype) {
            case OUT_PP:
                c = cc1(ifile, ofile, ltoa(&pplist));
                break;

            case OUT_OBJ:
                if (ofile == NULL && strcmp(ifile, "-")) {
                    base = file_basename(ifile);
                    ofile = ch_file_ext(base, "o");
                }
                c = cc1(ifile, ofile, ltoa(&pplist));
                break;

            case OUT_DLL:
            case OUT_EXE:
                ofile = mktempfile("o");
                c = cc1(ifile, ofile, ltoa(&pplist));
                inputs[0] = ofile;
                break;

            default:
                die("unknown output type: %d", outputtype);
            }

            if (c && ofile)
                remove(ofile);
        }
    } else {
        list = NULL;
        append(&list, argv[0]);
        append(&list, "$1"); /* input file */
        append(&list, "-o");
        append(&list, "$0"); /* output file */
        if (outputtype == OUT_PP)
            append(&list, "-E");
        else
            append(&list, "-c");
        /* cc options */
        for (opt = ltoa(&cclist); *opt; opt++)
            append(&list, *opt);
        /* ld options: ignored. */

        cmd = ltoa(&list);
        for (int i = 0; i < ninputs; i++) {
            ifile = inputs[i];
            ofile = NULL;

            if (!c_source_file_p(ifile))
                continue;

            switch (outputtype) {
            case OUT_PP:
                break;

            case OUT_OBJ:
                if (ofile == NULL && strcmp(ifile, "-")) {
                    base = file_basename(ifile);
                    ofile = ch_file_ext(base, "o");
                }
                break;

            case OUT_DLL:
            case OUT_EXE:
                ofile = mktempfile("o");
                break;

            default:
                die("unknown output type: %d", outputtype);
            }

            cmd[1] = ifile;
            cmd[3] = ofile ? ofile : "-";
            if (exec(argv[0], cmd) != EXIT_SUCCESS) {
                c++;
                if (ofile)
                    remove(ofile);
            }
            inputs[i] = ofile;
        }
    }

    if (c == 0 && (outputtype == OUT_EXE || outputtype == OUT_DLL))
        c = do_ld(inputs, output, ltoa(&ldlist));

    return c;
}
