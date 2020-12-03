#include "cc.h"

static int printed_line = 1;
static int last_line = 1;
static int dirty_line_p;

const char *main_input_filename;
const char *main_output_filename;

static void maybe_print_lines(int diff)
{
    if (diff > 5) {
        if (dirty_line_p) {
            dirty_line_p = 0;
            fputs("\n", stdout);
        }
        printed_line += diff;
        printf("# %d\n", printed_line);
    } else if (diff > 0) {
        while (diff) {
            fputs("\n", stdout);
            printed_line++;
            diff--;
        }
        dirty_line_p = 0;
    }
}

static void cb_file_change(int line, int reason)
{
    const char *oname;
    int oline;

    switch (reason) {
    case FC_ENTER:
        if (line > 1) {
            cpp_resolve_location(line - 1, NULL, &oline);
            maybe_print_lines(oline - printed_line);
        }
        /* fall thru */

    case FC_LEAVE:
        if (dirty_line_p) {
            dirty_line_p = 0;
            fputs("\n", stdout);
        }
        cpp_resolve_location(line, &oname, &oline);
        printed_line = oline;
        printf("# %d \"%s\"\n", oline, oname);
        break;

    case FC_RENAME:
        if (dirty_line_p) {
            dirty_line_p = 0;
            fputs("\n", stdout);
        }
        printed_line = hist->offset;
        if (hist->name)
            printf("# %d \"%s\"\n", printed_line, hist->name);
        else
            printf("# %d\n", printed_line);
        break;
    }
    last_line = line;
}

static void cb_line_change(int line, struct token *tok)
{
    int d;
    
    if (tok->id == TOK_EOF)
        return;
    d = line - last_line;
    if (d > 0)
        maybe_print_lines(d);
    last_line = line;
}

static void preprocess(void)
{
    struct token result;

    do {
        cpp_get_token(&result);
        if (result.id == TOK_EOF) {
            if (dirty_line_p) {
                dirty_line_p = 0;
                fputs("\n", stdout);
            }
            break;
        }
        if (result.flags & TOK_FLAG_PREV_WHITE)
            fputs(" ", stdout);
        fputs(tok2s(&result), stdout);
        dirty_line_p = 1;
    } while (1);
}

int cc1(const char *ifile, const char *ofile, char **opts)
{
    char *opt;

    /* stdin as input */
    if (ifile && ifile[0] == '-' && ifile[1] == 0)
        ifile = "";
    /* stdout as output */
    if (ofile && ofile[0] == '-' && ofile[1] == 0)
        ofile = "";

    main_input_filename = ifile;
    main_output_filename = ofile;

    /* set callbacks for standalone mode */
    if (options.preprocess_only) {
        cpp_callbacks.file_change = cb_file_change;
        cpp_callbacks.line_change = cb_line_change;
    }

    fmtinit();
    tokens_init();
    types_init();
    cpp_init(ifile);
    
    for (; (opt = *opts); opts++)
        if (opt[1] == 'D')
            cpp_define(opt + 2);
        else if (opt[1] == 'U')
            cpp_undef(opt + 2);
        else if (opt[1] == 'I')
            cpp_add_include(opt + 2, 0);

    /* GNU C Common Predefined Macros */
    if (options.optimize > 0)
        cpp_define("__OPTIMIZE__");
    /* FIXME: auto set -fPIC when -DPIC */
    if (cpp_macro_defined_p("PIC"))
        options.pic = 1;

    if (options.preprocess_only) {
        preprocess();
    } else {
        ginit();
        parse();
        gfini();
    }

    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
