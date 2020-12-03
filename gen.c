#include "cc.h"

struct cse {
    int lab;
    long value;
};

struct swtch {
    struct type *type;
    struct cse *cases;
    int cases_used;
    int cases_alloc;
    int default_lab;
};

static int brk;
static int cnt;
static struct swtch *swtch;

int genlab(int count)
{
    static int lab = 1;
    
    assert(count > 0);
    lab += count;
    return lab - count;
}

static void nodconst(struct node *p, struct type *ty, long val)
{
    p->op = OCONST;
    p->type = ty;
    p->v.i = convltox(val, ty->etype);
    p->left = NULL;
    p->right = NULL;
}

static void freeswtch(struct swtch *p)
{
    free(p->cases);
    free(p);
}

static struct swtch *newswtch(void)
{
    return zmalloc(sizeof (struct swtch));
}

static void addcase(struct swtch *s, struct node *p, int lab)
{
    int i;
    struct cse *c;

    for (i = 0; i < s->cases_used; i++) {
        c = &s->cases[i];
        if (c->value == p->v.i)
            error(p, "case '%ld' redeclared", p->v.i);
    }

    if (s->cases_used + 1 > s->cases_alloc) {
        s->cases_alloc = 2 * s->cases_used + 10;
        s->cases = xrealloc(s->cases, s->cases_alloc * sizeof (struct cse));
    }

    c = &s->cases[s->cases_used++];
    c->lab = lab;
    c->value = p->v.i;
}

static void boolgen(struct node *p, int lab, int invert)
{
    int c;

    if (p == NULL)
        goto c_true;    /* treated as true */
    
    conv(p);
    if (p->type == NULL)
        goto c_false;
    if (tcompat(p, NULL, p->type, tnot)) {
        p->type = NULL;
        goto c_false;
    }
    c = vbconst(p);
    if (c == 0)
        goto c_false;
    if (c == 1)
        goto c_true;

    gtest(p, lab, invert);
    return;

c_true:
    if (!invert)
        gbranch(OGOTO, lab, 0);
    return;

c_false:
    if (invert)
        gbranch(OGOTO, lab, 0);
}

/**
 we can't eliminate such code now before running at least one pass.
 
    if (0) {
    label:
        printf("maybe reachable even inside if-false statement");
    }
    
    goto label;
 */
static void gen(struct node *p)
{
    struct node nod, nod1, nod2;
    struct node *l, *r;
    int i, lab, sbrk, scnt;
    struct swtch *sswtch;
    struct cse *cse;

loop:
    if (p == NULL)
        return;

    switch (p->op) {
    case OLIST:
    case OCOMMA:
        gen(p->left);
        p = p->right;
        goto loop;

    case OIF:
        lab = genlab(2);
        boolgen(p->left, lab, 1);
        gen(p->right->left);
        if (p->right->right) {
            gbranch(OGOTO, lab + 1, 0);
            glabel(lab);
            gen(p->right->right);
            glabel(lab + 1);
        } else {
            glabel(lab);
        }
        break;

    case OWHILE:
        lab = genlab(2);
        sbrk = brk;
        scnt = cnt;
        cnt = lab;
        brk = lab + 1;
        glabel(lab);
        boolgen(p->left, lab + 1, 1);
        gen(p->right);
        gbranch(OGOTO, lab, 0);
        glabel(lab + 1);
        brk = sbrk;
        cnt = scnt;
        break;

    case ODOWHILE:
        lab = genlab(3);
        sbrk = brk;
        scnt = cnt;
        cnt = lab + 1;
        brk = lab + 2;
        glabel(lab);
        gen(p->right);
        glabel(lab + 1);
        boolgen(p->left, lab, 0);
        glabel(lab + 2);
        brk = sbrk;
        cnt = scnt;
        break;

    case OFOR:
        lab = genlab(3);
        scnt = cnt;
        sbrk = brk;
        cnt = lab + 1;
        brk = lab + 2;
        gen(p->left->left);
        l = p->left->right;
        glabel(lab);
        boolgen(l->left, lab + 2, 1);
        gen(p->right);
        glabel(lab + 1);
        gen(l->right);
        gbranch(OGOTO, lab, 0);
        glabel(lab + 2);
        cnt = scnt;
        brk = sbrk;
        break;

    case OSWITCH:
        l = p->left;
        conv(l);
        if (l->type == NULL)
            break;
        if (!typei[l->type->etype]) {
            error(l, "switch expression must have integer type");
            break;
        }
        if (l->type->size < types[TINT]->size) {
            if (l->op != OCONST) {
                r = node(OXXX, NULL, NULL);
                *r = *l;
                l->op = OCAST;
                l->left = r;
                l->right = NULL;
                l->addable = 0;
            }
            l->type = types[TINT];
        }
        sswtch = swtch;
        swtch = newswtch();
        swtch->type = l->type;
        lab = genlab(2);
        sbrk = brk;
        brk = lab + 1;
        /* free the register to prevent corruption in cases */
        if (l->op != OCONST) {
            regalloc(&nod, l, NULL);
            gexpr(l, &nod);
            regfree(&nod);
        }
        gbranch(OGOTO, lab, 0);
        gen(p->right);
        gbranch(OGOTO, lab + 1, 0);
        glabel(lab);
        /* switch code */
        for (i = 0; i < swtch->cases_used; i++) {
            cse = &swtch->cases[i];
            if (l->op == OCONST) {
                if (l->v.i == cse->value) {
                    gbranch(OGOTO, cse->lab, 0);
                    break;
                }
            } else {
                nodconst(&nod1, swtch->type, cse->value);
                nodnew(&nod2, OEQ, &nod, &nod1);
                nod2.type = types[TINT];
                gtest(&nod2, cse->lab, 0);
            }
        }
        if (swtch->default_lab)
            gbranch(OGOTO, swtch->default_lab, 0);
        glabel(lab + 1);
        freeswtch(swtch);
        brk = sbrk;
        swtch = sswtch;
        break;

    case OCASE:
        if (swtch == NULL) {
            error(p, "'case' not in switch statement");
            break;
        }
        lab = genlab(1);
        l = p->left;
        if (l) {
            conv(l);
            if (l->type == NULL)
                break;
            if (!typei[l->type->etype]) {
                error(l, "'case' expression must have integer type");
                break;
            }
            if (l->op != OCONST) {
                error(l, "'case' expression must be a constant");
                break;
            }
            l->v.i = convltox(l->v.i, swtch->type->etype);
            addcase(swtch, l, lab);
        } else {
            if (swtch->default_lab)
                error(l, "label 'default' redeclared");
            swtch->default_lab = lab;
        }
        glabel(lab);
        gen(p->right);
        break;

    case OLABEL:
        if (p->left->label == 0)
            p->left->label = genlab(1);
        glabel(p->left->label);
        gen(p->right);
        break;

    case OGOTO:
        l = p->left;
        if (l == NULL)
            break;
        if (l->complex == 0) {
            error(l, "label '%s' not declared", l->sym->name);
            break;
        }
        if (l->label == 0)
            l->label = genlab(1);
        gbranch(OGOTO, l->label, 0);
        break;

    case OBREAK:
        if (brk == 0) {
            error(p, "'break' not in a loop or switch statement");
            break;
        }
        gbranch(OGOTO, brk, 0);
        break;

    case OCONTINUE:
        if (cnt == 0) {
            error(p, "'continue' not in a loop statement");
            break;
        }
        gbranch(OGOTO, cnt, 0);
        break;

    case ORETURN:
        l = p->left;
        if (p->type->etype == TVOID) {
            /* return cast to void is allowed. */
            if (l) {
                conv(l);
                if (l->type && l->type->etype != TVOID)
                    error(l, "return non-void value in void function");
                gexpr(l, NULL);
            }
        } else {
            if (l == NULL) {
                error(p, "missing returning value");
                goto gret;
            }
            l = node(OCAST, l, NULL);
            l->type = p->type;
            conv(l);
            greturn(l);
        }
    gret:
        gbranch(ORETURN, 0, 0);
        break;

    default:
        conv(p);
        gexpr(p, NULL);
        break;
    }
}

void codegen(struct node *p, struct node *fn, struct node *params)
{
    brk = 0;
    cnt = 0;
    swtch = NULL;
    gen(p);
}