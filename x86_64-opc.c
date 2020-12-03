#include "cc.h"
#include "x86_64.h"
#include <inttypes.h>
#include <limits.h>

#define RL(rc)              ((rc) & 7)
#define RH(rc)              (((rc) >> 3) & 1)
#define ENC(f, t)           (((f) << 8) | ((t) & 0xFF))
#define ModRM(mod, reg, rm) (((mod) << 6) | (RL(reg) << 3) | RL(rm))
#define SIB(s, i, b)        (((s) << 6) | (RL(i) << 3) | RL(b))

/* opcode encoding context */
struct opc_state {
    int opcode;
    int flags;
    short op;
    char info;
    char sz;        /* operand size */
    char asz;       /* address size mode (0:default, 1:non-default) */
    char wrxb;
    char mod;
    char reg;
    char rm;
    char index;
    char base;
    char scale;
    char nocode;
    int disp;
    long imm;
};

static struct opc_state opc;
static struct section *text_sec;

/* op,from,to,opcode,flags,info */
static struct opc opctab[] = {
    ANOP,    0,       0,       0x90,     0, OPF_BWLQ|OPF_NW,

    AMOV,    OPC_REG, OPC_RM,  0x88,     0, OPF_R|OPF_B,
    AMOV,    OPC_REG, OPC_RM,  0x89,     0, OPF_R|OPF_WLQ,
    AMOV,    OPC_RM,  OPC_REG, 0x8a,     0, OPF_R|OPF_B,
    AMOV,    OPC_RM,  OPC_REG, 0x8b,     0, OPF_R|OPF_WLQ,
    AMOV,    OPC_IMM, OPC_REG, 0xb0,     0, OPF_IX|OPF_RX|OPF_B,
    AMOV,    OPC_IMM, OPC_REG, 0xb8,     0, OPF_IX|OPF_RX|OPF_WLQ,
    AMOV,    OPC_IMM, OPC_RM,  0xc6,     0, OPF_IX|OPF_DGT|OPF_B,
    AMOV,    OPC_IMM, OPC_RM,  0xc7,     0, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    
    AMOVSX,  OPC_RM,  OPC_REG, 0x63,     0, OPF_R|OPF_Q|OPF_SRC_L,
    AMOVSX,  OPC_RM,  OPC_REG, 0xbf0f,   0, OPF_R|OPF_LQ|OPF_SRC_W,
    AMOVSX,  OPC_RM,  OPC_REG, 0xbe0f,   0, OPF_R|OPF_WLQ|OPF_SRC_B,
    AMOVZX,  OPC_RM,  OPC_REG, 0xb70f,   0, OPF_R|OPF_LQ|OPF_SRC_W,
    AMOVZX,  OPC_RM,  OPC_REG, 0xb60f,   0, OPF_R|OPF_WLQ|OPF_SRC_B,
    
    AADD,    OPC_REG, OPC_RM,  0x00,     0, OPF_R|OPF_B,
    AADD,    OPC_REG, OPC_RM,  0x01,     0, OPF_R|OPF_WLQ,
    AADD,    OPC_RM,  OPC_REG, 0x02,     0, OPF_R|OPF_B,
    AADD,    OPC_RM,  OPC_REG, 0x03,     0, OPF_R|OPF_WLQ,
    AADD,    OPC_IMM, OPC_RM,  0x80,     0, OPF_IX|OPF_DGT|OPF_B,
    AADD,    OPC_IMM, OPC_RM,  0x81,     0, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    AADD,    OPC_IMM, OPC_RM,  0x83,     0, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    
    ASUB,    OPC_REG, OPC_RM,  0x28,     0, OPF_R|OPF_B,
    ASUB,    OPC_REG, OPC_RM,  0x29,     0, OPF_R|OPF_WLQ,
    ASUB,    OPC_RM,  OPC_REG, 0x2a,     0, OPF_R|OPF_B,
    ASUB,    OPC_RM,  OPC_REG, 0x2b,     0, OPF_R|OPF_WLQ,
    ASUB,    OPC_IMM, OPC_RM,  0x80,     5, OPF_IX|OPF_DGT|OPF_B,
    ASUB,    OPC_IMM, OPC_RM,  0x81,     5, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    ASUB,    OPC_IMM, OPC_RM,  0x83,     5, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    
    AMUL,    OPC_RM,  OPC_REG, 0xaf0f,   0, OPF_R|OPF_WLQ,
    AMUL,    OPC_IMM, OPC_REG, 0x69,     0, OPF_IX|OPF_R|OPF_WLQ|OPF_IMM_N64,
    AMUL,    OPC_IMM, OPC_REG, 0x6b,     0, OPF_IX|OPF_R|OPF_WLQ|OPF_IMM8,
    
    ADIV,    0,       OPC_RM,  0xf7,     7, OPF_DGT|OPF_WLQ,
    AUDIV,   0,       OPC_RM,  0xf7,     6, OPF_DGT|OPF_WLQ,
    
    AAND,    OPC_IMM, OPC_RM,  0x80,     4, OPF_IX|OPF_DGT|OPF_B,
    AAND,    OPC_IMM, OPC_RM,  0x81,     4, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    AAND,    OPC_IMM, OPC_RM,  0x83,     4, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    AAND,    OPC_REG, OPC_RM,  0x20,     0, OPF_R|OPF_B,
    AAND,    OPC_REG, OPC_RM,  0x21,     0, OPF_R|OPF_WLQ,
    AAND,    OPC_RM,  OPC_REG, 0x22,     0, OPF_R|OPF_B,
    AAND,    OPC_RM,  OPC_REG, 0x23,     0, OPF_R|OPF_WLQ,
    
    AOR,     OPC_IMM, OPC_RM,  0x80,     1, OPF_IX|OPF_DGT|OPF_B,
    AOR,     OPC_IMM, OPC_RM,  0x81,     1, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    AOR,     OPC_IMM, OPC_RM,  0x83,     1, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    AOR,     OPC_REG, OPC_RM,  0x08,     0, OPF_R|OPF_B,
    AOR,     OPC_REG, OPC_RM,  0x09,     0, OPF_R|OPF_WLQ,
    AOR,     OPC_RM,  OPC_REG, 0x0a,     0, OPF_R|OPF_B,
    AOR,     OPC_RM,  OPC_REG, 0x0b,     0, OPF_R|OPF_WLQ,
    
    ASAL,    OPC_IMM, OPC_RM,  0xc0,     4, OPF_IX|OPF_DGT|OPF_B,
    ASAL,    OPC_IMM, OPC_RM,  0xc1,     4, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    ASAL,    OPC_REG, OPC_RM,  0xd2,     4, OPF_R|OPF_DGT|OPF_B,
    ASAL,    OPC_REG, OPC_RM,  0xd3,     4, OPF_R|OPF_DGT|OPF_WLQ,
    
    ASAR,    OPC_IMM, OPC_RM,  0xc0,     7, OPF_IX|OPF_DGT|OPF_B,
    ASAR,    OPC_IMM, OPC_RM,  0xc1,     7, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    ASAR,    OPC_REG, OPC_RM,  0xd2,     7, OPF_R|OPF_DGT|OPF_B,
    ASAR,    OPC_REG, OPC_RM,  0xd3,     7, OPF_R|OPF_DGT|OPF_WLQ,
    
    ASHR,    OPC_IMM, OPC_RM,  0xc0,     5, OPF_IX|OPF_DGT|OPF_B,
    ASHR,    OPC_IMM, OPC_RM,  0xc1,     5, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    ASHR,    OPC_REG, OPC_RM,  0xd2,     5, OPF_R|OPF_DGT|OPF_B,
    ASHR,    OPC_REG, OPC_RM,  0xd3,     5, OPF_R|OPF_DGT|OPF_WLQ,
    
    AXOR,    OPC_IMM, OPC_RM,  0x80,     6, OPF_IX|OPF_DGT|OPF_B,
    AXOR,    OPC_IMM, OPC_RM,  0x81,     6, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    AXOR,    OPC_IMM, OPC_RM,  0x83,     6, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    AXOR,    OPC_REG, OPC_RM,  0x30,     0, OPF_R|OPF_B,
    AXOR,    OPC_REG, OPC_RM,  0x31,     0, OPF_R|OPF_WLQ,
    AXOR,    OPC_RM,  OPC_REG, 0x32,     0, OPF_R|OPF_B,
    AXOR,    OPC_RM,  OPC_REG, 0x33,     0, OPF_R|OPF_WLQ,
    
    ANEG,    0,       OPC_RM,  0xf7,     3, OPF_DGT|OPF_WLQ,
    ANEG,    0,       OPC_RM,  0xf6,     3, OPF_DGT|OPF_B,
    
    ANOT,    0,       OPC_RM,  0xf7,     2, OPF_DGT|OPF_WLQ,
    ANOT,    0,       OPC_RM,  0xf6,     2, OPF_DGT|OPF_B,
    
    ALEA,    OPC_MEM, OPC_REG, 0x8d,     0, OPF_R|OPF_WLQ|OPF_SRC_ANY,
    
    ATEST,   OPC_REG, OPC_RM,  0x85,     0, OPF_R|OPF_WLQ,
    ATEST,   OPC_REG, OPC_RM,  0x84,     0, OPF_R|OPF_B,
    ATEST,   OPC_IMM, OPC_RM,  0xf7,     0, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    ATEST,   OPC_IMM, OPC_RM,  0xf6,     0, OPF_IX|OPF_DGT|OPF_B,
    
    ACMP,    OPC_IMM, OPC_RM,  0x80,     7, OPF_IX|OPF_DGT|OPF_B,
    ACMP,    OPC_IMM, OPC_RM,  0x81,     7, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM_N64,
    ACMP,    OPC_IMM, OPC_RM,  0x83,     7, OPF_IX|OPF_DGT|OPF_WLQ|OPF_IMM8,
    ACMP,    OPC_REG, OPC_RM,  0x38,     0, OPF_R|OPF_B,
    ACMP,    OPC_REG, OPC_RM,  0x39,     0, OPF_R|OPF_WLQ,
    ACMP,    OPC_RM,  OPC_REG, 0x3a,     0, OPF_R|OPF_B,
    ACMP,    OPC_RM,  OPC_REG, 0x3b,     0, OPF_R|OPF_WLQ,
    
    AJMP,    0,       OPC_RM,  0xff,     4, OPF_DGT|OPF_Q|OPF_NW,
    AJMP,    0,       OPC_REL, 0Xeb,     0, OPF_CX|OPF_B|OPF_NW,
    AJMP,    0,       OPC_REL, 0xe9,     0, OPF_CX|OPF_L|OPF_NW,
    
    AJE,     0,       OPC_REL, 0x74,     0, OPF_CX|OPF_B|OPF_NW,
    AJE,     0,       OPC_REL, 0x840f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJNE,    0,       OPC_REL, 0x75,     0, OPF_CX|OPF_B|OPF_NW,
    AJNE,    0,       OPC_REL, 0x850f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJA,     0,       OPC_REL, 0x77,     0, OPF_CX|OPF_B|OPF_NW,
    AJA,     0,       OPC_REL, 0x870f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJAE,    0,       OPC_REL, 0x73,     0, OPF_CX|OPF_B|OPF_NW,
    AJAE,    0,       OPC_REL, 0x830f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJB,     0,       OPC_REL, 0x72,     0, OPF_CX|OPF_B|OPF_NW,
    AJB,     0,       OPC_REL, 0x820f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJBE,    0,       OPC_REL, 0x76,     0, OPF_CX|OPF_B|OPF_NW,
    AJBE,    0,       OPC_REL, 0x860f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJG,     0,       OPC_REL, 0x7f,     0, OPF_CX|OPF_B|OPF_NW,
    AJG,     0,       OPC_REL, 0x8f0f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJGE,    0,       OPC_REL, 0x7d,     0, OPF_CX|OPF_B|OPF_NW,
    AJGE,    0,       OPC_REL, 0x8d0f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJL,     0,       OPC_REL, 0x7c,     0, OPF_CX|OPF_B|OPF_NW,
    AJL,     0,       OPC_REL, 0x8c0f,   0, OPF_CX|OPF_L|OPF_NW,
    
    AJLE,    0,       OPC_REL, 0x7e,     0, OPF_CX|OPF_B|OPF_NW,
    AJLE,    0,       OPC_REL, 0x8e0f,   0, OPF_CX|OPF_L|OPF_NW,
    
    ASETE,   0,       OPC_RM,  0x940f,   0, OPF_DGT|OPF_B,  /* fake DGT */
    ASETNE,  0,       OPC_RM,  0x950f,   0, OPF_DGT|OPF_B,
    ASETA,   0,       OPC_RM,  0x970f,   0, OPF_DGT|OPF_B,
    ASETAE,  0,       OPC_RM,  0x930f,   0, OPF_DGT|OPF_B,
    ASETB,   0,       OPC_RM,  0x920f,   0, OPF_DGT|OPF_B,
    ASETBE,  0,       OPC_RM,  0x960f,   0, OPF_DGT|OPF_B,
    ASETG,   0,       OPC_RM,  0x9f0f,   0, OPF_DGT|OPF_B,
    ASETGE,  0,       OPC_RM,  0x9d0f,   0, OPF_DGT|OPF_B,
    ASETL,   0,       OPC_RM,  0x9c0f,   0, OPF_DGT|OPF_B,
    ASETLE,  0,       OPC_RM,  0x9e0f,   0, OPF_DGT|OPF_B,
    
    APUSH,   0,       OPC_REG, 0x50,     0, OPF_RX|OPF_WLQ|OPF_NW,
    APUSH,   0,       OPC_RM,  0xff,     6, OPF_DGT|OPF_WLQ|OPF_NW,
    APUSH,   0,       OPC_IMM, 0x68,     0, OPF_IX|OPF_WLQ|OPF_IMM32|OPF_NW,
    APUSH,   0,       OPC_IMM, 0x6a,     0, OPF_IX|OPF_BWLQ|OPF_IMM8|OPF_NW,
    
    APOP,    0,       OPC_REG, 0x58,     0, OPF_RX|OPF_WLQ|OPF_NW,
    APOP,    0,       OPC_RM,  0x8f,     0, OPF_DGT|OPF_WLQ|OPF_NW,
    
    ALEAVE,  0,       0,       0xc9,     0, OPF_WLQ|OPF_NW,
    
    ACALL,   0,       OPC_RM,  0xff,     2, OPF_DGT|OPF_WLQ|OPF_NW,
    ACALL,   0,       OPC_REL, 0xe8,     0, OPF_CX|OPF_WL|OPF_NW,
    
    ARET,    0,       0,       0xc3,     0, OPF_WLQ|OPF_NW,

    AREPMOVS,  OPC_MEM, OPC_MEM, 0xa5,     0, OPF_WLQ|OPF_REP,
    AREPSTOS,  0,       OPC_MEM, 0xab,     0, OPF_WLQ|OPF_REP,
    
    AMOVAPS,   OPC_REG, OPC_RM,  0x290f,   0, OPF_R|OPF_LQ|OPF_NP|OPF_NW,
    
    AMOVQ,     OPC_REG, OPC_RM,  0xd60f66, 0, OPF_R|OPF_Q|OPF_NW,
    AMOVQ,     OPC_RM,  OPC_REG, 0x7e0ff3, 0, OPF_R|OPF_Q|OPF_NW,
    
    AMOVSS,    OPC_RM,  OPC_REG, 0x100ff3, 0, OPF_R|OPF_L|OPF_NW,
    AMOVSS,    OPC_REG, OPC_RM,  0x110ff3, 0, OPF_R|OPF_L|OPF_NW,
    
    AMOVSD,    OPC_RM,  OPC_REG, 0X100ff2, 0, OPF_R|OPF_Q|OPF_NW,
    AMOVSD,    OPC_REG, OPC_RM,  0x110ff2, 0, OPF_R|OPF_Q|OPF_NW,
    
    AADDSS,    OPC_RM,  OPC_REG, 0x580ff3, 0, OPF_R|OPF_L|OPF_NW,
    AADDSD,    OPC_RM,  OPC_REG, 0x580ff2, 0, OPF_R|OPF_Q|OPF_NW,
    ASUBSS,    OPC_RM,  OPC_REG, 0x5c0ff3, 0, OPF_R|OPF_L|OPF_NW,
    ASUBSD,    OPC_RM,  OPC_REG, 0x5c0ff2, 0, OPF_R|OPF_Q|OPF_NW,
    AMULSS,    OPC_RM,  OPC_REG, 0x590ff3, 0, OPF_R|OPF_L|OPF_NW,
    AMULSD,    OPC_RM,  OPC_REG, 0x590ff2, 0, OPF_R|OPF_Q|OPF_NW,
    ADIVSS,    OPC_RM,  OPC_REG, 0x5e0ff3, 0, OPF_R|OPF_L|OPF_NW,
    ADIVSD,    OPC_RM,  OPC_REG, 0x5e0ff2, 0, OPF_R|OPF_Q|OPF_NW,
    ACOMISS,   OPC_RM,  OPC_REG, 0x2f0f,   0, OPF_R|OPF_L|OPF_NP|OPF_NW,
    ACOMISD,   OPC_RM,  OPC_REG, 0x2f0f66, 0, OPF_R|OPF_Q|OPF_NW,
    ACVTSI2SS, OPC_RM,  OPC_REG, 0x2a0ff3, 0, OPF_R|OPF_L|OPF_SRC_LQ,
    ACVTSI2SD, OPC_RM,  OPC_REG, 0x2a0ff2, 0, OPF_R|OPF_Q|OPF_SRC_LQ,
    ACVTSS2SI, OPC_RM,  OPC_REG, 0x2d0ff3, 0, OPF_R|OPF_LQ|OPF_SRC_L,
    ACVTSD2SI, OPC_RM,  OPC_REG, 0x2d0ff2, 0, OPF_R|OPF_LQ|OPF_SRC_Q,
    ACVTSS2SD, OPC_RM,  OPC_REG, 0x5a0ff3, 0, OPF_R|OPF_Q|OPF_SRC_L|OPF_NW,
    ACVTSD2SS, OPC_RM,  OPC_REG, 0x5a0ff2, 0, OPF_R|OPF_L|OPF_SRC_Q|OPF_NW,
    -1,
};

static unsigned char regcodes[NREG];
static struct init regcodes_init[] = {
    RAX,        0,  0,  R8,         8,  0,
    RCX,        1,  0,  R9,         9,  0,
    RDX,        2,  0,  R10,        10, 0,
    RBX,        3,  0,  R11,        11, 0,
    RSP,        4,  0,  R12,        12, 0,
    RBP,        5,  0,  R13,        13, 0,
    RSI,        6,  0,  R14,        14, 0,
    RDI,        7,  0,  R15,        15, 0,
    XMM0,       0,  0,  XMM0 + 8,   8,  0,
    XMM0 + 1,   1,  0,  XMM0 + 9,   9,  0,
    XMM0 + 2,   2,  0,  XMM0 + 10,  10, 0,
    XMM0 + 3,   3,  0,  XMM0 + 11,  11, 0,
    XMM0 + 4,   4,  0,  XMM0 + 12,  12, 0,
    XMM0 + 5,   5,  0,  XMM0 + 13,  13, 0,
    XMM0 + 6,   6,  0,  XMM0 + 14,  14, 0,
    XMM0 + 7,   7,  0,  XMM0 + 15,  15, 0,
    -1
};

static int sizemap[] = {
    0, OPF_B, OPF_W, 0, OPF_L, 0, 0, 0, OPF_Q,
};

static int sizemap1[] = {
    0, OPF_SRC_B, OPF_SRC_W, 0, OPF_SRC_L, 0, 0, 0, OPF_SRC_Q,
};

static void out8(int c)
{
    sec_add_data(text_sec, &c, 1);
}

static void out16(int i)
{
    out8(i);
    out8(i >> 8);
}

static void out32(int i)
{
    out8(i);
    out8(i >> 8);
    out8(i >> 16);
    out8(i >> 24);
}

static void out64(int64_t i)
{
    out8(i);
    out8(i >> 8);
    out8(i >> 16);
    out8(i >> 24);
    out8(i >> 32);
    out8(i >> 40);
    out8(i >> 48);
    out8(i >> 56);
}

static void outx(unsigned int i)
{
    while (i) {
        out8(i);
        i = i >> 8;
    }
}

static void encode_rm(struct node *rm)
{
    long disp;

    if (rm == NULL)
        return;
    
    if (rm->reg <= 0) {
        error(rm, "bad indirect operand");
        return;
    }
    
    opc.rm = rm->reg;
    switch (rm->op) {
    case OREGISTER:    
        opc.mod = 3;
        break;

    case OINDREG:
        /* $disp(reg) or $disp(base, index, $scale) */
        /* No check: always using default address size */        
        disp = rm->offset;
        if (disp == 0)
            opc.mod = 0;
        else if (disp <= CHAR_MAX && disp >= CHAR_MIN)
            opc.mod = 1;
        else
            opc.mod = 2;
        if (disp > INT_MAX || disp < INT_MIN)
            warn(rm, "displacement overflow: %lx", disp);
        opc.disp = disp;

        /* special case: RSP means none, using SIB instead */
        if (opc.rm == RSP) {
            opc.index = RSP;
            opc.base = RSP;
            opc.scale = 0;
        }
        /* special case: RBP means disp32, using SIB instead */
        if (opc.rm == RBP)
            if (opc.mod == 0)
                opc.mod = 1;
        /* special case: RIP-relative addressing: $disp32(%rip) */
        if (opc.rm == RIP) {
            opc.mod = 0;
            opc.rm = RBP;
            opc.disp = 0;   /* rewrite to zero and using reloc */
        }

        if (rm->index > 0) {
            opc.index = rm->index;
            opc.base = opc.rm;
            opc.rm = RSP;    /* indicating following SIB byte */
            opc.scale = rm->scale;
            if (opc.scale < 0 || opc.scale > 3) {
                error(rm, "bad scale(%ld), must be 0,1,2,3", opc.scale);
                opc.scale = 0;
            }
        }
        break;
    }
}

static void encode_wrxb(void)
{
    int wrxb, reg, rm, index, base;

    wrxb = -1;
    index = base = 0;
    reg = regcodes[opc.reg];
    rm = regcodes[opc.rm];
    if (opc.index > 0) {
        index = regcodes[opc.index];
        base = regcodes[opc.base];
    }
    if (reg > 7 || rm > 7 || index > 7 || base > 7) {
        wrxb = 0x40;
        wrxb |= RH(reg) << 2;
        wrxb |= RH(index) << 1;
        if (opc.base > 0)
            wrxb |= RH(base);
        else
            wrxb |= RH(rm);
        if (opc.flags & OPF_RX)
            wrxb |= RH(reg);
    }
    /* 
     * Intel manual 2.2.1.7 Default 64-Bit Operand Size
     *  Two groups of instruction don't need a REX prefix:
     *    - near branch
     *    - All instructions implicitly reference RSP except far branches.
     */
    if (opc.sz == 8 && (opc.flags & OPF_NW) == 0) {
        if (wrxb >= 0)
            wrxb |= 0x8;
        else
            wrxb = 0x48;
    }
    opc.wrxb = wrxb;
}

/* find location of '0f' escape byte in opcode */
static int find0f(void)
{
    unsigned int opcode = opc.opcode;
    int i = 0;

    while (opcode) {
        if ((opcode & 0xFF) == 0x0F)
            break;
        opcode >>= 8;
        i++;
    }

    if (opcode)
        return i;
    
    return 0;
}

static void initopc(struct opc *p, int sz)
{
    memset(&opc, 0, sizeof(opc));
    opc.opcode = p->opcode;
    opc.flags = p->flags;
    opc.op = p->op;
    opc.info = p->info;
    opc.sz = sz;
    opc.wrxb = -1;      /* fill with invalid data */
    opc.mod = 4;
}

static void outopc(long *reloc)
{
    int modrm, c, i;
    unsigned int opcode;

    if (opc.nocode)
        return;
    
    if (opc.asz)
        out8(0x67);
    if (opc.sz == 2)
        if ((opc.flags & OPF_NP) == 0)
            out8(0x66);
    if (opc.flags & OPF_REP)
        out8(0xf3);
    if (opc.wrxb >= 0) {
        opcode = opc.opcode;
        c = find0f();
        for (i = 0; opcode; i++) {
            if (i == c)
                out8(opc.wrxb);
            out8(opcode);
            opcode >>= 8;
        }
    } else {
        outx(opc.opcode);
    }
    
    if (opc.flags & (OPF_R | OPF_DGT)) {
        if (opc.flags & OPF_DGT)
            c = opc.info;
        else
            c = regcodes[opc.reg];
        modrm = ModRM(opc.mod, c, regcodes[opc.rm]);
        out8(modrm);
        /* compute SIB byte */
        if (opc.index > 0) {
            c = SIB(opc.scale, regcodes[opc.index], regcodes[opc.base]);
            out8(c);
        }
        /* RIP-relative addressing */
        if (opc.mod == 0 && opc.rm == RBP) {
            if (reloc)
                *reloc = text_sec->data_len;
            out32(opc.disp);
        }
        if (opc.mod == 1)
            out8(opc.disp);
        if (opc.mod == 2)
            out32(opc.disp);
    } else if (opc.flags & OPF_CX) {
        /* RIP-relative addressing */
        if (opc.mod == 0 && opc.rm == RBP) {
            if (reloc)
                *reloc = text_sec->data_len;
            if (opc.sz == 1)
                out8(opc.disp);
            else if (opc.sz == 2)
                out16(opc.disp);
            else
                out32(opc.disp);
        }
    }

    if (opc.flags & OPF_IX) {
        if (opc.flags & OPF_IMM8)
            out8(opc.imm);
        else if (opc.flags & OPF_IMM32)
            out32(opc.imm);
        else if (opc.sz == 1)
            out8(opc.imm);
        else if (opc.sz == 2)
            out16(opc.imm);
        else if (opc.sz == 4 || opc.flags & OPF_IMM_N64)
            out32(opc.imm);
        else
            out64(opc.imm);
    }
}

static void genopc(struct opc *p, struct node *f, struct node *t)
{
    struct node *n;

    switch (ENC(p->from, p->to)) {
    case ENC(OPC_MEM, OPC_REG):
    case ENC(OPC_RM, OPC_REG):
        n = f, f = t, t = n;
        /* fall thru */

    case ENC(OPC_REG, OPC_RM):
        opc.reg = f->reg;
        encode_rm(t);
        encode_wrxb();
        break;

    case ENC(OPC_IMM, OPC_RM):
        opc.imm = f->v.i;
        if (p->flags & OPF_IMM_N64)
            if (opc.imm > INT_MAX || opc.imm < INT_MIN)
                warn(f, "immediate overflow: %lx", opc.imm);
        /* fall thru */

    case ENC(0, OPC_RM):
        encode_rm(t);
        encode_wrxb();
        break;

    case ENC(OPC_IMM, OPC_REG):
        opc.imm = f->v.i;
        /* special case: IMUL */
        if (opc.op == AMUL)
            encode_rm(t);
        /* fall thru */

    case ENC(0, OPC_REG):
        opc.reg = t->reg;
        if (opc.flags & OPF_RX)
            opc.opcode |= RL(regcodes[opc.reg]);
        encode_wrxb();
        break;

    case ENC(0, OPC_IMM):
        opc.imm = t->v.i;
        break;

    case ENC(0, OPC_REL):
        encode_rm(t);
        break;

    case ENC(OPC_MEM, OPC_MEM):
    case ENC(0, OPC_MEM):
        encode_wrxb();
        break;

    case 0:
        break;

    default:
        opc.nocode = 1;
        error(t, "unknown opcode encoding %d(%x, %x)", p->op, p->from, p->to);
        break;
    }
}

static int nodenc(struct node *p)
{
    if (p) {
        if (p->op == OREGISTER)
            return OPC_REG;
        if (p->op == OINDREG) {
            if (p->reg == RIP)
                return OPC_MEM | OPC_REL;
            return OPC_MEM;
        }
        if (p->op == OCONST)
            return OPC_IMM;
    }
    return 0;
}

void gopc(int op, struct node *f, struct node *t, long *reloc)
{
    struct opc *p, *p1;
    int tsz, tc, fsz, fc;
    int fenc, tenc;
    
    if (t == NULL)
        tsz = typsize[TLONG];
    else
        tsz = t->type->size;

    if (f) {
        fsz = f->type->size;
        if (fsz < 0 || fsz >= NELEMS(sizemap1))
            fc = OPF_SRC_X;
        else
            fc = sizemap1[fsz];
        if (fc == 0)
            fc = OPF_SRC_X;
    } else {
        fsz = fc = 0;
    }

    fenc = nodenc(f);
    tenc = nodenc(t);

    /* special case: func call */
    if (op == ACALL) {
        tsz = typsize[TINT];
        if (tenc & OPC_REL)
            tenc = OPC_REL; /* direct call */
    }

    if (tsz < 0 || tsz >= NELEMS(sizemap) || (tc = sizemap[tsz]) == 0) {
        error(t, "bad operand size(%d) with type '%T'", tsz, t->type);
        return;
    }

    for (p = opctab, p1 = NULL; p->op > 0; p++) {
        if (p->op != op)
            continue;
        if ((p->flags & tc) == 0)
            continue;
        if (p->flags & OPF_SRC_ANY)
            if ((p->flags & fc) == 0)
                continue;
        if (p->from)
            if ((fenc & p->from) == 0)
                continue;
        if (p->to)
            if ((tenc & p->to) == 0)
                continue;
        /* optimization for shorter opcode */
        if (p->from == OPC_IMM)
            if (f->op == OCONST && f->v.i <= CHAR_MAX && f->v.i >= CHAR_MIN)
                if ((p->flags & OPF_IMM8) == 0) {
                    p1 = p;
                    continue;
                }
        break;
    }

    if (p->op < 0)
        p = p1;
    if (p == NULL) {
        error(t, "opcode %d(%d,%d)/%d not found", op, tenc, fenc, tsz);
        return;
    }
    /* special instruction check */
    if (p->op == ASAL || p->op == ASAR || p->op == ASHR)
        if (fenc == OPC_REG)
            if (f->reg != RCX) {
                error(f, "source operand must be CL register");
                return;
            }
    if ((p->flags & OPF_SRC_ANY) == 0)
        if (fsz && tsz != fsz) {
            error(t, "operands size not match: op=%d,%d!=%d", op, fsz, tsz);
            return;
        }

    /* infer operand size */
    if (op == ACVTSI2SS || op == ACVTSI2SD)
        tsz = fsz;
    if (op == AMOVAPS)
        tsz = 16;

    initopc(p, tsz);
    genopc(p, f, t);
    outopc(reloc);
}

void opc_init(struct section *s)
{
    struct init *p;

    for (p = regcodes_init; p->code >= 0; p++)
        regcodes[p->code] = p->value;

    text_sec = s;
}