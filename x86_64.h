#ifndef X86_64_H
#define X86_64_H

#define GPR_P(reg)  ((reg) >= RAX && (reg) <= R15)
#define SSE_P(reg)  ((reg) >= XMM0 && (reg) <= XMM15)

enum {
    RXXX,

    /*
      Registers for allocation
       To keep it simple, callee preserved registers are not used.
       Don't reorder or regalloc will break.
     */
    RAX,
    RCX,
    RDX,
    RDI,
    RSI,
    R8,
    R9,
    R10,
    R11,

    /* garbage */
    RSP,
    RBP,
    RBX,
    R12,
    R13,
    R14,
    R15,
    
    RIP,    /* for RIP-relative addressing asm instr */

    XMM0,
    XMM15 = XMM0 + 15,
    NREG
};

/* asm op */
enum {
    AXXX,
    ANOP,
    AMOV,
    AMOVSX,
    AMOVZX,
    AADD,
    ASUB,
    AMUL,
    ADIV,
    AUDIV,
    AAND,
    AOR,
    ASAL,
    ASAR,       /* unsigned shift right */
    ASHR,       /* signed shift right */
    AXOR,
    ANOT,
    ANEG,
    ALEA,
    ACALL,
    AJMP,
    AJE,
    AJNE,
    AJA,        /* unsigned */
    AJAE,
    AJB,
    AJBE,
    AJG,        /* signed */
    AJGE,
    AJL,
    AJLE,
    ASETE,      /* set on condition */
    ASETNE,
    ASETA,
    ASETAE,
    ASETB,
    ASETBE,
    ASETG,
    ASETGE,
    ASETL,
    ASETLE,
    ATEST,
    ACMP,
    ALEAVE,
    ARET,
    APUSH,
    APOP,
    AREPMOVS,
    AREPSTOS,
    ACVTSI2SS,
    ACVTSI2SD,
    ACVTSS2SI,
    ACVTSS2SD,
    ACVTSD2SI,
    ACVTSD2SS,
    AMOVSS,
    AMOVSD,
    AMOVQ,          /* SSE move quadword */
    AMOVAPS,
    AADDSS,
    AADDSD,
    ASUBSS,
    ASUBSD,
    AMULSS,
    AMULSD,
    ADIVSS,
    ADIVSD,
    ACOMISS,
    ACOMISD,
    NAOP
};

/* opcode flags */
#define OPF_R           (1 << 0)    /* enable both reg and r/m in ModRM */
#define OPF_RX          (1 << 1)    /* register encoded in lower 3 bits of opcode */
#define OPF_IX          (1 << 2)    /* immediate operand */
#define OPF_CX          (1 << 3)    /* 1,2,4,6,8,10 byte value following opcode */
#define OPF_DGT         (1 << 4)    /* enable only r/m, fill reg with digit */
#define OPF_B           (1 << 5)    /* accept operand size: 1 byte */
#define OPF_W           (1 << 6)    /* accept operand size: 2 bytes */
#define OPF_L           (1 << 7)    /* accept operand size: 4 bytes */
#define OPF_Q           (1 << 8)    /* accept operand size: 8 bytes */
#define OPF_SRC_B       (1 << 9)    /* 1 byte source operand */
#define OPF_SRC_W       (1 << 10)   /* 2 byte source operand */
#define OPF_SRC_L       (1 << 11)   /* 4 byte source operand */
#define OPF_SRC_Q       (1 << 12)   /* 8 byte source operand */
#define OPF_SRC_X       (1 << 13)   /* any size except 1,2,4,8 */
#define OPF_IMM8        (1 << 14)   /* accept only imm8 */
#define OPF_IMM32       (1 << 15)   /* accept only imm32 */
#define OPF_IMM_N64     (1 << 16)   /* imm64 not allowed */
#define OPF_NP          (1 << 17)   /* 66/F2/F3 prefixes not allowed */
#define OPF_NW          (1 << 18)   /* no REX.W when only 8 byte is met */
#define OPF_REP         (1 << 19)   /* REP/REPE/REPZ instructions */

#define OPF_WL      (OPF_W | OPF_L)     /* accept 2,4 */
#define OPF_WLQ     (OPF_WL | OPF_Q)    /* accept 2,4,8 */
#define OPF_LQ      (OPF_L | OPF_Q)     /* accept 4,8 */
#define OPF_BWLQ    (OPF_B | OPF_WLQ)   /* accept 1,2,4,8 */

#define OPF_SRC_LQ      (OPF_SRC_L | OPF_SRC_Q)
#define OPF_SRC_BWLQ    (OPF_SRC_B | OPF_SRC_W | OPF_SRC_L | OPF_SRC_Q)
#define OPF_SRC_ANY     (OPF_SRC_BWLQ | OPF_SRC_X)

/* opcode enc */
#define OPC_REG     (1 << 0)    /* register */
#define OPC_MEM     (1 << 1)    /* memory */
#define OPC_IMM     (1 << 2)    /* immediate */
#define OPC_REL     (1 << 3)    /* relative offset to PC */

#define OPC_RM      (OPC_REG | OPC_MEM)

/* x86 instruction */
struct opc {
    short op;           /* asm op */
    char from;          /* from encoding */
    char to;            /* to encoding */
    int opcode;         /* opcode: 1,2 or 3 bytes */
    char info;          /* payload */
    int flags;          /* opcode flags */
};

/* x86_64-asm.c */
extern void opc_init(struct section *);
extern void gopc(int, struct node *, struct node *, long *);

#endif  /* X86_64_H */