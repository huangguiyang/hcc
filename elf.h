#ifndef ELF_H
#define ELF_H

#include <inttypes.h>

typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t  Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t  Elf64_Sxword;

#define EI_NIDENT   16

typedef struct {
    unsigned char e_ident[EI_NIDENT];       /* ELF identification */
    Elf64_Half    e_type;                   /* Object file type */
    Elf64_Half    e_machine;                /* Machine type */
    Elf64_Word    e_version;                /* Object file version */
    Elf64_Addr    e_entry;                  /* Entry point address */
    Elf64_Off     e_phoff;                  /* Program header offset */
    Elf64_Off     e_shoff;                  /* Section Header offset */
    Elf64_Word    e_flags;                  /* Processor-specific flags */
    Elf64_Half    e_ehsize;                 /* ELF header size */
    Elf64_Half    e_phentsize;              /* Size of program header entry */
    Elf64_Half    e_phnum;                  /* Number of program header entries */
    Elf64_Half    e_shentsize;              /* Size of section header entry */
    Elf64_Half    e_shnum;                  /* Number of section header entries */
    Elf64_Half    e_shstrndx;               /* Section name string table index */
} Elf64_Ehdr;

/* e_ident index */
#define EI_MAG0         0       /* File identification */
#define EI_MAG1         1
#define EI_MAG2         2
#define EI_MAG3         3
#define EI_CLASS        4       /* File class */
#define EI_DATA         5       /* Data encoding */
#define EI_VERSION      6       /* File version */
#define EI_OSABI        7       /* OS/ABI identification */
#define EI_ABIVERSION   8       /* ABI version */
#define EI_PAD          9       /* start of padding bytes */

/* e_ident[EI_CLASS] */
#define ELFCLASSNONE    0       /* Invalid class */
#define ELFCLASS32      1       /* 32-bit objects */
#define ELFCLASS64      2       /* 64-bit objects */

/* e_ident[EI_DATA] */
#define ELFDATANONE     0       /* Invalid data encoding */
#define ELFDATA2LSB     1       /* little-endian */
#define ELFDATA2MSB     2       /* big-endian */

/* e_ident[EI_OSABI] */
#define ELFOSABI_SYSV   0       /* System V ABI */
#define ELFOSABI_HPUX   1       /* HP-UX operating system */
#define ELFOSABI_STANDALONE 255 /* Standalone (embedded) application */

/* e_ident[EI_VERSION] */
#define EV_NONE         0       /* Invalid version */
#define EV_CURRENT      1       /* Current version */

/* e_type */
#define ET_NONE         0       /* No file type */
#define ET_REL          1       /* Relocatable object file */
#define ET_EXEC         2       /* Executable file */
#define ET_DYN          3       /* Shared object file */
#define ET_CORE         4       /* Core file */
#define ET_LOSS         0xFE00  /* Environnment-specific use */
#define ET_HIOS         0xFEFF
#define ET_LOPROC       0xFF00  /* Processor-specific use */
#define ET_HIPROC       0xFFFF

/* e_machine */
#define EM_NONE         0       /* No machine */
#define EM_M32          1       /* AT&T WE 32100 */
#define EM_SPARC        2       /* SPARC */
#define EM_386          3       /* Intel 80386 */
#define EM_68K          4       /* Motorola 68000 */
#define EM_88K          5       /* Motorola 88000 */
#define EM_486          6       /* Intel 80486 */
#define EM_860          7       /* Intel 80860 */
#define EM_MIPS         8       /* MIPS RS3000 Big-Endian */
#define EM_S370         9       /* BM System/370 Processor */
#define EM_MIPS_RS4_BE  10      /* MIPS RS4000 Big-Endian */
#define EM_960          19      /* Intel 80960 */
#define EM_PPC          20      /* PowerPC */
#define EM_ARM          40      /* ARM 32-bit arch (AARCH32) */
#define EM_ALPHA        41      /* Digital Alpha */
#define EM_IA_64        50      /* Intel IA-64 */
#define EM_X86_64       62      /* AMD x86-64 arch */
#define EM_VAX          75      /* Digital VAX */
#define EM_OPENRISC     92      /* OpenRISC 32-bit embedded processor */
#define EM_AARCH64      183     /* ARM 64-bit arch (AARCH64) */

/* special section indices */
#define SHN_UNDEF       0
#define SHN_LOPROC      0xFF00  /* Processor-specific use */
#define SHN_HIPROC      0xFF1F
#define SHN_LOOS        0xFF20  /* Environment-specific use */
#define SHN_HIOS        0xFF3F
#define SHN_ABS         0xFFF1  /* Absolute value */
#define SHN_COMMON      0xFFF2  /* Common block (C tentative declaration) */

/* section header entries */
typedef struct {
    Elf64_Word    sh_name;      /* Section name */
    Elf64_Word    sh_type;      /* Section type */
    Elf64_Xword   sh_flags;     /* Section attributes */
    Elf64_Addr    sh_addr;      /* Virtual address in memory */
    Elf64_Off     sh_offset;    /* Offset in file */
    Elf64_Xword   sh_size;      /* Size of section */
    Elf64_Word    sh_link;      /* Link to other section */
    Elf64_Word    sh_info;      /* Misc innformation */
    Elf64_Xword   sh_addralign; /* Address alignment boundary */
    Elf64_Xword   sh_entsize;   /* Size of entries, if section has table */
} Elf64_Shdr;

/* sh_type */
#define SHT_NULL        0           /* Marks an unused section header */
#define SHT_PROGBITS    1           /* Info defined by the program */
#define SHT_SYMTAB      2           /* Linker symbol table */
#define SHT_STRTAB      3           /* String table */
#define SHT_RELA        4           /* "Rela" type relocation entries */
#define SHT_HASH        5           /* Symbol hash table */
#define SHT_DYNAMIC     6           /* Dynamic linking tables */
#define SHT_NOTE        7           /* Note info */
#define SHT_NOBITS      8           /* Uninitialized space */
#define SHT_REL         9           /* "Rel" type relocation entries */
#define SHT_SHLIB       10          /* Reserved */
#define SHT_DYNSYM      11          /* Dynamic loader symbol table */
#define SHT_LOOS        0x60000000  /* Environment-specific use */
#define SHT_HIOS        0x6FFFFFFF
#define SHT_LOPROC      0x70000000  /* Processor-specific use */
#define SHT_HIPROC      0x7FFFFFFF

/* sh_flags */
#define SHF_WRITE       0x1         /* Writable */
#define SHF_ALLOC       0x2         /* Allocated in memory */
#define SHF_EXECINSTR   0x4         /* Executable instructions */
#define SHF_MASKOS      0x0F000000  /* Environment-specific use */
#define SHF_MASKPROC    0xF0000000  /* Processor-specific use */

/* symbol table entry */
typedef struct {
    Elf64_Word      st_name;    /* Symbol name */
    unsigned char   st_info;    /* Type and Binding attributes */
    unsigned char   st_other;   /* Reserved */
    Elf64_Half      st_shndx;   /* Section table index */
    Elf64_Addr      st_value;   /* Symbol value */
    Elf64_Xword     st_size;    /* Size of object */
} Elf64_Sym;

/* st_info */
#define ELF64_ST_BIND(info)         ((info) >> 4)
#define ELF64_ST_TYPE(info)         ((info) & 0xF)
#define ELF64_ST_INFO(bind, type)   (((bind) << 4) + ((type) & 0xF))

/* symbol bindings (high-order four bits of st_info) */
#define STB_LOCAL       0       /* Not visible outside the object file */
#define STB_GLOBAL      1       /* Global symbol, visible to all object files */
#define STB_WEAK        2       /* Global scope with lower precedence than global symbols */
#define STB_LOOS        10      /* Environment-specific use */
#define STB_HIOS        12
#define STB_LOPROC      13      /* Processor-specific use */
#define STB_HIPROC      15

/* symbol types (low-order four bits of st_info) */
#define STT_NOTYPE      0       /* No type */
#define STT_OBJECT      1       /* Data object */
#define STT_FUNC        2       /* Function entry point */
#define STT_SECTION     3       /* Symbol is associated with a section */
#define STT_FILE        4       /* Source file associated with the object file */
#define STT_LOSS        10      /* Environment-specific use */
#define STT_HIOS        12
#define STT_LOPROC      13      /* Processor-specific use */
#define STT_HIPROC      15

/* 386 relocation types */
#define R_386_NONE      0       /* none */
#define R_386_32        1       /* word32: S+A */
#define R_386_PC32      2       /* word32: S+A-P */
#define R_386_GOT32     3       /* word32: G+A-P */
#define R_386_PLT32     4       /* word32: L+A-P */
#define R_386_COPY      5       /* none */
#define R_386_GLOB_DAT  6       /* word32: S */
#define R_386_JMP_SLOT  7       /* word32: S */
#define R_386_RELATIVE  8       /* word32: B+A */
#define R_386_GOTOFF    9       /* word32: S+A-GOT */
#define R_386_GOTPC     10      /* word32: GOT+A-P */

/* AMD x86-64 relocations.  */
#define R_X86_64_NONE		0	/* No reloc */
#define R_X86_64_64		    1	/* Direct 64 bit  */
#define R_X86_64_PC32		2	/* PC relative 32 bit signed */
#define R_X86_64_GOT32		3	/* 32 bit GOT entry */
#define R_X86_64_PLT32		4	/* 32 bit PLT address */
#define R_X86_64_COPY		5	/* Copy symbol at runtime */
#define R_X86_64_GLOB_DAT	6	/* Create GOT entry */
#define R_X86_64_JUMP_SLOT	7	/* Create PLT entry */
#define R_X86_64_RELATIVE	8	/* Adjust by program base */
#define R_X86_64_GOTPCREL	9	/* 32 bit signed PC relative offset to GOT */
#define R_X86_64_32		    10	/* Direct 32 bit zero extended */
#define R_X86_64_32S		11	/* Direct 32 bit sign extended */
#define R_X86_64_16		    12	/* Direct 16 bit zero extended */
#define R_X86_64_PC16		13	/* 16 bit sign extended pc relative */
#define R_X86_64_8		    14	/* Direct 8 bit sign extended  */
#define R_X86_64_PC8		15	/* 8 bit sign extended pc relative */
#define R_X86_64_DTPMOD64	16	/* ID of module containing symbol */
#define R_X86_64_DTPOFF64	17	/* Offset in module's TLS block */
#define R_X86_64_TPOFF64	18	/* Offset in initial TLS block */
#define R_X86_64_TLSGD		19	/* 32 bit signed PC relative offset to \
                                    two GOT entries for GD symbol */
#define R_X86_64_TLSLD		20	/* 32 bit signed PC relative offset to \
                                    two GOT entries for LD symbol */
#define R_X86_64_DTPOFF32	21	/* Offset in TLS block */
#define R_X86_64_GOTTPOFF	22	/* 32 bit signed PC relative offset to \
                                    GOT entry for IE symbol */
#define R_X86_64_TPOFF32	23	/* Offset in initial TLS block */
#define R_X86_64_PC64		24	/* PC relative 64 bit */
#define R_X86_64_GOTOFF64	25	/* 64 bit offset to GOT */
#define R_X86_64_GOTPC32	26	/* 32 bit signed pc relative offset to GOT */
#define R_X86_64_GOT64		27	/* 64-bit GOT entry offset */
#define R_X86_64_GOTPCREL64	28	/* 64-bit PC relative offset to GOT entry */
#define R_X86_64_GOTPC64	29	/* 64-bit PC relative offset to GOT */
#define R_X86_64_GOTPLT64	30 	/* like GOT64, says PLT entry needed */
#define R_X86_64_PLTOFF64	31	/* 64-bit GOT relative offset to PLT entry */
#define R_X86_64_SIZE32		32	/* Size of symbol plus 32-bit addend */
#define R_X86_64_SIZE64		33	/* Size of symbol plus 64-bit addend */
#define R_X86_64_GOTPC32_TLSDESC 34	/* GOT offset for TLS descriptor.  */
#define R_X86_64_TLSDESC_CALL   35	/* Marker for call through TLS descriptor.  */
#define R_X86_64_TLSDESC        36	/* TLS descriptor.  */
#define R_X86_64_IRELATIVE	37	/* Adjust indirectly by program base */
#define R_X86_64_RELATIVE64	38	/* 64-bit adjust by program base */
					/* 39 Reserved was R_X86_64_PC32_BND */
					/* 40 Reserved was R_X86_64_PLT32_BND */
#define R_X86_64_GOTPCRELX	41	/* Load from 32 bit signed pc relative offset \
                                to GOT entry without REX prefix, relaxable.  */
#define R_X86_64_REX_GOTPCRELX	42	/* Load from 32 bit signed pc relative offset \
                                to GOT entry with REX prefix, relaxable.  */
#define R_X86_64_NUM		43

/* "Rel" relocation */
typedef struct {
    Elf64_Addr      r_offset;   /* Address of reference */
    Elf64_Xword     r_info;     /* Symbol index and type of relocation */
} Elf64_Rel;

/* "Rela" relocation */
typedef struct {
    Elf64_Addr      r_offset;   /* Address of reference */
    Elf64_Xword     r_info;     /* Symbol index and type of relocation */
    Elf64_Sxword    r_addend;   /* Constant part of expression */
} Elf64_Rela;

/* r_info */
#define ELF64_R_SYM(i)      ((i) >> 32)
#define ELF64_R_TYPE(i)     ((i) & 0xFFFFFFFFL)
#define ELF64_R_INFO(s, t)  (((s) << 32) + ((t) & 0xFFFFFFFFL))

typedef struct {
    Elf64_Word      p_type;     /* Type of segment */
    Elf64_Word      p_flags;    /* Segment attributes */
    Elf64_Off       p_offset;   /* Offset in file */
    Elf64_Addr      p_vaddr;    /* Virtual address in memory */
    Elf64_Addr      p_paddr;    /* Reserved */
    Elf64_Xword     p_filesz;   /* Size of segment in file */
    Elf64_Xword     p_memsz;    /* Size of segment in memory */
    Elf64_Xword     p_align;    /* Alignment of segment */
} Elf64_Phdr;

/* p_type */
#define PT_NULL     0           /* Unused entry */
#define PT_LOAD     1           /* Loadable segment */
#define PT_DYNAMIC  2           /* Dynamic linking tables */
#define PT_INTERP   3           /* Program interpreter path name */
#define PT_NOTE     4           /* Note sections */
#define PT_SHLIB    5           /* Reserved */
#define PT_PHDR     6           /* Program header table */
#define PT_LOOS     0x60000000  /* Environment-specific use */
#define PT_HIOS     0x6FFFFFFF
#define PT_LOPROC   0x70000000  /* Processor-specific use */
#define PT_HIPROC   0x7FFFFFFF

/* p_flags */
#define PF_X        0x1         /* Execute permission */
#define PF_W        0x2         /* Write permission */
#define PF_R        0x4         /* Read permission */
#define PF_MASKOS   0x00FF0000  /* Environment-specific use */
#define PF_MASKPROC 0xFF000000  /* Processor-specific use */

/* dynamic */
typedef struct {
    Elf64_Sxword    d_tag;  /* dynamic table entry type */
    union {
        Elf64_Xword d_val;
        Elf64_Addr  d_ptr;
    } d_un;
} Elf64_Dyn;

/* d_tag */
#define DT_NULL     0       /* Mask the end of the dynamic entry */
#define DT_NEEDED   1
#define DT_PLTRELSZ 2
#define DT_PLTGOT   3
#define DT_HASH     4       /* Address of the symbol hash table */
#define DT_STRTAB   5
#define DT_SYMTAB   6
#define DT_RELA     7
#define DT_RELASZ   8
#define DT_RELAENT  9
#define DT_STRSZ    10
#define DT_SYMENT   11
#define DT_INIT     12      /* Address of the initialization function */
#define DT_FINI     13      /* Address of the termination function */
#define DT_SONAME   14
#define DT_RPATH    15
#define DT_SYMBOLIC 16
#define DT_REL      17
#define DT_RELSZ    18
#define DT_RELENT   19
#define DT_PLTREL   20
#define DT_DEBUG    21
#define DT_TEXTREL  22
#define DT_JMPREL   23
#define DT_BIND_NOW 24
#define DT_INIT_ARRAY   25
#define DT_FINI_ARRAY   26
#define DT_INIT_ARRAYSZ 27
#define DT_FINI_ARRAYSZ 28
#define DT_LOOS     0x60000000
#define DT_HIOS     0x6FFFFFFF
#define DT_LOPROC   0x70000000
#define DT_HIPROC   0x7FFFFFFF

#endif  /* ELF_H */