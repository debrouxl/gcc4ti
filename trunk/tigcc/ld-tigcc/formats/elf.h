/* elf.h: Definitions for m68k-elf object files

   Copyright (C) 2024 Peter Lafreniere <peter@n8pjl.ca>

   Based on GNU C Library header <elf.h> which is:
   Copyright (C) 1995-2024 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#ifndef ELF_H
#define ELF_H

#include "../generic.h"
#include "../integers.h"

#define EI_NIDENT 16

#define ELF_MACH_M68K 4
#define ELF_OBJ_FILE_TYPE 1

// ELF file header
typedef struct ATTRIBUTE_PACKED {
	TI1 Ident[EI_NIDENT];
	TI2 Type;
	TI2 Machine;
	TI4 Version;
	TI4 Entry;
	TI4 ProgramHeadOff;
	TI4 SectionHeadOff;
	TI4 Flags;
	TI2 HeaderSize;
	TI2 ProgramHeadSize;
	TI2 ProgramHeadCount;
	TI2 SectionHeadSize;
	TI2 SectionHeadCount;
	TI2 SectionNameIdx;
} ELF_HEADER;

typedef struct ATTRIBUTE_PACKED {
	TI4 NameIdx;
	TI4 Type;
	TI4 Flags;
	TI4 Address;
	TI4 FileOffset;	
	TI4 Size;
	TI4 Link;
	TI4 Info;
	TI4 Alignment;
	TI4 EntrySize;
} ELF_SECTION;

typedef ELF_SECTION ELF_SECTIONS[];

typedef struct ATTRIBUTE_PACKED {
	TI4 NameIdx;
	TI4 Value;
	TI4 Size;
	TI1 Info;
	TI1 Visibility;
	TI2 SectionIdx;
} ELF_SYMBOL;

typedef ELF_SYMBOL ELF_SYMBOLS[];

typedef struct ATTRIBUTE_PACKED {
	TI4 Offset;
	TI4 Info;
} ELF_RELOC;

typedef ELF_RELOC ELF_RELOCS[];

typedef struct ATTRIBUTE_PACKED {
	TI4 Offset;
	TI4 Info;
	TI4 Addend;
} ELF_RELOCA;

typedef ELF_RELOCA ELF_RELOCAS[];

/*
typedef struct {
  TI4 p_type;	
  TI4 p_offset;
  TI4 p_vaddr;
  TI4 p_paddr;
  TI4 p_filesz;
  TI4 p_memsz;
  TI4 p_flags;
  TI4 p_align;
} ELF_PROGRAM;
*/

/* Fields in the e_ident array.  The EI_* macros are indices into the
   array.  The macros under each EI_* macro are the values the byte
   may have.  */

#define	ELFMAG		0x7f454c46

#define EI_CLASS	4
#define ELFCLASS32	1

#define EI_DATA		5
#define ELFDATA2MSB	2

#define EI_VERSION	6
#define EV_CURRENT	1

/* Legal values for sh_type (section type).  */

#define SHT_NULL	  0		/* Section header table entry unused */
#define SHT_PROGBITS	  1		/* Program data */
#define SHT_SYMTAB	  2		/* Symbol table */
#define SHT_STRTAB	  3		/* String table */
#define SHT_RELA	  4		/* Relocation entries with addends */
#define SHT_HASH	  5		/* Symbol hash table */
#define SHT_DYNAMIC	  6		/* Dynamic linking information */
#define SHT_NOTE	  7		/* Notes */
#define SHT_NOBITS	  8		/* Program space with no data (bss) */
#define SHT_REL		  9		/* Relocation entries, no addends */
#define SHT_SHLIB	  10		/* Reserved */
#define SHT_DYNSYM	  11		/* Dynamic linker symbol table */
#define SHT_INIT_ARRAY	  14		/* Array of constructors */
#define SHT_FINI_ARRAY	  15		/* Array of destructors */
#define SHT_PREINIT_ARRAY 16		/* Array of pre-constructors */
#define SHT_GROUP	  17		/* Section group */
#define SHT_SYMTAB_SHNDX  18		/* Extended section indices */
#define SHT_RELR	  19            /* RELR relative relocations */
#define	SHT_NUM		  20		/* Number of defined types.  */
#define SHT_LOOS	  0x60000000	/* Start OS-specific.  */
#define SHT_GNU_ATTRIBUTES 0x6ffffff5	/* Object attributes.  */
#define SHT_GNU_HASH	  0x6ffffff6	/* GNU-style hash table.  */
#define SHT_GNU_LIBLIST	  0x6ffffff7	/* Prelink library list */
#define SHT_CHECKSUM	  0x6ffffff8	/* Checksum for DSO content.  */
#define SHT_LOSUNW	  0x6ffffffa	/* Sun-specific low bound.  */
#define SHT_SUNW_move	  0x6ffffffa
#define SHT_SUNW_COMDAT   0x6ffffffb
#define SHT_SUNW_syminfo  0x6ffffffc
#define SHT_GNU_verdef	  0x6ffffffd	/* Version definition section.  */
#define SHT_GNU_verneed	  0x6ffffffe	/* Version needs section.  */
#define SHT_GNU_versym	  0x6fffffff	/* Version symbol table.  */
#define SHT_HISUNW	  0x6fffffff	/* Sun-specific high bound.  */
#define SHT_HIOS	  0x6fffffff	/* End OS-specific type */
#define SHT_LOPROC	  0x70000000	/* Start of processor-specific */
#define SHT_HIPROC	  0x7fffffff	/* End of processor-specific */
#define SHT_LOUSER	  0x80000000	/* Start of application-specific */
#define SHT_HIUSER	  0x8fffffff	/* End of application-specific */

/* Legal values for sh_flags (section flags).  */

#define ELF_SECTION_WRITE	     (1 << 0)
#define ELF_SECTION_ALLOC	     (1 << 1)
#define ELF_SECTION_EXECINSTR	     (1 << 2)
#define ELF_SECTION_MERGE	     (1 << 4)
#define ELF_SECTION_STRINGS	     (1 << 5)
#define ELF_SECTION_INFO_LINK	     (1 << 6)
#define ELF_SECTION_LINK_ORDER	     (1 << 7)
#define ELF_SECTION_OS_NONCONFORMING (1 << 8)
#define ELF_SECTION_GROUP	     (1 << 9)
#define ELF_SECTION_TLS		     (1 << 10)
#define ELF_SECTION_COMPRESSED	     (1 << 11)


/* Motorola 68k specific definitions.  */

/* Values for Elf32_Ehdr.e_flags.  */
#define EF_CPU32	0x00810000

/* How to extract and insert information held in the r_info field.  */

#define ELF32_R_SYM(val)		((val) >> 8)
#define ELF32_R_TYPE(val)		((val) & 0xff)
#define ELF32_R_INFO(sym, type)		(((sym) << 8) + ((type) & 0xff))

/* m68k relocs.  */

#define R_68K_NONE	0		/* No reloc */
#define R_68K_32	1		/* Direct 32 bit  */
#define R_68K_16	2		/* Direct 16 bit  */
#define R_68K_8		3		/* Direct 8 bit  */
#define R_68K_PC32	4		/* PC relative 32 bit */
#define R_68K_PC16	5		/* PC relative 16 bit */
#define R_68K_PC8	6		/* PC relative 8 bit */
#define R_68K_GOT32	7		/* 32 bit PC relative GOT entry */
#define R_68K_GOT16	8		/* 16 bit PC relative GOT entry */
#define R_68K_GOT8	9		/* 8 bit PC relative GOT entry */
#define R_68K_GOT32O	10		/* 32 bit GOT offset */
#define R_68K_GOT16O	11		/* 16 bit GOT offset */
#define R_68K_GOT8O	12		/* 8 bit GOT offset */
#define R_68K_PLT32	13		/* 32 bit PC relative PLT address */
#define R_68K_PLT16	14		/* 16 bit PC relative PLT address */
#define R_68K_PLT8	15		/* 8 bit PC relative PLT address */
#define R_68K_PLT32O	16		/* 32 bit PLT offset */
#define R_68K_PLT16O	17		/* 16 bit PLT offset */
#define R_68K_PLT8O	18		/* 8 bit PLT offset */
#define R_68K_COPY	19		/* Copy symbol at runtime */
#define R_68K_GLOB_DAT	20		/* Create GOT entry */
#define R_68K_JMP_SLOT	21		/* Create PLT entry */
#define R_68K_RELATIVE	22		/* Adjust by program base */
#define R_68K_TLS_GD32      25          /* 32 bit GOT offset for GD */
#define R_68K_TLS_GD16      26          /* 16 bit GOT offset for GD */
#define R_68K_TLS_GD8       27          /* 8 bit GOT offset for GD */
#define R_68K_TLS_LDM32     28          /* 32 bit GOT offset for LDM */
#define R_68K_TLS_LDM16     29          /* 16 bit GOT offset for LDM */
#define R_68K_TLS_LDM8      30          /* 8 bit GOT offset for LDM */
#define R_68K_TLS_LDO32     31          /* 32 bit module-relative offset */
#define R_68K_TLS_LDO16     32          /* 16 bit module-relative offset */
#define R_68K_TLS_LDO8      33          /* 8 bit module-relative offset */
#define R_68K_TLS_IE32      34          /* 32 bit GOT offset for IE */
#define R_68K_TLS_IE16      35          /* 16 bit GOT offset for IE */
#define R_68K_TLS_IE8       36          /* 8 bit GOT offset for IE */
#define R_68K_TLS_LE32      37          /* 32 bit offset relative to
					   static TLS block */
#define R_68K_TLS_LE16      38          /* 16 bit offset relative to
					   static TLS block */
#define R_68K_TLS_LE8       39          /* 8 bit offset relative to
					   static TLS block */
#define R_68K_TLS_DTPMOD32  40          /* 32 bit module number */
#define R_68K_TLS_DTPREL32  41          /* 32 bit module-relative offset */
#define R_68K_TLS_TPREL32   42          /* 32 bit TP-relative offset */
/* Keep this the last entry.  */
#define R_68K_NUM	43

#define ELF32_ST_BIND(val)		(((unsigned char) (val)) >> 4)
#define ELF32_ST_TYPE(val)		((val) & 0xf)
#define ELF32_ST_INFO(bind, type)	(((bind) << 4) + ((type) & 0xf))

/* Legal values for ST_BIND subfield of st_info (symbol binding).  */

#define STB_LOCAL	0		/* Local symbol */
#define STB_GLOBAL	1		/* Global symbol */
#define STB_WEAK	2		/* Weak symbol */
#define	STB_NUM		3		/* Number of defined types.  */
#define STB_LOOS	10		/* Start of OS-specific */
#define STB_GNU_UNIQUE	10		/* Unique symbol.  */
#define STB_HIOS	12		/* End of OS-specific */
#define STB_LOPROC	13		/* Start of processor-specific */
#define STB_HIPROC	15		/* End of processor-specific */

/* Legal values for ST_TYPE subfield of st_info (symbol type).  */

#define STT_NOTYPE	0		/* Symbol type is unspecified */
#define STT_OBJECT	1		/* Symbol is a data object */
#define STT_FUNC	2		/* Symbol is a code object */
#define STT_SECTION	3		/* Symbol associated with a section */
#define STT_FILE	4		/* Symbol's name is file name */
#define STT_COMMON	5		/* Symbol is a common data object */
#define STT_TLS		6		/* Symbol is thread-local data object*/
#define	STT_NUM		7		/* Number of defined types.  */
#define STT_LOOS	10		/* Start of OS-specific */
#define STT_GNU_IFUNC	10		/* Symbol is indirect code object */
#define STT_HIOS	12		/* End of OS-specific */
#define STT_LOPROC	13		/* Start of processor-specific */
#define STT_HIPROC	15		/* End of processor-specific */

/* How to extract and insert information held in the st_other field.  */

#define ELF32_ST_VISIBILITY(o)	((o) & 0x03)

/* Symbol visibility specification encoded in the st_other field.  */
#define STV_DEFAULT	0		/* Default symbol visibility rules */
#define STV_INTERNAL	1		/* Processor specific hidden class */
#define STV_HIDDEN	2		/* Sym unavailable in other modules */
#define STV_PROTECTED	3		/* Not preemptible, not exported */

// This ensures that the file is indeed in ELF format.
// IsValidELFObjectFile () ensures that the file is in a _valid_ ELF format.
#define IsELFFile(File, FileSize) (FileSize > (SIZE)sizeof(ELF_HEADER) && ReadTI4(*(TI4 *)File) == ELFMAG)

static inline BOOLEAN IsValidELFObjectFile (const ELF_HEADER *ELFHeader)
{
	return (ReadI1 (ELFHeader->Ident[EI_CLASS]) == ELFCLASS32 &&
		ReadI1 (ELFHeader->Ident[EI_DATA]) == ELFDATA2MSB &&
		ReadI1 (ELFHeader->Ident[EI_VERSION]) == EV_CURRENT &&
		ReadTI2 (ELFHeader->Type) == ELF_OBJ_FILE_TYPE && 
		ReadTI4 (ELFHeader->Version) == EV_CURRENT &&
		ReadTI2 (ELFHeader->Machine) == ELF_MACH_M68K &&
		ReadTI2 (ELFHeader->SectionHeadSize) == sizeof (ELF_SECTION));
}

#endif /* ELF_H */
