/* coff.h: Definitions for COFF object files

   Copyright (C) 2002-2003 Sebastian Reichelt
   Copyright (C) 2005 Kevin Kofler

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

#ifndef COFF_H
#define COFF_H

#include "../generic.h"
#include "../integers.h"

// *** File Mapping Definitions ***

#define COFF_MACHINE_M68K 0x150
#define COFF_FLAG_32BIT_BE 0x0200
#define COFF_FLAG_NO_LINE_NUMBERS 0x0004

// File Header
typedef struct ATTRIBUTE_PACKED {
	TI2 Machine;
	TI2 SectionCount;
	TI4 TimeStamp;
	TI4 PSymbols;
	TI4 SymbolCount;
	TI2 OptHdrSize;
	TI2 Flags;
} COFF_HEADER;

#define COFF_SECTION_NAME_LEN 8

// Section Header
typedef struct ATTRIBUTE_PACKED {
	char Name[COFF_SECTION_NAME_LEN];
	TI4 PhysicalAddress;
	TI4 VirtualAddress;
	TI4 Size;
	TI4 PData;
	TI4 PRelocs;
	TI4 PLines;
	TI2 RelocCount;
	TI2 LineCount;
	TI4 Flags;
} COFF_SECTION;

typedef COFF_SECTION COFF_SECTIONS[];

// Section Flags
#define COFF_SECTION_TEXT 0x20
#define COFF_SECTION_DATA 0x40
#define COFF_SECTION_BSS  0x80

#ifdef COFF_TIGCC_EXTENSIONS
#define COFF_SECTION_MERGEABLE 0x1000000
#define COFF_SECTION_UNALIGNED 0x2000000
#endif

// Symbol Name
// (either directly or in string table)
typedef union ATTRIBUTE_PACKED {
	struct ATTRIBUTE_PACKED {
		ZI4 Zero;          // Check this with IsZero.
		TI4 StringOffset;  // If Zero is really zero, this is an offset into the string table.
	} StringRef;
	char Name[8];        // Otherwise, this is the name of the symbol.
} SYM_NAME;

// Symbol Entry
typedef struct ATTRIBUTE_PACKED {
	SYM_NAME Name;
	TI4 Value;
	TI2 Section;
	TI2 Type;
	TI1 Class;
	TI1 AuxSymbolCount;
} COFF_SYMBOL;

typedef COFF_SYMBOL COFF_SYMBOLS[];

#define COFF_SYMBOL_EXTERNAL 0x02
#define COFF_SYMBOL_LABEL 0x06

// Reloc Entry
typedef struct ATTRIBUTE_PACKED {
	TI4 Location;
	TI4 Symbol;
	TI2 Type;
} COFF_RELOC;

typedef COFF_RELOC COFF_RELOCS[];

#define COFF_RELOC_DIR2 0x01
#define COFF_RELOC_DIR4 0x06
#define COFF_RELOC_ABS1 0x0F
#define COFF_RELOC_ABS2 0x10
#define COFF_RELOC_ABS4 0x11
#define COFF_RELOC_REL1 0x12
#define COFF_RELOC_REL2 0x13
#define COFF_RELOC_REL4 0x14
#define COFF_RELOC_ABS4_NEG 0x45

#ifdef COFF_TIGCC_EXTENSIONS
#define COFF_RELOC_ABS2_NEG 0x7161
#define COFF_RELOC_ABS1_NEG 0x7162
#define COFF_RELOC_UNOPTIMIZABLE 0x8000
#endif


// Line Number Entry
typedef struct ATTRIBUTE_PACKED {
	TI4 LineAddress;
	TI2 LineNumber;
} COFF_LINE_NUM;

typedef COFF_LINE_NUM COFF_LINE_NUMS[];

// *** Helping Definitions ***

typedef struct {
	FILE_PTR PSections;
	COUNT SectionCount;
	FILE_PTR PSymbols;
	COUNT SymbolCount;
	FILE_PTR PStrings;
} COFF_INFO;

#define CreateCoffInfo(Header,Info) \
({ \
	(Info).PSections = sizeof (Header) + ReadTI2 ((Header).OptHdrSize); \
	(Info).SectionCount = ReadTI2 ((Header).SectionCount); \
	(Info).PSymbols = ReadTI4 ((Header).PSymbols); \
	(Info).SymbolCount = ReadTI4 ((Header).SymbolCount); \
	(Info).PStrings = (Info).PSymbols + (Info).SymbolCount * sizeof (COFF_SYMBOL); \
})

// *** File Type Check ***

// Check whether a file has the COFF format.
#define IsCOFFFile(File,FileSize) (((FileSize) >= ((SIZE) (sizeof (COFF_HEADER)))) && ((File) [0] == 0x01) && ((File) [1] == 0x50))

#endif
