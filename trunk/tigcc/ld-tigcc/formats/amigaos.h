/* amigaos.h: Definitions for AmigaOS object files

   Copyright (C) 2002-2003 Kevin Kofler

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

#ifndef AMIGAOS_H
#define AMIGAOS_H

#include "../generic.h"
#include "../integers.h"

// *** Constants ***

// Possible Hunk Flags
#define AMIGAOS_HUNK_FLAG_ADVISORY 0x20000000L
#define AMIGAOS_HUNK_FLAG_CHIP     0x40000000L
#define AMIGAOS_HUNK_FLAG_FAST     0x80000000L

#define AMIGAOS_HUNK_TYPE_MASK 0x3FFFFFFFL

// Possible Hunk Types
#define AMIGAOS_HUNK_UNIT             0x3E7
#define AMIGAOS_HUNK_NAME             0x3E8
#define AMIGAOS_HUNK_CODE             0x3E9
#define AMIGAOS_HUNK_DATA             0x3EA
#define AMIGAOS_HUNK_BSS              0x3EB
#define AMIGAOS_HUNK_RELOC_ABS4       0x3EC
#define AMIGAOS_HUNK_RELOC_REL2       0x3ED
#define AMIGAOS_HUNK_RELOC_REL1       0x3EE
#define AMIGAOS_HUNK_EXT              0x3EF
#define AMIGAOS_HUNK_SYMBOL           0x3F0
#define AMIGAOS_HUNK_DEBUG            0x3F1
#define AMIGAOS_HUNK_END              0x3F2
#define AMIGAOS_HUNK_HEADER           0x3F3
#define AMIGAOS_HUNK_OVERLAY          0x3F5
#define AMIGAOS_HUNK_BREAK            0x3F6
#define AMIGAOS_HUNK_DREL4            0x3F7
#define AMIGAOS_HUNK_DREL2            0x3F8
#define AMIGAOS_HUNK_DREL1            0x3F9
#define AMIGAOS_HUNK_LIB              0x3FA
#define AMIGAOS_HUNK_INDEX            0x3FB
#define AMIGAOS_HUNK_RELOC_ABS4_SHORT 0x3FC
#define AMIGAOS_HUNK_RELOC_REL4       0x3FD
#define AMIGAOS_HUNK_RELOC_ABS2       0x3FE

#ifdef AMIGAOS_TIGCC_EXTENSIONS
#define AMIGAOS_HUNK_RELOC_ABS4_POSNEG 0x716CC0L
#define AMIGAOS_HUNK_RELOC_ABS2_POSNEG 0x716CC1L
#define AMIGAOS_HUNK_RELOC_ABS1_POSNEG 0x716CC2L
#endif

// Possible Symbol Types in AMIGAOS_HUNK_EXT
// Definitions:
#define AMIGAOS_EXT_SYMBOL     0x00
#define AMIGAOS_EXT_DEF        0x01
#define AMIGAOS_EXT_ABS        0x02
#define AMIGAOS_EXT_RES        0x03
#define AMIGAOS_EXT_COMMON_DEF 0x04
// References:
#define AMIGAOS_EXT_REF_ABS4   0x81
#define AMIGAOS_EXT_COMMON     0x82
#define AMIGAOS_EXT_REF_REL2   0x83
#define AMIGAOS_EXT_REF_REL1   0x84
#define AMIGAOS_EXT_DEXT4      0x85
#define AMIGAOS_EXT_DEXT2      0x86
#define AMIGAOS_EXT_DEXT1      0x87
#define AMIGAOS_EXT_REF_REL4   0x88
#define AMIGAOS_EXT_REL_COMMON 0x89
#define AMIGAOS_EXT_REF_ABS2   0x8A
#define AMIGAOS_EXT_REF_ABS1   0x8B

// AmigaOS Reloc Flags (TIGCC extension)
#ifdef AMIGAOS_TIGCC_EXTENSIONS
#define AMIGAOS_RELOC_UNOPTIMIZABLE 0x80000000L
#endif

// *** File Type Check ***

// Check whether a file has the AmigaOS-hunks format.
#define IsAmigaOSFile(File,FileSize) (((FileSize) >= 12) && (ReadTI4 (*((TI4 *) (File))) == AMIGAOS_HUNK_UNIT) && (ReadTI4 (*((TI4 *) ((File) + (FileSize) - 4))) == AMIGAOS_HUNK_END))

#endif
