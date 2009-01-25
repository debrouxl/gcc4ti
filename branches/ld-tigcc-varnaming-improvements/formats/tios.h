/* tios.h: Definitions for TIOS files (including host-wrapped)

   Copyright (C) 2002-2003 Sebastian Reichelt

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

#ifndef TIOS_H
#define TIOS_H

#include "../generic.h"
#include "../integers.h"

// *** Constants ***

#define MAX_TIOS_FILE_SIZE 0xFFEA

#define TIOS_TAG_STR  0x2D
#define TIOS_TAG_PRGM 0xDC
#define TIOS_TAG_ASM  0xF3
#define TIOS_TAG_OTH  0xF8

#define TIOS_LINK_TYPE_STR  0x0C
#define TIOS_LINK_TYPE_PRGM 0x12
#define TIOS_LINK_TYPE_ASM  0x21
#define TIOS_LINK_TYPE_OTH  0x1C

// *** File Mapping Definitions ***

typedef struct ATTRIBUTE_PACKED {
	char Signature[8];   // "**TI92**", "**TI89**", or "**TI92P*".
	I1 Reserved1[2];     // 01 00
	char FolderName[8];  // Folder name.
	char Desc[40];       // Not used.
	I1 Reserved2[6];     // 01 00 52 00 00 00
	char VarName[8];     // Variable name.
	HI1 LinkType;        // Variable link type (see above).
	I1 Reserved3[3];     // 00 00 00
	HI4 FileSize;        // File size from Signature to CheckSum.
	I1 Reserved4[6];     // A5 5A 00 00 00 00
} TIOS_HOST_FILE_HEADER;

typedef struct ATTRIBUTE_PACKED {
	HI2 CheckSum;        // Checksum from DataSize to Tag.
} TIOS_HOST_FILE_FOOTER;

#endif
