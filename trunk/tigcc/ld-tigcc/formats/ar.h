/* ar.h: Definitions for archive files

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

#ifndef AR_H
#define AR_H

#include "../generic.h"
#include "../integers.h"

// *** Constants ***

#define AR_MEMBER_BOUNDARY 2
#define AR_MEMBER_PADDING_CHAR '\012'

// *** File Mapping Definitions ***

// Archive File Header
#define AR_FILE_HEADER_STRING "!<arch>\012"
#define AR_FILE_HEADER_SIZE 8

// Archive Member Header
typedef struct ATTRIBUTE_PACKED {
	char Name[16];  // '/'-terminated file member name.
	char Date[12];  // File time in ASCII-encoded decimal.
	char UID[6];    // User ID in ASCII-encoded decimal.
	char GID[6];    // Group ID in ASCII-encoded decimal.
	char Mode[8];   // File mode in ASCII-encoded octal.
	char Size[10];  // Size in ASCII-encoded decimal.
	char Magic[2];  // Magic string: "`\012".
} AR_MEMBER_HEADER;
#define AR_MEMBER_HEADER_SIZE (sizeof (AR_MEMBER_HEADER))

#define AR_MEMBER_MAX_NAME_LENGTH 15
#define AR_MEMBER_MAGIC "`\012"
#define AR_MEMBER_MAGIC_SIZE 2

// Archive Symbol Table
typedef struct ATTRIBUTE_PACKED {
	TI4 SymbolCount;
	TI4 PSymbols VAR_ARRAY;
} AR_SYMBOL_TABLE_HEADER;

// *** File Type Check ***

#include <string.h>

#define IsArchiveFile(File,FileSize) (((FileSize) > AR_FILE_HEADER_SIZE) && (!(strncmp ((const char *) (File), AR_FILE_HEADER_STRING, AR_FILE_HEADER_SIZE))))

#endif
