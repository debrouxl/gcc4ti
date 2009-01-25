/* data.h: Definitions for internal data handling

   Copyright (C) 2003 Sebastian Reichelt

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

#ifndef DATA_H
#define DATA_H

#include "../generic.h"
#include "../lists.h"
#include "../filestat.h"

#define MAX_SYM_LEN 255

struct ARCHIVE;
struct OBJECT_FILE;
struct SYMBOL;

struct SYMBOL_TABLE;

// Complete Archive
typedef struct ARCHIVE {
	struct {
		LIST_HEADER(struct OBJECT_FILE);
	} ObjectFiles;        // Object files contained in the archive.
	FILE_STATS FileStats; // Statistics of the file.
	COUNT SymbolCount;    // Number of symbols in the archive.
	// Export help
	SIZE ArFileSize;      // Size of the entire archive.
	struct SYMBOL_TABLE *SymbolTable; // Symbol table information.
} ARCHIVE;

// Object File
typedef struct OBJECT_FILE {
	LIST_ITEM_HEADER(struct OBJECT_FILE);
	ARCHIVE *Parent;
	I1 *Data;             // Pointer to file contents (needs to be freed at the end).
	SIZE Size;            // Size of file.
	struct {
		LIST_HEADER(struct SYMBOL);
	} Symbols;            // Exported symbols.
	const char *FileName; // File name.
	FILE_STATS FileStats; // Statistics of the file.
	// Export help
	SIZE ArMemberSize;    // Size of the archive member, including padding.
	OFFSET ArMemberOffset; // Offset of the member inside the archive file.
} OBJECT_FILE;

// Exported Symbol in an Object File
typedef struct SYMBOL {
	LIST_ITEM_HEADER(struct SYMBOL);
	OBJECT_FILE *Parent;
	char Name[MAX_SYM_LEN+1]; // Symbol name.
	SIZE NameLength;      // Length of the symbol name.
} SYMBOL;

// Archive Symbol Table in Internal Representation
typedef struct SYMBOL_TABLE {
	ARCHIVE *Parent;
	SIZE Size;            // Size of the symbol table when it is written to the archive.
	// Export help
	SIZE ArMemberSize;    // Size of the archive member, including padding.
	OFFSET ArMemberOffset; // Offset of the member inside the archive file.
	// Symbol array
	COUNT SymbolCount;    // Number of symbols.
	SYMBOL *Symbols VAR_ARRAY; // An array pointing to the symbols in this table.
} SYMBOL_TABLE;

#endif
