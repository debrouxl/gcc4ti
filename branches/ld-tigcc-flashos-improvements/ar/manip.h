/* manip.h: Routines to manipulate the internal data

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

#ifndef MANIP_H
#define MANIP_H

#include "../generic.h"
#include "data.h"
#include "../formats/ar.h"

// Free the archive tree.
void FreeArchive (ARCHIVE *Archive);

// Create a symbol table for an archive.
BOOLEAN CreateSymbolTable (ARCHIVE *Archive);

// Fill the "export help" fields of the archive.
BOOLEAN FillExportHelpFields (ARCHIVE *Archive);

// Get the offset of the first member of a file.
#define GetArStartOffset() (AR_FILE_HEADER_SIZE)
// Get the size of a member depending on the size of the original file.
#define GetArMemberSize(FileSize) ((AR_MEMBER_HEADER_SIZE + (FileSize) + AR_MEMBER_BOUNDARY - 1) / AR_MEMBER_BOUNDARY * AR_MEMBER_BOUNDARY)

#endif
