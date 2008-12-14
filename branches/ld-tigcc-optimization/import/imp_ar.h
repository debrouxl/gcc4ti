/* imp_ar.h: Routines to import an archive file

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

#ifndef IMP_AR_H
#define IMP_AR_H

#include "../generic.h"
#include "../data.h"

// Add an archive to the program's list of available archives.
// Do not free the data related to the File parameter after this.
BOOLEAN AddArchiveFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName);

// Import an object file inside an archive file into the internal data
// structures.
BOOLEAN ImportArchiveObject (PROGRAM *Program, ARCHIVE_OBJECT *Object);

// Import an exported symbol of an archive file into the internal data
// structures.
#define ImportArchiveSymbol(Program,Symbol) (ImportArchiveObject ((Program), (Symbol)->ObjectFile))

#endif
