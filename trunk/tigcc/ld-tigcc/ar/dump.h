/* dump.h: Routines to dump the internal data to a file

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

#ifndef DUMP_H
#define DUMP_H

#include "../generic.h"
#include "data.h"

#include <stdio.h>

// Dump everything.
void DumpArchive (FILE *File, const char *Indent, const ARCHIVE *Archive);

// Dump a single object file.
void DumpObjectFile (FILE *File, const char *Indent, const OBJECT_FILE *ObjectFile);

#endif
