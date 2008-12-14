/* dump.h: Routines to dump the internal data to a file

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

#ifndef DUMP_H
#define DUMP_H

#include "generic.h"

#ifdef ENABLE_DUMP

#include "data.h"

#include <stdio.h>

// Dump everything.
void DumpProgram (FILE *File, const char *Indent, const PROGRAM *Program);

// Dump a single section.
void DumpSection (FILE *File, const char *Indent, const SECTION *Section);

// Dump raw data.
void DumpData (FILE *File, const char *Indent, const I1 *Data, OFFSET Start, OFFSET End);

// Dump raw zero/uninitialized data.
void DumpDummyData (FILE *File, const char *Indent, const char *Dummy, OFFSET Start, OFFSET End);

#endif /* ENABLE_DUMP */

#endif
