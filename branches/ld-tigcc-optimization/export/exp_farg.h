/* exp_farg.h: Routines to export to a Fargo file

   Copyright (C) 2002-2003 Sebastian Reichelt
   Copyright (C) 2003 Kevin Kofler

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

#ifndef EXP_FARG_H
#define EXP_FARG_H

#include "../generic.h"

#ifdef FARGO_SUPPORT

#include "exp_def.h"
#include "../data.h"

// Get the file size needed to export the program into a Fargo file.
// Returns 0 on failure.
SIZE GetFargoFileSize (const PROGRAM *Program);
// Export the internal data structures into a Fargo file.
BOOLEAN ExportFargoFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc);

#endif /* FARGO_SUPPORT */

#endif
