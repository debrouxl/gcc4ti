/* exp_data.h: Routines to export a data file

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

#ifndef EXP_DATA_H
#define EXP_DATA_H

#include "../generic.h"

#ifdef DATA_VAR_SUPPORT

#include "exp_def.h"
#include "../data.h"

// Get the file size needed to export the data file.
// Returns 0 on failure (e.g. too large).
SIZE GetDataFileSize (const PROGRAM *Program);
// Export the data file.
BOOLEAN ExportDataFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc);

#endif /* DATA_VAR_SUPPORT */

#endif
