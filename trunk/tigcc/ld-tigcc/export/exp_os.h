/* exp_os.h: Routines to export to a Flash OS

   Copyright (C) 2004 Billy Charvet

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

#ifndef EXP_FLASH_OS_H
#define EXP_FLASH_OS_H

#include "../generic.h"

#ifdef FLASH_OS_SUPPORT

#include "exp_def.h"
#include "../data.h"

// Get the file size needed to export the program into a Flash OS.
// Returns 0 on failure.
SIZE GetFlashOSFileSize (const PROGRAM *Program);
// Export the internal data structures into a Flash OS.
BOOLEAN ExportFlashOSFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc);

#endif /* FLASH_OS_SUPPORT */

#endif
