/* export.h: Routines for file exports

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

#ifndef EXPORT_H
#define EXPORT_H

#include "../generic.h"
#include "exp_def.h"
#include "../intrface.h"
#include "../data.h"

// Export the internal data structures to external files, creating as many
// files as needed.
BOOLEAN ExportProgram (const PROGRAM *Program, OUTPUT_FILE_FUNCTION GetOutputFile, OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile);

// Non-embedded file output functions as defined in intrface.h.
#ifndef TARGET_EMBEDDED
#define MAX_NAME_LEN 8
extern const char *DestFile;
extern SIZE DestFileSize;
extern char ProgramFolder[MAX_NAME_LEN+1], ProgramName[MAX_NAME_LEN+1];
#ifdef DATA_VAR_SUPPORT
extern char DataFolder[MAX_NAME_LEN+1], DataName[MAX_NAME_LEN+1];
#endif /* DATA_VAR_SUPPORT */
extern BOOLEAN OutputBin;
extern BOOLEAN OutputBinMainOnly;
BOOLEAN GetOutputFile (INT_EXP_FILE *File, SIZE FileSize, unsigned int DestCalc, unsigned int FileRole, unsigned int FileFormat, unsigned int FileType, const char *Extension, BOOLEAN Executable, I4 *EffectiveSize);
void FinalizeOutputFile (INT_EXP_FILE *File);
#endif /* !TARGET_EMBEDDED */

#endif
