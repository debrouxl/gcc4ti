/* exp_dbg.h: Routines to export a debugging information file

   Copyright (C) 2005 Kevin Kofler

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

#ifndef EXP_DBG_H
#define EXP_DBG_H

#include "../generic.h"

#ifdef DEBUGGING_INFO_SUPPORT

#include "exp_def.h"
#include "../data.h"

// Export the debugging information to a .dbg file.
BOOLEAN ExportDebuggingInfo (const PROGRAM *Program, OUTPUT_FILE_FUNCTION GetOutputFile, OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile);

#endif /* DEBUGGING_INFO_SUPPORT */

#endif
