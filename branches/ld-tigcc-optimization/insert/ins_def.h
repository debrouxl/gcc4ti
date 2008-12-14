/* ins_def.h: Common definitions for insertions

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

#ifndef INSERT_DEF_H
#define INSERT_DEF_H

#include "../generic.h"
#include "../data.h"
#include "model/list.h"

// Create a new program-relative reloc at a specific data location,
// pointing to a specific location in the target section. Increase
// NewData to point behind the reloc.
RELOC *CreateProgramRelativeReloc (SECTION *Section, I1 **NewData, SECTION *TargetSection, OFFSET TargetLocation, SIZE Size);

// Allocate and initialize a LIB_CALL_USER_DATA structure.
BOOLEAN InitializeLibCallUserData (LIB_CALL_USER_DATA *UserData, PROGRAM *Program);

// Finalize and deallocate a LIB_CALL_USER_DATA structure.
void FinalizeLibCallUserData (LIB_CALL_USER_DATA *UserData);

#endif
