/* ins_def.c: Common definitions for insertions

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

#include "ins_def.h"

#include "../manip.h"

#include <stdlib.h>

// Create a new program-relative reloc at a specific data location,
// pointing to a specific location in the target section. Increase
// NewData to point behind the reloc.
RELOC *CreateProgramRelativeReloc (SECTION *Section, I1 **NewData, SECTION *TargetSection, OFFSET TargetLocation, SIZE Size)
{
	RELOC *Reloc = calloc (1, sizeof (RELOC));
	if (Reloc)
	{
		Reloc->Parent             = Section;
		Reloc->Location           = *NewData - Section->Data;
		*NewData += ((Reloc->Size = Size));
		Reloc->Target.Symbol      = TargetSection->SectionSymbol;
		Reloc->Target.SymbolName  = TargetSection->SectionSymbol->Name;
		Reloc->Target.Offset      = TargetLocation;
		SetRelocProgramRelative (Reloc);
		InsertReloc (Section, Reloc);
	}
	else
		Error (NULL, "Out of memory.");
	
	return Reloc;
}

// Allocate and initialize a LIB_CALL_USER_DATA structure.
BOOLEAN InitializeLibCallUserData (LIB_CALL_USER_DATA *UserData, PROGRAM *Program)
{
	// Allocate space for dynamic user data.
	if (!(UserData->Libs = calloc ((UserData->LibCount = Program->Libraries.ReferencedCount), sizeof (LIB_DATA))))
	{
		Error (NULL, "Out of memory while inserting libraries.");
		return FALSE;
	}
	
	{
		LIBRARY *CurLibrary;
		LIB_DATA *CurLib = UserData->Libs;
		
		// Query info about the libraries.
		for_each (CurLibrary, Program->Libraries)
		{
			if (CurLibrary->Referenced)
			{
				// Set the library pointer.
				CurLib->Lib = CurLibrary;
				// Allocate space for the imports.
				if (!(CurLib->Functions = calloc (CurLibrary->Highest + 1, sizeof (LIB_FUNCTION_DATA))))
				{
					Error (NULL, "Out of memory while inserting libraries.");
					return FALSE;
				}
				CurLib++;
			}
		}
	}
	
	return TRUE;
}

// Finalize and deallocate a LIB_CALL_USER_DATA structure.
void FinalizeLibCallUserData (LIB_CALL_USER_DATA *UserData)
{
	LIB_DATA *CurLibPtr = UserData->Libs;
	OFFSET CurLib;
	
	for (CurLib = 0; CurLib < UserData->LibCount; CurLib++, CurLibPtr++)
	{
		LIB_FUNCTION_DATA *Functions = CurLibPtr->Functions;
		if (Functions)
			free (Functions);
	}
	
	free (UserData->Libs);
}
