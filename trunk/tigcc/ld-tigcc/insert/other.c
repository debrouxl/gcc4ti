/* other.c: Routines to handle automatic insertion of section contents

   Copyright (C) 2003 Sebastian Reichelt, Kevin Kofler

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

#include "other.h"

#include "../integers.h"
#include "../manip.h"
#include "../special.h"

#include <stdlib.h>
#include <string.h>

// Insert the data required for the _nostub comment format.
BOOLEAN InsertNostubComments (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	
	// Export number.
	COUNT ExportCount = 0;
	
	// Highest export ID.
	COUNT HighestExport = -1;

	// Loop variables.
	SECTION *CurSection;
	SYMBOL *CurSymbol;
	
	// Get the number and the highest ID of _nostub comments.
	for_each (CurSection, Program->Sections)
	{
		for_each (CurSymbol, CurSection->Symbols)
		{
			if (CurSymbol->Exported && (!(strncmp (CurSymbol->Name, SYMPF_NOSTUB_DATA, sizeof (SYMPF_NOSTUB_DATA) - 1))))
			{
				OFFSET ExportNumber = GetExportNumber (CurSymbol->Name + (sizeof (SYMPF_NOSTUB_DATA_START) - 1));
				if (ExportNumber >= 0)
				{
					ExportCount++;
					if (ExportNumber > HighestExport)
						HighestExport = ExportNumber;
				}
			}
		}
	}
	
	// Do not output anything if no comments are specified.
	if (ExportCount)
	{
		// Allocate space for the comments.
		I1 *NewData = AllocateSpaceInSection (Section, 4 * ExportCount);
		
		// Apply the format documented in _nostub_comment_header.s.
		if (NewData)
		{
			COUNT CurrentExport;
			
			// We need this outer loop because the exports must be written out
			// in order (i.e. 0, 1, 6, 32768; not 1, 32768, 6, 0).
			for (CurrentExport = 0; CurrentExport <= HighestExport; CurrentExport++)
			{
				// For each section...
				for_each (CurSection, Program->Sections)
				{
					// For each symbol...
					for_each (CurSymbol, CurSection->Symbols)
					{
						// Check whether it belongs to the _nostub comment export we are looking for.
						if (CurSymbol->Exported && (!(strncmp (CurSymbol->Name, SYMPF_NOSTUB_DATA, sizeof (SYMPF_NOSTUB_DATA) - 1))))
						{
							OFFSET ExportNumber = GetExportNumber (CurSymbol->Name + (sizeof (SYMPF_NOSTUB_DATA_START) - 1));
							if (ExportNumber == CurrentExport)
							{
								// Insert the number of the export.
								WriteTI2 (*((TI2 *) NewData), ExportNumber);
								NewData += 2;
								
								{
									// Create a new reloc at the current location,
									// pointing to the exported symbol.
									RELOC *Reloc = calloc (1, sizeof (RELOC));
									if (Reloc)
									{
										Reloc->Parent = Section;
										Reloc->Location = NewData - Section->Data;
										NewData += ((Reloc->Size = 2));
										Reloc->Target.Symbol = CurSymbol;
										Reloc->Target.SymbolName = CurSymbol->Name;
										SetRelocProgramRelative (Reloc);
										InsertReloc (Section, Reloc);
									}
									else
									{
										Error (NULL, "Out of memory while inserting comments.");
										return FALSE;
									}
								}
							}
						}
					}
				}
			}
		}
		else
			return FALSE;
	}
	
	return TRUE;
}

#ifdef DATA_VAR_SUPPORT
// Insert the name of the data variable specified for the program.
BOOLEAN InsertDataVarName (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	
	// Do not output anything if no data variable is specified.
	if (Program->DataVarInfo->Name)
	{
		// Allocate space for the string.
		I1 *NewData = AllocateSpaceInSection (Section, strlen (Program->DataVarInfo->Name) + 1);
		
		// Copy the string into the allocated space.
		if (NewData)
			strcpy (NewData, Program->DataVarInfo->Name);
		else
			return FALSE;
	}
	
	return TRUE;
}
#endif /* DATA_VAR_SUPPORT */
