/* exp_data.c: Routines to export a data file

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

#include "exp_data.h"

#ifdef DATA_VAR_SUPPORT

// Get the file size needed to export the data file.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
SIZE GetDataFileSize (const PROGRAM *Program)
{
	// Get a pointer to the main section.
	SECTION *DataSection = Program->DataSection;
	
	if (!DataSection)
	{
		Error (NULL, "No data section.");
		return 0;
	}
	
	return DataSection->Size;
}

// Export the internal data structures into a TIOS file.
BOOLEAN ExportDataFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize ATTRIBUTE_UNUSED, ProgramCalcs DestCalc ATTRIBUTE_UNUSED)
{
	// A simple macro to make the code more readable.
#define FailWithError(Err...) ({ Error (Err); return FALSE; })
	
	const char *SectionFileName = NULL;
	OFFSET DataStart = 0;
	
	// Get a pointer to the data section.
	SECTION *DataSection = Program->DataSection;
	
	if (!DataSection)
		FailWithError (NULL, "No data section.");
	
	SectionFileName = DataSection->FileName;
	
	if (!(DataSection->Data))
		FailWithError (SectionFileName, "No section contents.");
	
	// Write out the section contents.
	DataStart = ExportTell (File);
	ExportWrite (File, DataSection->Data, DataSection->Size, 1);
	
	if (!(IsEmpty (DataSection->Relocs)))
	{
		RELOC *Reloc;
		
		for_each (Reloc, DataSection->Relocs)
		{
			// If this can be resolved to a calculator-dependent value, write the
			// value into the section data.
			if (!(EmitCalcBuiltinValue (Reloc, DestCalc, File, FileSize, DataStart)))
				FailWithError (SectionFileName, "Relocs inside data variable not supported.");
		}
	}
	
	if (!(IsEmpty (DataSection->ROMCalls)))
		FailWithError (SectionFileName, "ROM calls inside data variable not supported.");
	
	if (!(IsEmpty (DataSection->RAMCalls)))
		FailWithError (SectionFileName, "RAM calls inside data variable not supported.");
	
	if (!(IsEmpty (DataSection->LibCalls)))
		FailWithError (SectionFileName, "Library calls inside data variable not supported.");
	
	return TRUE;
	
#undef FailWithError
}

#endif /* DATA_VAR_SUPPORT */
