/* exp_farg.c: Routines to export to a Fargo file

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

#include "exp_farg.h"

#ifdef FARGO_SUPPORT

#include "../manip.h"

// Fargo TI-BASIC fork
static const I1 FARGO_TIBASIC_FORK[] = {
	0xE9,            // ENDSTACK_TAG
	0x12, 0xE4,      // 'EndPrgm'
	0x00, 0xE8,      // Newline
	0x19, 0xE4,      // 'Prgm'
	0xE5,            // No arguments, END_TAG only
	0x00, 0x00, 0x40 // Flags
};

// Get the file size needed to export the program into a Fargo file.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
SIZE GetFargoFileSize (const PROGRAM *Program)
{
	// Get a pointer to the main section.
	SECTION *MainSection = Program->MainSection;
	
	// There must be exactly one section.
	if (!MainSection)
	{
		Error (NULL, "No main section.");
		return 0;
	}
	
	// The program consists of a TI-Basic fork and the section.
	return (MainSection->Size + sizeof (FARGO_TIBASIC_FORK));
}

// Export the internal data structures into a Fargo file.
BOOLEAN ExportFargoFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize ATTRIBUTE_UNUSED, ProgramCalcs DestCalc ATTRIBUTE_UNUSED)
{
	// A simple macro to make the code more readable.
#define FailWithError(Err...) ({ Error (Err); return FALSE; })
	
	const char *SectionFileName = NULL;
	OFFSET DataStart = 0;
	
	// Get a pointer to the first section.
	SECTION *MainSection = Program->MainSection;
	
	if (!MainSection)
		FailWithError (NULL, "No main section.");
	
	// Get the section file name for error messages.
	SectionFileName = MainSection->FileName;
	
	// Startup code should always be marked as such. When merging
	// sections, the destination startup number is kept, so in the
	// end, the complete code is marked as a startup section (which
	// makes sense; it is startable).
	if (!(MainSection->StartupNumber))
		FailWithError (SectionFileName, "No startup section(s) defined.");
	
	if (!(MainSection->Data))
		FailWithError (SectionFileName, "No section contents.");
	
	// Write out the section contents first.
	DataStart = ExportTell (File);
	ExportWrite (File, MainSection->Data, MainSection->Size, 1);
	
	// Write out the TI-BASIC fork.
	ExportWrite (File, FARGO_TIBASIC_FORK, sizeof (FARGO_TIBASIC_FORK), 1);

	if (!(IsEmpty (MainSection->Relocs)))
	{
		RELOC *Reloc;
		
		for_each (Reloc, MainSection->Relocs)
		{
			// If this can be resolved to a calculator-dependent value, write the
			// value into the section data.
			if (!(EmitCalcBuiltinValue (Reloc, DestCalc, File, FileSize, DataStart)))
				FailWithError (SectionFileName, "Fargo does not support AMS relocs.");
		}
	}
	
	if ((!(IsEmpty (MainSection->ROMCalls))) && (!(MainSection->ROMCalls.Handled)))
		FailWithError (SectionFileName, "Fargo does not support ROM calls. Use `tios__0000' and so on.");
	
	if ((!(IsEmpty (MainSection->RAMCalls))) && (!(MainSection->RAMCalls.Handled)))
		FailWithError (SectionFileName, "Fargo does not support RAM calls. Use `kernel__0000' and so on.");
	
	if ((!(IsEmpty (MainSection->LibCalls))) && (!(MainSection->LibCalls.Handled)))
		FailWithError (SectionFileName, "Library calls are not supported in this mode.");
	
	return TRUE;
	
#undef FailWithError
}

#endif /* FARGO_SUPPORT */
