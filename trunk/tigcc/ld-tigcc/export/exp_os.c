/* exp_os.c: Routines to export to a Flash OS

   Copyright (C) 2004 Billy Charvet
   Copyright (C) 2004 Kevin Kofler
   Copyright (C) 2004 Sebastian Reichelt

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

#include "exp_os.h"

#ifdef FLASH_OS_SUPPORT

#include "../manip.h"
#include "../special.h"

// Get the file size needed to export the program into a Flash OS.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
SIZE GetFlashOSFileSize (const PROGRAM *Program)
{
	// Get a pointer to the main section.
	SECTION *MainSection = Program->MainSection;
	
	if (!MainSection)
	{
		Error (NULL, "No main section.");
		return 0;
	}
	
	{
		SIZE Size = MainSection->Size;
		if (Size & 0x1)
			Size++;
		return Size;
	}
}

// Export the internal data structures into a Flash OS.
BOOLEAN ExportFlashOSFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc)
{
	// A simple macro to make the code more readable.
#define FailWithError(Err...) ({ Error (Err); return FALSE; })
	
	// Get a pointer to the main section.
	const char *SectionFileName = NULL;
	OFFSET DataStart = 0;
	I4 ROMBase;
	const I1 ZeroByte = 0;
	
	SECTION *MainSection = Program->MainSection;
	
	switch (DestCalc)
	{
		case CALC_TI89:
		case CALC_V200:
			ROMBase = 0x200000;
			break;
		
		case CALC_TI92PLUS:
			ROMBase = 0x400000;
			break;
		
		case CALC_TI89 | CALC_FLAG_TITANIUM:
			ROMBase = 0x800000;
			break;
		
		case CALC_TI92:
			FailWithError (NULL, "Flash upgrades are not available for the TI-92.");
		
		default:
			FailWithError (NULL, "Unknown calculator model.");
	}
	
	if (!MainSection)
		FailWithError (NULL, "No main section.");
	
	SectionFileName = MainSection->FileName;
	
	if (!(MainSection->StartupNumber || (Program->Type == PT_NOSTUB)))
		FailWithError (SectionFileName, "No startup section(s) defined.");
	
	if (!(MainSection->Data))
		FailWithError (SectionFileName, "No section contents.");
	
	// Write out the section contents first.
	DataStart = ExportTell (File);
	ExportWrite (File, MainSection->Data, MainSection->Size, 1);
	// Pad the main section to an even length.
	if (MainSection->Size & 0x1)
		ExportWrite (File, &ZeroByte, 1, 1);
	
	if (!(IsEmpty (MainSection->Relocs)))
	{
		RELOC *Reloc;
		
		// Write out the relocation entries.
		for (Reloc = GetLast (MainSection->Relocs); Reloc; Reloc = GetPrev (Reloc))
		{
			// Get the current file name for error messages.
			const char *CurFileName = GetFileName (MainSection, Reloc->Location);
			
			// If this can be resolved to a calculator-dependent value, write the
			// value into the section data.
			if (EmitCalcBuiltinValue (Reloc, DestCalc, File, FileSize, DataStart))
				continue;
			
			// We can only emit relocs with a target symbol in the same section.
			if (!(Reloc->Target.Symbol))
				FailWithError (CurFileName, "Unresolved reference to `%s'.", Reloc->Target.SymbolName);
			if (Reloc->Target.Symbol->Parent != MainSection)
				FailWithError (CurFileName, "Cannot emit reloc to `%s' in different section.", Reloc->Target.SymbolName);
			
			// We can only emit 4-byte absolute relocs.
			if (Reloc->Relative || (Reloc->Size != 4))
				FailWithError (CurFileName, "Cannot emit %ld byte %s reloc to `%s'.", (long) Reloc->Size, Reloc->Relative ? "relative" : "absolute", Reloc->Target.SymbolName);
			
			{
				OFFSET TargetLocation = GetLocationOffset (MainSection, &(Reloc->Target)) + Reloc->FixedOffset;
				
				TargetLocation += (OFFSET) (ROMBase + 0x12000);
				ExportSeek (File, DataStart + Reloc->Location);
				ExportWriteTI (File, TargetLocation, Reloc->Size, TRUE, TRUE);
				
				// Do not increase the statistics, since that would give a false
				// impression that the relocation entries actually take up some
				// space in the OS.
			}
		}
	}
	
	if ((!(IsEmpty (MainSection->ROMCalls))) && (!(MainSection->ROMCalls.Handled)))
		FailWithError (SectionFileName, "ROM calls are not supported in Flash OSs.");
	
	if ((!(IsEmpty (MainSection->RAMCalls))) && (!(MainSection->RAMCalls.Handled)))
		FailWithError (SectionFileName, "RAM calls are not supported in Flash OSs.");
	
	if ((!(IsEmpty (MainSection->LibCalls))) && (!(MainSection->LibCalls.Handled)))
		FailWithError (SectionFileName, "Library calls are not supported in Flash OSs.");
	
	return TRUE;
	
#undef FailWithError
}

#endif /* FLASH_OS_SUPPORT */
