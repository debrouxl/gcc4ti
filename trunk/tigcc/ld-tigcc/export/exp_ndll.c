/* exp_ndll.c: Routines to export to a Nostub DLL file

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

#include "exp_ndll.h"

#ifdef NOSTUB_DLL_SUPPORT

#include "../manip.h"
#include "../special.h"

// Get the file size needed to export the program into a Nostub DLL file.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
SIZE GetNostubDLLFileSize (const PROGRAM *Program)
{
	// Get a pointer to the main section.
	SECTION *MainSection = Program->MainSection;
	
	if (!MainSection)
	{
		Error (NULL, "No main section.");
		return 0;
	}
	
	{
		// Start with two size bytes and the size of the section.
		// Add 2 for the two null bytes at the beginning of the reloc table.
		SIZE Size = 2 + MainSection->Size + 2;
		
		if (!(IsEmpty (MainSection->Relocs)))
		{
			RELOC *Reloc;
			
			// Add the size needed for the relocs.
			for_each (Reloc, MainSection->Relocs)
			{
				// If this can be resolved to a calculator-dependent value, ignore the
				// reloc for now.
				if (IsPlainCalcBuiltin (Reloc))
					continue;
				
				// We can only emit 4-byte absolute relocs.
				if (Reloc->Relative || (Reloc->Size != 4))
				{
					Error (GetFileName (MainSection, Reloc->Location), "Cannot emit %ld byte %s reloc to `%s'.", (long) Reloc->Size, Reloc->Relative ? "relative" : "absolute", Reloc->Target.SymbolName);
					Size = 0;
				}
				
				if (Size)
					// A reloc takes 4 bytes: 2 bytes for the location and 2 bytes for the target offset.
					Size += 4;
			}
		}
		
		// Add 1 for the embedded tag byte.
		Size ++;
		
		return Size;
	}
}

// Export the internal data structures into a Nostub DLL file.
BOOLEAN ExportNostubDLLFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize, ProgramCalcs DestCalc ATTRIBUTE_UNUSED)
{
	// A simple macro to make the code more readable.
#define FailWithError(Err...) ({ Error (Err); return FALSE; })
	
	const char *SectionFileName = NULL;
	OFFSET DataStart = 0;
	
	COUNT EmittedRelocCount = 0;
	
	// Get a pointer to the main section.
	SECTION *MainSection = Program->MainSection;
	
	if (!MainSection)
		FailWithError (NULL, "No main section.");
	
	// Get the section file name for error messages.
	SectionFileName = MainSection->FileName;
	
	if (!(MainSection->Data))
		FailWithError (SectionFileName, "No section contents.");
	
	// Write out the embedded size.
	ExportWriteTI2 (File, FileSize - 2);
	
	// Write out the section contents first.
	DataStart = ExportTell (File);
	ExportWrite (File, MainSection->Data, MainSection->Size, 1);
	
	// Write out two zero bytes as a separator.
	ExportWriteTI2 (File, 0);
	
	if (!(IsEmpty (MainSection->Relocs)))
	{
		RELOC *Reloc;
		
		// Write out the relocation table.
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
			
			if (((I2) Reloc->Location) != Reloc->Location)
				FailWithError (CurFileName, "Cannot emit reloc outside of limited program range to `%s'.", Reloc->Target.SymbolName);
			
			{
				OFFSET TargetLocation = GetLocationOffset (MainSection, &(Reloc->Target)) + Reloc->FixedOffset;
				
				if (((I2) TargetLocation) != TargetLocation)
					FailWithError (CurFileName, "Cannot emit reloc to `%s' (Offset %ld; Location 0x%lX) outside of limited program range.", Reloc->Target.SymbolName, (long) (Reloc->Target.Offset + Reloc->FixedOffset), (long) (TargetLocation));
				
				// Everything seems to be correct.
				ExportWriteTI2 (File, TargetLocation);
				ExportWriteTI2 (File, Reloc->Location);
				
				// Increase the statistics.
				EmittedRelocCount++;
			}
		}
	}
	
	// Write out the embedded tag byte (ASM_TAG).
	ExportWriteTI1 (File, 0xF3);
	
	if ((!(IsEmpty (MainSection->ROMCalls))) && (!(MainSection->ROMCalls.Handled)))
		FailWithError (SectionFileName, "ROM calls are not supported in this mode.");
	
	if ((!(IsEmpty (MainSection->RAMCalls))) && (!(MainSection->RAMCalls.Handled)))
		FailWithError (SectionFileName, "RAM calls are not supported in this mode.");
	
	if ((!(IsEmpty (MainSection->LibCalls))) && (!(MainSection->LibCalls.Handled)))
		FailWithError (SectionFileName, "Library calls are not supported in this mode.");
	
	if (Program->OptimizeInfo->NativeRelocCount < EmittedRelocCount)
		Program->OptimizeInfo->NativeRelocCount = EmittedRelocCount;
	
	return TRUE;
	
#undef FailWithError
}

#endif /* NOSTUB_DLL_SUPPORT */
