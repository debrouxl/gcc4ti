/* exp_dbg.c: Routines to export debugging information

   Copyright (C) 2002-2004 Sebastian Reichelt
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

#ifdef DEBUGGING_INFO_SUPPORT

/* The format exported by these routines is a COFF file containing both the
   actual program and the debugging information (which is stripped out of the
   on-calc executable). They are actually object files, but with a ".dbg"
   extension to distinguish them from ordinary object files. These files can be
   read in by GDB as integrated into TiEmu.

   Note that this means that this has to be plain vanilla COFF, NO TIGCC
   extensions such as unoptimizable relocs are allowed, unless you are willing
   to implement these in GDB/BFD as well. And I really mean IMPLEMENT, not
   ignore, cluttering this file with information not used by GDB is useless. As
   this format isn't used for linking anyway, the restrictions on the format are
   not a problem, and it is pointless to add things not used by GDB. Using
   debugging information files for linking purposes or any other non-debugging
   purpose is NOT supported. This is NOT a "relocatable linking" feature.

   Implementing actual relocatable linking (linking multiple object files into a
   single object file) starting from this code is probably possible, but this
   code needs to be separate, as relocatable linking SHOULD use TIGCC extensions
   in order not to lose information. You'll also have to special-case the
   relocatable linking all over the place for things like global imports,
   startup sections, merging sections and so on.

   -- Kevin Kofler */

#include "exp_dbg.h"
#include "../formats/coff.h"
#include <string.h>

static SIZE CountSectionCOFFSize (const SECTION *Section)
{
	SIZE Size = sizeof(COFF_SECTION);
	const SYMBOL *Symbol;
	const RELOC *Reloc;

	if (!Section)
		return 0;

	Size += Section->Size;

	for_each (Symbol, Section->Symbols)
	{
		COUNT NameLen = strlen (Symbol->Name);
		// Symbol table entry
		Size += sizeof (COFF_SYMBOL);
		// String table entry
		if (NameLen > 8)
			Size += NameLen + 1;
	}

	for_each (Reloc, Section->Relocs)
	{
		// Ignore calc builtins.
		if (IsCalcBuiltinLocation (&Reloc->Location))
			continue;

		// Reloc table entry
		Size += sizeof (COFF_RELOC);
	}

	return Size;
}

// Get the file size needed to export the debugging information file.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
static SIZE GetDebuggingInfoFileSize (const PROGRAM *Program)
{
	SIZE Size = sizeof(COFF_HEADER);

	Size += CountSectionCOFFSize (Program->MainSection);
	Size += CountSectionCOFFSize (Program->DataSection);
	Size += CountSectionCOFFSize (Program->BSSSection);
	Size += CountSectionCOFFSize (Program->DebuggingInfoSection[DI_STAB-1]);
	Size += CountSectionCOFFSize (Program->DebuggingInfoSection[DI_STABSTR-1]);

	return Size;
}

// Export the internal data structures into a TIOS file.
static BOOLEAN ExportDebuggingInfoFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize ATTRIBUTE_UNUSED)
{
	// A simple macro to make the code more readable.
#define FailWithError(Err...) ({ Error (Err); return FALSE; })

	// STUB
	return FALSE;

#undef FailWithError
}

// Export the debugging information to a .dbg file.
// This function is a modified version of ExportProgramToFormat.
BOOLEAN ExportDebuggingInfo (const PROGRAM *Program, OUTPUT_FILE_FUNCTION GetOutputFile, OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile)
{
	BOOLEAN Success;
	EXP_FILE File;
	SIZE Size;
	I4 EffectiveSize = 0;
	
	// Fill the output file struct with zeroes.
	memset (&File, 0, sizeof (File));
	
	// Get the file size needed to export the file.
	Size = GetDebuggingInfoFileSize (Program);
	
	// A size of 0 indicates an error.
	if (Size <= 0)
		return FALSE;
	
	// Create an output file with the specified format.
	if (!(GetOutputFile (&(File.File), Size, 0, FR_DEBUGGING_INFO, FF_GDB_COFF, 0, NULL, FALSE, &EffectiveSize)))
		return FALSE;
	
	// Export the program into the output file.
	Success = ExportDebuggingInfoFile (Program, &File, Size);
	
	// Finalize and close the output file.
	if (FinalizeOutputFile)
		FinalizeOutputFile (&(File.File));
	
	return Success;
}

#endif /* DEBUGGING_INFO_SUPPORT */
