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
#include "../manip.h"
#include "../special.h"
#include <string.h>

#define MAX_NAMED_SECTION (3+DI_LAST)
#define FIRST_SECTION_ID_WITH_ZERO_VADDR 4

typedef struct {
	OFFSET FileOffset;
	OFFSET VirtualOffset;
	EXP_FILE *File;
	COUNT COFFSectionNumber;
} SectionOffsets;

static OFFSET VAddrs[MAX_NAMED_SECTION+1];

static COUNT SectionID;

static const char *SectionFullNames[MAX_NAMED_SECTION] =
                  {".text", ".data", ".bss", ".deleted", ".stab", ".stabstr",
                   ".debug_abbrev", ".debug_aranges",
                   ".debug_frame", ".debug_info", ".debug_line",
                   ".debug_loc", ".debug_macinfo",
                   ".debug_pubnames", ".debug_ranges", ".debug_str",
                   ".eh_frame"};

// These really ought to be written in LISP rather than C, but...
static void ApplyIfNonNull(void (*f)(const SECTION *, void *),
                           const SECTION *Section, void *UserData)
{
	if (Section)
		f (Section, UserData);
	SectionID++;
}

static void MapToAllSections(const PROGRAM *Program, void (*f)(const SECTION *, void *), void *UserData)
{
	COUNT i;
	SectionID = 0;
	ApplyIfNonNull (f, Program->MainSection, UserData);
	ApplyIfNonNull (f, Program->DataSection, UserData);
	ApplyIfNonNull (f, Program->BSSSection, UserData);
	for (i = 0; i < DI_LAST; i++)
		ApplyIfNonNull (f, Program->DebuggingInfoSection[i], UserData);
}

static void CountSectionCOFFSize (const SECTION *Section, void *UserData)
{
	SIZE Size = sizeof(COFF_SECTION);
	const SYMBOL *Symbol;
	const RELOC *Reloc;

	// The accumulated size should always be at least sizeof(COFF_HEADER) + 4.
	// Everything else means there was an error.
	if (!*(SIZE *)UserData)
		return;

	if (Section->Data)
		Size += Section->Size;

	// Section symbol
	{
		COUNT NameLen = strlen (SectionFullNames[SectionID]);
		// Symbol table entry
		Size += sizeof (COFF_SYMBOL);
		// String table entry
		if (NameLen > 8)
			Size += NameLen + 1;
	}

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
		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;
				
		// We can only emit 4-byte absolute relocs.
		if (Reloc->Relative || (Reloc->Size != 4))
		{
			Error (GetFileName (Section, Reloc->Location), "Cannot emit %ld byte %s reloc to `%s'.", (long) Reloc->Size, Reloc->Relative ? "relative" : "absolute", Reloc->Target.SymbolName);
			*(SIZE *)UserData = 0;
			return;
		}

		// Reloc table entry
		Size += sizeof (COFF_RELOC);
	}

	*(SIZE *)UserData += Size;
}

// Get the file size needed to export the debugging information file.
// Returns 0 on failure.
// Call this function once to receive necessary error messages.
static SIZE GetDebuggingInfoFileSize (const PROGRAM *Program)
{
	// The "+ 4" is for the size of the string table.
	SIZE Size = sizeof(COFF_HEADER) + 4;

	MapToAllSections (Program, CountSectionCOFFSize, &Size);

	return Size;
}

static void CompareSections (const SECTION *Section, void *UserData)
{
	if (*(const SECTION **)UserData == Section)
		*(const SECTION **)UserData = NULL;
}

static void AddOne (const SECTION *Section ATTRIBUTE_UNUSED, void *UserData)
{
	(*(COUNT *)UserData)++;
}

static void CountSymbols (const SECTION *Section, void *UserData)
{
	const SYMBOL *Symbol;

	(*(COUNT *)UserData)++; // section symbol

	for_each (Symbol, Section->Symbols)
	{
		(*(COUNT *)UserData)++;
	}
}

static void CountSymbolTableOffset (const SECTION *Section, void *UserData)
{
	SIZE Size = 0;
	const RELOC *Reloc;

	if (Section->Data)
		Size = Section->Size;

	for_each (Reloc, Section->Relocs)
	{
		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;

		// Reloc table entry
		Size += sizeof (COFF_RELOC);
	}

	*(OFFSET *)UserData += Size;
}

static void WriteSectionHeader (const SECTION *Section, void *UserData)
{
	static const char SectionNames[MAX_NAMED_SECTION][COFF_SECTION_NAME_LEN] =
	                  {".text", ".data", ".bss", ".deleted", ".stab",
	                   ".stabstr", ".debug_a"/*bbrev*/, ".debug_a"/*ranges*/,
	                   ".debug_f"/*rame*/, ".debug_i"/*nfo*/, ".debug_l"/*ine*/,
	                   ".debug_l"/*oc*/, ".debug_m"/*acinfo*/,
	                   ".debug_p"/*ubnames*/, ".debug_r"/*anges*/,
	                   ".debug_s"/*tr*/, ".eh_fram"/*e*/};
	static const I4 SectionFlags[MAX_NAMED_SECTION] =
	                {COFF_SECTION_TEXT, COFF_SECTION_DATA, COFF_SECTION_BSS,
	                 COFF_SECTION_TEXT, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	SIZE Size = 0;
	const RELOC *Reloc;
	COUNT RelocCount = 0;
	COFF_SECTION COFFSectionHeader;
	OFFSET VAddr = SectionID < FIRST_SECTION_ID_WITH_ZERO_VADDR ?
	               ((SectionOffsets *)UserData)->VirtualOffset : 0;

	VAddrs[++(((SectionOffsets *)UserData)->COFFSectionNumber)] = VAddr;

	if (Section->Data)
		Size = Section->Size;

	for_each (Reloc, Section->Relocs)
	{
		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;

		// Reloc table entry
		Size += sizeof (COFF_RELOC);
		RelocCount++;
	}

	memcpy (COFFSectionHeader.Name, SectionNames[SectionID], COFF_SECTION_NAME_LEN);
	WriteTI4 (COFFSectionHeader.PhysicalAddress, ((SectionOffsets *)UserData)->VirtualOffset);
	WriteTI4 (COFFSectionHeader.VirtualAddress, VAddr);
	WriteTI4 (COFFSectionHeader.Size, Section->Size);
	if (Section->Data)
	{
		WriteTI4 (COFFSectionHeader.PData, ((SectionOffsets *)UserData)->FileOffset);
		WriteTI4 (COFFSectionHeader.PRelocs, ((SectionOffsets *)UserData)->FileOffset + Section->Size);
	}
	else
	{
		WriteTI4 (COFFSectionHeader.PData, 0);
		WriteTI4 (COFFSectionHeader.PRelocs, ((SectionOffsets *)UserData)->FileOffset);
	}
	WriteTI4 (COFFSectionHeader.PLines, 0);
	WriteTI2 (COFFSectionHeader.RelocCount, RelocCount);
	WriteTI2 (COFFSectionHeader.LineCount, 0);
	WriteTI4 (COFFSectionHeader.Flags, SectionFlags[SectionID]);
	ExportWrite (((SectionOffsets *)UserData)->File, &COFFSectionHeader, sizeof (COFF_SECTION), 1);

	((SectionOffsets *)UserData)->FileOffset += Size;
	((SectionOffsets *)UserData)->VirtualOffset += Section->Size;
}

typedef struct {
	const SECTION *Section;
	COUNT Current;
	COUNT Number;
} COFFSectionNumberStruct;

static void FindCOFFSectionNumber (const SECTION *Section, void *UserData)
{
	((COFFSectionNumberStruct *)UserData)->Current++;
	if (((COFFSectionNumberStruct *)UserData)->Section == Section)
		((COFFSectionNumberStruct *)UserData)->Number = ((COFFSectionNumberStruct *)UserData)->Current;
}

static COUNT GetCOFFSectionNumber (const SECTION *Section)
{
	COFFSectionNumberStruct COFFSectionNumber = {Section, 0, 0};
	MapToAllSections (Section->Parent, FindCOFFSectionNumber, &COFFSectionNumber);
	return COFFSectionNumber.Number;
}

typedef struct {
	const SYMBOL *Symbol;
	COUNT Current;
	COUNT Number;
} COFFSymbolNumberStruct;

static void FindCOFFSymbolNumber (const SECTION *Section, void *UserData)
{
	const SYMBOL *Symbol;

	if (((COFFSymbolNumberStruct *)UserData)->Number) return;

	((COFFSymbolNumberStruct *)UserData)->Current++; // section symbol

	for_each (Symbol, Section->Symbols)
	{
		if (((COFFSymbolNumberStruct *)UserData)->Symbol == Symbol)
		{
			((COFFSymbolNumberStruct *)UserData)->Number = ((COFFSymbolNumberStruct *)UserData)->Current;
			return;
		}
		((COFFSymbolNumberStruct *)UserData)->Current++;
	}
}

static COUNT GetCOFFSymbolNumber (const SYMBOL *Symbol)
{
	COFFSymbolNumberStruct COFFSymbolNumber = {Symbol, 0, 0};
	MapToAllSections (Symbol->Parent->Parent, FindCOFFSymbolNumber, &COFFSymbolNumber);
	return COFFSymbolNumber.Number;
}

static void PatchRelocOffsetsIn (const SECTION *Section, void *UserData ATTRIBUTE_UNUSED)
{
	const RELOC *Reloc;

	for_each (Reloc, Section->Relocs)
	{
		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;

		if (Section->Data)
		{
			const SECTION *RelocSection = Reloc->Target.Symbol->Parent;
			OFFSET RelocVirtualOffset = GetLocationOffset (RelocSection, &(Reloc->Target)) + Reloc->FixedOffset;
			RelocVirtualOffset += VAddrs[GetCOFFSectionNumber (RelocSection)];
			WriteTI4 (*(TI4 *)(Section->Data + Reloc->Location), RelocVirtualOffset);
		}
		else
			Error (GetFileName (Section, Reloc->Location), "Reloc at %lx in BSS section.", (unsigned long) Reloc->Relation);
	}
}

static void WriteSectionContents (const SECTION *Section, void *UserData)
{
	const RELOC *Reloc;
	COUNT COFFSectionNumber = ++(((SectionOffsets *)UserData)->COFFSectionNumber);
	OFFSET VAddr = VAddrs[COFFSectionNumber];

	// Write section data.
	if (Section->Data)
		ExportWrite (((SectionOffsets *)UserData)->File, Section->Data, Section->Size, 1);

	// Write reloc table.
	for_each (Reloc, Section->Relocs)
	{
		COFF_RELOC COFFReloc;

		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;
				
		WriteTI4 (COFFReloc.Location, VAddr + Reloc->Location);
		WriteTI4 (COFFReloc.Symbol, GetCOFFSymbolNumber (Reloc->Target.Symbol));
		WriteTI2 (COFFReloc.Type, COFF_RELOC_ABS4);

		ExportWrite (((SectionOffsets *)UserData)->File, &COFFReloc, sizeof (COFF_RELOC), 1);
	}
}

static void PatchRelocOffsetsOut (const SECTION *Section, void *UserData ATTRIBUTE_UNUSED)
{
	const RELOC *Reloc;

	for_each (Reloc, Section->Relocs)
	{
		// If this can be resolved to a calculator-dependent value, ignore the
		// reloc for now.
		if (IsPlainCalcBuiltin (Reloc))
			continue;

		if (Section->Data)
			WriteTI4 (*(TI4 *)(Section->Data + Reloc->Location), 0);
	}
}

static void WriteSymbolTable (const SECTION *Section, void *UserData)
{
	const SYMBOL *Symbol;
	COUNT COFFSectionNumber = ++(((SectionOffsets *)UserData)->COFFSectionNumber);
	OFFSET VAddr = VAddrs[COFFSectionNumber];

	// Create a new section symbol with exactly the intended name to get around
	// the 8 char section name limit.
	{
		COFF_SYMBOL COFFSymbol;
		const char *SectionName = SectionFullNames[SectionID];
		COUNT NameLen = strlen (SectionName);

		if (NameLen > 8)
		{
			// String table entry
			WriteTI4(*(TI4*)(COFFSymbol.Name.Name), 0);
			WriteTI4(COFFSymbol.Name.StringRef.StringOffset, ((SectionOffsets *)UserData)->FileOffset);
			((SectionOffsets *)UserData)->FileOffset += NameLen + 1;
		}
		else
		{
			memset (COFFSymbol.Name.Name, 0, 8);
			strncpy (COFFSymbol.Name.Name, SectionName, 8);
		}
		WriteTI4 (COFFSymbol.Value, VAddr);
		WriteTI2 (COFFSymbol.Section, COFFSectionNumber);
		WriteTI2 (COFFSymbol.Type, 0);
		WriteTI1 (COFFSymbol.Class, COFF_SYMBOL_LABEL);
		WriteTI1 (COFFSymbol.AuxSymbolCount, 0);

		ExportWrite (((SectionOffsets *)UserData)->File, &COFFSymbol, sizeof (COFF_SYMBOL), 1);
	}

	for_each (Symbol, Section->Symbols)
	{
		COFF_SYMBOL COFFSymbol;
		COUNT NameLen = strlen (Symbol->Name);

		if (NameLen > 8)
		{
			// String table entry
			WriteTI4(*(TI4*)(COFFSymbol.Name.Name), 0);
			WriteTI4(COFFSymbol.Name.StringRef.StringOffset, ((SectionOffsets *)UserData)->FileOffset);
			((SectionOffsets *)UserData)->FileOffset += NameLen + 1;
		}
		else
		{
			memset (COFFSymbol.Name.Name, 0, 8);
			strncpy (COFFSymbol.Name.Name, Symbol->Name, 8);
		}
		WriteTI4 (COFFSymbol.Value, VAddr + Symbol->Location);
		WriteTI2 (COFFSymbol.Section, COFFSectionNumber);
		WriteTI2 (COFFSymbol.Type, 0);
		WriteTI1 (COFFSymbol.Class, Symbol->Exported ? COFF_SYMBOL_EXTERNAL : COFF_SYMBOL_LABEL);
		WriteTI1 (COFFSymbol.AuxSymbolCount, 0);

		ExportWrite (((SectionOffsets *)UserData)->File, &COFFSymbol, sizeof (COFF_SYMBOL), 1);
	}
}

static void WriteStringTable (const SECTION *Section, void *UserData)
{
	const SYMBOL *Symbol;

	// Section symbol
	{
		COUNT NameLen = strlen (SectionFullNames[SectionID]);

		if (NameLen > 8)
		{
			// String table entry
			ExportWrite ((EXP_FILE *)UserData, SectionFullNames[SectionID], NameLen + 1, 1);
		}
	}

	for_each (Symbol, Section->Symbols)
	{
		COUNT NameLen = strlen (Symbol->Name);

		if (NameLen > 8)
		{
			// String table entry
			ExportWrite ((EXP_FILE *)UserData, Symbol->Name, NameLen + 1, 1);
		}
	}
}

// Export the internal data structures into a TIOS file.
static BOOLEAN ExportDebuggingInfoFile (const PROGRAM *Program, EXP_FILE *File, SIZE FileSize ATTRIBUTE_UNUSED)
{
	COFF_HEADER COFFHeader;
	COUNT SectionCount = 0;
	COUNT SymbolCount = 0;
	SectionOffsets CurrentSectionOffsets;
	OFFSET SymbolTableOffset;
	const SECTION *Section;

	// Check if the file is valid
	for_each (Section, Program->Sections)
	{
		const SECTION *UserData = Section;
		MapToAllSections (Program, CompareSections, &UserData);
		if (UserData)
			Warning (Section->FileName, "Ignoring unmerged section %s",
			         Section->SectionSymbol->Name);
	}

	// Write the COFF header
	MapToAllSections (Program, AddOne, &SectionCount);
	MapToAllSections (Program, CountSymbols, &SymbolCount);
	SymbolTableOffset = sizeof (COFF_HEADER) + SectionCount * sizeof (COFF_SECTION);
	MapToAllSections (Program, CountSymbolTableOffset, &SymbolTableOffset);
	WriteTI2 (COFFHeader.Machine, COFF_MACHINE_M68K);
	WriteTI2 (COFFHeader.SectionCount, SectionCount);
	WriteTI4 (COFFHeader.TimeStamp, 0);
	WriteTI4 (COFFHeader.PSymbols, SymbolTableOffset);
	WriteTI4 (COFFHeader.SymbolCount, SymbolCount);
	WriteTI2 (COFFHeader.OptHdrSize, 0);
	WriteTI2 (COFFHeader.Flags, (COFF_FLAG_32BIT_BE | COFF_FLAG_NO_LINE_NUMBERS));
	ExportWrite (File, &COFFHeader, sizeof (COFF_HEADER), 1);

	// Write the section headers
	CurrentSectionOffsets.FileOffset = sizeof (COFF_HEADER) + SectionCount * sizeof (COFF_SECTION);
	CurrentSectionOffsets.VirtualOffset = 0;
	CurrentSectionOffsets.File = File;
	CurrentSectionOffsets.COFFSectionNumber = 0;
	MapToAllSections (Program, WriteSectionHeader, &CurrentSectionOffsets);

	// Write the section contents
	MapToAllSections (Program, PatchRelocOffsetsIn, NULL);
	CurrentSectionOffsets.COFFSectionNumber = 0;
	MapToAllSections (Program, WriteSectionContents, &CurrentSectionOffsets);
	MapToAllSections (Program, PatchRelocOffsetsOut, NULL);

	// Write the symbol table
	CurrentSectionOffsets.FileOffset = 4; // skip the string table size
	CurrentSectionOffsets.COFFSectionNumber = 0;
	MapToAllSections (Program, WriteSymbolTable, &CurrentSectionOffsets);

	// Write the string table
	ExportWriteTI4 (File, CurrentSectionOffsets.FileOffset); // size
	MapToAllSections (Program, WriteStringTable, File);

	return TRUE;;
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
