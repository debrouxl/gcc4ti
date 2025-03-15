/* imp_elf.c: Routines to import an ELF file

   Copyright (C) 2024 Peter Lafreniere <peter@n8pjl.ca> 

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

#ifdef ELF_SUPPORT

#include "imp_elf.h"

#include "../../formats/elf.h"

#include <stdlib.h>
#include <string.h>


BOOLEAN ArImportELFFile (OBJECT_FILE *ObjectFile)
{

// Call this for a nice and clean failing exit.
#define FailMsg(msg) ({ Error (ObjectFile->FileName, msg); return FALSE; })
#define Fail() FailMsg("Corrupt ELF object file.")
#define TestMem(Ptr) ({ if (!(Ptr)) FailMsg("Out of memory."); })
	
// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) (((const I1 *) (Ptr)) >= ObjectFile->Data && ((const I1 *) (Ptr)) + sizeof (Type) <= ObjectFile->Data + ObjectFile->Size)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Fail (); } })

	const ELF_HEADER *ELFHeader		= (ELF_HEADER *)ObjectFile->Data;
	const ELF_SYMBOLS *ELFSymbols	= NULL;
	const ELF_SECTIONS *ELFSections;
	const char *SymNames			= NULL;
	const char *SecNames;
	SIZE SecNamesSize;
	SIZE SymNamesSize;
	COUNT SecCount;
	COUNT SymCount;
	OFFSET SecIdx;
	OFFSET SymIdx;
	
	if (!IsValidELFObjectFile (ELFHeader))
		FailMsg ("Invalid ELF object file");

	ELFSections = (ELF_SECTIONS *)(ObjectFile->Data + ReadTI4 (ELFHeader->SectionHeadOff));
	SecCount = ReadTI2 (ELFHeader->SectionHeadCount);
	if (IsZero (SecCount))
		Fail();

	// *** Initialize section string table data ***
	{
		OFFSET SectionNameIdx = ReadTI2 (ELFHeader->SectionNameIdx);
		const ELF_SECTION *Section;

		if (IsZero (SectionNameIdx)) {
			Fail ();
		} else if (SectionNameIdx == 0xFFFF) {
			Section = &(*ELFSections) [0];
			TestInFile (Section, ELF_SECTION);
			SectionNameIdx = ReadTI4 (Section->Link);
		}

		Section = &(*ELFSections) [SectionNameIdx];
		TestInFile (Section, ELF_SECTION);

		SecNames		= (char *)(ObjectFile->Data + ReadTI4 (Section->FileOffset));
		SecNamesSize	= ReadTI4 (Section->Size);

		// Bounds check the string table here rather than for each string
		// And make sure that the section is null-terminated
		if (SecNames + SecNamesSize > (char *)ObjectFile->Data + ObjectFile->Size ||
		    !IsZero (SecNames [SecNamesSize - 1]))
			Fail ();
	}

	// *** Locate symbol table and symbol string data ***
	for (SecIdx = 0; SecIdx < SecCount; SecIdx++) {
		const ELF_SECTION *Section;
		const char *Name;
		I4 Type;

		Section = &(*ELFSections) [SecIdx];
		TestInFile (Section, ELF_SECTION);

		Type = ReadTI4 (Section->Type);

		// If this is the symbol table
		if (Type == SHT_SYMTAB) {
			if (ELFSymbols != NULL)
				FailMsg ("Multiple ELF symbol tables found");

			// Make sure that the symbols are the right format
			if (ReadTI4 (Section->EntrySize) != sizeof (ELF_SYMBOL))
				Fail ();

			ELFSymbols = (ELF_SYMBOLS *)(ObjectFile->Data + ReadTI4 (Section->FileOffset));
			SymCount = ReadTI4 (Section->Size) / sizeof (ELF_SYMBOL);
			continue;
		}

		// Bounds check the name
		if ((SIZE) ReadTI4 (Section->NameIdx) > SecNamesSize)
			Fail ();

		Name = &SecNames [ReadTI4 (Section->NameIdx)];

		// If this is the symbol string table
		if (Type == SHT_STRTAB && !strncmp (Name, ".strtab", 7)) {
			SymNames = (char *)(ObjectFile->Data + ReadTI4 (Section->FileOffset));
			SymNamesSize = ReadTI4 (Section->Size);

			// Bounds check the string table here rather than for each string
			// And make sure that the section is null-terminated
			if (SymNames + SymNamesSize > (char *)ObjectFile->Data + ObjectFile->Size ||
		    	!IsZero (SymNames [SymNamesSize - 1]))
				Fail ();
		}
	}

	if (ELFSymbols == NULL || SymNames == NULL)
		Fail ();

	// For each symbol
	for (SymIdx = 0; SymIdx < SymCount; SymIdx++) {
		const ELF_SYMBOL *ELFSymbol;
		const char *Name;
		OFFSET SymSecIdx;

		ELFSymbol = &(*ELFSymbols) [SymIdx];
		// We've already checked this for every symbol in the table
		//TestInFile(ELFSymbol, ELF_SYMBOL);

		// Bounds check the name
		if ((SIZE) ReadTI4 (ELFSymbol->NameIdx) > SymNamesSize)
			Fail ();
		
		// If the symbol has no name, this should point to a null byte
		Name = &SymNames [ReadTI4 (ELFSymbol->NameIdx)];

		SymSecIdx = ReadTI2 (ELFSymbol->SectionIdx);

		if (!IsZero (*Name) && !IsZero (SymSecIdx) && SymSecIdx < SecCount) {
			char Visibility;
			char Bind;

			Visibility = ReadTI1 (ELFSymbol->Visibility);
			Bind = ELF32_ST_BIND (ReadTI1 (ELFSymbol->Info));

			// If the symbol is exported, emit it
			//
			// Note that all visibilities are treated as default when linking,
			// which can lead to unexpected results.
			//
			// The same applies to weak binding. 
			if (Visibility != STV_HIDDEN && (Bind == STB_GLOBAL || Bind == STB_WEAK)) {
				SYMBOL *ARSym = calloc (1, sizeof (SYMBOL));
				TestMem (ARSym);
		
				ARSym->Parent = ObjectFile;

				ARSym->NameLength = strnlen (Name, MAX_SYM_LEN);
				memcpy (ARSym->Name, Name, ARSym->NameLength);

				Append (ObjectFile->Symbols, ARSym);
				ObjectFile->Parent->SymbolCount++;
			}
		}
	}

	return TRUE;
}

#endif /* ELF_SUPPORT */
