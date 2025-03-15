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

#include "../data.h"
#include "../formats/elf.h"
#include "../integers.h"
#include "../manip.h"
#include "../special.h"

#include <stdlib.h>
#include <string.h>

typedef struct {
	SECTION *Section;
	const char *Name;
} SEC_INFO;

typedef struct {
	union {
		SYMBOL *Symbol;
		const char *Name;
	};
	BOOLEAN HasSymbol;
} SYM_INFO;

// Call this for a nice and clean failing exit.
#define FailMsg(msg) ({ if (SecInfo) free (SecInfo); if (SymInfo) free (SymInfo); Error (FileName, msg); return FALSE; })
#define Fail() FailMsg("Corrupt ELF object file.")
#define TestMem(Ptr) ({ if (!(Ptr)) FailMsg("Out of memory."); })
	
// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) (((const I1 *) (Ptr)) >= File && ((const I1 *) (Ptr)) + sizeof (Type) <= File + FileSize)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Fail (); } })

// We split the relocation code into a macro to support both .rel and .rela
// structs.
#define ImportRelocSection(RelocType, SecNameLen, ADDITIONAL_STATEMENTS) do {												\
	const char *TargetSecName;															\
	const RelocType *Relocs;															\
	SECTION *Section = NULL;															\
	COUNT RelocCount;																	\
	OFFSET Idx;																			\
																						\
	if (SecNameSize <= SecNameLen)														\
		Fail ();																		\
																						\
	TargetSecName = SecName + SecNameLen;												\
																						\
	/* Find the section with that name */												\
	for (Idx = 0; Idx < SecCount; Idx++) {												\
		if (SecInfo[Idx].Name && !strcmp (TargetSecName, SecInfo[Idx].Name)) {			\
			Section = SecInfo[Idx].Section;												\
			break;																		\
		}																				\
	}																					\
																						\
	/* Either that section doesn't exist or we pruned it earlier */						\
	if (!Section)																		\
		FailMsg ("Relocation table found for unallocated section");						\
																						\
	/* Make sure that the entries are the right format */								\
	if (ReadTI4 (ELFSection->EntrySize) != sizeof (RelocType))							\
		Fail ();																		\
																						\
	Relocs = (const RelocType *)(File + ReadTI4 (ELFSection->FileOffset));				\
	RelocCount = ReadTI4 (ELFSection->Size) / sizeof (RelocType);						\
																						\
	for (Idx = 0; Idx < RelocCount; Idx++) {											\
		const RelocType *ELFReloc;														\
		RELOC *LDReloc;																	\
		I4 Info;																		\
																						\
		ELFReloc = &Relocs[Idx];														\
		TestInFile (ELFReloc, RelocType);												\
																						\
		LDReloc = calloc (1, sizeof (RELOC));											\
		TestMem (LDReloc);																\
																						\
		Info = ReadTI4 (ELFReloc->Info);												\
																						\
		LDReloc->Parent = Section;														\
		LDReloc->Location = ReadTI4 (ELFReloc->Offset);									\
																						\
		/* TODO: figure out Reloc->Unoptimizable */										\
																						\
		/* Dispatch based on location */												\
		switch (ELF32_R_TYPE (Info)) {													\
			case R_68K_32:																\
				LDReloc->Size = 4;														\
				break;																	\
			case R_68K_16:																\
				LDReloc->Size = 2;														\
				break;																	\
			case R_68K_8:																\
				LDReloc->Size = 1;														\
			case R_68K_PC32:															\
				LDReloc->Size		= 4;												\
				LDReloc->Relative	= TRUE;												\
				break;																	\
			case R_68K_PC16:															\
				LDReloc->Size		= 2;												\
				LDReloc->Relative	= TRUE;												\
				break;																	\
			case R_68K_PC8:																\
				LDReloc->Size		= 1;												\
				LDReloc->Relative	= TRUE;												\
			default:																	\
				Warning (FileName, "Ignoring reloc %d at 0x%x in section '%s' with unknown type '0x%X'.",	\
						 Idx, LDReloc->Location, TargetSecName, ELF32_R_TYPE (Info));	\
		}																				\
																						\
		if ((COUNT) ELF32_R_SYM (Info) >= SymCount)										\
			Fail ();																	\
																						\
		if (SymInfo[ELF32_R_SYM (Info)].HasSymbol) {									\
			LDReloc->Target.Symbol		= SymInfo[ELF32_R_SYM (Info)].Symbol;			\
			LDReloc->Target.SymbolName	= LDReloc->Target.Symbol->Name;					\
		} else {																		\
			LDReloc->Target.SymbolName	= strdup (SymInfo[ELF32_R_SYM (Info)].Name);	\
		}																				\
																						\
		ADDITIONAL_STATEMENTS															\
																						\
		InsertReloc (Section, LDReloc);													\
	} 																					\
} while (0)

BOOLEAN ImportELFFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName)
{
	const ELF_HEADER *ELFHeader = (ELF_HEADER *)File;
	const ELF_SYMBOLS *ELFSymbols = NULL;
	const ELF_SECTIONS *ELFSections;
	BOOLEAN InitializeBSS = TRUE;
	const char *SymNames = NULL;
	BOOLEAN AllRelocs = FALSE;
	SEC_INFO *SecInfo = NULL;
	SYM_INFO *SymInfo = NULL;
	const char *SecNames;
	SIZE SecNamesSize;
	SIZE SymNamesSize;
	COUNT SecCount;
	COUNT SymCount;
	OFFSET SecIdx;
	OFFSET SymIdx;
	
	if (!IsValidELFObjectFile (ELFHeader))
		FailMsg ("Invalid ELF object file");

	ELFSections = (ELF_SECTIONS *)(File + ReadTI4 (ELFHeader->SectionHeadOff));
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
			Section = &(*ELFSections)[0];
			TestInFile (Section, ELF_SECTION);
			SectionNameIdx = ReadTI4 (Section->Link);
		}

		Section = &(*ELFSections)[SectionNameIdx];
		TestInFile (Section, ELF_SECTION);

		SecNames = (char *)(File + ReadTI4 (Section->FileOffset));
		SecNamesSize = ReadTI4 (Section->Size);

		// Bounds check the string table here rather than for each string
		// And make sure that the section is null-terminated
		if (SecNames + SecNamesSize > (char *)File + FileSize ||
		    !IsZero (SecNames[SecNamesSize - 1]))
			Fail ();
	}

	// *** Locate symbol table and symbol string data ***
	for (SecIdx = 0; SecIdx < SecCount; SecIdx++) {
		const ELF_SECTION *Section;
		const char *Name;
		I4 Type;

		Section = &(*ELFSections)[SecIdx];
		TestInFile (Section, ELF_SECTION);

		Type = ReadTI4 (Section->Type);

		// If this is the symbol table
		if (Type == SHT_SYMTAB) {
			if (ELFSymbols != NULL)
				FailMsg ("Multiple ELF symbol tables found");

			// Make sure that the symbols are the right format
			if (ReadTI4 (Section->EntrySize) != sizeof (ELF_SYMBOL))
				Fail ();

			ELFSymbols = (ELF_SYMBOLS *)(File + ReadTI4 (Section->FileOffset));
			SymCount = ReadTI4 (Section->Size) / sizeof (ELF_SYMBOL);
			continue;
		}

		// Bounds check the name
		if ((SIZE) ReadTI4 (Section->NameIdx) > SecNamesSize)
			Fail ();

		Name = &SecNames[ReadTI4 (Section->NameIdx)];

		// If this is the symbol string table
		if (Type == SHT_STRTAB && !strncmp (Name, ".strtab", 7)) {
			SymNames = (char *)(File + ReadTI4 (Section->FileOffset));
			SymNamesSize = ReadTI4 (Section->Size);

			// Bounds check the string table here rather than for each string
			// And make sure that the section is null-terminated
			if (SymNames + SymNamesSize > (char *)File + FileSize ||
		    	    !IsZero (SymNames[SymNamesSize - 1]))
				Fail ();
		}
	}

	if (ELFSymbols == NULL || SymNames == NULL)
		Fail ();

	// *** Handle any special symbols before we import anything else ***

	for (SymIdx = 0; SymIdx < SymCount; SymIdx++) {
		const ELF_SYMBOL *Symbol;
		const char *Name;
		
		Symbol = &(*ELFSymbols)[SymIdx];
		TestInFile(Symbol, ELF_SYMBOL);

		// Bounds check the name
		if ((SIZE) ReadTI4 (Symbol->NameIdx) > SymNamesSize)
			Fail ();
		
		// If the symbol has no name, this should point to a null byte
		Name = &SymNames[ReadTI4 (Symbol->NameIdx)];
		
		if (!(strcmp (Name, SYM_OMIT_BSS_INIT)))
			InitializeBSS = FALSE;
		else if (!(strcmp (Name, SYM_ALL_RELOCS)))
			AllRelocs = TRUE;
		else if (!(strcmp (Name, SYM_IGNORE_GLOBAL_IMPORTS)))
			Program->IgnoreGlobalImports = TRUE;
	}

	// *** Import Sections ***
	SecInfo = calloc (SecCount, sizeof (SEC_INFO));
	TestMem (SecInfo);

	// For each section, again...
	for (SecIdx = 0; SecIdx < SecCount; SecIdx++) {
		const ELF_SECTION *ELFSection;
		SECTION *LDSection;
		const char *Name;
		SIZE NameSize;
		SIZE Size;
		I4 Flags;
		I4 Align;
		I4 Type;

		ELFSection = &(*ELFSections)[SecIdx];
		// This test has already been done
		//TestInFile(ELFSection, ELF_SECTION);

		Flags	= ReadTI4 (ELFSection->Flags);
		Type	= ReadTI4 (ELFSection->Type);
		Size	= ReadTI4 (ELFSection->Size);
		Align	= ReadTI4 (ELFSection->Alignment);

		// We can't guarantee that load addresses will be preserved,
		// which is fine as object files never set this field.
		if (!IsZeroI4 (ELFSection->Address))
			Fail ();

		// Skip sections that don't appear in the final executable
		if (!(Flags & ELF_SECTION_ALLOC))
			continue;

		// Bounds check the name
		if ((SIZE) ReadTI4 (ELFSection->NameIdx) > SecNamesSize)
			Fail ();

		Name = &SecNames[ReadTI4 (ELFSection->NameIdx)];

		// We know that there is a null byte between Name and the end of the file
		// so this is safe
		NameSize = strlen(Name);

		// *** Actually import the section ***

		LDSection = calloc (1, sizeof (SECTION));
		TestMem(LDSection);

		LDSection->Parent	= Program;
		LDSection->Size		= Size;

		if (Type == SHT_PROGBITS && !IsZero (Size)) {
			// Bounds check the file data
			if ((SIZE)ReadTI4 (ELFSection->FileOffset) + Size > FileSize)
				Fail ();
			
			LDSection->Data = malloc (Size);
			TestMem(LDSection->Data);

			memcpy (LDSection->Data, File + ReadTI4 (ELFSection->FileOffset), Size);		
		}
		// else LDSection->Data is already null

		LDSection->Initialized	= LDSection->Data || InitializeBSS;
		LDSection->Code			= !!(Flags & ELF_SECTION_EXECINSTR);

		// TODO: figure out exactly what it means to be mergeable
		// Is it enough for the section to be read-only and not executable?
		LDSection->Mergeable = FALSE;
		LDSection->Unaligned = IsZero (Align) || Align == 1;

		// See if the section is a startup section,
		// This may need to be moved before we prune non-allocated sections
		LDSection->StartupNumber = GetStartupSectionNumber (Name, NameSize);

		LDSection->Constructors	= NameSize == 6 && IsZero (strncmp (Name, ".ctors", 6));
		LDSection->Destructors	= NameSize == 6 && IsZero (strncmp (Name, ".dtors", 6));

		// While we set CanCutRanges the same way as COFF importing,
		// there is currently no toolchain for m68k-elf that works with
		// range cutting.
		LDSection->CanCutRanges	= AllRelocs;
		LDSection->FileName		= FileName;

		// TODO: Debugging info

		// Append/insert the section.
		InsertSection (Program, LDSection);
				
		// Create a section symbol for this section.
		if (!(CreateSectionSymbol (LDSection, Name))) {
			if (LDSection->Data)
				free (LDSection->Data);
			free (LDSection);
			Fail ();
		}

		// Add to table for quick lookup when importing symbols
		SecInfo[SecIdx].Section	= LDSection;
		SecInfo[SecIdx].Name	= Name;
	}

	// *** Import Symbols ***
	SymInfo = calloc (SymCount, sizeof (SYM_INFO));
	TestMem (SymInfo);

	// For each symbol (again)
	for (SymIdx = 0; SymIdx < SymCount; SymIdx++) {
		const ELF_SYMBOL *ELFSymbol;
		const char *Name;
		OFFSET SymSecIdx;

		ELFSymbol = &(*ELFSymbols)[SymIdx];
		// We've already checked this for every symbol in the table
		//TestInFile(ELFSymbol, ELF_SYMBOL);

		// Bounds check the name
		if ((SIZE) ReadTI4 (ELFSymbol->NameIdx) > SymNamesSize)
			Fail ();
		
		// If the symbol has no name, this should point to a null byte
		Name					= &SymNames[ReadTI4 (ELFSymbol->NameIdx)];
		SymInfo[SymIdx].Name	= Name;

		// Skip linker control symbols and global imports 
		if (HandleSpecialSymbol (Program, Name))
			continue;

		SymSecIdx = ReadTI2 (ELFSymbol->SectionIdx);

		if (/*!IsZero (*Name) && */!IsZero (SymSecIdx) && SymSecIdx < SecCount && SecInfo[SymSecIdx].Section) {
			char Visibility;
			SYMBOL *LDSym;
			char Info;

			Visibility	= ReadTI1 (ELFSymbol->Visibility);
			Info		= ReadTI1 (ELFSymbol->Info);

			SymInfo[SymIdx].HasSymbol = TRUE;

			// TODO: add support for visibility in core linker
			if (ELF32_ST_VISIBILITY (Visibility) != STV_DEFAULT)
				Warning (FileName, "Ignoring non-default visiblity for symbol '%s'.",
			             Name);

			// If the symbol is a section, we already made a symbol for it
			if (ELF32_ST_TYPE(Info) == STT_SECTION) {
				SymInfo[SymIdx].Symbol = SecInfo[SymSecIdx].Section->SectionSymbol;
			// Otherwise make a new symbol
			} else {
				LDSym = calloc (1, sizeof (SYMBOL));
				TestMem (LDSym);
		
				LDSym->Parent	= SecInfo[SymSecIdx].Section;
				LDSym->Location	= ReadTI4 (ELFSymbol->Value);
				strncpy (LDSym->Name, Name, MAX_SYM_LEN);
				LDSym->Exported = ELF32_ST_BIND (Info) != STB_LOCAL;

				// TODO: add support for weak symbols in core linker
				if (ELF32_ST_BIND (Info) == STB_WEAK)
					Warning (FileName, "Weak symbols not supported, symbol '%s' is being bound normally.",
				             Name);

				InsertSymbol (SecInfo[SymSecIdx].Section, LDSym);
		
				SymInfo[SymIdx].Symbol = LDSym;
			}
		}
	}

	// *** Import relocation information ***

	// Loop over sections yet again...
	for (SecIdx = 0; SecIdx < SecCount; SecIdx++) {
		const ELF_SECTION *ELFSection;
		const char *SecName;
		SIZE SecNameSize;
		I4 SecType;

		ELFSection = &(*ELFSections)[SecIdx];
		// Redundant. We've already checked this for every section
		//TestInFile(ELFSection, ELF_SECTION);
		
		SecType = ReadTI4 (ELFSection->Type);

		// Only process relocation tables
		if (SecType != SHT_REL && SecType != SHT_RELA)
			continue;

		// Bounds check the name
		if ((SIZE) ReadTI4 (ELFSection->NameIdx) > SecNamesSize)
			Fail ();

		SecName		= &SecNames[ReadTI4 (ELFSection->NameIdx)];
		SecNameSize	= strlen (SecName);

		// Only handle relocation tables that begin with rel or rela
		if (SecType == SHT_REL && !strncmp (SecName, ".rel", 4))
			ImportRelocSection (ELF_RELOC, 4,);
		else if (SecType == SHT_RELA && !strncmp (SecName, ".rela", 5))
			ImportRelocSection (ELF_RELOCA, 5, LDReloc->Target.Offset = ReadTI4 (ELFReloc->Addend););
	}

	free (SecInfo);
	free (SymInfo);
	return TRUE;
}

#endif /* ELF_SUPPORT */
