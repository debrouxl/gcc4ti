/* imp_coff.c: Routines to import a COFF file

   Copyright (C) 2002-2003 Sebastian Reichelt
   Copyright (C) 2003-2005 Kevin Kofler

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

#include "imp_coff.h"

#ifdef COFF_SUPPORT

#include "../formats/coff.h"
#include "../manip.h"
#include "../special.h"
#include "../bincode/fix_m68k.h"

#include <stdlib.h>
#include <string.h>

// Extra Information for a COFF Section
typedef struct {
	SECTION *Section;
	char Name[COFF_SECTION_NAME_LEN+1];
	OFFSET VAddr;
} SEC_INFO;

// Extra Information for a COFF Symbol
typedef struct {
	SYMBOL *Symbol;
} SYM_INFO;

// Import a COFF file into the internal data structures.
BOOLEAN ImportCOFFFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName)
{
	// Call this for a nice and clean failing exit.
#define Fail() ({ if (SymInfo) free (SymInfo); if (SecInfo) free (SecInfo); return FALSE; })
#define TestMem(Ptr) ({ if (!(Ptr)) { Error (FileName, "Out of memory."); Fail (); } })
	
	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) (((const I1 *) (Ptr)) >= File && ((const I1 *) (Ptr)) + sizeof (Type) <= File + FileSize)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Error (FileName, "Corrupt COFF object file."); Fail (); } })
	
	// Local Variables
	COFF_INFO FileInfo;
	const COFF_HEADER *COFFHeader = (const COFF_HEADER *) File;
	const COFF_SECTIONS *COFFSections;
	const COFF_SYMBOLS *COFFSymbols;
	const char *COFFStringTable;
	SEC_INFO *SecInfo = NULL;
	SYM_INFO *SymInfo = NULL;
	BOOLEAN InitializeBSS = TRUE;
	BOOLEAN AllRelocs = FALSE;
	OFFSET CurCOFFSectionNumber, CurCOFFSymbolNumber, CurCOFFRelocNumber;
	
	// Create file information in FileInfo.
	CreateCoffInfo (*COFFHeader, FileInfo);
	// Get pointer to list of sections.
	COFFSections = (const COFF_SECTIONS *) (File + FileInfo.PSections);
	// Get pointer to list of symbols.
	COFFSymbols = (const COFF_SYMBOLS *) (File + FileInfo.PSymbols);
	// Get pointer to string table.
	COFFStringTable = (const char *) (File + FileInfo.PStrings);
	
	// *** Process File-Local Special Symbols ***
	
	// For each symbol...
	for (CurCOFFSymbolNumber = 0; CurCOFFSymbolNumber < FileInfo.SymbolCount; CurCOFFSymbolNumber++)
	{
		// Get pointer to this symbol.
		const COFF_SYMBOL *CurCOFFSymbol = &((*COFFSymbols) [CurCOFFSymbolNumber]);
		TestInFile (CurCOFFSymbol, COFF_SYMBOL);
		
		// Only process long symbol names.
		if (IsZero (CurCOFFSymbol->Name.StringRef.Zero))
		{
			// If the name is special, set the appropriate flag.
			const char *SymName = COFFStringTable + ReadTI4 (CurCOFFSymbol->Name.StringRef.StringOffset);
			TestInFile (SymName, char);
			if (!(strcmp (SymName, SYM_OMIT_BSS_INIT)))
				InitializeBSS = FALSE;
			else if (!(strcmp (SymName, SYM_ALL_RELOCS)))
				AllRelocs = TRUE;
			else if (!(strcmp (SymName, SYM_IGNORE_GLOBAL_IMPORTS)))
				Program->IgnoreGlobalImports = TRUE;
		}
		
		// Skip corresponding auxiliary symbols.
		CurCOFFSymbolNumber += ReadTI1 (CurCOFFSymbol->AuxSymbolCount);
	}
	
	// *** Import Sections ***
	
	// Create extra section information table.
	SecInfo = calloc (FileInfo.SectionCount, sizeof (SEC_INFO));
	TestMem (SecInfo);
	
	// For each section...
	for (CurCOFFSectionNumber = 0; CurCOFFSectionNumber < FileInfo.SectionCount; CurCOFFSectionNumber++)
	{
		OFFSET StartupNumber;
		// Get pointer to this section.
		const COFF_SECTION *CurCOFFSection = &((*COFFSections) [CurCOFFSectionNumber]);
		TestInFile (CurCOFFSection, COFF_SECTION);
		// Put name and virtual address into section information.
		strncpy (SecInfo[CurCOFFSectionNumber].Name, CurCOFFSection->Name, sizeof (CurCOFFSection->Name));
		SecInfo[CurCOFFSectionNumber].VAddr = ReadTI4 (CurCOFFSection->VirtualAddress);
		
		// At first, determine whether it is a startup section.
		// This is very important, as we cannot omit it.
		StartupNumber = GetStartupSectionNumber (CurCOFFSection->Name, sizeof (CurCOFFSection->Name));
		
		if (IsZeroI4 (CurCOFFSection->Size))
		{
			// Section is empty.
			// Check if the number of relocs is empty as well. If not, there
			// is probably an error in the object file.
			if (!(IsZeroI2 (CurCOFFSection->RelocCount)))
				Warning (FileName, "Empty section %ld has relocs.", (long) CurCOFFSectionNumber);
		}

		{
			I4 Flags = ReadTI4 (CurCOFFSection->Flags);
			
			// Try to allocate data for the section, if necessary.
			BOOLEAN HasData = (!(Flags & COFF_SECTION_BSS)) && (!IsZeroI4 (CurCOFFSection->Size)) && (!(IsZeroI4 (CurCOFFSection->PData)));
			SIZE Size = ReadTI4 (CurCOFFSection->Size);
			I1 *Data = NULL;
			
			if (HasData)
			{
				const I1 *SrcData = File + ReadTI4 (CurCOFFSection->PData);
				TestInFile (SrcData, I1 [Size]);
				TestMem ((Data = calloc (Size, 1)));
				memcpy (Data, SrcData, Size);
			}
			
			{
				char *SectionName = SecInfo[CurCOFFSectionNumber].Name;
				
				// Create a new section.
				SECTION *Section = calloc (1, sizeof (SECTION));
				TestMem (Section);
				Section->Parent = Program;
				
				// Initialize the section.
				Section->Data = Data;
				Section->Size = Size;
				// If neither the code flag nor the data flag is specified, default to data for non-startup sections and to code for startup sections.
				Section->Code = Data && ((Flags & COFF_SECTION_TEXT) || (StartupNumber && (!(Flags & COFF_SECTION_DATA))));
#ifdef COFF_TIGCC_EXTENSIONS
				// Read TIGCC COFF section flags.
				Section->Mergeable = !!(Flags & COFF_SECTION_MERGEABLE);
				Section->Unaligned = !!(Flags & COFF_SECTION_UNALIGNED);
#endif
				Section->Initialized   = Data || InitializeBSS;
				Section->StartupNumber = StartupNumber;
				Section->Constructors  = (!(strncmp (SectionName, ".ctors", 8)));
				Section->Destructors   = (!(strncmp (SectionName, ".dtors", 8)));
#ifdef DEBUGGING_INFO_SUPPORT
				if (!strncmp (SectionName, ".stab", 8))
					Section->DebuggingInfoType = DI_STAB;
				else if (!strncmp (SectionName, ".stabstr", 8))
					Section->DebuggingInfoType = DI_STABSTR;
				else if (!strncmp (SectionName, ".debug_a", 8))
					Section->DebuggingInfoType = DI_DEBUG_ABBREV;
					/* (This might also be .debug_aranges, we need to wait for
					    the section symbol to disambiguate.) */
				else if (!strncmp (SectionName, ".debug_f", 8))
					Section->DebuggingInfoType = DI_DEBUG_FRAME;
				else if (!strncmp (SectionName, ".debug_i", 8))
					Section->DebuggingInfoType = DI_DEBUG_INFO;
				else if (!strncmp (SectionName, ".debug_l", 8))
					Section->DebuggingInfoType = DI_DEBUG_LINE;
					/* (This might also be .debug_loc, we need to wait for
					    the section symbol to disambiguate.) */
				else if (!strncmp (SectionName, ".debug_m", 8))
					Section->DebuggingInfoType = DI_DEBUG_MACINFO;
				else if (!strncmp (SectionName, ".debug_p", 8))
					Section->DebuggingInfoType = DI_DEBUG_PUBNAMES;
				else if (!strncmp (SectionName, ".debug_r", 8))
					Section->DebuggingInfoType = DI_DEBUG_RANGES;
				else if (!strncmp (SectionName, ".debug_s", 8))
					Section->DebuggingInfoType = DI_DEBUG_STR;
				else if (!strncmp (SectionName, ".eh_fram", 8))
					Section->DebuggingInfoType = DI_EH_FRAME;
#endif
				Section->CanCutRanges  = AllRelocs;
				Section->FileName      = FileName;
				
				// Append/insert the section.
				InsertSection (Program, Section);
				
				// Create a section symbol for this section.
				if (!(CreateSectionSymbol (Section, SectionName)))
					Fail ();
				
				// Put this section into the extra information table.
				SecInfo[CurCOFFSectionNumber].Section = Section;
			}
			// Now we are stuck: We should import the symbols all at once,
			// when all of the sections have been imported. We cannot import
			// the relocation entries now because they reference symbols. So
			// the only good way is to import all sections first, then import
			// all symbols, and then the relocs for each section.
		}
	}
	
	// *** Import Symbols ***
	
	// Create extra symbol information table.
	SymInfo = calloc (FileInfo.SymbolCount, sizeof (SYM_INFO));
	TestMem (SymInfo);
	
	// For each symbol...
	for (CurCOFFSymbolNumber = 0; CurCOFFSymbolNumber < FileInfo.SymbolCount; CurCOFFSymbolNumber++)
	{
		// Get pointer to this symbol.
		const COFF_SYMBOL *CurCOFFSymbol = &((*COFFSymbols) [CurCOFFSymbolNumber]);
		TestInFile (CurCOFFSymbol, COFF_SYMBOL);
		
		{
			// Temporary placeholder for symbol section.
			OFFSET SymSection = (SI2) (ReadTI2 (CurCOFFSymbol->Section));
			// Temporary placeholder for symbol value.
			OFFSET SymVal = (SI4) (ReadTI4 (CurCOFFSymbol->Value));
			// Temporary placeholder for symbol class.
			I1 SymClass = (ReadTI1 (CurCOFFSymbol->Class));
			// Temporary placeholder for symbol name.
			char SymName[MAX_SYM_LEN+1];
			// Set to zero for terminating zero byte.
			memset (SymName, 0, MAX_SYM_LEN + 1);
			// COFF symbol names are stored in a strange way. This is how to get them.
			if (IsZero (CurCOFFSymbol->Name.StringRef.Zero))
			{
				const char *Str = COFFStringTable + ReadTI4 (CurCOFFSymbol->Name.StringRef.StringOffset);
				TestInFile (Str, char);
				strncpy (SymName, Str, MAX_SYM_LEN);
			}
			else
				strncpy (SymName, CurCOFFSymbol->Name.Name, 8);
			
			// Handle the symbol if it is special for this linker, only proceed if not.
			if (!(HandleSpecialSymbol (Program, SymName)))
			{
				// Check if the section number is nonzero. If it is zero, the symbol
				// is somewhat special.
				if (SymSection)
				{
					// Check if the section number is positive and the class is
					// <100. If not, the symbol is very special, e.g. absolute
					// or debugging; so ignore it.
					if (SymSection > 0 && SymClass < 100)
					{
						// Check if the section number is valid.
						if (SymSection <= FileInfo.SectionCount)
						{
							SECTION *Section = SecInfo[SymSection-1].Section;
							
							// The symbol is a real label in a real section.
							if (Section)
							{
								BOOLEAN Exported = (ReadTI1 (CurCOFFSymbol->Class) == COFF_SYMBOL_EXTERNAL);
								OFFSET Location = SymVal - SecInfo[SymSection-1].VAddr;
								
								if ((Location == 0) && (!Exported) && (!(strncmp (SymName, SecInfo[SymSection-1].Name, COFF_SECTION_NAME_LEN))))
								{
									// Use the section symbol instead.
									SymInfo[CurCOFFSymbolNumber].Symbol = Section->SectionSymbol;
									// Rename the section symbol to the full section name as stored in the symbol.
									CreateSectionSymbol (Section, SymName);
#ifdef DEBUGGING_INFO_SUPPORT
									// Disambiguate DWARF 2 section names with conflicting 8 char abbreviations.
									if (!strcmp (SymName, ".debug_aranges"))
										Section->DebuggingInfoType = DI_DEBUG_ARANGES;
									else if (!strcmp (SymName, ".debug_loc"))
										Section->DebuggingInfoType = DI_DEBUG_LOC;
#endif
								}
								else
								{
									// Simply create a new symbol entry in that section.
									SYMBOL *Symbol = calloc (1, sizeof (SYMBOL));
									TestMem (Symbol);
									Symbol->Parent   = Section;
									Symbol->Location = Location;
									strcpy (Symbol->Name, SymName);
									Symbol->Exported = Exported;
									InsertSymbol (Section, Symbol);
									
									// Put this symbol in the extra information table.
									SymInfo[CurCOFFSymbolNumber].Symbol = Symbol;
								}
							}
							else
								Warning (FileName, "Ignoring symbol in unimported section number %ld.", (long) SymSection);
						}
						else
							Warning (FileName, "Ignoring symbol in nonexisting section number %ld.", (long) SymSection);
					}
				}
				else
				{
					// The section number is zero. This may have two reasons:
					// If the value is zero as well, this is an external reference.
					// If the value is positive, the symbol is a common uninitialized
					// symbol (i.e. BSS).
					if (SymVal > 0)
					{
						// The symbol is a common uninitialized symbol. Make one and
						// put it in the extra information table.
						if (!((SymInfo[CurCOFFSymbolNumber].Symbol = MakeCommonSymbol (Program, SymName, SymVal, InitializeBSS, FileName))))
							Fail ();
					}
					// Otherwise, the symbol is an external reference or some strange
					// unknown thing; we cannot handle it any further here, as we
					// cannot assign it to a section.
				}
			}
		}
		
		// Skip corresponding auxiliary symbols.
		CurCOFFSymbolNumber += ReadTI1 (CurCOFFSymbol->AuxSymbolCount);
	}
	
	// *** Import Relocation Entries ***
	
	// For each section...
	for (CurCOFFSectionNumber = 0; CurCOFFSectionNumber < FileInfo.SectionCount; CurCOFFSectionNumber++)
	{
		const RELOC *RelocHint = NULL;
		
		// Get pointer to this section.
		const COFF_SECTION *CurCOFFSection = &((*COFFSections) [CurCOFFSectionNumber]);
		// Get pointer to section in internal data structure.
		SECTION *Section = SecInfo[CurCOFFSectionNumber].Section;
		// Only proceed if we didn't omit the section.
		if (Section)
		{
			COUNT RelocCount = ReadTI2 (CurCOFFSection->RelocCount);
			const COFF_RELOCS *CurCOFFRelocs = (const COFF_RELOCS *) (File + ReadTI4 (CurCOFFSection->PRelocs));
			// For each reloc...
			for (CurCOFFRelocNumber = 0; CurCOFFRelocNumber < RelocCount; CurCOFFRelocNumber++)
			{
				OFFSET SymNum, COFFLocation;
				// Get pointer to this reloc.
				const COFF_RELOC *CurCOFFReloc = &((*CurCOFFRelocs) [CurCOFFRelocNumber]);
				TestInFile (CurCOFFReloc, COFF_RELOC);
				// Read the location (for better error messages).
				COFFLocation = ReadTI4 (CurCOFFReloc->Location);
				// Temporary placeholder for symbol number.
				SymNum = ReadTI4 (CurCOFFReloc->Symbol);
				// Check if the symbol number is valid.
				if (SymNum >= 0 && SymNum < FileInfo.SymbolCount)
				{
					// Determine the reloc's size and relation.
					SIZE RelocSize = 0;
					BOOLEAN Relative = FALSE;
					BOOLEAN Negative = FALSE;
					BOOLEAN Unoptimizable = FALSE;
					I2 RelocType = ReadTI2 (CurCOFFReloc->Type);
#ifdef COFF_TIGCC_EXTENSIONS
					Unoptimizable = (RelocType & COFF_RELOC_UNOPTIMIZABLE);
					RelocType &= ~COFF_RELOC_UNOPTIMIZABLE;
#endif
					switch (RelocType)
					{
						case COFF_RELOC_REL1:
							Relative = TRUE;
						case COFF_RELOC_ABS1:
							RelocSize = 1;
							break;
						case COFF_RELOC_REL2:
							Relative = TRUE;
						case COFF_RELOC_ABS2:
							RelocSize = 2;
							break;
						case COFF_RELOC_REL4:
							Relative = TRUE;
						case COFF_RELOC_ABS4:
							RelocSize = 4;
							break;
#ifdef COFF_TIGCC_EXTENSIONS
						case COFF_RELOC_ABS1_NEG:
							RelocSize = 1;
							Negative = TRUE;
							break;
						case COFF_RELOC_ABS2_NEG:
							RelocSize = 2;
							Negative = TRUE;
							break;
#endif
						case COFF_RELOC_ABS4_NEG:
							RelocSize = 4;
							Negative = TRUE;
							break;
					}
					// Check whether we were able to translate the reloc type.
					if (RelocSize > 0)
					{
						OFFSET Location = COFFLocation - SecInfo[CurCOFFSectionNumber].VAddr;
						SYMBOL *Symbol = SymInfo[SymNum].Symbol;
						
						// Check if we really imported the target symbol.
						if (Symbol)
						{
							// Yes, we did.
							// Find the virtual address of the target section.
							// We will need this to subtract the virtual location of the target symbol.
							OFFSET VAddr = 0;
							OFFSET COFFSec;
							for (COFFSec = 0; COFFSec < FileInfo.SectionCount; COFFSec++)
							{
								if (SecInfo[COFFSec].Section == Symbol->Parent)
								{
									VAddr = SecInfo[COFFSec].VAddr;
									break;
								}
							}
							
							// Handle negative relocs by searching for matching positive ones.
							if (Negative)
							{
								// Search for a matching positive reloc.
								RELOC *PositiveReloc = FindMatchingReloc (Section, Location, RelocSize, FALSE, NULL, RelocHint);
								
								// We can only do something with the negative reloc
								// if we have found a matching positive one.
								if (PositiveReloc)
								{
									// Make the positive reloc relative, with
									// this reloc's target as the relation.
									LOCATION *Relation = calloc (1, sizeof (LOCATION));
									TestMem (Relation);
									Relation->Symbol     = Symbol;
									Relation->SymbolName = Relation->Symbol->Name;
									PositiveReloc->Relative = TRUE;
									PositiveReloc->Relation = Relation;
									// Subtract the target offset for this reloc
									// from the positive reloc's fixed offset.
									PositiveReloc->FixedOffset += VAddr + Symbol->Location;
									if (RelocSize <= 1) {
										PositiveReloc->FixedOffset = (SI1) PositiveReloc->FixedOffset;
									} else if (RelocSize <= 2) {
										PositiveReloc->FixedOffset = (SI2) PositiveReloc->FixedOffset;
									}
									HandleLocation (PositiveReloc, Relation);
									
									RelocHint = PositiveReloc;
								}
								else
									Warning (FileName, "Removing negative reloc at 0x%lX in section %ld to `%s' with no matching positive reloc.", (long) Location, (long) CurCOFFSectionNumber, Symbol->Name);
							}
							else // Positive
							{
								OFFSET Offset = 0;
								
								// Now all we need to do is to set up a reloc to this symbol.
								RELOC *Reloc = calloc (1, sizeof (RELOC));
								TestMem (Reloc);
								Reloc->Parent            = Section;
								Reloc->Location          = Location;
								Reloc->Target.Symbol     = Symbol;
								Reloc->Target.SymbolName = Reloc->Target.Symbol->Name;
								Reloc->Size              = RelocSize;
								Reloc->Relative          = Relative;
								Reloc->Unoptimizable     = Unoptimizable;
								
								// Calculate the reloc's target offset.
								// The section contents contain the virtual
								// address of the target.
								if ((Location >= 0) && (Location + RelocSize <= Section->Size))
								{
									if (Section->Data)
									{
										Offset = ReadSTI (Section->Data + Location, RelocSize);
										memset (Section->Data + Location, 0, RelocSize);
									}
									else
										Warning (FileName, "Adding reloc %ld at 0x%lX in section %ld without data to `%s'.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, Reloc->Target.SymbolName);
								}
								else
									Warning (FileName, "Adding reloc %ld at 0x%lX outside of section %ld to `%s'.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, Reloc->Target.SymbolName);
								// Subtract the virtual address of the target symbol,
								// since we only want the difference to the symbol.
								Offset -= VAddr + Symbol->Location;
								// Relative relocs contain the difference between
								// the virtual target address and the current
								// virtual address. So we have to add the current
								// virtual address to get only the offset.
								if (Relative)
									Offset += Location + SecInfo[CurCOFFSectionNumber].VAddr;
								
								// For compatibility with files generated by
								// unpatched versions of GNU as, treat offsets from
								// section symbols as TargetOffset rather than
								// FixedOffset.
								// Apply architecture-specific fixes to the target offset.
								// These fixes are not needed if the correct target
								// symbol is referenced.
								if (Symbol == Symbol->Parent->SectionSymbol)
									Reloc->Target.Offset = ((Section->Code && Symbol->Parent->Code) ? M68kFixTargetOffset (Offset, RelocSize, Relative) : Offset);
								
								// Calculate the remaining part of the offset.
								Reloc->FixedOffset = Offset - Reloc->Target.Offset;
								if (RelocSize <= 1) {
									Reloc->FixedOffset = (SI1) Reloc->FixedOffset;
								} else if (RelocSize <= 2) {
									Reloc->FixedOffset = (SI2) Reloc->FixedOffset;
								}
								
								// Append the reloc to the linked list.
								InsertReloc (Section, Reloc);
							}
						}
						else
						{
							// No, we didn't. We have to find the cause of this.
							// If it is an externally defined symbol, set up a
							// reloc anyway. If not, give a warning.
							const COFF_SYMBOL *CurCOFFSymbol = &((*COFFSymbols) [SymNum]);
							// Check whether it is an external symbol.
							if (IsZeroI2 (CurCOFFSymbol->Section) && IsZeroI4 (CurCOFFSymbol->Value))
							{
								// Read the symbol name.
								char *SymName = calloc (MAX_SYM_LEN + 1, 1);
								TestMem (SymName);
								// COFF symbol names are stored in a strange way. This is how to get them.
								if (IsZero (CurCOFFSymbol->Name.StringRef.Zero))
								{
									const char *Str = COFFStringTable + ReadTI4 (CurCOFFSymbol->Name.StringRef.StringOffset);
									TestInFile (Str, char);
									strncpy (SymName, Str, MAX_SYM_LEN);
								}
								else
									strncpy (SymName, CurCOFFSymbol->Name.Name, 8);
								
								// Set up the reloc, without a specified symbol.
								// Handle negative relocs by searching for matching positive ones.
								if (Negative)
								{
									// Search for a matching positive reloc.
									RELOC *PositiveReloc = FindMatchingReloc (Section, Location, RelocSize, FALSE, NULL, RelocHint);
									
									// We can only do something with the negative reloc
									// if we have found a matching positive one.
									if (PositiveReloc)
									{
										// Make the positive reloc relative, with
										// this reloc's target as the relation.
										LOCATION *Relation = calloc (1, sizeof (LOCATION));
										TestMem (Relation);
										Relation->SymbolName = SymName;
										PositiveReloc->Relative = TRUE;
										PositiveReloc->Relation = Relation;
										HandleLocation (PositiveReloc, Relation);
										
										RelocHint = PositiveReloc;
									}
									else
										Warning (FileName, "Removing negative reloc at 0x%lX in section %ld to `%s' with no matching positive reloc.", (long) Location, (long) CurCOFFSectionNumber, SymName);
								}
								else // Positive
								{
									RELOC *Reloc = calloc (1, sizeof (RELOC));
									TestMem (Reloc);
									Reloc->Parent            = Section;
									Reloc->Location          = Location;
									Reloc->Target.SymbolName = SymName;
									Reloc->Size              = RelocSize;
									Reloc->Relative          = Relative;
									Reloc->Unoptimizable     = Unoptimizable;
									
									// Calculate the reloc's target offset
									// (which is written into the fixed offset).
									// The section contents contain the virtual
									// address of the target.
									if (Location >= 0 && Location + RelocSize <= Section->Size)
									{
										if (Section->Data)
										{
											Reloc->FixedOffset = ReadSTI (Section->Data + Location, RelocSize);
											memset (Section->Data + Location, 0, RelocSize);
										}
										else
											Warning (FileName, "Adding reloc %ld at 0x%lX in section %ld without data to `%s'.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, Reloc->Target.SymbolName);
									}
									else
										Warning (FileName, "Adding reloc %ld at 0x%lX outside of section %ld to `%s'.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, Reloc->Target.SymbolName);
									// Relative relocs contain the difference between
									// the virtual target address (which is 0 or some
									// small offset in this case) and the current
									// virtual address. So we have to add the current
									// virtual address to get only the offset.
									if (Relative)
										Reloc->FixedOffset += Location + SecInfo[CurCOFFSectionNumber].VAddr;
									
									if (RelocSize <= 1) {
										Reloc->FixedOffset = (SI1) Reloc->FixedOffset;
									} else if (RelocSize <= 2) {
										Reloc->FixedOffset = (SI2) Reloc->FixedOffset;
									}

									// Add the reloc to the section.
									InsertReloc (Section, Reloc);
								}
							}
							else
								Warning (FileName, "Ignoring reloc %ld at 0x%lX in section %ld to unimported symbol %ld.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, (long) SymNum);
						}
					}
					else
						Warning (FileName, "Ignoring reloc %ld at 0x%lX in section %ld with unknown type `0x%lX'.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, (long) (ReadTI2 (CurCOFFReloc->Type)));
				}
				else
					Warning (FileName, "Ignoring reloc %ld at 0x%lX in section %ld to nonexisting symbol %ld.", (long) CurCOFFRelocNumber, (long) COFFLocation, (long) CurCOFFSectionNumber, (long) SymNum);
			}
		}
	}
	
	// *** Finished ***
	
	// Free allocated memory and exit with a positive result.
	free (SymInfo);
	free (SecInfo);
	return TRUE;
	
#undef TestInFile
#undef IsInFile
#undef TestMem
#undef Fail
}

#endif /* COFF_SUPPORT */
