/* imp_coff.c: Routines to import a COFF file

   Copyright (C) 2003 Sebastian Reichelt

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

#include "../../formats/coff.h"

#include <stdlib.h>
#include <string.h>

// Import the exported symbols of a COFF file.
BOOLEAN ArImportCOFFFile (OBJECT_FILE *ObjectFile)
{
	// Call this for a nice and clean failing exit.
#define Fail() ({ return FALSE; })
#define TestMem(Ptr) ({ if (!(Ptr)) { Error (FileName, "Out of memory."); Fail (); } })

	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) ((((const I1 *) (Ptr)) >= File) && (((const I1 *) (Ptr)) + sizeof (Type) <= File + FileSize))
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Error (FileName, "Corrupt COFF object file."); Fail (); } })

	// Local Variables
	const I1 *File = ObjectFile->Data;
	SIZE FileSize = ObjectFile->Size;
	const char *FileName = ObjectFile->FileName;
	COFF_INFO FileInfo;
	const COFF_HEADER *COFFHeader = (const COFF_HEADER *) File;
	const COFF_SECTIONS *COFFSections;
	const COFF_SYMBOLS *COFFSymbols;
	const char *COFFStringTable;
	OFFSET CurCOFFSymbolNumber;
	
	if (!File)
		return FALSE;
	
	// Create file information in FileInfo.
	CreateCoffInfo (*COFFHeader, FileInfo);
	// Get pointer to list of sections.
	COFFSections = (const COFF_SECTIONS *) (File + FileInfo.PSections);
	// Get pointer to list of symbols.
	COFFSymbols = (const COFF_SYMBOLS *) (File + FileInfo.PSymbols);
	// Get pointer to string table.
	COFFStringTable = (const char *) (File + FileInfo.PStrings);
	
	// For each symbol...
	for (CurCOFFSymbolNumber = 0; CurCOFFSymbolNumber < FileInfo.SymbolCount; CurCOFFSymbolNumber++)
	{
		// Get pointer to this symbol.
		const COFF_SYMBOL *CurCOFFSymbol = &((*COFFSymbols) [CurCOFFSymbolNumber]);
		TestInFile (CurCOFFSymbol, COFF_SYMBOL);
		
		{
			// Check whether the symbol is exported.
			if (ReadTI1 (CurCOFFSymbol->Class) == COFF_SYMBOL_EXTERNAL)
			{
				// Of course, a single attribute for exported symbols would
				// be too easy. In COFF, finding out whether it is exported
				// is a little complicated and dirty: If the symbol is
				// "external" but lives in a specific section, this means
				// that it is exported. If it does not live in a section, but
				// still has a value, it is a common symbol; I think we can
				// look at "external" common symbols as exported, too, even
				// though multiple files can export them at once. If it does
				// not have a section or value, it is either an imported
				// symbol (which should be ignored) or an exported symbol
				// which is declared but never defined (and since this is a
				// convenient way of passing information to the linker
				// without cluttering the source code, we should not ignore
				// this kind of symbol). The only way to distinguish these
				// two kinds of symbols seems to be to look for a reloc using
				// the symbol. If a reloc exists, it is definitely an
				// imported symbol. If none exists, it is hopefully an
				// exported symbol.
				
				BOOLEAN Exported = TRUE;
				
				if ((IsZeroI2 (CurCOFFSymbol->Section)) && (IsZeroI4 (CurCOFFSymbol->Value)))
				{
					OFFSET CurCOFFSectionNumber;
					
					// For each section...
					for (CurCOFFSectionNumber = 0; (CurCOFFSectionNumber < FileInfo.SectionCount) && Exported; CurCOFFSectionNumber++)
					{
						// Get pointer to this section.
						const COFF_SECTION *CurCOFFSection = &((*COFFSections) [CurCOFFSectionNumber]);
						TestInFile (CurCOFFSection, COFF_SECTION);
						
						{
							// Get info about relocs.
							COUNT RelocCount = ReadTI2 (CurCOFFSection->RelocCount);
							const COFF_RELOCS *CurCOFFRelocs = (const COFF_RELOCS *) (File + ReadTI4 (CurCOFFSection->PRelocs));
							OFFSET CurCOFFRelocNumber;
							
							// For each reloc...
							for (CurCOFFRelocNumber = 0; CurCOFFRelocNumber < RelocCount; CurCOFFRelocNumber++)
							{
								// Get pointer to this reloc.
								const COFF_RELOC *CurCOFFReloc = &((*CurCOFFRelocs) [CurCOFFRelocNumber]);
								TestInFile (CurCOFFReloc, COFF_RELOC);
								
								// Test if the reloc points to the current symbol.
								if ((OFFSET) (ReadTI4 (CurCOFFReloc->Symbol)) == CurCOFFSymbolNumber)
								{
									// It did. So the symbol is imported, not exported.
									Exported = FALSE;
									break;
								}
							}
						}
					}
				}
				
				// Only add symbols which are really exported.
				if (Exported)
				{
					// Create a new symbol inside the archive.
					SYMBOL *Symbol = calloc (1, sizeof (SYMBOL));
					TestMem (Symbol);
					
					Symbol->Parent = ObjectFile;
					
					// COFF symbol names are stored in a strange way. This is how to get them.
					if (IsZero (CurCOFFSymbol->Name.StringRef.Zero))
					{
						const char *Str = COFFStringTable + ReadTI4 (CurCOFFSymbol->Name.StringRef.StringOffset);
						TestInFile (Str, char);
						strncpy (Symbol->Name, Str, MAX_SYM_LEN);
					}
					else
						strncpy (Symbol->Name, CurCOFFSymbol->Name.Name, 8);
					Symbol->NameLength = strlen (Symbol->Name);
					
					// Add the symbol to the linked list.
					Append (ObjectFile->Symbols, Symbol);
					ObjectFile->Parent->SymbolCount++;
				}
			}
		}
		
		// Skip corresponding auxiliary symbols.
		CurCOFFSymbolNumber += ReadTI1 (CurCOFFSymbol->AuxSymbolCount);
	}
	
	// Exit with a positive result.
	return TRUE;
	
#undef TestInFile
#undef IsInFile
#undef TestMem
#undef Fail
}

#endif /* COFF_SUPPORT */
