/* imp_ar.c: Routines to import an archive file

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

#include "imp_ar.h"

#include "../formats/ar.h"
#include "import.h"
#include "../manip.h"
#include "../special.h"

#include <string.h>
#include <stdlib.h>

// Add an archive to the program's list of available archives.
// Do not free the data related to the File parameter after this.
BOOLEAN AddArchiveFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName)
{
	// Check if memory allocation was successful.
#define TestMem(Ptr) ({ if (!(Ptr)) { Error (FileName, "Out of memory."); return FALSE; } })
	
	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) (((const I1 *) (Ptr)) >= File && ((const I1 *) (Ptr)) + sizeof (Type) <= File + FileSize)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Error (FileName, "Corrupt archive file."); return FALSE; } })
	
	if (IsArchiveFile (File, FileSize))
	{
		const AR_MEMBER_HEADER *SymbolTableHeader = (const AR_MEMBER_HEADER *) (File + AR_FILE_HEADER_SIZE);
		const AR_SYMBOL_TABLE_HEADER *SymbolTable = (const AR_SYMBOL_TABLE_HEADER *) (SymbolTableHeader + 1);
		TestInFile (SymbolTableHeader, AR_MEMBER_HEADER);
		
		// Check the magic string.
		if (!(strncmp (SymbolTableHeader->Magic, AR_MEMBER_MAGIC, sizeof (SymbolTableHeader->Magic))))
		{
			// Check whether the file contains a symbol table (Name is "/", padded with spaces).
			if (SymbolTableHeader->Name [0] == '/' && SymbolTableHeader->Name [1] == ' ')
			{
				COUNT SymCount;
				const char *SymbolTableStrings;
				
				TestInFile (&(SymbolTable->SymbolCount), TI4);
				TestInFile (&(SymbolTable->PSymbols), TI4 [SymCount = ReadTI4 (SymbolTable->SymbolCount)]);
				SymbolTableStrings = (const char *) (&(SymbolTable->PSymbols [SymCount]));
				
				// Add a new entry to the program's archive files.
				{
					ARCHIVE *Archive = calloc (1, sizeof (ARCHIVE));
					
					TestMem (Archive);
					
					Archive->Parent = Program;
					Archive->Data = File;
					Archive->Size = FileSize;
					Archive->FileName = FileName;
					Append (Program->Archives, Archive);
					
					{
						OFFSET CurSym;
						const char *CurStr = SymbolTableStrings;
						
						// Add all symbols to our own list of exported symbols.
						for (CurSym = 0; CurSym < SymCount; CurSym++)
						{
							TestInFile (CurStr, char);
							
							{
								ARCHIVE_SYMBOL *Symbol = calloc (1, sizeof (ARCHIVE_SYMBOL));
								
								TestMem (Symbol);
								
								Symbol->Parent = Archive;
								Symbol->Name = CurStr;
								Symbol->ObjectFile = GetArchiveObject (Archive, ReadTI4 (SymbolTable->PSymbols [CurSym]));
								if (strstr (CurStr, SYMOP_NOT))
									Symbol->ContainsInversion = TRUE;
								Append (Archive->Symbols, Symbol);
								CheckGlobalImports (Program, Symbol);
							}
							
							CurStr += strlen (CurStr) + 1;
						}
					}
				}
			
				return TRUE;
			}
			else
			{
				Error (FileName, "Archive has no symbol table.");
				return FALSE;
			}
		}
		else
		{
			Error (FileName, "Corrupt archive file (magic string mismatch).");
			return FALSE;
		}
	}
	else
	{
		Error (FileName, "Not an archive file.");
		return FALSE;
	}
	
#undef TestInFile
#undef IsInFile
#undef TestMem
}

// Import an object file inside an archive file into the internal data
// structures.
BOOLEAN ImportArchiveObject (PROGRAM *Program, ARCHIVE_OBJECT *Object)
{
	// Check if a given object with a given type is completely inside the file.
#define IsInFile(Ptr,Type) (((const I1 *) (Ptr)) >= Archive->Data && ((const I1 *) (Ptr)) + sizeof (Type) <= Archive->Data + Archive->Size)
#define TestInFile(Ptr,Type) ({ if (!(IsInFile (Ptr, Type))) { Error (Archive->FileName, "Corrupt archive file."); return FALSE; } })
	
	// Safety check for the ImportArchiveSymbol macro.
	if (!Object)
		return FALSE;
	
	// Do not import a file twice.
	if (!(Object->Imported))
	{
		ARCHIVE *Archive = Object->Parent;
		
		// Find the location of the appropriate object file.
		const AR_MEMBER_HEADER *ObjectFileHeader = (const AR_MEMBER_HEADER *) (Archive->Data + Object->FileOffset);
		const I1 *ObjectFile = (const I1 *) (ObjectFileHeader + 1);
		SIZE ObjectFileSize;
		
		TestInFile (ObjectFileHeader, AR_MEMBER_HEADER);
		
		// Check the magic string.
		if (!(strncmp (ObjectFileHeader->Magic, AR_MEMBER_MAGIC, sizeof (ObjectFileHeader->Magic))))
		{
			// Get the size of the file.
			ObjectFileSize = strtol (ObjectFileHeader->Size, NULL, 10);
			
			// Check whether the file is completely inside the archive.
			TestInFile (ObjectFile, I1 [ObjectFileSize]);
			
			if (ObjectFileSize > 0)
			{
				// Mark the file as imported.
				Object->Imported = TRUE;
				
				// Import the file.
				return (ImportObjectFile (Program, ObjectFile, ObjectFileSize, Archive->FileName));
			}
			else
			{
				Error (Archive->FileName, "Corrupt object in archive.");
				return FALSE;
			}
		}
		else
		{
			Error (Archive->FileName, "Corrupt archive file (magic string mismatch).");
			return FALSE;
		}
	}
	else
		return TRUE;
	
#undef TestInFile
#undef IsInFile
}
