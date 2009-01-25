/* manip.c: Routines to manipulate the internal data

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

#include "manip.h"

#include <stdlib.h>
#include <string.h>

// Free the archive tree.
void FreeArchive (ARCHIVE *Archive)
{
	OBJECT_FILE *ObjectFile, *NextObjectFile;
	
	// Free symbol table, if one exists.
	if (Archive->SymbolTable)
		free (Archive->SymbolTable);
	
	// Free all object files.
	for (ObjectFile = GetLast (Archive->ObjectFiles); ObjectFile; ObjectFile = NextObjectFile)
	{
		SYMBOL *Symbol, *NextSymbol;
		
		NextObjectFile = GetPrev (ObjectFile);
		
		if (ObjectFile->Data)
			free (ObjectFile->Data);
		
		// Free all symbols.
		for (Symbol = GetLast (ObjectFile->Symbols); Symbol; Symbol = NextSymbol)
		{
			NextSymbol = GetPrev (Symbol);
			free (Symbol);
		}
		
		free (ObjectFile);
	}
	
	// Set all data to 0.
	memset (Archive, 0, sizeof (ARCHIVE));
}

// Create a symbol table for an archive.
BOOLEAN CreateSymbolTable (ARCHIVE *Archive)
{
	if (Archive->SymbolTable)
	{
		free (Archive->SymbolTable);
		Archive->SymbolTable = NULL;
	}
	
	// Only create a symbol table if the file contains symbols.
	if (Archive->SymbolCount > 0)
	{
		SYMBOL_TABLE *SymbolTable = calloc (1, sizeof (SYMBOL_TABLE) + (Archive->SymbolCount * sizeof (SYMBOL *)));
		
		if (SymbolTable)
		{
			OBJECT_FILE *ObjectFile;
			
			SymbolTable->Parent = Archive;
			SymbolTable->Size = 4; // The first 4 bytes represent the number of symbols.
			
			// Add all symbols to the symbol table.
			for_each (ObjectFile, Archive->ObjectFiles)
			{
				SYMBOL *Symbol;
				for_each (Symbol, ObjectFile->Symbols)
				{
					if (SymbolTable->SymbolCount < Archive->SymbolCount)
					{
						SymbolTable->Symbols [SymbolTable->SymbolCount++] = Symbol;
						SymbolTable->Size += 4 + Symbol->NameLength + 1; // 4 bytes for the file offset, 1 byte for the terminating null character.
					}
					else
					{
						Error (NULL, "Internal symbol table inconsistency.");
						goto BreakSymbolLoop;
					}
				}
			}
BreakSymbolLoop:
			
			Archive->SymbolTable = SymbolTable;
			
			return TRUE;
		}
		else
			Error (NULL, "Not enough memory to create symbol table.");
	}
	
	return FALSE;
}

// Fill the "export help" fields of the archive.
BOOLEAN FillExportHelpFields (ARCHIVE *Archive)
{
	// The symbol table (if it exists) is always the first member.
	if (Archive->SymbolTable)
	{
		Archive->SymbolTable->ArMemberOffset = GetArStartOffset ();
		Archive->SymbolTable->ArMemberSize = GetArMemberSize (Archive->SymbolTable->Size);
	}
	
	// Get the dimensions of each archive member.
	{
		OBJECT_FILE *ObjectFile, *PrevObjectFile = NULL;
		
		for_each (ObjectFile, Archive->ObjectFiles)
		{
			if (PrevObjectFile)
				ObjectFile->ArMemberOffset = PrevObjectFile->ArMemberOffset + PrevObjectFile->ArMemberSize;
			else if (Archive->SymbolTable)
				ObjectFile->ArMemberOffset = Archive->SymbolTable->ArMemberOffset + Archive->SymbolTable->ArMemberSize;
			else
				ObjectFile->ArMemberOffset = GetArStartOffset ();
			
			ObjectFile->ArMemberSize = GetArMemberSize (ObjectFile->Size);
			
			PrevObjectFile = ObjectFile;
		}
		
		// Update the size of the archive.
		if (PrevObjectFile)
			Archive->ArFileSize = PrevObjectFile->ArMemberOffset + PrevObjectFile->ArMemberSize;
	}
	
	return TRUE;
}
