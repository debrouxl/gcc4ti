/* exp_ar.c: Routines to export an archive file

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

#include "exp_ar.h"

#include "../../formats/ar.h"

#include <string.h>

// Export the internal data structures into an archive file.
BOOLEAN ExportArchiveFile (const ARCHIVE *Archive, EXP_FILE *File, BOOLEAN NoNames)
{
	// Call this instead of sprintf to overwrite the terminating null character with a space.
#define iprintf(str,fmt...) (str [sprintf (str, fmt)] = ' ')
	
	SYMBOL_TABLE *SymbolTable = Archive->SymbolTable;
	
	// Write the file header.
	ExportWrite (File, AR_FILE_HEADER_STRING, AR_FILE_HEADER_SIZE, 1);
	
	// Write the symbol table.
	if (SymbolTable)
	{
		AR_MEMBER_HEADER MemberHeader;
		
		// Fill the entire header with spaces.
		memset (&MemberHeader, ' ', sizeof (MemberHeader));
		
		MemberHeader.Name [0] = '/';
		
		// Insert the attributes
		iprintf (MemberHeader.Date, "%lu", (long) (StatGetModificationTime (Archive->FileStats)));
		iprintf (MemberHeader.UID, "%lu", (long) (StatGetUID (Archive->FileStats)));
		iprintf (MemberHeader.GID, "%lu", (long) (StatGetGID (Archive->FileStats)));
		iprintf (MemberHeader.Mode, "%lo", (long) (StatGetMode (Archive->FileStats)));
		iprintf (MemberHeader.Size, "%lu", (long) (SymbolTable->Size));
		memcpy (MemberHeader.Magic, AR_MEMBER_MAGIC, AR_MEMBER_MAGIC_SIZE);
		
		// Write the header.
		ExportWrite (File, &MemberHeader, AR_MEMBER_HEADER_SIZE, 1);
		
		// Write the data.
		{
			OFFSET CurSymbol;
			
			ExportWriteI4 (File, SymbolTable->SymbolCount);
			
			for (CurSymbol = 0; CurSymbol < SymbolTable->SymbolCount; CurSymbol++)
			{
				SYMBOL *Symbol = SymbolTable->Symbols [CurSymbol];
				if (Symbol)
					ExportWriteI4 (File, Symbol->Parent->ArMemberOffset);
			}
			
			for (CurSymbol = 0; CurSymbol < SymbolTable->SymbolCount; CurSymbol++)
			{
				SYMBOL *Symbol = SymbolTable->Symbols [CurSymbol];
				if (Symbol)
					ExportWrite (File, Symbol->Name, strlen (Symbol->Name) + 1, 1);
			}
		}
		
		// Write the padding.
		ExportFill (File, SymbolTable->ArMemberOffset + SymbolTable->ArMemberSize, AR_MEMBER_PADDING_CHAR);
	}
	
	// Write the members.
	{
		OBJECT_FILE *ObjectFile;
		OFFSET CurObjectFile = 1;
		
		for_each (ObjectFile, Archive->ObjectFiles)
		{
			AR_MEMBER_HEADER MemberHeader;
			
			// Fill the entire header with spaces.
			memset (&MemberHeader, ' ', sizeof (MemberHeader));
			
			if (NoNames)
				// Use an arbitrary name.
				iprintf (MemberHeader.Name, "fl%lu.o/", (long) CurObjectFile);
			else
			{
				SIZE NameLen;
				
				// Seach for the last slash.
				const char *Name = strrchr (ObjectFile->FileName, '/');
				// If none is found, use the last backslash instead.
				if (!Name)
					Name = strrchr (ObjectFile->FileName, '\\');
				
				// Use only the file name without a path.
				if (Name)
					Name++;
				else
					Name = ObjectFile->FileName;
				
				// Insert the name into the header.
				NameLen = strlen (Name);
				if (NameLen > AR_MEMBER_MAX_NAME_LENGTH)
					NameLen = AR_MEMBER_MAX_NAME_LENGTH;
				memcpy (MemberHeader.Name, Name, NameLen);
				MemberHeader.Name [NameLen] = '/';
			}
			
			// Insert the attributes
			iprintf (MemberHeader.Date, "%lu", (long) (StatGetModificationTime (ObjectFile->FileStats)));
			iprintf (MemberHeader.UID, "%lu", (long) (StatGetUID (ObjectFile->FileStats)));
			iprintf (MemberHeader.GID, "%lu", (long) (StatGetGID (ObjectFile->FileStats)));
			iprintf (MemberHeader.Mode, "%lo", (long) (StatGetMode (ObjectFile->FileStats)));
			iprintf (MemberHeader.Size, "%lu", (long) (ObjectFile->Size));
			memcpy (MemberHeader.Magic, AR_MEMBER_MAGIC, AR_MEMBER_MAGIC_SIZE);
			
			// Write the header.
			ExportWrite (File, &MemberHeader, AR_MEMBER_HEADER_SIZE, 1);
			
			// Write the data.
			ExportWrite (File, ObjectFile->Data, 1, ObjectFile->Size);
			
			// Write the padding.
			ExportFill (File, ObjectFile->ArMemberOffset + ObjectFile->ArMemberSize, AR_MEMBER_PADDING_CHAR);
			
			CurObjectFile++;
		}
	}
	
	return TRUE;
}
