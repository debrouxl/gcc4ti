/* main.c: Main entry point for ar-tigcc, handling the command line input

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

#include "../generic.h"
#include "../intrface.h"
#include "data.h"
#include "manip.h"
#include "import/import.h"
#include "export/exp_ar.h"

#ifdef ENABLE_DUMP
#include "dump.h"
#endif /* ENABLE_DUMP */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __WIN32__
// This definition seems to be missing in MinGW.
extern void *alloca (size_t);
#endif /* __WIN32__ */

#define RESULT_OK             0
#define RESULT_GENERAL_ERROR  1
#define RESULT_EXPORT_ERROR   2
#define RESULT_STRANGE_ERROR  3

#ifdef TARGET_EMBEDDED
extern ERROR_FUNCTION ErrorFunction;
EXP_CREATE_ARCHIVE ()
{
	const char **CurFile;
#else /* !TARGET_EMBEDDED */
int main (int ArgCount, const char **Args)
{
	int CurArg;
	const char *DestFile = NULL;
	BOOLEAN NoNames = FALSE;
#endif /* !TARGET_EMBEDDED */
	
	int Result = RESULT_GENERAL_ERROR;
#ifdef ENABLE_DUMP
	BOOLEAN Dump = FALSE;
#endif /* ENABLE_DUMP */
	
	ARCHIVE Archive;
	
	// Check the sizes of basic integer types.
	if (sizeof (I1) != 1 || sizeof (I2) != 2 || sizeof (I4) != 4 || sizeof (SI1) != 1 || sizeof (SI2) != 2 || sizeof (SI4) != 4 || sizeof (OFFSET) < sizeof (SI4))
	{
		Error (NULL, "Generic type size error!");
		return RESULT_STRANGE_ERROR;
	}
	
	// Initialize.
	memset (&Archive, 0, sizeof (Archive));
#ifdef TARGET_EMBEDDED
	ErrorFunction = ErrorMessage;
#endif /* TARGET_EMBEDDED */
	
#ifdef TARGET_EMBEDDED
	for (CurFile = ObjectFiles; *CurFile; CurFile++)
	{
		FILE *File = fopen (*CurFile, "rb");
		if (File)
		{
			SIZE Size;
			fseek (File, 0, SEEK_END);
			Size = ftell (File);
			rewind (File);
			{
				I1 *Data = malloc (Size);
				if (Data)
				{
					if (fread (Data, Size, 1, File) == 1)
					{
						// Create a new object file inside the current archive.
						OBJECT_FILE *ObjectFile = calloc (1, sizeof (OBJECT_FILE));
						
						if (!ObjectFile)
						{
							Error (*CurFile, "Out of memory.");
							break;
						}
						
						ObjectFile->Parent = &Archive;
						ObjectFile->Data = Data;
						ObjectFile->Size = Size;
						ObjectFile->FileName = *CurFile;
						GetFileStats (*CurFile, ObjectFile->FileStats);
						Append (Archive.ObjectFiles, ObjectFile);
						
						// Update the statistics of the archive accordingly.
						if (StatFileIsNewer (ObjectFile->FileStats, Archive.FileStats))
							StatCopyAttributes (Archive.FileStats, ObjectFile->FileStats);
						
						// Try to import the object file's contents.
						ArImportObjectFile (ObjectFile);
					}
					else
						Error (*CurFile, "Unable to read file.");
				}
				else
					Error (*CurFile, "Not enough memory to load file.");
			}
			fclose (File);
		}
		else
			Error (*CurFile, "Unable to open file.");
	}
#else /* !TARGET_EMBEDDED */
#include "main_opt.inc"
#endif /* !TARGET_EMBEDDED */
	
	{
		OBJECT_FILE *FirstFile = GetFirst (Archive.ObjectFiles);
		
		if (FirstFile)
		{
			// Set the destination file, in case it has not been set yet.
			if (!DestFile)
			{
				// alloca should not fail in any case, so we don't generate an error message.
				DestFile = alloca (strlen (FirstFile->FileName) + 2 + 1);
				if (DestFile)
				{
					strcpy ((char *) DestFile, FirstFile->FileName);
					strcat ((char *) DestFile, ".a");
				}
			}
			
#ifdef ENABLE_DUMP
			if (Dump)
				DumpArchive (stdout, NULL, &Archive);
#endif /* ENABLE_DUMP */
			
			// Create the symbol table information.
			CreateSymbolTable (&Archive);
			
			// Create other necessary information.
			FillExportHelpFields (&Archive);
			
			if (DestFile && (GetArchiveFileSize (&Archive) > 0))
			{
				// Write the archive.
				FILE *File = fopen (DestFile, "wb");
				if (File)
				{
					BOOLEAN ThisResult;
					if ((ThisResult = ExportArchiveFile (&Archive, File, NoNames)))
					{
						if (Result == RESULT_GENERAL_ERROR)
							Result = RESULT_OK;
					}
					else
						Result = RESULT_EXPORT_ERROR;
					fclose (File);
					if (!ThisResult)
						remove (DestFile);
				}
				else
					Error (DestFile, "Unable to create file.");
			}
			else
				Result = RESULT_EXPORT_ERROR;
		}
		else
			Error (DestFile, "Cannot create empty archive.");
	}
		
Cleanup: ATTRIBUTE_UNUSED
	// Final Cleanup.
	FreeArchive (&Archive);
	
	return Result;
}
