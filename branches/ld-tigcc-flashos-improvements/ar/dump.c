/* dump.c: Routines to dump the internal data to a file

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

#include "dump.h"

#include <stdlib.h>
#include <string.h>

#define FixIndent \
	SIZE NewLen = (Indent ? strlen (Indent) : 0) + 2; \
	char NextIndent[NewLen+1]; \
	if (!Indent) \
		Indent = ""; \
	strcpy (NextIndent, Indent); \
	NextIndent [NewLen - 2] = ' '; \
	NextIndent [NewLen - 1] = ' '; \
	NextIndent [NewLen] = 0;

void DumpArchive (FILE *File, const char *Indent, const ARCHIVE *Archive)
{
	OBJECT_FILE *ObjectFile;
	
	FixIndent;
	
	for_each (ObjectFile, Archive->ObjectFiles)
	{
		if (ObjectFile->FileName)
			fprintf (File, "%s%s\n", Indent, ObjectFile->FileName);
		else
			fprintf (File, "%s(unnamed)\n", Indent);
		
		DumpObjectFile (File, NextIndent, ObjectFile);
	}
}

void DumpObjectFile (FILE *File, const char *Indent, const OBJECT_FILE *ObjectFile)
{
	SYMBOL *Symbol;
	
	FixIndent;
	
	for_each (Symbol, ObjectFile->Symbols)
	{
		fprintf (File, "%s%s\n", Indent, Symbol->Name);
	}
}
