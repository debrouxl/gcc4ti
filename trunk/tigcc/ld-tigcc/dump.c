/* dump.c: Routines to dump the internal data to a file

   Copyright (C) 2002-2004 Sebastian Reichelt
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

#include "dump.h"

#ifdef ENABLE_DUMP

#include "integers.h"
#include "manip.h"

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

static void PrintOffset (FILE *File, OFFSET Offset)
{
	if (Offset)
	{
		if (Offset > 0)
			fprintf (File, " + %ld", (long) Offset);
		else
			fprintf (File, " - %ld", (long) (-Offset));
	}
}

void DumpProgram (FILE *File, const char *Indent, const PROGRAM *Program)
{
	const SECTION *Section;
	const GLOBAL_IMPORT *GlobalImport;
	
	FixIndent;
	
	if (Program->Frozen)
		fprintf (File, "%sFrozen\n", Indent);
	
	for_each (Section, Program->Sections)
	{
		fprintf (File, "%sSection\n", Indent);
		DumpSection (File, NextIndent, Section);
	}
	
	if (!(IsEmpty (Program->GlobalImports)))
	{
		fprintf (File, "%sGlobal Imports\n", Indent);
		for_each (GlobalImport, Program->GlobalImports)
			fprintf (File, "%s  %s\n", Indent, GlobalImport->SymbolName);
	}
}

void DumpSection (FILE *File, const char *Indent, const SECTION *Section)
{
	FixIndent;
	
	if (Section->FileName)
		fprintf (File, "%sFile: %s\n", Indent, Section->FileName);
	if (Section->StartupNumber)
		fprintf (File, "%sStartup Section: %ld\n", Indent, (long) (Section->StartupNumber));
	if (Section->Essential)
		fprintf (File, "%sEssential\n", Indent);
	if (Section->Frozen)
		fprintf (File, "%sFrozen\n", Indent);
	if (Section->CanCutRanges)
		fprintf (File, "%sCan Cut Ranges\n", Indent);
	if (Section->Mergeable)
		fprintf (File, "%sMergeable\n", Indent);
	if (Section->Unaligned)
		fprintf (File, "%sUnaligned\n", Indent);
	fprintf (File, Section->Code ? "%sCode\n" : "%sData\n", Indent);
	
	{
		const SEGMENT  *NextSegment = GetFirst (Section->Segments);
		const SYMBOL   *NextSymbol  = GetFirst (Section->Symbols);
		const RELOC    *NextReloc   = GetFirst (Section->Relocs);
		const ROM_CALL *NextROMCall = GetFirst (Section->ROMCalls);
		const RAM_CALL *NextRAMCall = GetFirst (Section->RAMCalls);
		const LIB_CALL *NextLibCall = GetFirst (Section->LibCalls);
		
		OFFSET CurPos = 0;
		SIZE SectionSize = Section->Size;
		
		while ((CurPos < SectionSize) || NextSegment || NextSymbol || NextReloc || NextROMCall || NextRAMCall || NextLibCall)
		{
			OFFSET NextPos = MAX_OFFSET;
			OFFSET StartPos;
			
			const char *CheckPos (void)
			{
				return ((CurPos == StartPos) && (CurPos <= SectionSize) ? "" : " (!)");
			}
			
			// Segments
			while (NextSegment && (CurPos >= (StartPos = NextSegment->Location.Start->Location)))
			{
				fprintf (File, "%s%s Segment: %s (%s%s)\n", Indent, CheckPos (), NextSegment->FileName ? : "", NextSegment->Code ? "Code" : "Data", NextSegment->CanCutRanges ? ", Can Cut Ranges" : "");
				NextSegment = GetNext (NextSegment);
			}
			if (NextSegment && (NextPos > NextSegment->Location.Start->Location))
				NextPos = NextSegment->Location.Start->Location;
			
			// Symbols
			while (NextSymbol && (CurPos >= (StartPos = NextSymbol->Location)))
			{
				fprintf (File, "%s%s %s:%s\n", Indent, CheckPos (), NextSymbol->Name, NextSymbol->Exported ? "" : " (local)");
				NextSymbol = GetNext (NextSymbol);
			}
			if (NextSymbol && (NextPos > NextSymbol->Location))
				NextPos = NextSymbol->Location;
			
			// Relocs
			if (NextReloc && (CurPos >= (StartPos = NextReloc->Location)))
			{
				OFFSET EndPos = StartPos + NextReloc->Size;
				fprintf (File, "%s 0x%.06lX%s: <%ldB%s: %s%s", NextIndent, (long) StartPos, CheckPos (), (long) NextReloc->Size, NextReloc->Unoptimizable ? " U" : "", NextReloc->Target.SymbolName, NextReloc->Target.Symbol ? (NextReloc->Target.Symbol->Parent == NextReloc->Parent ? "" : " (->)") : " (?)");
				if (NextReloc->Target.Offset)
					fprintf (File, "%+ld", (long) (NextReloc->Target.Offset));
				if (NextReloc->Relative)
				{
					if (NextReloc->Relation)
					{
						fprintf (File, " - %s%s%s", NextReloc->Relation->Offset ? "(" : "", NextReloc->Relation->SymbolName, NextReloc->Relation->Symbol ? (NextReloc->Relation->Symbol->Parent == NextReloc->Parent ? "" : " (->)") : " (?)");
						if (NextReloc->Relation->Offset)
							fprintf (File, "%+ld)", (long) (NextReloc->Relation->Offset));
					}
					else
						fprintf (File, " (rel)");
				}
				PrintOffset (File, NextReloc->FixedOffset);
				fprintf (File, ">%s\n", IsZeroDataRange (Section, StartPos, EndPos) ? "" : " (!)");
				if (CurPos < EndPos)
					CurPos = EndPos;
				NextReloc = GetNext (NextReloc);
			}
			if (NextReloc && (NextPos > NextReloc->Location))
				NextPos = NextReloc->Location;
			
			// ROM Calls
			if (NextROMCall && (CurPos >= (StartPos = NextROMCall->Location)))
			{
				OFFSET EndPos = StartPos + NextROMCall->Size;
				fprintf (File, "%s 0x%.06lX%s: <%ldB: ROM Call 0x%lX", NextIndent, (long) StartPos, CheckPos (), (long) NextROMCall->Size, (long) (NextROMCall->Number));
				PrintOffset (File, NextROMCall->FixedOffset);
				fprintf (File, ">%s\n", IsZeroDataRange (Section, StartPos, EndPos) ? "" : " (!)");
				if (CurPos < EndPos)
					CurPos = EndPos;
				NextROMCall = GetNext (NextROMCall);
			}
			if (NextROMCall && (NextPos > NextROMCall->Location))
				NextPos = NextROMCall->Location;
			
			// RAM Calls
			if (NextRAMCall && (CurPos >= (StartPos = NextRAMCall->Location)))
			{
				OFFSET EndPos = StartPos + NextRAMCall->Size;
				fprintf (File, "%s 0x%.06lX%s: <%ldB: RAM Call 0x%lX", NextIndent, (long) StartPos, CheckPos (), (long) NextRAMCall->Size, (long) (NextRAMCall->Number));
				PrintOffset (File, NextRAMCall->FixedOffset);
				fprintf (File, ">%s\n", IsZeroDataRange (Section, StartPos, EndPos) ? "" : " (!)");
				if (CurPos < EndPos)
					CurPos = EndPos;
				NextRAMCall = GetNext (NextRAMCall);
			}
			if (NextRAMCall && (NextPos > NextRAMCall->Location))
				NextPos = NextRAMCall->Location;
			
			// Library Calls
			if (NextLibCall && (CurPos >= (StartPos = NextLibCall->Location)))
			{
				OFFSET EndPos = StartPos + NextLibCall->Size;
				fprintf (File, "%s 0x%.06lX%s: <%ldB: Library Call %s::0x%lX", NextIndent, (long) StartPos, CheckPos (), (long) NextLibCall->Size, NextLibCall->Library->Name, (long) (NextLibCall->Number));
				PrintOffset (File, NextLibCall->FixedOffset);
				fprintf (File, ">%s\n", IsZeroDataRange (Section, StartPos, EndPos) ? "" : " (!)");
				if (CurPos < EndPos)
					CurPos = EndPos;
				NextLibCall = GetNext (NextLibCall);
			}
			if (NextLibCall && (NextPos > NextLibCall->Location))
				NextPos = NextLibCall->Location;
			
			if (CurPos > SectionSize)
				fprintf (File, "%s (Section Size Exceeded!)\n", NextIndent);
			
			// Section Data
			if (CurPos < NextPos)
			{
				OFFSET EndPos = NextPos;
				if (EndPos > SectionSize)
					EndPos = SectionSize;
				if (CurPos < EndPos)
				{
					if (Section->Data)
						DumpData (File, NextIndent, Section->Data, CurPos, EndPos);
					else
						DumpDummyData (File, NextIndent, Section->Initialized ? "00" : "??", CurPos, EndPos);
				}
				CurPos = NextPos;
			}
		}
	}
}

void DumpData (FILE *File, const char *Indent, const I1 *Data, OFFSET Start, OFFSET End)
{
	OFFSET CurPos;
	
	FixIndent;
	
	fprintf (File, "%s 0x%.06lX:", Indent, (long) Start);
	for (CurPos = Start; CurPos < End; CurPos++)
		fprintf (File, " %.02hX", (short) (Data [CurPos]));
	fprintf (File, "\n");
}

void DumpDummyData (FILE *File, const char *Indent, const char *Dummy, OFFSET Start, OFFSET End)
{
	OFFSET CurPos;
	
	FixIndent;
	
	fprintf (File, "%s 0x%.06lX:", Indent, (long) Start);
	for (CurPos = Start; CurPos < End; CurPos++)
		fprintf (File, " %s", Dummy);
	fprintf (File, "\n");
}

#endif /* ENABLE_DUMP */
