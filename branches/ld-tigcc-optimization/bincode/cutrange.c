/* fix_cutr.c: Routines for range cutting

   Copyright (C) 2003-2005 Kevin Kofler
   Portions taken from manip.c, Copyright (C) 2002-2003 Sebastian Reichelt

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

#include "cutrange.h"

#include "../manip.h"

#include <stdlib.h>
#include <string.h>

// Check whether the segments which overlap with the interval [Start,End)
// allow range-cutting.
BOOLEAN CanCutRange (SECTION *Section, OFFSET Start, OFFSET End)
{
	// If the section is as a whole does not allow range-cutting,
	// the individual segments implicitly do not allow this either.
	if (!(Section->CanCutRanges))
		return FALSE;
	else
	{
		SEGMENT *Segment;
		for (Segment = FindSegment (Section, Start, End); Segment && (Segment->Location.Start->Location < End); Segment = GetNext (Segment))
		{
			if (!(Segment->CanCutRanges))
				return FALSE;
		}
		return TRUE;
	}
}

// Adjust a referenced location to reflect the removal of the
// specified range. The location needs to be adjusted if the range
// is between the location's symbol and the place it actually points
// to.
static void AdjustLocationForRangeCut (RELOC *Reloc, LOCATION *Location, OFFSET Start, OFFSET End)
{
	// Length of the range that has been cut.
	OFFSET RangeLength = End - Start;
	
	// Get the location of the target symbol and of the actual target.
	OFFSET SymbolLocation = Location->Symbol->Location;
	OFFSET TargetLocation = SymbolLocation + Location->Offset;
	
	// Make sure the symbol is not in the range we will cut.
	if (!((SymbolLocation > Start) && (SymbolLocation < End)))
	{
		// Make sure the target is not in the range we will cut.
		if ((TargetLocation > Start) && (TargetLocation < End))
		{
			Warning (GetFileName (Reloc->Parent, Reloc->Location), "Reloc at 0x%lX in section `%s' pointing to %s%+ld in a range which is being optimized away.",
					 (unsigned long) Reloc->Location, Reloc->Parent->SectionSymbol->Name, Location->Symbol->Name, (long) Location->Offset);
			// Set location value to Start.
			Location->Offset = Start - SymbolLocation;
		}
		else
		{
			// We will adjust the symbol address in the next step, if
			// the range that has been cut is between SymbolLocation and
			// TargetLocation. Two cases need adjustment now:
			// 1. The target is >= End, but the actual symbol is <= Start.
			//    This implies that the offset is positive.
			//    We need to subtract RangeLength from the offset, moving
			//    the target closer to the symbol.
			// 2. The target is <= Start, but the actual symbol is >= End.
			//    This implies that the offset is negative.
			//    We need to add RangeLength to the offset, moving the
			//    target closer to the symbol.
			
			// Case 1:
			if ((TargetLocation >= End) && (SymbolLocation <= Start))
				Location->Offset -= RangeLength;
			// Case 2:
			else if ((TargetLocation <= Start) && (SymbolLocation >= End))
				Location->Offset += RangeLength;
		}
	}
}

// Cut the specified range (the interval [Start,End)) out of
// the specified section. All items within this range are removed. All relocs
// refering to targets or relations behind End (taking Symbol and Offset,
// but NOT FixedOffset, into account) are adjusted.
void CutRange (SECTION *Section, OFFSET Start, OFFSET End)
{
	PROGRAM *Program = Section->Parent;
	
	// Length of the range about to be cut.
	OFFSET RangeLength = End - Start;
	
	// Adjust the size.
	SIZE NewSize = Section->Size - RangeLength;
	
	if (Section->Data)
	{
		// Move the data.
		memmove (Section->Data + Start, Section->Data + End, Section->Size - End);
	}
	
	Section->Size = NewSize;
	
	// Adjust reloc targets and locations. We need to adjust relocs _targeting_
	// this section, so we need to check each section in the program.
	{
		SECTION *RelocSection;
		
		// For each section...
		for_each (RelocSection, Program->Sections)
		{
			RELOC *Reloc;
			
			// For each reloc...
			for_each (Reloc, RelocSection->Relocs)
			{
				// Only adjust relocs targeting this section.
				if (Reloc->Target.Symbol && (Reloc->Target.Symbol->Parent == Section))
					AdjustLocationForRangeCut (Reloc, &(Reloc->Target), Start, End);
				
				// Only adjust relations targeting this section.
				if (Reloc->Relation && Reloc->Relation->Symbol && (Reloc->Relation->Symbol->Parent == Section))
					AdjustLocationForRangeCut (Reloc, Reloc->Relation, Start, End);
			}
		}
	}
	
	// Now adjust symbol positions.
	{
		SYMBOL *Symbol;
		
		// For each symbol inside or behind the range...
		// (Ignore symbols at Start itself, they are not really inside the
		// range and cause spurious warnings. So start the search at Start + 1.)
		for (Symbol = FindSymbolAtPos (Section, Start + 1, TRUE); Symbol; Symbol = GetNext (Symbol))
		{
			// If the position was inside the range, put it at Start.
			if (Symbol->Location < End)
			{
				Symbol->Location = Start;
				Warning (GetFileName (Section, Symbol->Location), "Symbol `%s' at 0x%lX in section `%s' in a range which is being optimized away.", Symbol->Name, (unsigned long) Symbol->Location, Section->SectionSymbol->Name);
			}
			// If the position was behind the range, adjust it.
			else
				Symbol->Location -= RangeLength;
		}
	}
	
	// Now adjust the positions of all non-symbol items.
	// Define a macro to make moving items more simple.
#define MoveItems(Type,Item,ItemName) \
({ \
	Type *Item; \
	for (Item = Find##Item##AtPos (Section, Start, TRUE); Item; Item = GetNext (Item)) \
	{ \
		if (Item->Location + Item->Size > Start) \
		{ \
			if (Item->Location < End) \
			{ \
				Warning (GetFileName (Item->Parent, Item->Location), "%s at 0x%lX in section `%s' in a range which is being optimized away.", ItemName, (unsigned long) Item->Location, Section->SectionSymbol->Name); \
				if (Item->Location > Start) \
					Item->Location = Start; \
			} \
			else \
				Item->Location -= RangeLength; \
		} \
	} \
})
	
	MoveItems (RELOC,    Reloc,   "Reloc");
	MoveItems (ROM_CALL, ROMCall, "ROM call");
	MoveItems (RAM_CALL, RAMCall, "RAM call");
	MoveItems (LIB_CALL, LibCall, "Library reference");
	
#undef MoveItems
}

// Do the cleanup needed after cutting ranges in a section.
void FinalizeRangeCutting (SECTION *Section)
{
	if (Section->Data)
	{
		// Reduce the allocated size.
		I1 *NewData = realloc (Section->Data, Section->Size);
		
		// If NewData returns NULL, this means that either there was a weird "out
		// of memory" error (in this case, Section->Data is still valid), or
		// NewSize was 0. In the second case, it is all right to set Section->Data
		// to NULL.
		if (NewData || (!(Section->Size)))
			Section->Data = NewData;
	}
}
