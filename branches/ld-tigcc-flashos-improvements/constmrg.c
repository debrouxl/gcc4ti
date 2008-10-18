/* constmrg.c: Routines to merge constants

   Copyright (C) 2004 Kevin Kofler

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

#include "generic.h"
#include "data.h"
#include "constmrg.h"
#include "manip.h"
#include "bincode/cutrange.h"

#include <stdlib.h>
#include <string.h>

#ifndef min
#define min(a,b) ({typeof(a) __a = (a); typeof(b) __b = (b); (__a < __b) ? __a : __b;})
#endif
#ifndef max
#define max(a,b) ({typeof(a) __a = (a); typeof(b) __b = (b); (__a > __b) ? __a : __b;})
#endif

// Merge constants (including strings) in sections marked Mergeable to avoid
// duplication. A constant is considered to be an interval enclosed within 2
// label groups, where a label group is the equivalence class formed by the
// symbols at a given offset. A constant is said unaligned if the section
// containing it also has the Unaligned flag set, and aligned otherwise. The
// value of a constant is said to be the contents of the interval defining it.
// The algorithm used is a prefix merge algorithm:
// * If the value of a constant A is a prefix of the value of a constant B, and
//   A is unaligned and/or B is aligned, then A is identified with B. 
// * If the value of a constant A is a strict prefix of the value of a constant
//   B, A is aligned and B is unaligned, then B is split into a new aligned
//   section, and A is identified with the so-created B.
// Identification works the following way: The label group defining A is moved
// into the label group defining B. Then, if A was constituting an entire
// section, the section is deleted. Otherwise, A is removed using the
// range-cutting API.
// The linker doesn't look for identical constants in the same section. It is
// the compiler's job to merge constants in this case.
void MergeConstants (PROGRAM *Program)
{
	// Loop through all constants.
	SECTION *Section1, *NextSection1;
	for (Section1 = GetFirst (Program->Sections); Section1; Section1 = NextSection1)
	{
		SYMBOL *Symbol1, *NextSymbol1;
		NextSection1 = GetNext (Section1);
		if (!Section1->Mergeable) continue; // Ignore non-mergeable sections.
		for (Symbol1 = GetFirst(Section1->Symbols); Symbol1; Symbol1 = NextSymbol1)
		{
			// Compute the length of the constant.
			OFFSET Constant1Start = Symbol1->Location, Constant1End, Constant1Length;
			SECTION *Section2, *NextSection2;
			NextSymbol1 = FindSymbolAtPos (Section1, Symbol1->Location + 1, TRUE);
			if (Constant1Start < 0)
			{
				Warning (Section1->FileName, "Symbol at negative location -0x%lx",
				         - (long) Symbol1->Location);
				continue;
			}
			Constant1End = NextSymbol1 ? NextSymbol1->Location : Section1->Size;
			Constant1Length = Constant1End - Constant1Start;
			// Loop through all constants in the following sections.
			for (Section2 = NextSection1; Section2; Section2 = NextSection2)
			{
				SYMBOL *Symbol2, *NextSymbol2;
				NextSection2 = GetNext (Section2);
				if (!Section2->Mergeable) continue; // Ignore non-mergeable sections.
				for (Symbol2 = GetFirst(Section2->Symbols); Symbol2; Symbol2 = NextSymbol2)
				{
					// Compute the length of the constant.
					OFFSET Constant2Start = Symbol2->Location, Constant2End, Constant2Length;
					NextSymbol2 = FindSymbolAtPos (Section2, Symbol2->Location + 1, TRUE);
					if (Constant2Start < 0)
					{
						Warning (Section2->FileName, "Symbol at negative location -0x%lx",
						         - (long) Symbol2->Location);
						continue;
					}
					Constant2End = NextSymbol2 ? NextSymbol2->Location : Section2->Size;
					Constant2Length = Constant2End - Constant2Start;
					// Check if one constant is a prefix of the other.
					if (memcmp(Section1->Data + Constant1Start, Section2->Data + Constant2Start,
					           min (Constant1Length, Constant2Length)))
						continue;
					// Now we know it is. We must now decide which constant to merge into which.
					// Try direct merge of Constant2 into Constant1 first.
					if ((Constant2Length <= Constant1Length) && (!Section1->Unaligned || Section2->Unaligned))
					{
						// Move the label group of Constant2 into Constant1.
						SYMBOL *CurSymbol, *NextSymbol;
						for (CurSymbol = Symbol2; CurSymbol && CurSymbol->Location == Constant2Start; CurSymbol = NextSymbol)
						{
							NextSymbol = GetNext (CurSymbol);
							// Don't move the section symbol.
							if (CurSymbol == Section2->SectionSymbol) continue;
							Unlink (Section2->Symbols, CurSymbol);
							CurSymbol->Parent = Section1;
							CurSymbol->Location = Constant1Start;
							InsertBefore (Section1->Symbols, CurSymbol, NextSymbol1);
						}
						// Delete Constant2.
						if (Constant2Length == Section2->Size && !NextSymbol2)
						{
							// Move the section symbol. We are deleting the section, so we need to
							// put the symbol somewhere else. It should never be referenced anyway,
							// but if it is, we don't want the linker to segfault. Do this here,
							// because even when not explicitly skipping it in the loop above, it
							// won't always be caught by the loop.
							Unlink (Section2->Symbols, Section2->SectionSymbol);
							Section2->SectionSymbol->Parent = Section1;
							Section2->SectionSymbol->Location = Constant1Start;
							InsertBefore (Section1->Symbols, Section2->SectionSymbol, NextSymbol1);
							// If the Section2 was the next Section1, we must update NextSection1.
							if (Section2 == NextSection1)
								NextSection1 = GetNext (NextSection1);
							// Free the section.
							FreeSection (Section2);
						}
						else
						{
							// We don't check CanCutRange here. Mergeable ranges are supposed to be always cuttable.
							CutRange (Section2, Constant2Start, Constant2End);
							// Realloc the data now, we don't know if we will cut other ranges.
							FinalizeRangeCutting (Section2);
						}
						// Proceed normally. If we deleted Section2, then NextSymbol2 is NULL and we'll exit the inner loop
						// immediately. Otherwise, we can continue with NextSymbol2.
					}
					// Try direct merge of Constant1 into Constant2.
					else if ((Constant1Length <= Constant2Length) && (!Section2->Unaligned || Section1->Unaligned))
					{
						// Move the label group of Constant1 into Constant2.
						SYMBOL *CurSymbol, *NextSymbol;
						for (CurSymbol = Symbol1; CurSymbol && CurSymbol->Location == Constant1Start; CurSymbol = NextSymbol)
						{
							NextSymbol = GetNext (CurSymbol);
							// Don't move the section symbol.
							if (CurSymbol == Section1->SectionSymbol) continue;
							Unlink (Section1->Symbols, CurSymbol);
							CurSymbol->Parent = Section2;
							CurSymbol->Location = Constant2Start;
							InsertBefore (Section2->Symbols, CurSymbol, NextSymbol2);
						}
						// Delete Constant1.
						if (Constant1Length == Section1->Size && !NextSymbol1)
						{
							// Move the section symbol. We are deleting the section, so we need to
							// put the symbol somewhere else. It should never be referenced anyway,
							// but if it is, we don't want the linker to segfault. Do this here,
							// because even when not explicitly skipping it in the loop above, it
							// won't always be caught by the loop.
							Unlink (Section1->Symbols, Section1->SectionSymbol);
							Section1->SectionSymbol->Parent = Section2;
							Section1->SectionSymbol->Location = Constant2Start;
							InsertBefore (Section2->Symbols, Section1->SectionSymbol, NextSymbol2);
							// Free the section.
							FreeSection (Section1);
						}
						else
						{
							// We don't check CanCutRange here. Mergeable ranges are supposed to be always cuttable.
							CutRange (Section1, Constant1Start, Constant1End);
							// Realloc the data now, we don't know if we will cut other ranges.
							FinalizeRangeCutting (Section1);
						}
						// Exit the inner loops. We need a new Symbol1. (We will get back to Symbol2 later.)
						goto NewSymbol1;
					}
					// Otherwise we have to throw away both Constant1 and Constant2 and create a completely new section.
					else
					{
						// Create a new section, initialize it, and append it to the list of sections.
						SECTION *Section = calloc (1, sizeof (SECTION));
						if (!Section)
						{
OutOfMem:
							Warning (NULL, "Out of memory during constant merging.");
							return;
						}
						Section->Parent = Program;
						Section->Initialized = TRUE;
						Section->Mergeable = TRUE;
						Section->Size = max (Constant1Length, Constant2Length);
						Section->Data = malloc (Section->Size);
						if (!Section->Data)
						{
FreeAndOutOfMem:
							free (Section);
							goto OutOfMem;
						}
						memcpy (Section->Data, Constant1Length >= Constant2Length ? Section1->Data + Constant1Start : Section2->Data + Constant2Start, Section->Size);
						Section->FileName = Constant1Length >= Constant2Length ? Section1->FileName : Section2->FileName;
						// Create a new section symbol. We can't use the Constant1 symbol because we can't move section symbols into other sections,
						// but we may have to move the Constant1 symbol into another section.
						if (!CreateSectionSymbol (Section, ".constmerge.auto")) goto FreeAndOutOfMem;
						Append (Program->Sections, Section);
						{
							SYMBOL *CurSymbol, *NextSymbol;
							// Move the label group of Constant1 into the new section.
							for (CurSymbol = Symbol1; CurSymbol && CurSymbol->Location == Constant1Start; CurSymbol = NextSymbol)
							{
								NextSymbol = GetNext (CurSymbol);
								// Don't move the section symbol.
								if (CurSymbol == Section1->SectionSymbol) continue;
								Unlink (Section1->Symbols, CurSymbol);
								CurSymbol->Parent = Section;
								CurSymbol->Location = 0;
								Append (Section->Symbols, CurSymbol);
							}
							// Move the label group of Constant2 into the new section.
							for (CurSymbol = Symbol2; CurSymbol && CurSymbol->Location == Constant2Start; CurSymbol = NextSymbol)
							{
								NextSymbol = GetNext (CurSymbol);
								// Don't move the section symbol.
								if (CurSymbol == Section2->SectionSymbol) continue;
								Unlink (Section2->Symbols, CurSymbol);
								CurSymbol->Parent = Section;
								CurSymbol->Location = 0;
								Append (Section->Symbols, CurSymbol);
							}
						}
						// Delete Constant1.
						if (Constant1Length == Section1->Size && !NextSymbol1)
						{
							// Move the section symbol. We are deleting the section, so we need to
							// put the symbol somewhere else. It should never be referenced anyway,
							// but if it is, we don't want the linker to segfault. Do this here,
							// because even when not explicitly skipping it in the loop above, it
							// won't always be caught by the loop.
							Unlink (Section1->Symbols, Section1->SectionSymbol);
							Section1->SectionSymbol->Parent = Section;
							Append (Section->Symbols, Section1->SectionSymbol);
							// Free the section.
							FreeSection (Section1);
						}
						else
						{
							// We don't check CanCutRange here. Mergeable ranges are supposed to be always cuttable.
							CutRange (Section1, Constant1Start, Constant1End);
							// Realloc the data now, we don't know if we will cut other ranges.
							FinalizeRangeCutting (Section1);
						}
						// Delete Constant2.
						if (Constant2Length == Section2->Size && !NextSymbol2)
						{
							// Move the section symbol. We are deleting the section, so we need to
							// put the symbol somewhere else. It should never be referenced anyway,
							// but if it is, we don't want the linker to segfault. Do this here,
							// because even when not explicitly skipping it in the loop above, it
							// won't always be caught by the loop.
							Unlink (Section2->Symbols, Section2->SectionSymbol);
							Section2->SectionSymbol->Parent = Section;
							Append (Section->Symbols, Section2->SectionSymbol);
							// If the Section2 was the next Section1, we must update NextSection1.
							if (Section2 == NextSection1)
								NextSection1 = GetNext (NextSection1);
							// Free the section.
							FreeSection (Section2);
						}
						else
						{
							// We don't check CanCutRange here. Mergeable ranges are supposed to be always cuttable.
							CutRange (Section2, Constant2Start, Constant2End);
							// Realloc the data now, we don't know if we will cut other ranges.
							FinalizeRangeCutting (Section2);
						}
						// Exit the inner loops. We need a new Symbol1. (If another symbol matches, we will get back to it later.)
						goto NewSymbol1;
					}
				}
			}
NewSymbol1:;
		}
	}
}

