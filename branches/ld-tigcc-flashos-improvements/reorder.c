/* reorder.c: Routines to reorder sections

   Copyright (C) 2004-2005 Kevin Kofler
   Copyright (C) 2004 Sebastian Reichelt

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

#include "reorder.h"

#include "data.h"
#include "manip.h"
#include "bincode/fix_m68k.h" // We need this to estimate the possible gains.
#include "bincode/m68k.h"

#include <stdlib.h>

// MAIN ENTRY POINT:

static void ReorderNonStartupSections (PROGRAM *Program);
static void ReorderStartupSections (PROGRAM *Program);

// Reorder the sections to make references as short as possible. Uses heuristics
// to avoid combinatorial explosion.
void ReorderSections (PROGRAM *Program)
{
	ReorderStartupSections(Program);
	ReorderNonStartupSections(Program);
}


// REORDERING ROUTINES USING GLOBAL HEURISTICS (Sebastian Reichelt):
// The following functions handle section reordering using global heuristics.
// They are used to reorder non-startup sections.

// Maximum length of the search for related sections.
#define MAX_RELATED_SEARCH_LENGTH 100

// Maximum size of intentional unbalancing.
#define MAX_UNBALANCED_SIZE 4096

// Comparison function for qsort.
static int SectionComparisonFunction (const void *Section1Ptr, const void *Section2Ptr)
{
	const SECTION *Section1 = *((SECTION **) Section1Ptr), *Section2 = *((SECTION **) Section2Ptr);
	return (Section2->Relocs.OptRefCount * Section1->Size - Section1->Relocs.OptRefCount * Section2->Size);
}

static COUNT GetRelocCountFromTo (const SECTION *Src, const SECTION *Dest)
{
	COUNT Result = 0;
	RELOC *Reloc;
	for_each (Reloc, Src->Relocs)
	{
		SYMBOL *TargetSymbol = Reloc->Target.Symbol;
		if (TargetSymbol && TargetSymbol->Parent == Dest && M68kIsRelocOptimizable (Reloc))
			Result++;
	}
	return Result;
}

static COUNT GetRelocCountBetween (const SECTION *Section1, const SECTION *Section2)
{
	return (GetRelocCountFromTo (Section1, Section2) + GetRelocCountFromTo (Section2, Section1));
}

static BOOLEAN AreSectionsRelated (const SECTION *Section1, const SECTION *Section2)
{
	if (Section1 && Section2)
	{
		COUNT Relationship = M68kGetSectionRelationship (Section1, Section2);
		return (Relationship > Section1->Size || Relationship > Section2->Size);
	}
	return FALSE;
}

static BOOLEAN CanMoveSection (const SECTION *Section)
{
	return (!(AreSectionsRelated (GetPrev (Section), Section) || AreSectionsRelated (Section, GetNext (Section))));
}

static BOOLEAN CanMoveSectionToFront (const SECTION *Section, const SECTION *Dest)
{
	if (!(CanMoveSection (Section)))
		return FALSE;
	else
	{
		SECTION *CheckSection;
		SIZE TotalSize = Section->Size;
		for (CheckSection = (Dest ? GetNext (Dest) : GetFirst (Section->Parent->Sections)); CheckSection; CheckSection = GetNext (CheckSection))
		{
			TotalSize += CheckSection->Size;
			if (TotalSize > M68K_REL_MAX && GetRelocCountBetween (Section, CheckSection) > 0)
				return FALSE;
		}
		return TRUE;
	}
}

static BOOLEAN CanMoveSectionToBack (const SECTION *Section, const SECTION *Dest)
{
	if (!(CanMoveSection (Section)))
		return FALSE;
	else
	{
		SECTION *CheckSection;
		SIZE TotalSize = Section->Size;
		for (CheckSection = (Dest ? GetPrev (Dest) : GetLast (Section->Parent->Sections)); CheckSection; CheckSection = GetPrev (CheckSection))
		{
			TotalSize += CheckSection->Size;
			if (TotalSize > M68K_REL_MAX && GetRelocCountBetween (Section, CheckSection) > 0)
				return FALSE;
		}
		return TRUE;
	}
}

// Reorder non-startup sections to make references as short as possible. Uses
// global heuristics to avoid combinatorial explosion.
static void ReorderNonStartupSections (PROGRAM *Program)
{
	SECTION **Sections = calloc (CountItems (Program->Sections, SECTION), sizeof (SECTION *));
	if (!Sections)
	{
		Warning (NULL, "Out of memory reordering sections.");
		return;
	}
	{
		COUNT SectionCount = 0;
		SECTION *InsertPos = NULL;
		
		// Build an array of all sections, and unlink those sections from the
		// section list.
		SECTION *Section = GetFirst (Program->Sections), *NextSection;
		if (Section && Program->Type == PT_NOSTUB)
			Section = GetNext (Section);
		for (; Section; Section = NextSection)
		{
			NextSection = GetNext (Section);
			if (!(Section->StartupNumber))
			{
				if (Section->Handled)
				{
					// The first handled section is a good place to insert the sorted sections.
					if (!InsertPos)
						InsertPos = Section;
				}
				else
				{
					Sections[SectionCount++] = Section;
					Unlink (Program->Sections, Section);
				}
			}
		}
		
		// Sort this array according to the number of references.
		qsort (Sections, SectionCount, sizeof (SECTION *), SectionComparisonFunction);
		
		// Insert the sections in a new order, putting heavily referenced sections
		// in the middle of the program.
		// The reason this works best is actually very specific to M68k and TIOS.
		{
			SECTION *FirstPos = (InsertPos ? GetPrev (InsertPos) : GetLast (Program->Sections)), *SecondPos = InsertPos;
			SIZE FirstSize = 0, SecondSize = 0;
			OFFSET CurSectionIdx;
			for (CurSectionIdx = 0; CurSectionIdx < SectionCount; CurSectionIdx++)
			{
				SECTION *Section = Sections[CurSectionIdx];
				if (Section)
				{
					BOOLEAN InsertAtSecondPos = FALSE;
					if (!(Section->Relocs.RelativeRefs))
					{
						if (!(Section->Relocs.StartupRefs))
							InsertAtSecondPos = SecondSize < FirstSize;
						if (SecondSize - FirstSize <= MAX_UNBALANCED_SIZE && FirstSize - SecondSize <= MAX_UNBALANCED_SIZE)
						{
							// Try to find out which of the two sides is actually better,
							// i.e. which causes fewer absolute relocs.
							COUNT FirstRelocCount = 0, SecondRelocCount = 0;
							BOOLEAN FirstTrouble = FALSE, SecondTrouble = FALSE;
							SIZE TotalSize = FirstSize + SecondSize + Section->Size;
							SIZE SizeLeft = TotalSize;
							SECTION *CheckSection;
							for (CheckSection = (FirstPos ? GetNext (FirstPos) : GetFirst (Program->Sections)); CheckSection && CheckSection != SecondPos && SizeLeft > M68K_REL_MAX; CheckSection = GetNext (CheckSection))
							{
								COUNT RelocCount = GetRelocCountBetween (Section, CheckSection);
								if (RelocCount > 0)
								{
									FirstTrouble = TRUE;
									if (!(SecondSize - FirstSize + 2 * CheckSection->Size <= MAX_UNBALANCED_SIZE && CanMoveSectionToBack (CheckSection, SecondPos)))
										FirstRelocCount += RelocCount;
								}
								SizeLeft -= CheckSection->Size;
							}
							SizeLeft = TotalSize;
							for (CheckSection = (SecondPos ? GetPrev (SecondPos) : GetLast (Program->Sections)); CheckSection && CheckSection != FirstPos && SizeLeft > M68K_REL_MAX; CheckSection = GetPrev (CheckSection))
							{
								COUNT RelocCount = GetRelocCountBetween (Section, CheckSection);
								if (RelocCount > 0)
								{
									SecondTrouble = TRUE;
									if (!(FirstSize - SecondSize + 2 * CheckSection->Size <= MAX_UNBALANCED_SIZE && CanMoveSectionToFront (CheckSection, FirstPos)))
										SecondRelocCount += RelocCount;
								}
								SizeLeft -= CheckSection->Size;
							}
							if (FirstRelocCount != SecondRelocCount)
								InsertAtSecondPos = SecondRelocCount > FirstRelocCount;
							else if (FirstTrouble && (!SecondTrouble))
								InsertAtSecondPos = FALSE;
							else if (SecondTrouble && (!FirstTrouble))
								InsertAtSecondPos = TRUE;
						}
					}
					{
						OFFSET RelatedSectionIdx = CurSectionIdx + 1;
						OFFSET RelatedSectionSearchEnd = CurSectionIdx + MAX_RELATED_SEARCH_LENGTH;
						if (RelatedSectionSearchEnd > SectionCount)
							RelatedSectionSearchEnd = SectionCount;
						do {
							// Insert the section.
							if (InsertAtSecondPos)
							{
								SIZE OriginalDifference = SecondSize - FirstSize;
								InsertBefore (Program->Sections, Section, SecondPos);
								SecondSize += Section->Size;
								if (OriginalDifference <= MAX_UNBALANCED_SIZE)
								{
									SIZE SizeLeft = FirstSize + SecondSize;
									SECTION *CheckSection, *NextSection;
									for (CheckSection = (FirstPos ? GetNext (FirstPos) : GetFirst (Program->Sections)); CheckSection && CheckSection != SecondPos && SizeLeft > M68K_REL_MAX; CheckSection = NextSection)
									{
										NextSection = GetNext (CheckSection);
										if (GetRelocCountBetween (Section, CheckSection) > 0 && OriginalDifference + 2 * CheckSection->Size <= MAX_UNBALANCED_SIZE && CanMoveSectionToBack (CheckSection, SecondPos))
										{
											Unlink (Program->Sections, CheckSection);
											InsertBefore (Program->Sections, CheckSection, SecondPos);
											SecondSize += CheckSection->Size;
											FirstSize  -= CheckSection->Size;
										}
										SizeLeft -= CheckSection->Size;
									}
								}
							}
							else
							{
								SIZE OriginalDifference = FirstSize - SecondSize;
								InsertAfter (Program->Sections, Section, FirstPos);
								FirstSize += Section->Size;
								if (OriginalDifference <= MAX_UNBALANCED_SIZE)
								{
									SIZE SizeLeft = FirstSize + SecondSize;
									SECTION *CheckSection, *NextSection;
									for (CheckSection = (SecondPos ? GetPrev (SecondPos) : GetLast (Program->Sections)); CheckSection && CheckSection != FirstPos && SizeLeft > M68K_REL_MAX; CheckSection = NextSection)
									{
										NextSection = GetPrev (CheckSection);
										if (GetRelocCountBetween (Section, CheckSection) > 0 && OriginalDifference + 2 * CheckSection->Size <= MAX_UNBALANCED_SIZE && CanMoveSectionToFront (CheckSection, FirstPos))
										{
											Unlink (Program->Sections, CheckSection);
											InsertAfter (Program->Sections, CheckSection, FirstPos);
											FirstSize  += CheckSection->Size;
											SecondSize -= CheckSection->Size;
										}
										SizeLeft -= CheckSection->Size;
									}
								}
							}
							{
								SECTION *RelatedSection = NULL;
								// Look for a closely related section, which should be put right
								// next to this one.
								for (; RelatedSectionIdx < RelatedSectionSearchEnd && (!RelatedSection); RelatedSectionIdx++)
								{
									SECTION *SearchSection = Sections[RelatedSectionIdx];
									if (SearchSection)
									{
										COUNT Relationship;
										if (InsertAtSecondPos)
											Relationship = M68kGetSectionRelationship (Section, SearchSection);
										else
											Relationship = M68kGetSectionRelationship (SearchSection, Section);
										if (Relationship > SearchSection->Size)
										{
											RelatedSection = SearchSection;
											Sections[RelatedSectionIdx] = NULL;
										}
									}
								}
								Section = RelatedSection;
							}
						} while (Section);
					}
				}
			}
			
			// Look at the effect of putting sections referenced by startup sections
			// at the beginning. If it doesn't seem to have any negative effect, then
			// do it.
			{
				if (FirstPos)
					InsertPos = GetNext (FirstPos);
				else
					InsertPos = GetFirst (Program->Sections);
				{
					SECTION *FirstProblem;
					SIZE ProblemSize = 0;
					for (FirstProblem = InsertPos; FirstProblem && ProblemSize <= M68K_REL_MAX; FirstProblem = GetNext (FirstProblem))
						ProblemSize += FirstProblem->Size;
					{
						SECTION *Section, *NextSection;
						for (Section = InsertPos; Section && Section != SecondPos; Section = NextSection)
						{
							NextSection = GetNext (Section);
							if (Section->Relocs.StartupRefs && CanMoveSection (Section))
							{
								BOOLEAN CanMove = TRUE;
								SECTION *CheckSection;
								for (CheckSection = FirstProblem; CheckSection && CheckSection != SecondPos; CheckSection = GetNext (CheckSection))
								{
									if (GetRelocCountBetween (Section, CheckSection) > 0)
									{
										CanMove = FALSE;
										break;
									}
								}
								if (CanMove)
								{
									Unlink (Program->Sections, Section);
									InsertAfter (Program->Sections, Section, FirstPos);
								}
							}
						}
					}
				}
			}
		}
	}
	free (Sections);
}


// REORDERING ROUTINES USING LOCAL HEURISTICS (Kevin Kofler):
// The following functions handle reordering of startup sections which share one
// and the same startup number and can thus be arbitrarily reordered. The
// current implementation is based on my (Kevin Kofler's) original section
// reordering algorithm.

// Reorder the sections to make references as short as possible. Backtrack when
// a solution is impossible (due to hard-coded short references). Returns 1 on
// success, 0 on failure, -1 on memory overflow.
static SI1 ReorderSectionsRecurse(PROGRAM *Program, COUNT SectionCount,
                                  SECTION **Sections, COUNT RecursionDepth);

// Find the first section that has not yet been handled.
static SECTION *FindNextSection(PROGRAM *Program, SECTION **Sections,
                                COUNT RecursionDepth);

// Compute an estimation of the win obtained by putting this section next.
// Returns IMPOSSIBLE if doing so would actually invalidate a reference. Note
// that, due to the heuristic employed, placing this section even later can only
// make things worse, so we can immediately return FALSE if this happens.
static COUNT ComputeGoodness(SECTION **Sections, COUNT RecursionDepth,
                             SECTION *CurrentSection);
#define PLACELAST ((COUNT)-1)
#define IMPOSSIBLE ((COUNT)-2)

// Comparison function for qsort.
static int TaggedSectionComparisonFunction(const void *TaggedSection1,
                                           const void *TaggedSection2);

// Reorder startup sections to make references as short as possible. Uses
// local heuristics to avoid combinatorial explosion.
static void ReorderStartupSections(PROGRAM *Program)
{
	SI1 Result;
	COUNT RecursionDepth = 0;
	COUNT SectionCount = CountItems(Program->Sections,SECTION);
	SECTION **Sections = malloc(SectionCount * sizeof(SECTION *));
	if (!Sections)
	{
		Warning(NULL, "Out of memory reordering sections.");
		return;
	}
	if (Program->Type == PT_NOSTUB)
	{
		*Sections = GetFirst(Program->Sections);
		RecursionDepth++;
	}
	Result = ReorderSectionsRecurse(Program, SectionCount, Sections, RecursionDepth);
	if (Result > 0)
	{
		// Reorder our linked list of sections:
		SECTION *Section, *NextSection;
		COUNT i;
		// First unlink them all.
		for (Section = GetFirst(Program->Sections); Section; Section = NextSection)
		{
			NextSection = GetNext (Section);
			Unlink(Program->Sections, Section);
		}
		// Then append them in the order given by the array.
		for (i = 0; i < SectionCount; i++)
			Append(Program->Sections, Sections[i]);
	}
	else if (!Result)
	{
		Warning(NULL, "Section reordering failed.");
	}
	free(Sections);
}

typedef struct {
	SECTION *Section;
	COUNT Goodness;
} TAGGEDSECTION;

// Reorder the sections to make references as short as possible. Backtrack when
// a solution is impossible (due to hard-coded short references). Returns 1 on
// success, 0 on failure, -1 on memory overflow.
static SI1 ReorderSectionsRecurse(PROGRAM *Program, COUNT SectionCount,
                                  SECTION **Sections, COUNT RecursionDepth)
{
	// If there are no more sections to reorder, return immediately.
	if (RecursionDepth == SectionCount)
		return 1;
	else
	{
		TAGGEDSECTION *TaggedSections = malloc(SectionCount
		                                       * sizeof(TAGGEDSECTION)),
		              *CurrentTaggedSection = TaggedSections;
		SECTION *CurrentSection;
		if (!TaggedSections)
		{
			Warning(NULL, "Out of memory reordering sections.");
			return -1;
		}
		CurrentSection = FindNextSection(Program, Sections, RecursionDepth);
		if (CurrentSection)
		{
			SECTION **PCurrentSection;
			OFFSET StartupNumber = CurrentSection->StartupNumber;
			COUNT TaggedSectionCount;
			// If the current section is not a startup section, it means we are
			// done with the startup sections. So just add all the remaining
			// sections in their current order, to be rearranged by global
			// reordering.
			if (!StartupNumber)
			{
				SECTION *RemainingSection;
				for (RemainingSection = CurrentSection; RemainingSection;
				     RemainingSection = GetNext (RemainingSection))
				{
					Sections[RecursionDepth++] = RemainingSection;
				}
				free(TaggedSections);
				return 1;
  			}
			// Compute an estimation of the savings for placing each of the sections
			// next.
			for(; CurrentSection; CurrentSection = GetNext(CurrentSection))
			{
				COUNT Goodness;
				// Search in reverse order to avoid having to skip over all those old
				// startup sections each time.
				for (PCurrentSection = Sections + (RecursionDepth - 1);
				     PCurrentSection >= Sections; PCurrentSection--)
				{
					if (*PCurrentSection == CurrentSection)
						goto AlreadyHandled;
				}
				Goodness = ComputeGoodness(Sections, RecursionDepth,
				                           CurrentSection);
				// IMPOSSIBLE means this section cannot be placed here, and
				// placing it later can only make things worse, so backtrack
				// immediately.
				if (Goodness == IMPOSSIBLE)
				{
					Warning(NULL, "Impossible section arrangement rejected at "
					              "recursion depth %ld.", (long) RecursionDepth);
					free(TaggedSections);
					return 0;
				}
				if (CurrentSection->StartupNumber == StartupNumber)
				{
					CurrentTaggedSection->Section = CurrentSection;
					(CurrentTaggedSection++)->Goodness = Goodness;
				}
				AlreadyHandled:;
			}
			TaggedSectionCount = CurrentTaggedSection - TaggedSections;
			// Sort by decreasing estimated savings.
			qsort(TaggedSections, TaggedSectionCount, sizeof(TAGGEDSECTION),
			      TaggedSectionComparisonFunction);
			// Try the best one first, then the second best and so on. Note that
			// backtracking is ONLY used when there are hardcoded sizes which are
			// not satisfied. Therefore, the easiest way to avoid using exponential
			// time is to not hardcode any short references. That's what linker
			// optimization is for!
			for (CurrentTaggedSection = TaggedSections;
			     CurrentTaggedSection < TaggedSections + TaggedSectionCount;
			     CurrentTaggedSection++)
			{
				SI1 Result;
				Sections[RecursionDepth] = CurrentTaggedSection->Section;
				Result = ReorderSectionsRecurse(Program, SectionCount, Sections,
				                                RecursionDepth + 1);
				if (Result) /* can be 1 or -1, in both cases we don't want to */
				{           /* try the next section */
					free(TaggedSections);
					return Result;
				}
			}
			Warning(NULL, "Cannot find a valid section order at recursion depth "
			              "%ld.", (long) RecursionDepth);
		}
		free(TaggedSections);
		return 0;
	}
}

// Find the first section that has not yet been handled.
static SECTION *FindNextSection(PROGRAM *Program, SECTION **Sections,
                                COUNT RecursionDepth)
{
	SECTION *CurrentSection, **PCurrentSection;
	// Check for a section that hasn't been handled by reordering yet. There
	// should be at least one such section.
	for_each (CurrentSection, Program->Sections)
	{
		// Search in reverse order to avoid having to skip over all those old
		// startup sections each time.
		for (PCurrentSection = Sections + (RecursionDepth - 1);
		     PCurrentSection >= Sections; PCurrentSection--)
		{
			if (*PCurrentSection == CurrentSection)
				goto AlreadyHandled;
		}
		return CurrentSection;
		AlreadyHandled:;
	}
	return NULL;
}

// Compute an estimation of the win obtained by putting this section next.
// Returns IMPOSSIBLE if doing so would actually invalidate a reference. Note
// that, due to the heuristic employed, placing this section even later can only
// make things worse, so we can immediately return FALSE if this happens.
// The estimates used are machine-specific. See M68kComputeRelocGoodness.
static COUNT ComputeGoodness(SECTION **Sections, COUNT RecursionDepth,
                             SECTION *CurrentSection)
{
	SECTION *HandledSection, **PHandledSection;
	COUNT Goodness = 0;
	OFFSET ExtraOffset = 0;
	// If the current section is already handled, putting it in front will not
	// save us anything.
	if (CurrentSection->Handled)
		return PLACELAST;
	// For each handled section, in reverse order...
	for (PHandledSection = Sections + (RecursionDepth - 1);
	     PHandledSection >= Sections; PHandledSection--)
	{
		RELOC *Reloc;
		HandledSection = *PHandledSection;
		// Look for references FROM the handled section TO the current section.
		for_each (Reloc, HandledSection->Relocs)
		{
			if (!Reloc->Relation && Reloc->Target.Symbol
			    && Reloc->Target.Symbol->Parent == CurrentSection)
			{
				OFFSET Offset = Reloc->Target.Symbol->Location
				                + Reloc->Target.Offset
				                + (HandledSection->Size - Reloc->Location)
				                + ExtraOffset + Reloc->FixedOffset;
				if (Reloc->Size == 2 && (Offset > 32767 || Offset < -32768))
					return IMPOSSIBLE;
				else if (Reloc->Size == 1 && (Offset > 127 || Offset < -128))
					return IMPOSSIBLE;
				Goodness += M68kComputeRelocGoodness(Offset, Reloc);
			}
		}
		// Look for references TO the handled section FROM the current section.
		for_each (Reloc, CurrentSection->Relocs)
		{
			if (!Reloc->Relation && Reloc->Target.Symbol
			    && Reloc->Target.Symbol->Parent == HandledSection)
			{
				OFFSET Offset = - Reloc->Location
				                - (HandledSection->Size
				                   - (Reloc->Target.Symbol->Location
				                      + Reloc->Target.Offset))
				                - ExtraOffset + Reloc->FixedOffset;
				if (Reloc->Size == 2 && (Offset > 32767 || Offset < -32768))
					return IMPOSSIBLE;
				else if (Reloc->Size == 1 && (Offset > 127 || Offset < -128))
					return IMPOSSIBLE;
				Goodness += M68kComputeRelocGoodness(Offset, Reloc);
			}
		}
		// Add the size of the handled section to the offset to account for.
		ExtraOffset += HandledSection->Size;
	}	
	return Goodness;
}

// Comparison function for qsort.
static int TaggedSectionComparisonFunction(const void *TaggedSection1,
                                           const void *TaggedSection2)
{
	if (((const TAGGEDSECTION *)TaggedSection1)->Goodness
	    > ((const TAGGEDSECTION *)TaggedSection2)->Goodness)
		return -1;
	else if (((const TAGGEDSECTION *)TaggedSection1)->Goodness
	    < ((const TAGGEDSECTION *)TaggedSection2)->Goodness)
		return 1;
	else
		return 0;
}

