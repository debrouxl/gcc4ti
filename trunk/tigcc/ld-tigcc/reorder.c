/* reorder.c: Routines to reorder sections

   Copyright (C) 2004 Sebastian Reichelt
   (original version was written by Kevin Kofler)

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

COUNT GetRelocCountFromTo (const SECTION *Src, const SECTION *Dest)
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

COUNT GetRelocCountBetween (const SECTION *Section1, const SECTION *Section2)
{
	return (GetRelocCountFromTo (Section1, Section2) + GetRelocCountFromTo (Section2, Section1));
}

BOOLEAN AreSectionsRelated (const SECTION *Section1, const SECTION *Section2)
{
	if (Section1 && Section2)
	{
		COUNT Relationship = M68kGetSectionRelationship (Section1, Section2);
		return (Relationship > Section1->Size || Relationship > Section2->Size);
	}
	return FALSE;
}

BOOLEAN CanMoveSection (const SECTION *Section)
{
	return (!(AreSectionsRelated (GetPrev (Section), Section) || AreSectionsRelated (Section, GetNext (Section))));
}

BOOLEAN CanMoveSectionToFront (const SECTION *Section, const SECTION *Dest)
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

BOOLEAN CanMoveSectionToBack (const SECTION *Section, const SECTION *Dest)
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

// Reorder the sections to make references as short as possible. Uses heuristics
// to avoid combinatorial explosion.
void ReorderSections (PROGRAM *Program)
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
