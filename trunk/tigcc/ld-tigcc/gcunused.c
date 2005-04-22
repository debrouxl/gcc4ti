/* gcunused.c: Routines to remove unused sections

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

#include "gcunused.h"

#include <string.h>

#include "manip.h"
#include "special.h"

// Recursively mark the target section of the specified location, if any (i.e.
// if the location is non-NULL), as referenced.
#define MarkLocation(Location) \
	if ((Location) && (Location)->Symbol) \
		MarkSection ((Location)->Symbol->Parent)


// Mark a section as referenced. If it was not already marked, follow all relocs
// from this section and recursively mark the target sections of all the relocs.
static void MarkSection (SECTION *Section)
{
	RELOC *Reloc;

	// If the section is already marked, or if it is a debugging information
	// section, return immediately.
	if (Section->Referenced || Section->DebuggingInfoType)
		return;

	// Mark the section right now to avoid infinite recursion.
	Section->Referenced = TRUE;

	// Recursively mark the target section and the relation section of all
	// relocs in this section
	for_each (Reloc, Section->Relocs)
	{
		MarkLocation (&(Reloc->Target));
		MarkLocation (Reloc->Relation);
	}
}

// Free a section if it is no longer referenced. Update the ReferencedLibCount
// accordingly.
static void RemoveSectionIfUnused (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	SECTION *OtherSection;
	LIB_CALL *LibCall, *OtherSecLibCall;
	
	// Don't free the section if it is still referenced.
	if (Section->Referenced)
		return;

	// If this section references any libraries, and if it was the last one to
	// reference them, we need to mark the library as no longer referenced.
	for_each (LibCall, Section->LibCalls)
	{
		LIBRARY *Library = LibCall->Library;
		if (Library->Referenced)
		{
			for_each (OtherSection, Program->Sections)
			{
				// Not this section!
				if (OtherSection == Section) continue;
				for_each (OtherSecLibCall, OtherSection->LibCalls)
				{
					// If this library is still referenced, forget it.
					if (OtherSecLibCall->Library == Library) goto NextLibCall;
				}
			}
			// The library is no longer referenced after this section is removed.
			Library->Referenced = FALSE;
			Program->Libraries.ReferencedCount--;
		}
NextLibCall:;
	}

	// Now free the section, or mark it as deleted if we need to keep it for
	// debugging information purposes.
#ifdef DEBUGGING_INFO_SUPPORT
	if (Program->HaveDebuggingInfo)
		Section->DebuggingInfoType = DI_DELETED;
	else
#endif /* DEBUGGING_INFO_SUPPORT */
		FreeSection (Section);
}

// Remove all unused sections.
void RemoveUnusedSections (PROGRAM *Program)
{
	SECTION *Section, *NextSection;
	
	for_each (Section, Program->Sections)
	{
		// Ignore debugging information sections.
		if (Section->DebuggingInfoType)
			continue;

		// If the section is an essential section, mark it (and all sections it
		// references) as referenced.
		if (Section->Essential)
			MarkSection (Section);
	}
	
	// For each section...
	for (Section = GetFirst (Program->Sections); Section; Section = NextSection)
	{
		// Get the next section now, since GetNext won't work once the section
		// has been freed.
		NextSection = GetNext (Section);
		
		// Ignore debugging information sections.
		if (Section->DebuggingInfoType)
			continue;

		// Remove the section if it is unused.
		RemoveSectionIfUnused (Section);
	}
}

// The following 2 functions are currently needed only for external data
// variable support.
#ifdef DATA_VAR_SUPPORT

// Mark the section containing __main as Referenced. This is a kludge
// compensating for the fact that the startup section referencing __main has not
// been imported at that stage.
void MarkMainSection (PROGRAM *Program)
{
	SECTION *Section;
		
	// For each section...
	for_each (Section, Program->Sections)
	{
		SYMBOL *Symbol;
			
		// Ignore debugging information sections.
		if (Section->DebuggingInfoType)
			continue;

		// For each symbol...
		for_each (Symbol, Section->Symbols)
		{
			// If the name matches, we have found the right symbol.
			if (Symbol->Exported && (!(strcmp ("__main", Symbol->Name))))
				MarkSection (Section);
		}
	}
}

// Clear the Referenced flag of all sections, in order to be able to run a
// second RemoveUnusedSections pass at a later point.
void ResetReferencedFlags (PROGRAM *Program)
{
	SECTION *Section;

	for_each (Section, Program->Sections)
	{
		// Ignore debugging information sections.
		if (Section->DebuggingInfoType)
			continue;

		Section->Referenced = FALSE;
	}
}

#endif /* DATA_VAR_SUPPORT */

