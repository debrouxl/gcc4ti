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

#include "manip.h"
#include "special.h"

// Free a section if it is no longer referenced. Update the ReferencedLibCount
// accordingly.
static BOOLEAN RemoveSectionIfUnused (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	SECTION *OtherSection;
	LIB_CALL *LibCall, *OtherSecLibCall;
	
	// Don't free the section if it is still referenced.
	if (Section->Referenced)
		return FALSE;

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

	// Now free the section.
	FreeSection (Section);
	
	return TRUE;
}

// Remove all unused sections.
void RemoveUnusedSections (PROGRAM *Program)
{
	SECTION *Section, *NextSection;
	
	// For each section...
	for (Section = GetFirst (Program->Sections); Section; Section = NextSection)
	{
		// Get the next section now, since GetNext won't work once the section
		// has been freed.
		NextSection = GetNext (Section);
		
		// Remove the section if it is unused.
		RemoveSectionIfUnused (Section);
	}
}

