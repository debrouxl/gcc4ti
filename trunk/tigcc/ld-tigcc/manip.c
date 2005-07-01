/* manip.c: Routines to manipulate the internal data

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

#include "manip.h"

#include "integers.h"
#include "special.h"
#include "import/imp_ar.h"
#include "bincode/fix_m68k.h"

#include <stdlib.h>
#include <string.h>

// Free the program tree.
void FreeProgram (PROGRAM *Program)
{
	SECTION *Section, *NextSection;
	LIBRARY *Library, *NextLibrary;
	ARCHIVE *Archive, *NextArchive;
	GLOBAL_IMPORT *GlobalImport, *NextGlobalImport;
	
	for (GlobalImport = GetLast (Program->GlobalImports); GlobalImport; GlobalImport = NextGlobalImport)
	{
		NextGlobalImport = GetPrev (GlobalImport);
		free (GlobalImport);
	}
	
	for (Archive = GetLast (Program->Archives); Archive; Archive = NextArchive)
	{
		ARCHIVE_OBJECT *Object, *NextObject;
		ARCHIVE_SYMBOL *Symbol, *NextSymbol;
		
		NextArchive = GetPrev (Archive);
		
		for (Object = GetLast (Archive->ObjectFiles); Object; Object = NextObject)
		{
			NextObject = GetPrev (Object);
			free (Object);
		}
		
		for (Symbol = GetLast (Archive->Symbols); Symbol; Symbol = NextSymbol)
		{
			NextSymbol = GetPrev (Symbol);
			free (Symbol);
		}
		
		free ((I1 *) (Archive->Data));
		free (Archive);
	}
	
	for (Library = GetLast (Program->Libraries); Library; Library = NextLibrary)
	{
		NextLibrary = GetPrev (Library);
		free (Library);
	}
	
	for (Section = GetLast (Program->Sections); Section; Section = NextSection)
	{
		NextSection = GetPrev (Section);
		FreeSection (Section);
	}
	
	// Set all data to 0.
	memset (Program, 0, sizeof (PROGRAM));
}

// Free a section. The section is assumed not to be referenced.
void FreeSection (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	
	// Need to free the data, if any.
	if (Section->Data)
		free (Section->Data);
	
	// Define a macro to make things easier.
#define FreeItems(Type,Item) \
({ \
	Type *Item, *Next##Item; \
	for (Item = GetLast (Section->Item##s); Item; Item = Next##Item) \
	{ \
		Next##Item = GetPrev (Item); \
		free (Item); \
	} \
})
	
	FreeItems (SEGMENT,  Segment);
	FreeItems (LIB_CALL, LibCall);
	FreeItems (RAM_CALL, RAMCall);
	FreeItems (ROM_CALL, ROMCall);
	
	{
		RELOC *Reloc, *NextReloc;
		for (Reloc = GetLast (Section->Relocs); Reloc; Reloc = NextReloc)
		{
			NextReloc = GetPrev (Reloc);
			FreeReloc (Reloc);
		}
	}
	
	FreeItems (SYMBOL,   Symbol);
	
#undef FreeItems
	
	if (Program->MainSection == Section)
		Program->MainSection = NULL;
	if (Program->BSSSection == Section)
		Program->BSSSection = NULL;
	if (Program->DataSection == Section)
		Program->DataSection = NULL;
	
	Unlink (Program->Sections, Section);
	free (Section);
}

// Free a relocation entry.
void FreeReloc (RELOC *Reloc)
{
	SECTION *Section = Reloc->Parent;
	Unlink (Section->Relocs, Reloc);
	// Need to free the symbol name, if it is not a reference to Symbol->Name.
	FreeLocationSymbolName (Section, &(Reloc->Target));
	// Need to free the relation if it exists.
	FreeRelocRelation (Reloc);
	free (Reloc);
}

// Free the relation reference of a relocation entry, and set it to
// NULL.
void FreeRelocRelation (RELOC *Reloc)
{
	LOCATION *Relation = Reloc->Relation;
	SECTION *Section = Reloc->Parent;
	Reloc->Relation = NULL;
	if (Relation && (Relation != &(Section->Parent->EntryPoint)))
	{
		FreeLocationSymbolName (Section, Relation);
		free (Relation);
	}
}

// Free the symbol name of a location, if this is necessary.
// If a symbol is already known, set the symbol name to its name.
// Decrease the number of unresolved relocs in the section.
void FreeLocationSymbolName (SECTION *Section, LOCATION *Location)
{
	if (((!(Location->Symbol)) || (Location->SymbolName != Location->Symbol->Name)) && Location->SymbolName && (Location != &(Section->Parent->EntryPoint)))
	{
		if (--Section->Relocs.UnresolvedCount < 0)
			Warning (Section->FileName, "Internal unresolved count mismatch.");
		free ((char *) Location->SymbolName);
	}
	Location->SymbolName = (Location->Symbol ? Location->Symbol->Name : NULL);
}

// Create a section symbol for the given section, if none has been
// created yet. If there already is one, set its name accordingly.
// Return the section symbol.
SYMBOL *CreateSectionSymbol (SECTION *Section, const char *SectionName)
{
	char *SymName;
	
	// Create a new symbol.
	SYMBOL *Symbol = (Section->SectionSymbol ? : calloc (1, sizeof (SYMBOL)));
	
	if (!Symbol)
	{
		Error (Section->FileName, "Out of memory.");
		return NULL;
	}
	
	// Set the basic attributes of the symbol.
	Symbol->Parent = Section;
	// Insert the section's file name in front of the section name.
	if (Section->FileName)
	{
		strncpy (Symbol->Name, Section->FileName, MAX_SYM_LEN);
		SymName = Symbol->Name + strlen (Symbol->Name);
		if (SectionName && (SymName < Symbol->Name + MAX_SYM_LEN))
			*(SymName++) = ' ';
	}
	else
		SymName = Symbol->Name;
	// Insert the section name.
	if (SectionName)
		strncpy (SymName, SectionName, Symbol->Name + MAX_SYM_LEN - SymName);
	
	if (!(Section->SectionSymbol))
	{
		// Insert the symbol at the beginning of the linked list.
		Push (Section->Symbols, Symbol);
		// Mark the symbol as the section's beginning symbol.
		Section->SectionSymbol = Symbol;
	}
	
	return Symbol;
}

// Create a segment for the entire given section, if none has been
// created yet. If the section already contains segments, return
// NULL.
SEGMENT *CreateSectionSegment (SECTION *Section)
{
	if ((IsEmpty (Section->Segments)) && (Section->Size > 0))
	{
		SEGMENT *Segment = calloc (1, sizeof (SEGMENT));
		if (!Segment)
		{
			Error (Section->FileName, "Out of memory.");
			return NULL;
		}
		
		if (!(CreateSectionMarkers (&(Segment->Location), Section)))
		{
			free (Segment);
			return NULL;
		}
		
		Segment->Code         = Section->Code;
		Segment->CanCutRanges = Section->CanCutRanges;
		Segment->FileName     = Section->FileName;
		
		Push (Section->Segments, Segment);
		
		return Segment;
	}
	
	return NULL;
}

// If the section is a startup section, insert it at the place where
// it belongs. Otherwise, append it to the end of the program.
void InsertSection (PROGRAM *Program, SECTION *Section)
{
	// Check if it is a startup section.
	if (Section->StartupNumber)
	{
		// Find the location where we have to insert the
		// section.
		SECTION *PrevStartupSection = FindStartupSection (Program, Section->StartupNumber);
		
		// If the entry point is already fixed, and this will insert the
		// section in front of the entry point symbol, the program will
		// probably fail.
		if (Program->EntryPoint.Symbol && (PrevStartupSection == GetFirst (Program->Sections)))
			Warning (Section->FileName, "Inserting startup section with number %ld in front of entry point.", (long) Section->StartupNumber);
		
		// Cannot remove startup sections.
		Section->Essential = TRUE;
		
		// Insert the section where it belongs.
		InsertAfter (Program->Sections, Section, PrevStartupSection);
	}
	// If it is not a startup section, append it at the end.
	else
		Append (Program->Sections, Section);
}

// Find the last startup section below or at StartupNumber.
// If there is no such section, the function returns NULL.
SECTION *FindStartupSection (const PROGRAM *Program, OFFSET StartupNumber)
{
	SECTION *Section;
	
	// For each section...
	for_each (Section, Program->Sections)
	{
		// If it is not a startup section, or the startup number is higher,
		// the previous section was the one (may be NULL).
		if ((!(Section->StartupNumber)) || (Section->StartupNumber > StartupNumber))
			return (GetPrev (Section));
	}
	
	// After all the startup sections that were below the number, the
	// list ended. Or maybe there are no sections at all. In any case,
	// this is the correct thing to do.
	return (GetLast (Program->Sections));
}

// Insert the symbol at the correct place in the section.
void InsertSymbol (SECTION *Section, SYMBOL *Symbol)
{
	const SYMBOL *NextSymbol = FindSymbolAtPos (Section, Symbol->Location, TRUE);
	InsertBefore (Section->Symbols, Symbol, NextSymbol);
}

// Increase the counters for references between the two sections, if the sections
// are different.
static void IncRefCounts (SECTION *SourceSection, SECTION *TargetSection, const RELOC *Reloc)
{
	if (SourceSection != TargetSection)
	{
		if (Reloc->Relative && (!(Reloc->Relation)))
		{
			SourceSection->Relocs.RelativeRefs = TRUE;
			TargetSection->Relocs.RelativeRefs = TRUE;
		}
		else if (M68kIsRelocOptimizable (Reloc))
		{
			SourceSection->Relocs.OptRefCount++;
			TargetSection->Relocs.OptRefCount++;
		}
		if (SourceSection->StartupNumber || TargetSection->StartupNumber)
		{
			SourceSection->Relocs.StartupRefs = TRUE;
			TargetSection->Relocs.StartupRefs = TRUE;
		}
	}
}

// Insert the reloc at the correct place in the section, and update the appropriate
// counters. Also possibly mark the target section as referenced.
void InsertReloc (SECTION *Section, RELOC *Reloc)
{
	const RELOC *NextReloc = FindRelocAtPos (Section, Reloc->Location, TRUE);
	InsertBefore (Section->Relocs, Reloc, NextReloc);
	HandleLocation (Reloc, &(Reloc->Target));
	HandleLocation (Reloc, Reloc->Relation);
	{
		SYMBOL *TargetSymbol = Reloc->Target.Symbol;
		if (TargetSymbol)
			IncRefCounts (Section, TargetSymbol->Parent, Reloc);
	}
}

// Find the item preceding or following the given location. May return NULL.
#define DefineFindItemAtPos(Type,Item,LocationSuffix) \
Type *Find##Item##AtPos (const SECTION *Section, OFFSET Location, BOOLEAN Following) \
{ \
	/* Find the place so that Item->Location is always less than Location */ \
	/* and Next##Item->Location is always greater or equal. */ \
	Type *Item = NULL, *Next##Item = NULL; \
	if (Location > Section->Size >> 1) \
	{ \
		Item = GetLast (Section->Item##s); \
		while (Item && (Item->Location LocationSuffix >= Location)) \
		{ \
			Next##Item = Item; \
			Item = GetPrev (Item); \
		} \
	} \
	else \
	{ \
		Next##Item = GetFirst (Section->Item##s); \
		while (Next##Item && (Next##Item->Location LocationSuffix < Location)) \
		{ \
			Item = Next##Item; \
			Next##Item = GetNext (Next##Item); \
		} \
	} \
	/* If we are looking for something that follows, the case is clear. */ \
	/* Otherwise, we need to check whether the next item is also OK. */ \
	if (Following || (Next##Item && (Next##Item->Location LocationSuffix <= Location))) \
		return Next##Item; \
	else \
		return Item; \
}

DefineFindItemAtPos (SYMBOL,   Symbol,  );
DefineFindItemAtPos (RELOC,    Reloc,   );
DefineFindItemAtPos (ROM_CALL, ROMCall, );
DefineFindItemAtPos (RAM_CALL, RAMCall, );
DefineFindItemAtPos (LIB_CALL, LibCall, );
DefineFindItemAtPos (SEGMENT,  Segment, .Start->Location);

#undef DefineFindItemAtPos

// Get the file name where the code at a given location in the
// section came from. May return NULL.
const char *GetFileName (const SECTION *Section, OFFSET Location)
{
	const SEGMENT *Segment = FindSegmentAtPos (Section, Location, FALSE);
	
	// Check whether the location is inside the segment.
	// It can be at the beginning, but not at the very end.
	if (Segment && (Location < Segment->Location.End->Location) && Segment->FileName)
		// Return the segment's file name.
		return Segment->FileName;
	
	// If no segment was found, return the section's file name.
	return Section->FileName;
}

// Return the offset of the specified location into the section. If the
// offset is unknown, emit a warning and return 0.
OFFSET GetLocationOffset (const SECTION *Section, const LOCATION *Location)
{
	if (Location && Location->Symbol && (Location->Symbol->Parent == Section))
		return Location->Symbol->Location + Location->Offset;
	else
	{
		Warning (Section->FileName, "Symbol `%s' is not in section `%s'; using arbitrary location.", Location->SymbolName ? : "(unknown)", Section->SectionSymbol->Name);
		return 0;
	}
}

// Increase the counters and reference information necessary when this
// location is used in the specified reloc.
void HandleLocation (RELOC *Reloc, LOCATION *Location)
{
	if (Location)
	{
		SECTION *Section = Reloc->Parent;
		if (!Location->Symbol)
		{
			Section->Relocs.UnresolvedCount++;
		}
	}
}

// Point the location to the appropriate symbol, if one is found.
SYMBOL *ResolveLocation (PROGRAM *Program, SECTION *Section, LOCATION *Location)
{
	if (Location->Symbol)
		return Location->Symbol;
	else
	{
		SECTION *CurSection;
		
		// For each section...
		for_each (CurSection, Program->Sections)
		{
			SYMBOL *CurSymbol;
			
			// For each symbol...
			for_each (CurSymbol, CurSection->Symbols)
			{
				// If the name matches, we have found the right symbol.
				if (CurSymbol->Exported && (!(strcmp (Location->SymbolName, CurSymbol->Name))))
				{
					// Set up the reloc accordingly, freeing its
					// destination string.
					Location->Symbol = CurSymbol;
					FreeLocationSymbolName (Section, Location);
					return CurSymbol;
				}
			}
		}
		
		return NULL;
	}
}

// If the reloc's target symbol name identifies a ROM, RAM,
// or library call, replace it with the appropriate item.
// This must be called in order, as the items are appended
// unconditionally.
static BOOLEAN ResolveSpecialExternalSymbolReloc (RELOC *Reloc)
{
	SECTION *Section = Reloc->Parent;
	PROGRAM *Program = Section->Parent;
	
	const char *CurFileName = NULL;
	
	// Translate the reloc's symbol name.
	void *SymRef = NULL;
	OFFSET SymNum = 0;
	SpecialExternalSymbolTypes SymType = TranslateSpecialExternalSymbol (Program, (char *) (Reloc->Target.SymbolName), &SymRef, &SymNum);
	
	if (SymType != ST_NORMAL)
		CurFileName = GetFileName (Section, Reloc->Location);
	
	// Replace the reloc with the another item.
	switch (SymType)
	{
		case ST_ROM_CALL:
			{
				// Create a new ROM call.
				ROM_CALL *ROMCall = calloc (1, sizeof (ROM_CALL));
				if (!ROMCall)
				{
					Error (CurFileName, "Out of memory while resolving relocs.");
					return FALSE;
				}
				// Assign the attributes of the reloc to the ROM call.
				ROMCall->Parent      = Section;
				ROMCall->Location    = Reloc->Location;
				ROMCall->Number      = SymNum;
				ROMCall->Size        = Reloc->Size;
				ROMCall->FixedOffset = Reloc->Target.Offset + Reloc->FixedOffset;
				if (Reloc->Relative)
					Warning (CurFileName, "ROM calls cannot be relative; changing to absolute.");
				// Remove the reloc.
				FreeReloc (Reloc);
				// Add the ROM call.
				Append (Section->ROMCalls, ROMCall);
				// Adjust ROM call statistics.
				if (Program->HighestROMCall < SymNum)
					Program->HighestROMCall = SymNum;
				Section->ROMCalls.Handled = FALSE;
			}
			return TRUE;
		
		case ST_RAM_CALL:
		case ST_EXTRA_RAM:
			{
				// Create a new ROM call.
				RAM_CALL *RAMCall = calloc (1, sizeof (RAM_CALL));
				if (!RAMCall)
				{
					Error (CurFileName, "Out of memory while resolving relocs.");
					return FALSE;
				}
				// Assign the attributes of the reloc to the ROM call.
				RAMCall->Parent       = Section;
				RAMCall->Location     = Reloc->Location;
				RAMCall->Number       = SymNum;
				RAMCall->Size         = Reloc->Size;
				RAMCall->FixedOffset  = Reloc->Target.Offset + Reloc->FixedOffset;
				RAMCall->ExtraRAMAddr = (SymType == ST_EXTRA_RAM);
				if (Reloc->Relative)
					Warning (CurFileName, "RAM calls cannot be relative; changing to absolute.");
				// Remove the reloc.
				FreeReloc (Reloc);
				// Add the RAM call.
				Append (Section->RAMCalls, RAMCall);
				// Adjust RAM call statistics.
				if (Program->HighestRAMCall < SymNum)
					Program->HighestRAMCall = SymNum;
				Section->RAMCalls.Handled = FALSE;
			}
			return TRUE;
		
		case ST_LIB_CALL:
			{
				// Create a new library call.
				LIB_CALL *LibCall = calloc (1, sizeof (LIB_CALL));
				if (!LibCall)
				{
					Error (CurFileName, "Out of memory while resolving relocs.");
					return FALSE;
				}
				// Assign the attributes of the reloc to the library call.
				LibCall->Parent      = Section;
				LibCall->Location    = Reloc->Location;
				LibCall->Library     = SymRef;
				LibCall->Number      = SymNum;
				LibCall->Size        = Reloc->Size;
				LibCall->FixedOffset = Reloc->Target.Offset + Reloc->FixedOffset;
				if (Reloc->Relative)
					Warning (CurFileName, "Library calls cannot be relative; changing to absolute.");
				// Remove the reloc.
				FreeReloc (Reloc);
				// Add the library call.
				Append (Section->LibCalls, LibCall);
				if (LibCall->Library->Highest < SymNum)
					LibCall->Library->Highest = SymNum;
				if (!(LibCall->Library->Referenced))
				{
					LibCall->Library->Referenced = TRUE;
					Program->Libraries.ReferencedCount++;
				}
				Section->LibCalls.Handled = FALSE;
			}
			return TRUE;
		
		default:
			return FALSE;
	}
}

// Resolve the reloc's target, if possible.
// Force: Fail on unresolvable reference.
static BOOLEAN ResolveRelocTarget (RELOC *Reloc, BOOLEAN Force)
{
	SECTION *Section = Reloc->Parent;
	PROGRAM *Program = Section->Parent;
	
	// If the target needs to be resolved, then try to do this.
	if (!(Reloc->Target.Symbol))
	{
		COUNT LoopCount = 0;
		
		do {
			BOOLEAN TryAgain = FALSE;
			SYMBOL *Symbol;
			
			// Resolve it as a reloc to a symbol.
			if ((Symbol = ResolveLocation (Program, Section, &(Reloc->Target))))
			{
				IncRefCounts (Section, Symbol->Parent, Reloc);
				return TRUE;
			}
			// Resolve it as a ROM/RAM/library call.
			else if (ResolveSpecialExternalSymbolReloc (Reloc))
				return TRUE;
			// Resolve it as a special ld-exported symbol reloc.
			else if (ResolveSpecialSymbolRelocTarget (Reloc, &TryAgain))
			{
				if (!TryAgain)
					return TRUE;
			}
			// Import the symbol from an archive.
			else if ((!(Program->Frozen)) && (ImportSymbolFromArchive (Program, Reloc->Target.SymbolName)))
				// Continue to loop.
				;
			// It seems the reloc is currently unresolvable.
			else if (Force)
			{
				Error (GetFileName (Section, Reloc->Location), "Unresolved reference to `%s'.", Reloc->Target.SymbolName);
				return FALSE;
			}
			
			// Increase loop counter.
			LoopCount++;
			// Only re-loop twice. This enables us to use the
			// "TryAgain" from ResolveSpecialSymbolReloc and
			// ImportSymbolFromArchive, but prevents us from
			// entering an infinite loop.
		} while (LoopCount <= 2);
		
		return FALSE;
	}
	
	return TRUE;
}

// Resolve the reloc's relation, if possible.
// Force: Fail on unresolvable reference.
static BOOLEAN ResolveRelocRelation (RELOC *Reloc, BOOLEAN Force)
{
	SECTION *Section = Reloc->Parent;
	PROGRAM *Program = Section->Parent;
	
	// If the relation needs to be resolved, then try to do this.
	if (Reloc->Relation && (!(Reloc->Relation->Symbol)))
	{
		COUNT LoopCount = 0;
		
		do {
			BOOLEAN TryAgain = FALSE;
			
			// Resolve it as a (negative) reloc to a symbol.
			if (ResolveLocation (Program, Section, Reloc->Relation))
				return TRUE;
			// Resolve it as a special ld-exported symbol reloc.
			else if (ResolveSpecialSymbolRelocRelation (Reloc, &TryAgain))
			{
				if (!TryAgain)
					return TRUE;
			}
			// Import the symbol from an archive.
			else if ((!(Program->Frozen)) && (ImportSymbolFromArchive (Program, Reloc->Relation->SymbolName)))
				// Continue to loop.
				;
			// It seems the reloc is currently unresolvable.
			else if (Force)
			{
				Error (GetFileName (Section, Reloc->Location), "Unresolved reference to `%s'.", Reloc->Relation->SymbolName);
				return FALSE;
			}
			
			// Increase loop counter.
			LoopCount++;
			// Only re-loop twice. This enables us to use the
			// "TryAgain" from ResolveSpecialSymbolReloc and
			// ImportSymbolFromArchive, but prevents us from
			// entering an infinite loop.
		} while (LoopCount <= 2);
		
		return FALSE;
	}
	
	return TRUE;
}

// Resolve the reloc, if possible.
// Force: Fail on unresolvable references.
BOOLEAN ResolveReloc (RELOC *Reloc, BOOLEAN Force)
{
	// First, resolve the relation. This might add the
	// resulting value to the reloc's target offset.
	BOOLEAN Result = ResolveRelocRelation (Reloc, Force);
	// Then resolve the target.
	if (ResolveRelocTarget (Reloc, Force))
		return Result;
	else
		return FALSE;
}

// Resolve all relocs in the program, if possible.
// Force: Fail on unresolvable references.
BOOLEAN ResolveRelocs (PROGRAM *Program, BOOLEAN Force)
{
	BOOLEAN Result = TRUE;
	
	SECTION *Section;
	
	// For each section...
	for_each (Section, Program->Sections)
	{
		// Only do something if it contains unresolved references at all.
		if (Section->Relocs.UnresolvedCount > 0)
		{
			RELOC *Reloc, *NextReloc;
			
			// For each reloc...
			for (Reloc = GetFirst (Section->Relocs); Reloc; Reloc = NextReloc)
			{
				NextReloc = GetNext (Reloc);
				
				// Try to resolve it.
				if (!(ResolveReloc (Reloc, Force)))
					Result = FALSE;
			}
		}
	}
	
	return Result;
}

// Optimize the location to have the least possible offset.
void OptimizeLocation (LOCATION *Location)
{
	if (Location)
	{
		SYMBOL *OldSymbol = Location->Symbol;
		
		// Only proceed if the location points to a real symbol.
		if (OldSymbol)
		{
			// Find the symbol the reloc really points to.
			SYMBOL *NewSymbol = FindSymbolAtPos (OldSymbol->Parent, OldSymbol->Location, FALSE);
			
			// If we found a symbol that is closer, use that one.
			if (NewSymbol && (NewSymbol->Location != OldSymbol->Location))
			{
				Location->Offset += OldSymbol->Location - NewSymbol->Location;
				Location->Symbol = NewSymbol;
				Location->SymbolName = NewSymbol->Name;
			}
		}
	}
}

// Optimize all relocs to have the least possible target and relation offset.
void OptimizeRelocs (PROGRAM *Program)
{
	SECTION *Section;
	
	// For each section...
	for_each (Section, Program->Sections)
	{
		RELOC *Reloc;
		
		// For each reloc...
		for_each (Reloc, Section->Relocs)
		{
			// Optimize the target.
			OptimizeLocation (&(Reloc->Target));
			
			// Optimize the relation, if any.
			OptimizeLocation (Reloc->Relation);
		}
	}
}

// Merge section Src into Dest.
SECTION *MergeSections (SECTION *Dest, SECTION *Src)
{
	PROGRAM *Program = Dest->Parent;
	SIZE OrigSize, OrigSizePadded = 0;
	
	// Merging a section into itself should succeed without doing anything.
	if (Dest == Src)
		return Dest;
	
	// Handle the insertions at the end of the destination section, assuming
	// that the source section contains everything we need to output. This
	// is the best we can do.
	HandleSectionContents (Dest, Src);
	
#ifdef FLASH_OS_SUPPORT
	// In Flash OS mode, if we are merging the startup and normal parts, pad the
	// startup part to 32 KB.
	if ((Program->Type == PT_FLASH_OS) && Dest->StartupNumber && (!(Src->StartupNumber)))
		OrigSizePadded = 0x8000;
#endif /* FLASH_OS_SUPPORT */
	
	// Fix the code for the two sections.
	if (Dest->Code)
		M68kFixCodePreMerge (Dest, Src, OrigSizePadded);
	
	// If the destination size is fixed, disable range cutting.
	if (OrigSizePadded)
		DisableRangeCutting (Dest);
	
	// Create segments covering the entire source and destination sections.
	CreateSectionSegment (Dest);
	CreateSectionSegment (Src);
	
	// A startup section in a nostub program is against the rules of "nostub",
	// which means that no data except the raw program is included.
	if ((Program->Type == PT_NOSTUB) && (Dest->StartupNumber || Src->StartupNumber))
	{
		Warning (Dest->FileName, "Program declared as `nostub' contains startup sections.");
		// Switch to native mode to prevent more warnings, and because it makes sense.
		Program->Type = PT_NATIVE;
	}
	
	if (Program->Library && (Dest->StartupNumber < 0) && (Src->StartupNumber > 0))
		Warning (Dest->FileName, "Adding program startup section to library.");
	
	// Store the size of the destination section.
	OrigSize = Dest->Size;
	
#ifdef DEBUGGING_INFO_SUPPORT
	// Debugging information shouldn't be padded, the DWARF 2 format doesn't support padding.
	if (Dest->DebuggingInfoType)
		OrigSizePadded = OrigSize;
#endif /* DEBUGGING_INFO_SUPPORT */
	
	// If no size is specified yet, pad the destination section to have an even length.
	if (!OrigSizePadded)
		OrigSizePadded = GetPaddedSize (OrigSize, M68K_SECTION_ALIGNMENT);
	
	// Check whether the current size exceeds a fixed maximum.
	{
		SIZE MaxSize = OrigSizePadded;
#ifdef FLASH_OS_SUPPORT
		// In Flash OS mode, the maximum size for the startup part is actually 8 KB less.
		if ((Program->Type == PT_FLASH_OS) && Dest->StartupNumber && (!(Src->StartupNumber)))
			MaxSize -= 0x2000;
#endif /* FLASH_OS_SUPPORT */
		if (OrigSize > MaxSize)
			Warning (Dest->FileName, "Section size of %ld bytes is larger than requested size of %ld.", (long) OrigSize, (long) MaxSize);
	}
	
	// Check whether the padded size is really larger than the current size.
	// If not, this has already generated the warning above.
	// We check here (not above) because it's safer.
	if (OrigSizePadded < OrigSize)
		OrigSizePadded = OrigSize;
	
	// Adjust the size of the destination.
	Dest->Size = OrigSizePadded + Src->Size;
	// If one of the two sections has data, we need to keep it, so adjust the
	// other section accordingly. Otherwise, merging is simple.
	if (Dest->Data || Src->Data)
	{
		// Grow (or allocate) the data area of the destination section.
		if (Dest->Data)
		{
			Dest->Data = realloc (Dest->Data, Dest->Size);
			// Pad with zeroes if necessary.
			if (Dest->Data && (OrigSizePadded > OrigSize))
				memset (Dest->Data + OrigSize, 0, OrigSizePadded - OrigSize);
		}
		else if (Dest->Size)
			Dest->Data = calloc (Dest->Size, 1);
		if (Dest->Data || (!(Dest->Size)))
		{
			if (Dest->Data)
			{
				// Copy the data.
				if (Src->Data)
					memcpy (Dest->Data + OrigSizePadded, Src->Data, Src->Size);
				else
					memset (Dest->Data + OrigSizePadded, 0, Src->Size);
			}
		}
		else
		{
			Error (Dest->FileName, "Out of memory while merging sections.");
			return NULL;
		}
		Dest->Initialized = TRUE;
	}
	else
	{
		// Just need to merge attributes.
		Dest->Initialized |= Src->Initialized;
	}
	Dest->Mergeable = Dest->Mergeable && Src->Mergeable;
	Dest->Unaligned = Dest->Unaligned && Src->Unaligned;
	Dest->Essential |= Src->Essential;
	Dest->Constructors = Dest->Constructors && Src->Constructors;
	Dest->Destructors = Dest->Destructors && Src->Destructors;
	Dest->CanCutRanges |= Src->CanCutRanges;
	Dest->Frozen |= Src->Frozen;
	
	// Enable code optimizations if they are enabled for one of the sections.
	// It will be disabled on a segment basis if necessary.
	Dest->Code |= Src->Code;
	
#define MergeHandling(Items) (Dest->Items.Handled = (Dest->Items.Handled && Src->Items.Handled) || (Dest->Items.Handled && IsEmpty(Src->Items)) || (Src->Items.Handled && IsEmpty(Dest->Items)))
	
	MergeHandling (ROMCalls);
	MergeHandling (RAMCalls);
	MergeHandling (LibCalls);
	
#undef MergeHandling
	
	// Define a macro to make merging items more simple.
#define MergeItems(Type,Item) \
({ \
	Type *Item, *Next##Item; \
	for (Item = GetFirst (Src->Item##s); Item; Item = Next##Item) \
	{ \
		Next##Item = GetNext (Item); \
		Unlink (Src->Item##s, Item); \
		Item->Location += OrigSizePadded; \
		Append (Dest->Item##s, Item); \
		Item->Parent = Dest; \
	} \
})
	
	// Merge everything, except segments.
	MergeItems (SYMBOL,   Symbol);
	MergeItems (RELOC,    Reloc);
	MergeItems (ROM_CALL, ROMCall);
	MergeItems (RAM_CALL, RAMCall);
	MergeItems (LIB_CALL, LibCall);
	
#undef MergeItems
	
	// Merge segments.
	{
		SEGMENT *Segment, *NextSegment;
		for (Segment = GetFirst (Src->Segments); Segment; Segment = NextSegment)
		{
			NextSegment = GetNext (Segment);
			Unlink (Src->Segments,  Segment);
			Append (Dest->Segments, Segment);
		}
	}
	
	Dest->Relocs.UnresolvedCount += Src->Relocs.UnresolvedCount;
	Dest->Relocs.EmittedCount    += Src->Relocs.EmittedCount;
	
	// Free the source section.
	FreeSection (Src);
	
	return Dest;
}

// Merge all sections of the specified type.
SECTION *MergeAllSections (PROGRAM *Program, SECTION *Dest, BOOLEAN AcceptInitialized, BOOLEAN AcceptUninitialized, BOOLEAN AcceptZeroes, BOOLEAN AcceptContents, BOOLEAN AcceptData, BOOLEAN AcceptCode, BOOLEAN AcceptNonConstructors, BOOLEAN AcceptConstructors, BOOLEAN AcceptDestructors, BOOLEAN AcceptNonStartup, BOOLEAN AcceptStartup, DebuggingInfoTypes AcceptDebuggingInfo)
{
	BOOLEAN MergeForward = FALSE;
	SECTION *CurMergedSection = NULL, *Section, *NextSection;
	
	// For each section...
	for (Section = GetLast (Program->Sections); Section; Section = NextSection)
	{
		// Get the next section now, since GetNext won't work once the section
		// has been freed.
		NextSection = GetPrev (Section);
		
		// Check if the section meets the specified requirements.
		if ((!(Section->Handled))
			&& (AcceptInitialized     || (!(Section->Initialized)))
			&& (AcceptUninitialized   || Section->Initialized)
			&& (AcceptZeroes          || Section->Data || (!(Section->Size)) || (!(Section->Initialized)) || Section->StartupNumber)
			&& (AcceptContents        || (!(Section->Data) && Section->Size) || (!(Section->Initialized)))
			&& (AcceptData            || Section->Code)
			&& (AcceptCode            || (!(Section->Code)))
			&& (AcceptNonConstructors || Section->Constructors || Section->Destructors)
			&& (AcceptConstructors    || (!(Section->Constructors)))
			&& (AcceptDestructors     || (!(Section->Destructors)))
			&& (AcceptNonStartup      || Section->StartupNumber)
			&& (AcceptStartup         || (!(Section->StartupNumber)))
			&& (AcceptDebuggingInfo   == Section->DebuggingInfoType))
		{
			// If no current merged section has been specified, make this
			// one current. Otherwise, merge the current merged section
			// into this one.
			if (CurMergedSection)
			{
				if (MergeForward)
					CurMergedSection = MergeSections (CurMergedSection, Section);
				else
					CurMergedSection = MergeSections (Section, CurMergedSection);
				
				if (!CurMergedSection)
					return NULL;
			}
			else
			{
				CurMergedSection = Section;
				if (!MergeForward)
					M68kFixCode (Section);
			}
			
			// If an explicit destination section exists, and this section
			// is the destination, set a flag to append all following
			// sections at the end.
			if (CurMergedSection == Dest)
				MergeForward = TRUE;
		}
	}
	
	if (CurMergedSection)
	{
		M68kFixCode (CurMergedSection);
		HandleSectionContents (CurMergedSection, CurMergedSection);
	}
	
	return CurMergedSection;
}

// Get the size that would result from padding a section of size
// OrigSize to a multiple of Alignment.
SIZE GetPaddedSize (SIZE OrigSize, COUNT Alignment)
{
	return (((OrigSize + Alignment - 1) / Alignment) * Alignment);
}

// Pad the section so its size becomes a multiple of Alignment.
BOOLEAN PadSection (SECTION *Section, COUNT Alignment)
{
	SIZE OrigSize = Section->Size;
	SIZE NewSize = GetPaddedSize (OrigSize, Alignment);
	
	if (NewSize > OrigSize)
	{
		// Increase the size of the section data.
		if (Section->Data)
		{
			I1 *NewData = realloc (Section->Data, NewSize);
			if (NewData)
			{
				memset (NewData + OrigSize, 0, NewSize - OrigSize);
				Section->Data = NewData;
			}
			else
			{
				Error (NULL, "Out of memory.");
				return FALSE;
			}
		}
		
		// Increase the size field.
		Section->Size = NewSize;
	}
	
	return TRUE;
}

// Try to resolve and remove relative relocs.
BOOLEAN FixupRelativeRelocs (PROGRAM *Program)
{
	BOOLEAN Result = TRUE;
	SECTION *Section;
	
	// For each section...
	for_each (Section, Program->Sections)
	{
		// If it contains real data...
		if (Section->Data)
		{
			RELOC *Reloc, *NextReloc;
			
			// For each reloc in the section...
			for (Reloc = GetFirst (Section->Relocs); Reloc; Reloc = NextReloc)
			{
				// Get the next reloc now because we might remove this one.
				NextReloc = GetNext (Reloc);
				
				if (Reloc->Relative)
				{
					// Try to resolve and remove this reloc.
					if (!FixupRelativeReloc (Reloc))
						Result = FALSE;
				}
			}
		}
	}
	
	return Result;
}

// Try to resolve and remove a single relative reloc.
// This may cause the reloc to be freed.
BOOLEAN FixupRelativeReloc (RELOC *Reloc)
{
	BOOLEAN Result = FALSE;
	SECTION *Section = Reloc->Parent, *RelationSection;
	const char *CurFileName = GetFileName (Section, Reloc->Location);
	
	if (Reloc->Relation && Reloc->Relation->Symbol)
		RelationSection = Reloc->Relation->Symbol->Parent;
	else
		RelationSection = Section;
	
	// Check if we can resolve the reloc.
	if (Reloc->Relative && Reloc->Target.Symbol && ((!(Reloc->Relation)) || Reloc->Relation->Symbol))
	{
		if (Reloc->Target.Symbol->Parent == RelationSection)
		{
			// Now there are hidden relocs in the section.
			DisableRangeCutting (Section);
			
			if ((Reloc->Location >= 0) && (Reloc->Location + Reloc->Size <= Section->Size))
			{
				OFFSET Relation;
				
				// Check if the section contents at the reloc are zero.
				if (!(IsZeroDataRange (Section, Reloc->Location, Reloc->Location + Reloc->Size)))
					Warning (CurFileName, "Reloc at 0x%lX to `%s' on nonzero section contents. Overlapping with another?", (long) Reloc->Location, Reloc->Target.SymbolName);
				
				// Get the relative position of the target symbol (plus offset).
				Relation = GetLocationOffset (RelationSection, &(Reloc->Target)) + Reloc->FixedOffset;
				if (Reloc->Relation)
					Relation -= GetLocationOffset (RelationSection, Reloc->Relation);
				else
					Relation -= Reloc->Location;
				
				{
					// Try to tack the difference onto another reloc in order to avoid a "reloc on nonzero section contents" warning:
					RELOC *AnotherReloc = FindCompatibleReloc (Reloc, TRUE);
					if (AnotherReloc)
					{
						// Resolve the reloc by writing the address difference into the
						// other reloc's fixed offset.
						AnotherReloc->FixedOffset += Relation;
						Result = TRUE;
					}
					else
					{
						// Resolve the reloc by writing the relative position into the section.
						Result = AddTI (Section->Data + Reloc->Location, Reloc->Size, Relation, TRUE, Reloc->Relation != NULL);
						
						if (!Result)
							Error (CurFileName, "Symbol `%s' too far for size %ld reloc at 0x%lX.", Reloc->Target.SymbolName, (long) Reloc->Size, (long) Reloc->Location);
					}
				}
			}
			else
			{
				Warning (CurFileName, "Removing reloc at 0x%lX to `%s' outside of section.", (long) Reloc->Location, Reloc->Target.SymbolName);
				Result = TRUE;
			}
			
			if (Result)
				FreeReloc (Reloc);
		}
		else
			Error (CurFileName, "Relative reloc at 0x%lX to `%s' in different section.", (long) Reloc->Location, Reloc->Target.SymbolName);
	}
	else
		Error (CurFileName, "Cannot resolve reloc at 0x%lX to `%s'.", (long) Reloc->Location, Reloc->Target.SymbolName);
	
	return Result;
}

// Find a reloc with the same location and size as this one.
RELOC *FindCompatibleReloc (const RELOC *Reloc, BOOLEAN AllowRelative)
{
	return FindMatchingReloc (Reloc->Parent, Reloc->Location, Reloc->Size, AllowRelative, Reloc, Reloc);
}

// Find a reloc which matches the specified criteria.
RELOC *FindMatchingReloc (const SECTION *Section, OFFSET Location, SIZE Size, BOOLEAN AllowRelative, const void *Exception, const RELOC *Hint)
{
	if (Hint && (Hint->Parent != Section))
		Hint = NULL;
	
	// Find the first reloc at the position.
	while (Hint && (Hint->Location >= Location))
		Hint = GetPrev (Hint);
	while (Hint && (Hint->Location < Location))
		Hint = GetNext (Hint);
	if (!Hint)
		Hint = FindRelocAtPos (Section, Location, TRUE);
	
	// For each reloc that still has the right location...
	while (Hint && (Hint->Location == Location))
	{
		// Check whether it matches the criteria.
		if ((Hint->Size == Size)
		 && (AllowRelative || (!(Hint->Relative)))
		 && (Hint != Exception))
			return (RELOC *) Hint;
		
		Hint = GetNext (Hint);
	}
	
	return NULL;
}

// Make a reloc relative, and set its relation to the beginning of
// the section plus Offset. If Section is NULL, the program entry
// point is used as the relation.
BOOLEAN SetRelocRelation (RELOC *Reloc, SECTION *Section, OFFSET Offset)
{
	// Make this a relative reloc.
	Reloc->Relative = TRUE;
	// If it already has a relation, free it.
	FreeRelocRelation (Reloc);
	// Set the relation to the section's beginning.
	if (Section)
	{
		if ((Reloc->Relation = calloc (1, sizeof (LOCATION))))
		{
			Reloc->Relation->Symbol = Section->SectionSymbol;
			Reloc->Relation->SymbolName = Section->SectionSymbol->Name;
			Reloc->Relation->Offset = Offset;
		}
		else
		{
			Error (NULL, "Out of memory.");
			return FALSE;
		}
	}
	else
	{
		PROGRAM *Program = Reloc->Parent->Parent;
		// If Section is NULL, use the program's entry point, and
		// adjust the fixed offset instead of the relation's offset.
		Reloc->Relation = &(Program->EntryPoint);
		Reloc->FixedOffset -= Offset;
	}
	return TRUE;
}

// Make a reloc relative to the program entry point.
BOOLEAN SetRelocProgramRelative (RELOC *Reloc)
{
	return SetRelocRelation (Reloc, NULL, 0);
}

// Find or create a common symbol with specified name and size.
SYMBOL *MakeCommonSymbol (PROGRAM *Program, const char *SymName, SIZE Size, BOOLEAN Initialize, const char *FileName)
{
	// We need to search for a BSS section with a symbol with the same name,
	// and grow this section if necessary. If not found, we create a new
	// small BSS section.
	SECTION *CurSection;
	// For each section...
	for_each (CurSection, Program->Sections)
	{
		// Check only for sections with uninitialized or zeroed data.
		// Check if it has at least one symbol in it.
		if ((!(CurSection->Data)) && (!(IsEmpty (CurSection->Symbols))))
		{
			SYMBOL *CurSymbol;
			// For each symbol...
			for_each (CurSymbol, CurSection->Symbols)
			{
				// Check if the symbol name matches.
				if (CurSymbol->Exported && (!(strcmp (CurSymbol->Name, SymName))))
				{
					// Yes, we have found our symbol.
					// Check if the size is sufficient.
					const SYMBOL *NextSymbol = GetNext (CurSymbol);
					// If there is no symbol that follows this one, we
					// assume the symbol takes up all the space from its
					// label to the end of the section.
					OFFSET SymEnd = NextSymbol ? NextSymbol->Location : CurSection->Size;
					// Check if the size is sufficient.
					if (SymEnd - CurSymbol->Location >= Size)
						// We were lucky; we can use this symbol directly.
						return CurSymbol;
					else
					{
						// We need to grow the BSS section. However, we
						// should do this only if the symbol is the last
						// one; otherwise maybe we could adjust all other
						// symbol locations, but I don't know if that is
						// safe. Apart from that, sharing a common symbol
						// with a probably non-common (e.g. A68k) BSS
						// section is probably not a very good idea
						// anyway.
						if (!NextSymbol)
						{
							// Grow the section. If the symbol's location
							// is not zero, we need to add this location
							// to the section's new size.
							CurSection->Size = CurSymbol->Location + Size;
							return CurSymbol;
						}
					}
				}
			}
		}
	}
	
	// The symbol doesn't exist yet. Fine, we need to create
	// a new BSS block of specified size, with one symbol in
	// it, and refer to this symbol. This is a little bit of
	// work, but not very difficult.
	{
		// Create a new section, initialize it, and append it to the list of sections.
		SECTION *Section = calloc (1, sizeof (SECTION));
		if (!Section)
		{
			Error (FileName, "Out of memory.");
			return NULL;
		}
		Section->Parent = Program;
		Section->Initialized = Initialize;
		Section->Size = Size;
		Section->FileName = FileName;
		Append (Program->Sections, Section);
		
		{
			// Create a symbol in the new section.
			SYMBOL *Symbol = calloc (1, sizeof (SYMBOL));
			if (!Symbol)
			{
				Error (FileName, "Out of memory.");
				return NULL;
			}
			Symbol->Parent = Section;
			strncpy (Symbol->Name, SymName, MAX_SYM_LEN);
			Symbol->Exported = TRUE;
			Append (Section->Symbols, Symbol);
			
			// Since the new symbol is at the beginning of the section, it is
			// sufficient as a section symbol.
			Section->SectionSymbol = Symbol;
			
			// Return the symbol.
			return Symbol;
		}
	}
}

// Try to find and import a symbol with a given name in some archive.
BOOLEAN ImportSymbolFromArchive (PROGRAM *Program, const char *SymName)
{
	ARCHIVE *Archive;
	
	// For each archive...
	for_each (Archive, Program->Archives)
	{
		ARCHIVE_SYMBOL *Symbol;
		
		// For each symbol...
		for_each (Symbol, Archive->Symbols)
		{
			// If the name matches...
			if (!(strcmp (SymName, Symbol->Name)))
				// Import it.
				return (ImportArchiveSymbol (Program, Symbol));
		}
	}
	
	return FALSE;
}

// Create marker symbols at the beginning and end of the section, and write
// them to Marker. Returns NULL on error.
SECTION_MARKERS *CreateSectionMarkers (SECTION_MARKERS *Marker, SECTION *Section)
{
	// Check if the parameters are valid.
	if (Marker && Section)
	{
		SYMBOL *Symbol;

		// Mark the section as essential so it isn't removed.
		Section->Essential = TRUE;
		
		// Use the section symbol for the beginning.
		Marker->Start = Section->SectionSymbol;
		
		// Create a symbol at the end.
		Symbol = calloc (1, sizeof (SYMBOL));
		if (!Symbol)
		{
			Error (NULL, "Out of memory.");
			return NULL;
		}
		Symbol->Parent = Section;
		Symbol->Location = Section->Size;
		strcpy (Symbol->Name, Section->SectionSymbol->Name);
		if (strlen (Symbol->Name) + sizeof (" end") - 1 <= MAX_SYM_LEN)
			strcat (Symbol->Name, " end");
		Append (Section->Symbols, Symbol);
		Marker->End = Symbol;
		
		return Marker;
	}
	else
		return NULL;
}

// Get the object file at the specified position in the archive.
ARCHIVE_OBJECT *GetArchiveObject (ARCHIVE *Archive, FILE_PTR FileOffset)
{
	ARCHIVE_OBJECT *Object;
	
	// Look whether an object file at this position is already known.
	for_each (Object, Archive->ObjectFiles)
	{
		if (Object->FileOffset == FileOffset)
			// Yes, so no need to do anything.
			return Object;
	}
	
	// Allocate a new library.
	Object = calloc (1, sizeof (ARCHIVE_OBJECT));
	if (!Object)
	{
		Error (NULL, "Out of memory.");
		return NULL;
	}
	
	// Assign the data.
	Object->Parent = Archive;
	Object->FileOffset = FileOffset;
	
	// Add the object file.
	Append (Archive->ObjectFiles, Object);
	
	return Object;
}

// Get a reference to the library identified in the string. The library is
// added to the program's used libraries if necessary.
LIBRARY *GetLibrary (PROGRAM *Program, const char *LibName)
{
	LIBRARY *Library;
	
	// Look whether a library with this name already exists.
	for_each (Library, Program->Libraries)
	{
		if (!(strcmp (Library->Name, LibName)))
			// Yes, so no need to do anything.
			return Library;
	}
	
	// Allocate a new library.
	Library = calloc (1, sizeof (LIBRARY));
	if (!Library)
	{
		Error (NULL, "Out of memory.");
		return NULL;
	}
	
	// Assign the data.
	Library->Parent = Program;
	strncpy (Library->Name, LibName, MAX_SYM_LEN);
	
	// Add the library.
	Append (Program->Libraries, Library);
	
	return Library;
}

// Add a global import. This means that all archive members which export this
// symbol should be imported (unlike the import done by a reloc, which only
// imports the first member exporting the symbol).
GLOBAL_IMPORT *AddGlobalImport (PROGRAM *Program, const char *SymName)
{
	GLOBAL_IMPORT *Import = CreateGlobalImport (Program, SymName);

	if (Import)
		ResolveGlobalImport (Program, Import);

	return Import;
}

// First part of AddGlobalImport. Add a global import to the list.
GLOBAL_IMPORT *CreateGlobalImport (PROGRAM *Program, const char *SymName)
{
	GLOBAL_IMPORT *Import;
	
	// Look whether an import with this name already exists.
	for_each (Import, Program->GlobalImports)
	{
		if (!(strcmp (Import->SymbolName, SymName)))
			// Yes, so no need to do anything.
			// Otherwise, we could even end up in infinite recursion.
			// I don't know whether we should return NULL instead.
			return Import;
	}
	
	// Allocate a new import.
	Import = calloc (1, sizeof (GLOBAL_IMPORT));
	if (!Import)
	{
		Error (NULL, "Out of memory.");
		return NULL;
	}
	
	// Assign the data.
	Import->Parent = Program;
	strncpy (Import->SymbolName, SymName, MAX_SYM_LEN);
	
	// Add the import.
	Append (Program->GlobalImports, Import);
	
	return Import;
}

// Second part of AddGlobalImport only. Try to resolve the newly added global
// import against already available archives.
void ResolveGlobalImport (PROGRAM *Program, GLOBAL_IMPORT *Import)
{
	ARCHIVE *Archive;
	
	// For each archive...
	for_each (Archive, Program->Archives)
	{
		ARCHIVE_SYMBOL *Symbol;
		
		// For each symbol...
		for_each (Symbol, Archive->Symbols)
			// Check if the import and the symbol match.
			CheckGlobalImport (Import, Symbol);
	}
}

// Check whether the symbol matches any global import,
// performing the appropriate actions if necessary.
GLOBAL_IMPORT *CheckGlobalImports (PROGRAM *Program, ARCHIVE_SYMBOL *Symbol)
{
	GLOBAL_IMPORT *Import;
	
	for_each (Import, Program->GlobalImports)
		if (CheckGlobalImport (Import, Symbol))
			return Import;
	
	return NULL;
}

// Check whether the symbol matches the specified global import,
// performing the appropriate actions if necessary.
BOOLEAN CheckGlobalImport (GLOBAL_IMPORT *Import, ARCHIVE_SYMBOL *Symbol)
{
	PROGRAM *Program = Import->Parent;
	
	if (Symbol->ContainsInversion && (!(Program->GlobalImports.ResolveInversions)))
		return FALSE;
	
	// If the name matches, import the appropriate file.
	if (!(strcmp (Symbol->Name, Import->SymbolName)))
	{
		Import->Succeeded = TRUE;
		ImportArchiveSymbol (Program, Symbol);
		if ((Import == Program->BSSImport) && Program->BSSSection)
			Program->BSSSection->Handled = TRUE;
		return TRUE;
	}
	// If the symbol contains the name of the global import, it is probably an
	// ANDed combination with other imports. In that case, import it if all
	// conditions are met.
	else if (strstr (Symbol->Name, Import->SymbolName))
		return CheckMultiConditionSymbol (Program, Symbol);
	else
		return FALSE;
}

// Check whether all conditions are met for an archive symbol with multiple
// or inverted conditions. If they are, import the appropriate file, and
// increment the counters of all global imports fulfilling non-inverted
// conditions.
BOOLEAN CheckMultiConditionSymbol (PROGRAM *Program, ARCHIVE_SYMBOL *Symbol)
{
	BOOLEAN Result = TRUE;
	BOOLEAN ContainsBSSImport = FALSE;
	const char *SymName = Symbol->Name;
	const char *AndOperatorPos;
	
	if (Symbol->ContainsInversion && (!(Program->GlobalImports.ResolveInversions)))
		return FALSE;
	
	// Parse the symbol name from left to right, taking care of all operators.
	do {
		BOOLEAN Inverted = FALSE;
		unsigned long SymNameLen;
		GLOBAL_IMPORT *CurImport;
		
		// Check whether condition is inverted.
		if (!(strncmp (SYMOP_NOT, SymName, sizeof (SYMOP_NOT) - 1)))
		{
			if (!(Program->GlobalImports.ResolveInversions))
			{
				// This cannot happen actually due to the check at the
				// beginning, but it makes sense to check it anyway.
				Symbol->ContainsInversion = TRUE;
				return FALSE;
			}
			Inverted = TRUE;
			SymName += sizeof (SYMOP_NOT) - 1;
		}
		
		// Search for the next operator.
		AndOperatorPos = strstr (SymName, SYMOP_AND);
		if (AndOperatorPos)
			SymNameLen = AndOperatorPos - SymName;
		else
			SymNameLen = strlen (SymName);
		
		// Search for a global import which matches this part of the
		// symbol name.
		for_each (CurImport, Program->GlobalImports)
		{
			if ((!(strncmp (SymName, CurImport->SymbolName, SymNameLen))) && (SymNameLen == strlen (CurImport->SymbolName)))
			{
				Inverted = !Inverted;
				CurImport->Succeeded = TRUE;
				if (CurImport == Program->BSSImport)
					ContainsBSSImport = TRUE;
				break;
			}
		}
		
		// If the condition is inverted and a symbol is found, or it is
		// not inverted and no symbol is found, the import failed on this
		// symbol (at least for now).
		if (!Inverted)
			Result = FALSE;
		
		// Skip the operator.
		if (AndOperatorPos)
			SymName = AndOperatorPos + sizeof (SYMOP_AND) - 1;
	} while (AndOperatorPos);
	
	// All conditions have been met. Import the file.
	if (Result)
	{
		ImportArchiveSymbol (Program, Symbol);
		if (ContainsBSSImport && Program->BSSSection)
			Program->BSSSection->Handled = TRUE;
	}
	
	return Result;
}

// Resolve all remaining global imports. Usually, global imports are
// processed directly, but if a symbol contains an inverted condition, we
// have to wait until we really know that no such global import exists.
void ResolveRemainingGlobalImports (PROGRAM *Program)
{
	ARCHIVE *Archive;
	
	// All negated symbols which appear now are to be seen as "not succeeded".
	Program->GlobalImports.ResolveInversions = TRUE;
	
	// For each archive...
	for_each (Archive, Program->Archives)
	{
		ARCHIVE_SYMBOL *Symbol;
		
		// For each symbol...
		for_each (Symbol, Archive->Symbols)
		{
			// If it contains an inversion, we prevented it from being added
			// before. Import it now.
			if (Symbol->ContainsInversion)
				CheckMultiConditionSymbol (Program, Symbol);
		}
	}
}

// Check whether all global imports have succeeded at least once.
BOOLEAN CheckAllGlobalImports (PROGRAM *Program)
{
	BOOLEAN Result = TRUE;
	GLOBAL_IMPORT *Import;
	
	// For each import...
	for_each (Import, Program->GlobalImports)
	{
		// Check whether it has succeeded.
		if (!(Import->Succeeded))
		{
			Warning (NULL, "Unresolved global import `%s'.", Import->SymbolName);
			Result = FALSE;
		}
	}
	
	return Result;
}

// Find the first segment that overlaps with the specified range.
SEGMENT *FindSegment (const SECTION *Section, OFFSET Start, OFFSET End)
{
	// Find the first segment that affects this range.
	SEGMENT *Segment = FindSegmentAtPos (Section, Start, FALSE) ? : GetFirst (Section->Segments);
	
	// Check whether it really hits the specified range.
	if (Segment && (RangesOverlap (Segment->Location.Start->Location, Segment->Location.End->Location, Start, End)))
		return Segment;
	else
		return NULL;
}

// Check whether the specified range is declared to contain code.
BOOLEAN IsCodeRange (const SECTION *Section, OFFSET Start, OFFSET End)
{
	if (Section->Code)
	{
		// If no segments are specified, the entire section is one segment.
		if (IsEmpty (Section->Segments))
			return TRUE;
		// Otherwise, find the correct segment and look at it.
		else
		{
			const SEGMENT *Segment = FindSegment (Section, Start, End);
			return (Segment && Segment->Code);
		}
	}
	
	return FALSE;
}

// Check whether the section data in the specified range is zero. This only
// checks the actual data, not the relocs, ROM calls, etc.
BOOLEAN IsZeroDataRange (const SECTION *Section, OFFSET Start, OFFSET End)
{
	if (Section->Data)
	{
		OFFSET CurOffset;
		for (CurOffset = Start; CurOffset < End; CurOffset++)
		{
			if (!(IsZero (Section->Data [CurOffset])))
				return FALSE;
		}
		return TRUE;
	}
	else
		return Section->Initialized;
}

// Check whether the specified range contains only binary data, without
// relocs, ROM calls, or anything similar. The range includes Start, but not
// End.
// The Exception parameter specifies an item to skip when searching for
// possible problems.
BOOLEAN IsBinaryDataRange (const SECTION *Section, OFFSET Start, OFFSET End, const void *Exception)
{
	if ((Start < 0) || (End > Section->Size))
		return FALSE;
	{
		const SYMBOL *Symbol = FindSymbolAtPos (Section, Start, TRUE);
		while (Symbol && (Symbol->Location < End))
		{
			if ((Symbol != Exception) && (Symbol->Location > Start))
				return FALSE;
			Symbol = GetNext (Symbol);
		}
	}
	
	// Define a macro to make checking items more simple.
#define CheckItems(Type,Item) \
({ \
	const Type *Item = Find##Item##AtPos (Section, Start, FALSE) ? : GetFirst (Section->Item##s); \
	while (Item && (Item->Location < End)) \
	{ \
		if ((Item != Exception) && (RangesOverlap (Start, End, Item->Location, Item->Location + Item->Size))) \
			return FALSE; \
		Item = GetNext (Item); \
	} \
})
	
	CheckItems (RELOC,    Reloc);
	CheckItems (ROM_CALL, ROMCall);
	CheckItems (RAM_CALL, RAMCall);
	CheckItems (LIB_CALL, LibCall);
	
#undef CheckItems
	
	return TRUE;
}

// Check whether there is a reloc or symbol or something similar which
// prevents us from optimizing away some code at the end of the section.
// The Exception parameter specifies an item to skip when searching for
// possible problems.
BOOLEAN CanShrinkSection (const SECTION *Section, OFFSET Location, const void *Exception)
{
	// If we are not actually shrinking the section, either return value does
	// not make much sense. We return a negative result to stop the
	// optimization which is about to be done.
	if (Location >= Section->Size)
		return FALSE;
	else
	{
		// Define a macro to make checking items more simple.
#define CheckItems(Type,Item,LocationSuffix) \
({ \
	const Type *Item = Find##Item##AtPos (Section, Location LocationSuffix, TRUE); \
	while (Item) \
	{ \
		if (Item != Exception) \
			return FALSE; \
		Item = GetNext (Item); \
	} \
})
		
		CheckItems (SYMBOL,   Symbol,  + 1);
		CheckItems (RELOC,    Reloc,   );
		CheckItems (ROM_CALL, ROMCall, );
		CheckItems (RAM_CALL, RAMCall, );
		CheckItems (LIB_CALL, LibCall, );
		
#undef CheckItems
		
		return TRUE;
	}
}

// Cut the section off at the specified location. All items behind this
// location are removed.
void CutSection (SECTION *Section, OFFSET Location)
{
	if (Section->Size > Location)
	{
		Section->Size = Location;
		if (Section->Data)
		{
			I1 *NewData = realloc (Section->Data, Location);
			if (NewData || (!Location))
				Section->Data = NewData;
		}
		
		// Define a macro to make removing items more simple.
#define CutItems(Type,Item,ExtraCode) \
({ \
	Type *Item = Find##Item##AtPos (Section, Location, TRUE), *Next##Item; \
	while (Item) \
	{ \
		Next##Item = GetNext (Item); \
		if (!(ExtraCode)) \
		{ \
			Unlink (Section->Item##s, Item); \
			free (Item); \
		} \
		Item = Next##Item; \
	} \
})
		
		// Do not cut symbols, since they might be referenced.
		CutItems (RELOC,    Reloc,   ({ FreeReloc (Reloc); TRUE; }));
		CutItems (ROM_CALL, ROMCall, FALSE);
		CutItems (RAM_CALL, RAMCall, FALSE);
		CutItems (LIB_CALL, LibCall, FALSE);
		
#undef CutItems
		
	}
}

// Allocate space at the end of the section and return a pointer to it (NULL
// in case of error, or if Size is 0). The data is initialized to zero.
I1 *AllocateSpaceInSection (SECTION *Section, SIZE Size)
{
	if (Size)
	{
		// The old data size specifies how much we need to initialize to 0.
		SIZE OldSize = Section->Size, OldDataSize = (Section->Data ? OldSize : 0);
		// Allocate the new space. If the section does not have any data, allocate
		// data for it.
		I1 *NewData = realloc (Section->Data, OldSize + Size);
		// OldSize + Size should never be 0, so if NewData is NULL,
		// this really means "out of memory", and the original
		// pointer will still be valid.
		if (NewData)
		{
			Section->Data = NewData;
			// Set everything to 0.
			memset (NewData + OldDataSize, 0, OldSize + Size - OldDataSize);
			Section->Size += Size;
			return (NewData + OldSize);
		}
		else
			Error (NULL, "Out of memory.");
	}
	return NULL;
}

// Apply general architecture-specific code fixes and optimizations to the
// program.
void FixCode (PROGRAM *Program)
{
	SIZE OldSize, NewSize; // Used for repeated optimization.
	
	// When cutting ranges, do the optimizations in a loop
	// because optimizing might bring targets into range.
	do {
		SECTION *Section;
		
		// Initialize the size counts in each iteration of the outer loop.
		OldSize = 0;
		NewSize = 0;
		
		// For each section...
		for_each (Section, Program->Sections)
		{
			if (Section->Code)
			{
				// Count the section size before optimizing it.
				OldSize += Section->Size;
				
				// If it contains code, apply fixes.
				M68kFixCode (Section);
				
				// Count the section size after optimizing it.
				NewSize += Section->Size;
			}
		}
	} while (Program->OptimizeInfo->CutRanges && (NewSize < OldSize));
}

// Disable range cutting for an entire section.
void DisableRangeCutting (SECTION *Section)
{
	// Disable range cutting in the entire section.
	Section->CanCutRanges = FALSE;
	// Disable range cutting in all segments.
	{
		SEGMENT *Segment;
		for_each (Segment, Section->Segments)
			Segment->CanCutRanges = FALSE;
	}
}
