/* manip.h: Routines to manipulate the internal data

   Copyright (C) 2002-2003 Sebastian Reichelt
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

#ifndef MANIP_H
#define MANIP_H

#include "generic.h"
#include "data.h"

// Free the program tree.
void FreeProgram (PROGRAM *Program);
// Free a section. The section is assumed not to be referenced.
void FreeSection (SECTION *Section);
// Free a relocation entry.
void FreeReloc (RELOC *Reloc);
// Free the relation reference of a relocation entry, and set it to
// NULL.
void FreeRelocRelation (RELOC *Reloc);
// Free the symbol name of a location, if this is necessary.
// If a symbol is already known, set the symbol name to its name.
// Decrease the number of unresolved relocs in the section.
void FreeLocationSymbolName (SECTION *Section, LOCATION *Location);

// Create a section symbol for the given section, if none has been
// created yet. If there already is one, set its name accordingly.
// Return the section symbol.
SYMBOL *CreateSectionSymbol (SECTION *Section, const char *SectionName);

// Create a segment for the entire given section, if none has been
// created yet. If the section already contains segments, return
// NULL.
SEGMENT *CreateSectionSegment (SECTION *Section);

// If the section is a startup section, insert it at the place where
// it belongs. Otherwise, append it to the end of the program.
void InsertSection (PROGRAM *Program, SECTION *Section);

// Find the last startup section below or at StartupNumber.
// If there is no such section, the function returns NULL.
SECTION *FindStartupSection (const PROGRAM *Program, OFFSET StartupNumber);

// Insert the item at the correct place in the section.
void InsertSymbol (SECTION *Section, SYMBOL *Symbol);
void InsertReloc  (SECTION *Section, RELOC  *Reloc);

// Find the item preceding or following the given location. May return NULL.
#define DefineFindItemAtPos(Type,Item) Type *Find##Item##AtPos (const SECTION *Section, OFFSET Location, BOOLEAN Following)
DefineFindItemAtPos (SYMBOL,   Symbol);
DefineFindItemAtPos (RELOC,    Reloc);
DefineFindItemAtPos (ROM_CALL, ROMCall);
DefineFindItemAtPos (RAM_CALL, RAMCall);
DefineFindItemAtPos (LIB_CALL, LibCall);
#undef DefineFindItemAtPos

// Get the file name where the code at a given location in the
// section came from. May return NULL.
const char *GetFileName (const SECTION *Section, OFFSET Location);

// Return the offset of the specified location into the section. If the
// offset is unknown, emit a warning and return 0.
OFFSET GetLocationOffset (const SECTION *Section, const LOCATION *Location);
// Increase the counters and reference information necessary when this
// location is used in the specified reloc.
void HandleLocation (RELOC *Reloc, LOCATION *Location);

// Point the location to the appropriate symbol, if one is found.
SYMBOL *ResolveLocation (PROGRAM *Program, SECTION *Section, LOCATION *Location);
// Resolve the reloc, if possible.
// Force: Fail on unresolvable references.
BOOLEAN ResolveReloc (RELOC *Reloc, BOOLEAN Force);
// Resolve all relocs in the program, if possible.
// Force: Fail on unresolvable references.
BOOLEAN ResolveRelocs (PROGRAM *Program, BOOLEAN Force);

// Optimize the location to have the least possible offset.
void OptimizeLocation (LOCATION *Location);
// Optimize all relocs to have the least possible target and relation offset.
void OptimizeRelocs (PROGRAM *Program);

// Merge section Src into Dest.
// Returns Dest if successful.
SECTION *MergeSections (SECTION *Dest, SECTION *Src);
// Merge all sections of the specified type.
// Returns the merged section if successful (even if Dest is NULL).
SECTION *MergeAllSections (PROGRAM *Program, SECTION *Dest, BOOLEAN AcceptInitialized, BOOLEAN AcceptUninitialized, BOOLEAN AcceptZeroes, BOOLEAN AcceptContents, BOOLEAN AcceptData, BOOLEAN AcceptCode, BOOLEAN AcceptNonConstructors, BOOLEAN AcceptConstructors, BOOLEAN AcceptDestructors, BOOLEAN AcceptNonStartup, BOOLEAN AcceptStartup, DebuggingInfoTypes AcceptDebuggingInfo);

// Get the size that would result from padding a section of size
// OrigSize to a multiple of Alignment.
SIZE GetPaddedSize (SIZE OrigSize, COUNT Alignment);
// Pad the section so its size becomes a multiple of Alignment.
BOOLEAN PadSection (SECTION *Section, COUNT Alignment);

// Try to resolve and remove relative relocs.
BOOLEAN FixupRelativeRelocs (PROGRAM *Program);
// Try to resolve and remove a single relative reloc.
// This may cause the reloc to be freed.
BOOLEAN FixupRelativeReloc (RELOC *Reloc);

// Find a reloc with the same location and size as this one.
RELOC *FindCompatibleReloc (const RELOC *Reloc, BOOLEAN AllowRelative);

// Find a reloc which matches the specified criteria.
RELOC *FindMatchingReloc (const SECTION *Section, OFFSET Location, SIZE Size, BOOLEAN AllowRelative, const void *Exception, const RELOC *Hint);

// Make a reloc relative, and set its relation to the beginning of
// the section plus Offset. If Section is NULL, the program entry
// point is used as the relation.
BOOLEAN SetRelocRelation (RELOC *Reloc, SECTION *Section, OFFSET Offset);
// Make a reloc relative to the program entry point.
BOOLEAN SetRelocProgramRelative (RELOC *Reloc);

// Find or create a common symbol with specified name and size.
SYMBOL *MakeCommonSymbol (PROGRAM *Program, const char *SymName, SIZE Size, BOOLEAN Initialize, const char *FileName);

// Try to find and import a symbol with a given name in some archive.
BOOLEAN ImportSymbolFromArchive (PROGRAM *Program, const char *SymName);

// Create marker symbols at the beginning and end of the section, and write
// them to Marker. Returns NULL on error.
SECTION_MARKERS *CreateSectionMarkers (SECTION_MARKERS *Marker, SECTION *Section);

// Get the object file at the specified position in the archive.
ARCHIVE_OBJECT *GetArchiveObject (ARCHIVE *Archive, FILE_PTR FileOffset);

// Get a reference to the library identified in the string. The library is
// added to the program's used libraries if necessary.
LIBRARY *GetLibrary (PROGRAM *Program, const char *LibName);

// Add a global import. This means that all archive members which export this
// symbol should be imported (unlike the import done by a reloc, which only
// imports the first member exporting the symbol).
// If no member was imported because of this, a warning will be displayed at
// the end.
// This calls CreateGlobalImport and ResolveGlobalImport.
GLOBAL_IMPORT *AddGlobalImport (PROGRAM *Program, const char *SymName);
// First part of AddGlobalImport. Add a global import to the list.
GLOBAL_IMPORT *CreateGlobalImport (PROGRAM *Program, const char *SymName);
// Second part of AddGlobalImport. Try to resolve the newly added global
// import against already available archives.
void ResolveGlobalImport (PROGRAM *Program, GLOBAL_IMPORT *Import);

// Check whether the symbol matches any global import,
// performing the appropriate actions if necessary.
GLOBAL_IMPORT *CheckGlobalImports (PROGRAM *Program, ARCHIVE_SYMBOL *Symbol);
// Check whether the symbol matches the specified global import,
// performing the appropriate actions if necessary.
BOOLEAN CheckGlobalImport (GLOBAL_IMPORT *Import, ARCHIVE_SYMBOL *Symbol);

// Check whether all conditions are met for an archive symbol with multiple
// or inverted conditions. If they are, import the appropriate file, and
// increment the counters of all global imports fulfilling non-inverted
// conditions.
BOOLEAN CheckMultiConditionSymbol (PROGRAM *Program, ARCHIVE_SYMBOL *Symbol);

// Resolve all remaining global imports. Usually, global imports are
// processed directly, but if a symbol contains an inverted condition, we
// have to wait until we really know that no such global import exists.
void ResolveRemainingGlobalImports (PROGRAM *Program);

// Check whether all global imports have succeeded at least once.
BOOLEAN CheckAllGlobalImports (PROGRAM *Program);

// Find the first segment that overlaps with the specified range.
SEGMENT *FindSegment (const SECTION *Section, OFFSET Start, OFFSET End);

// Check whether the specified range is declared to contain code.
BOOLEAN IsCodeRange (const SECTION *Section, OFFSET Start, OFFSET End);

// Check whether the section data in the specified range is zero. This only
// checks the actual data, not the relocs, ROM calls, etc.
BOOLEAN IsZeroDataRange (const SECTION *Section, OFFSET Start, OFFSET End);

// Check whether the specified range contains only binary data, without
// relocs, ROM calls, or anything similar. The range includes Start, but not
// End.
// The Exception parameter specifies an item to skip when searching for
// possible problems.
BOOLEAN IsBinaryDataRange (const SECTION *Section, OFFSET Start, OFFSET End, const void *Exception);

// Check whether there is a reloc or symbol or something similar which
// prevents us from optimizing away some code at the end of the section.
// The Exception parameter specifies an item to skip when searching for
// possible problems.
BOOLEAN CanShrinkSection (const SECTION *Section, OFFSET Location, const void *Exception);

// Cut the section off at the specified location. All items behind this
// location are removed.
void CutSection (SECTION *Section, OFFSET Location);

// Allocate space at the end of the section and return a pointer to it (NULL
// in case of error, or if Size is 0). The data is initialized to zero.
I1 *AllocateSpaceInSection (SECTION *Section, SIZE Size);

// Apply general architecture-specific code fixes and optimizations to the
// program.
void FixCode (PROGRAM *Program);

// Disable range cutting for an entire section.
void DisableRangeCutting (SECTION *Section);

#endif
