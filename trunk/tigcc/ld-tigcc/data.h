/* data.h: Definitions for internal data handling

   Copyright (C) 2002-2004 Sebastian Reichelt
   Copyright (C) 2003-2005 Kevin Kofler
   Copyright (C) 2004 Billy Charvet

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

#ifndef DATA_H
#define DATA_H

#include "generic.h"
#include "lists.h"
#include "intrface.h"

#define MAX_SYM_LEN 255

struct PROGRAM;
struct SECTION;
struct SEGMENT;
struct SYMBOL;
struct RELOC;
struct ROM_CALL;
struct RAM_CALL;
struct LIB_CALL;
struct LIBRARY;
struct ARCHIVE;
struct ARCHIVE_OBJECT;
struct ARCHIVE_SYMBOL;
struct GLOBAL_IMPORT;

// Program Type Enumeration
// PT_NATIVE:     ld-tigcc native mode; requires the manual definition of at least one startup section; not for use with Fargo and nostub DLLs
// PT_NOSTUB:     Traditional nostub mode; execution starts at the beginning of the first section
// PT_KERNEL:     Traditional kernel mode; kernel stub is included automatically
// PT_NOSTUB_DLL: Nostub DLL mode; uses specific output format
// PT_FLASH_OS:   Flash OS mode: uses specific output and file format
// PT_FARGO:      Fargo II mode; uses specific output format
typedef enum {PT_NATIVE = 1, PT_NOSTUB = 2, PT_KERNEL = 0, PT_NOSTUB_DLL = 3, PT_FLASH_OS = 4, PT_FARGO = 5} ProgramTypes;

// Debugging Information Section Type Enumeration
// DI_NONE:           Not debugging information (same as FALSE)
// DI_DELETED:        Deleted section (referenced from debugging info only)
// DI_STAB:           Stabs symbol table
// DI_STABSTR:        Stabs string table
// DI_DEBUG_ABBREV:   DWARF 2 .debug_abbrev section
// DI_DEBUG_ARANGES:  DWARF 2 .debug_aranges section
// DI_DEBUG_FRAME:    DWARF 2 .debug_frame section
// DI_DEBUG_INFO:     DWARF 2 .debug_info section
// DI_DEBUG_LINE:     DWARF 2 .debug_line section
// DI_DEBUG_LOC:      DWARF 2 .debug_loc section
// DI_DEBUG_MACINFO:  DWARF 2 .debug_macinfo section
// DI_DEBUG_PUBNAMES: DWARF 2 .debug_pubnames section
// DI_DEBUG_RANGES:   DWARF 2 .debug_ranges section
// DI_DEBUG_STR:      DWARF 2 .debug_str section
// DI_EH_FRAME:       DWARF 2 .eh_frame section (used only for debugging in TIGCC)
// DI_LAST:           Last debugging information type, for iterating purposes
typedef enum {DI_NONE = FALSE, DI_DELETED = 1, DI_STAB = 2, DI_STABSTR = 3,
              DI_DEBUG_ABBREV = 4, DI_DEBUG_ARANGES = 5, DI_DEBUG_FRAME = 6,
              DI_DEBUG_INFO = 7, DI_DEBUG_LINE = 8, DI_DEBUG_LOC = 9,
              DI_DEBUG_MACINFO = 10, DI_DEBUG_PUBNAMES = 11,
              DI_DEBUG_RANGES = 12, DI_DEBUG_STR = 13, DI_EH_FRAME = 14,
              DI_LAST = 14} DebuggingInfoTypes;

typedef I4 VERSION;

// Named Location in a Section
typedef struct {
	struct SYMBOL *Symbol;  // Pointer to symbol, may be NULL.
	const char *SymbolName; // Symbol name. Must free if Symbol is NULL. Otherwise points to Symbol->Name. May not be NULL.
	OFFSET Offset;          // Offset in relation to the symbol. When optimizing, this is considered part of the address.
	BOOLEAN Builtin;        // Specifies whether the location is to be replaced by a built-in number.
} LOCATION;

// Section Markers
typedef struct {
	struct SYMBOL *Start, *End;
} SECTION_MARKERS;

// Complete Program
typedef struct PROGRAM {
	ProgramTypes Type;    // The target type of the program.
	BOOLEAN Library;      // Specifies whether this is a library.
	unsigned int Calcs;   // The calculators the program is designed for (constants defined by enum ProgramCalcs).
	VERSION Version;      // Version number of the program or library.
	I2 KernelFlags;       // Kernel program flags; only used in kernel mode.
	struct {
		LIST_HEADER(struct SECTION);
	} Sections;           // Sections in the program.
	struct {
		LIST_HEADER(struct LIBRARY);
		COUNT ReferencedCount; // The number of referenced libraries.
	} Libraries;          // Run-time libraries used by the program.
	struct {
		LIST_HEADER(struct ARCHIVE);
	} Archives;           // Available archives which object files can be imported from.
	struct {
		LIST_HEADER(struct GLOBAL_IMPORT);
		BOOLEAN ResolveInversions; // Specifies whether negated symbols should be processed. Otherwise, they are delayed.
	} GlobalImports;      // Global imports; see AddGlobalImport in manip.h.
	BOOLEAN
		ResolveAllBuiltins, // Specifies that all builtin symbols should be resolved now, e.g. to 0 or NULL in some cases.
		IgnoreGlobalImports, // Specifies that the __ref_all_... symbol has no effect.
		Frozen;           // Specifies that no positions, numbers, and sizes may be changed any more.
	LOCATION EntryPoint;  // Entry point of the program.
	struct SECTION
		*MainSection,     // Pointer to the main section, as soon as it is known. Usually only this section has to be written to the file.
		*DataSection,     // Pointer to the data section, if code and data are separated.
		*BSSSection;      // Pointer to the BSS section, if the program contains one.
#ifdef DEBUGGING_INFO_SUPPORT
	BOOLEAN HaveDebuggingInfo; // Flag set if any debugging information is present, so we don't emit a .dbg file if there is no debugging information anyway.
	struct SECTION
		*DebuggingInfoSection[DI_LAST]; // Pointers to the debugging information sections of each type.
#endif
	struct GLOBAL_IMPORT
		*BSSImport;       // Pointer to the global import which handles the BSS section.
	SECTION_MARKERS
		Constructors,     // Beginning and end of constructor section.
		Destructors;      // Beginning and end of destructor section.
	OFFSET
		HighestROMCall,   // Holds the highest ROM call number used.
		HighestRAMCall;   // Holds the highest RAM call number used.
#ifdef DATA_VAR_SUPPORT
	DATA_VAR_INFO
		*DataVarInfo;     // Data variable settings.
#endif /* DATA_VAR_SUPPORT */
	OPTIMIZE_INFO
		*OptimizeInfo;    // Optimization settings and results.
} PROGRAM;

// Section in a Program
typedef struct SECTION {
	LIST_ITEM_HEADER(struct SECTION);
	PROGRAM *Parent;
	SIZE Size;            // Size of the section, in bytes.
	I1 *Data;             // Pointer to section data, NULL if no data. Need to free this at the end.
	BOOLEAN Initialized;  // If Data is NULL, this indicates whether the section is initialized with zeroes; otherwise it is always true.
	BOOLEAN Code;         // Contains code. If this is true, Initialized must be true as well.
	BOOLEAN Mergeable;    // Specifies whether this section contains one or more constants which may be merged on a symbol basis.
	BOOLEAN Unaligned;    // Specifies whether this section can be placed at an unaligned address. If the section is mergeable, it is also assumed that each symbol can be unaligned.
	                      // (The latter case is the only one currently implemented.)
	BOOLEAN Essential;    // Specifies whether the section is essential (unremovable).
	BOOLEAN Referenced;   // Specifies whether the section is either essential itself or referenced somewhere (in a reloc) from an essential section.
	                      // This flag is significant only during RemoveUnusedSections. Before (or if not removing unused sections), it is always FALSE, afterwards, it is always TRUE.
	OFFSET StartupNumber; // If nonzero, specifies the location of the section in respect to other startup sections.
	DebuggingInfoTypes DebuggingInfoType; // Nonzero if this is a debugging information section, the actual value indicates what type of debugging information.
	BOOLEAN Constructors; // Is a vector of constructor functions.
	BOOLEAN Destructors;  // Is a vector of destructor functions.
	BOOLEAN CanCutRanges; // Range cutting is allowed at least in parts of the section (i.e. the section or segments in it do not contain any implicit relative relocs).
	BOOLEAN Frozen;       // Aside from trivial changes (i.e. changes to binary data or additions at the end), this section must stay as it is.
	BOOLEAN Handled;      // Specifies whether the section is already handled and doesn't need any further treatment. This implies Frozen.
	struct {
		LIST_HEADER(struct SEGMENT);
	} Segments;           // Sorted list of segments. If empty, this means the section consists of one big segment.
	struct {
		LIST_HEADER(struct SYMBOL);
	} Symbols;            // Sorted list of symbols (labels).
	struct {
		LIST_HEADER(struct RELOC);
		COUNT UnresolvedCount; // The number of unresolved relocs and reloc relations.
		COUNT EmittedCount; // The number of already emitted relocs (which have been removed or changed to program-relative ones).
		COUNT OptRefCount; // The number of optimizable references pointing from and to this section.
		BOOLEAN RelativeRefs; // Specifies whether there are relative references from or to this section.
		BOOLEAN StartupRefs; // Specifies whether this section references or is referenced by a startup section.
	} Relocs;             // Sorted list of relocation entries.
	struct {
		LIST_HEADER(struct ROM_CALL);
		BOOLEAN Handled;  // Specifies whether the program has requested information about ROM calls.
	} ROMCalls;           // Sorted list of ROM (AMS) calls.
	struct {
		LIST_HEADER(struct RAM_CALL);
		BOOLEAN Handled;  // Specifies whether the program has requested information about RAM calls.
	} RAMCalls;           // Sorted list of RAM (kernel) calls.
	struct {
		LIST_HEADER(struct LIB_CALL);
		BOOLEAN Handled;  // Specifies whether the program has requested information about library calls.
	} LibCalls;           // Sorted list of library calls.
	struct SYMBOL
		*SectionSymbol;   // Symbol pointing to the beginning of the section. May only be NULL on fatal errors.
	const char *FileName; // File where the section originated from. May be NULL.
} SECTION;

// Segment of a Section.
// Identifies a range that belongs to a specific pre-merge section.
typedef struct SEGMENT {
	LIST_ITEM_HEADER(struct SEGMENT);
	SECTION_MARKERS Location; // Symbols specifying the beginning and end of the segment.
	BOOLEAN Code;         // The segment contains code.
	BOOLEAN CanCutRanges; // Range cutting is allowed (i.e. the segment does not contain any implicit relative relocs).
	const char *FileName; // File where the segment originated from. May be NULL.
} SEGMENT;

// Symbol (Label) in a Section
typedef struct SYMBOL {
	LIST_ITEM_HEADER(struct SYMBOL);
	SECTION *Parent;
	OFFSET Location;      // Location of the symbol inside the section data.
	char Name[MAX_SYM_LEN+1]; // Symbol name.
	BOOLEAN Exported;     // Only exported symbols are possible targets for name resolution.
} SYMBOL;

// Relocation Entry in a Section
typedef struct RELOC {
	LIST_ITEM_HEADER(struct RELOC);
	SECTION *Parent;
	OFFSET Location;      // Location of the reloc inside the section data.
	SIZE Size;            // Size of the reloc, in bytes.
	LOCATION Target;      // The location of the target the reloc points to.
	BOOLEAN Relative;     // If true, the value to be inserted is relative to the location of the reloc. If false, it is the absolute address of the target.
	LOCATION *Relation;   // If Relative is true, this specifies that the value is not relative to the location of the reloc, but to this location. It must be freed.
	OFFSET FixedOffset;   // Fixed offset to add after relocation. When optimizing (i.e., removing code), this is NOT considered part of the address.
	BOOLEAN Unoptimizable;// If true, the reloc is not a source operand or branch target and can never be optimized.
} RELOC;

// ROM Call Reference in a Section
typedef struct ROM_CALL {
	LIST_ITEM_HEADER(struct ROM_CALL);
	SECTION *Parent;
	OFFSET Location;      // Location of the ROM call reference inside the section data.
	SIZE Size;            // Size of the ROM call reference, in bytes.
	OFFSET Number;        // Number of the ROM call.
	OFFSET FixedOffset;   // Offset in relation to the destination.
} ROM_CALL;

// RAM Call Reference in a Section
typedef struct RAM_CALL {
	LIST_ITEM_HEADER(struct RAM_CALL);
	SECTION *Parent;
	OFFSET Location;      // Location of the RAM call reference inside the section data.
	SIZE Size;            // Size of the RAM call reference, in bytes.
	OFFSET Number;        // Number of the RAM call.
	OFFSET FixedOffset;   // Offset in relation to the destination.
	BOOLEAN ExtraRAMAddr; // Specifies whether we mean a program-defined extra RAM table address.
} RAM_CALL;

// Run-Time Library Used by the Program.
typedef struct LIBRARY {
	LIST_ITEM_HEADER(struct LIBRARY);
	PROGRAM *Parent;
	char Name[MAX_SYM_LEN+1]; // Library Name.
	VERSION Version;      // Required minimum version number.
	OFFSET Highest;       // Holds the highest function number imported from this library.
	BOOLEAN Referenced;   // Library is actually used.
} LIBRARY;

// Library Call Reference in a Section
typedef struct LIB_CALL {
	LIST_ITEM_HEADER(struct LIB_CALL);
	SECTION *Parent;
	OFFSET Location;      // Location of the library call reference inside the section data.
	SIZE Size;            // Size of the library call reference, in bytes.
	LIBRARY *Library;     // Library the call references.
	OFFSET Number;        // Number of the library export.
	OFFSET FixedOffset;   // Offset in relation to the destination.
} LIB_CALL;

// Archive Available to the Program
typedef struct ARCHIVE {
	LIST_ITEM_HEADER(struct ARCHIVE);
	PROGRAM *Parent;
	const I1 *Data;       // Pointer to archive contents. Need to free this at the end.
	SIZE Size;            // Size of the archive, in bytes.
	struct {
		LIST_HEADER(struct ARCHIVE_OBJECT);
	} ObjectFiles;        // List of known object files.
	struct {
		LIST_HEADER(struct ARCHIVE_SYMBOL);
	} Symbols;            // List of exported symbols.
	const char *FileName; // File name of the archive.
} ARCHIVE;

// Object File in an Archive
typedef struct ARCHIVE_OBJECT {
	LIST_ITEM_HEADER(struct ARCHIVE_OBJECT);
	ARCHIVE *Parent;
	FILE_PTR FileOffset;  // Offset of the object file inside the archive.
	BOOLEAN Imported;     // Specifies whether the file has already been imported.
} ARCHIVE_OBJECT;

// Exported Symbol in an Archive
typedef struct ARCHIVE_SYMBOL {
	LIST_ITEM_HEADER(struct ARCHIVE_SYMBOL);
	ARCHIVE *Parent;
	const char *Name;     // Symbol name (pointer into Parent->Data).
	ARCHIVE_OBJECT *ObjectFile; // Object file this symbol belongs to.
	BOOLEAN ContainsInversion; // Specifies whether the symbol contains an inversion (NOT operator) for global imports.
} ARCHIVE_SYMBOL;

// Global Import
typedef struct GLOBAL_IMPORT {
	LIST_ITEM_HEADER(struct GLOBAL_IMPORT);
	PROGRAM *Parent;
	char SymbolName[MAX_SYM_LEN+1]; // Symbol name.
	BOOLEAN Succeeded;    // Specifies whether a symbol matching this import has been found.
} GLOBAL_IMPORT;

#endif
