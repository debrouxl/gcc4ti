/* intrface.h: Header file for the interface of the embedded versions

   Copyright (C) 2003-2004 Sebastian Reichelt
   Copyright (C) 2003-2004 Kevin Kofler
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

#ifndef INTRFACE_H
#define INTRFACE_H

#include "generic.h"

// This specifies the current version of the export interface. The number has
// to be incremented whenever the interface changes.
#define CURRENT_INTERFACE_VERSION 17

// Optimization Information and Program Statistics
typedef struct ATTRIBUTE_PACKED {
	const char
		*Name;              // Name of the data variable, with or without path. No data variable if NULL.
	B2
		CreateCopy,         // Create and work on a copy of the variable.
		CopyOnlyIfArchived; // Work on the original variable if it is not archived.
} DATA_VAR_INFO;

// Optimization Information and Program Statistics
typedef struct ATTRIBUTE_PACKED {
	B2
		RemoveUnused,       // Remove unused sections.
		OptimizeRelocs,     // Optimize relocs for better readability in dumps.
		OptimizeNOPs,       // Optimize trailing NOPs in formats that insert them.
		OptimizeReturns,    // Optimize subroutine branches followed by RTS into jumps.
		OptimizeBranches,   // Optimize branches (e.g. to reduce relocation overhead).
		OptimizeMoves,      // Optimize move, load, and push instructions.
		OptimizeTests,      // Optimize compare and test instructions.
		OptimizeCalcs,      // Optimize calculation instructions.
		UseFLineJumps,      // Use F-Line jumps and subroutine jumps.
		Use4ByteFLineJumps, // Use 4-byte F-Line jumps and subroutine jumps.
		CutRanges,          // Cut unneeded section ranges when optimizing.
		ReorderSections,    // Reorder sections to shorten references.
		MergeConstants;     // Merge constants and strings to avoid duplication.
	SI4
		ProgramSize,            // Size of the on-calc program variable.
		DataSize,               // Size of the data variable.
		BSSSize,                // Size of the BSS section.
		RelocCount,             // Number of absolute relocs in the program.
		NativeRelocCount,       // Number of absolute relocs passed to the output format.
		OptimizeBranchesResult, // Absolute relocs saved/savable by OptimizeBranches.
		OptimizeMovesResult,    // Absolute relocs saved/savable by OptimizeMoves.
		OptimizeTestsResult,    // Absolute relocs saved/savable by OptimizeTests.
		OptimizeCalcsResult,    // Absolute relocs saved/savable by OptimizeCalcs.
		UseFLineJumpsResult,    // Absolute relocs saved/savable by UseFLineJumps or Use4ByteFLineJumps.
		CutRangesResult,        // Bytes saved/savable by range-cutting.
		NearAssemblyResult;     // Bytes savable by using pc-relative assembly ('-l' option in GNU as). A value < 0 indicates that pc-relative assembly is impossible.
} OPTIMIZE_INFO;

// Calculator Type Enumeration
typedef enum {
	// Calculator Models
	CALC_TI92 = 0x01, CALC_TI89 = 0x02, CALC_TI92PLUS = 0x04, CALC_V200 = 0x08,
	// Calculator Flags
	CALC_FLAG_TITANIUM = 0x100
} ProgramCalcs;
#define HIGHEST_CALC CALC_V200

// File Role Constants (File Output)
typedef enum {FR_MAIN = 0, FR_DATA = 1, FR_DEBUGGING_INFO = 2} FileRoles;

// File Format Constants (File Output)
typedef enum {FF_NONE = -1, FF_TIOS = 0, FF_TIOS_UPGRADE = 1, FF_GDB_COFF = 2} FileFormats;

#ifdef TARGET_EMBEDDED

// This is the format of the exported GetInterfaceVersion function.
// The format must never change, otherwise using the wrong DLL will result in
// a crash.
#define EXP_GET_INTERFACE_VERSION() ATTRIBUTE_EXPORTED I4 GetInterfaceVersion (void)
extern EXP_GET_INTERFACE_VERSION ();

// Diagnostic Messages
typedef enum {MT_ERROR = 0, MT_WARNING = 1} MessageTypes;
typedef void (*ATTRIBUTE_INTERFACE ERROR_FUNCTION) (const char *FileName, const char *Text, I4 MessageType);

// File Output
typedef struct ATTRIBUTE_PACKED {
	I1 *Data;  // Pointer to the writable file data.
} INT_EXP_FILE;

typedef B2 (*ATTRIBUTE_INTERFACE OUTPUT_FILE_FUNCTION) (INT_EXP_FILE *File, I4 FileSize, I4 DestCalc, I4 FileRole, I4 FileFormat, I4 FileType, const char *Extension, B2 Executable, I4 *EffectiveSize);
typedef void (*ATTRIBUTE_INTERFACE OUTPUT_FILE_FINALIZE_FUNCTION) (INT_EXP_FILE *File);

// This is the format of the LinkFiles function of ld-tigcc.
#define EXP_LINK_FILES() ATTRIBUTE_EXPORTED I2 LinkFiles (const char **ObjectFiles, const char **ArchiveFiles, ERROR_FUNCTION ErrorMessage, OUTPUT_FILE_FUNCTION GetOutputFile, OUTPUT_FILE_FINALIZE_FUNCTION FinalizeOutputFile, B2 NativeMode, B2 FlashOS, B2 Fargo, DATA_VAR_INFO *DataVarInfo, OPTIMIZE_INFO *OptimizeInfo, B2 OmitBSSInitialization)
extern EXP_LINK_FILES ();

// This is the format of the CreateArchive function of ar-tigcc.
#define EXP_CREATE_ARCHIVE() ATTRIBUTE_EXPORTED I2 CreateArchive (const char *DestFile, const char **ObjectFiles, ERROR_FUNCTION ErrorMessage, B2 NoNames)
extern EXP_CREATE_ARCHIVE ();

#else /* !TARGET_EMBEDDED */

// File Output (internal)
typedef struct {
	FILE *File;               // Pointer to FILE struct from stdio.h.
	COUNT CheckSum;           // Checksum of all bytes written.
	BOOLEAN OutputBin;        // Reduces the output to just the program image.
	unsigned int FileFormat;  // File format as defined in intrface.h.
	unsigned int FileType;    // File tag for FF_TIOS.
	const char *Extension;    // File extension for FF_TIOS.
} INT_EXP_FILE;

typedef BOOLEAN (*OUTPUT_FILE_FUNCTION) (INT_EXP_FILE *File, SIZE FileSize, unsigned int DestCalc, unsigned int FileRole, unsigned int FileFormat, unsigned int FileType, const char *Extension, BOOLEAN Executable, I4 *EffectiveSize);
typedef void (*OUTPUT_FILE_FINALIZE_FUNCTION) (INT_EXP_FILE *File);

#endif /* !TARGET_EMBEDDED */

#endif
