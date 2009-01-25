/* list.h: List model definitions

   Copyright (C) 2003 Sebastian Reichelt, Kevin Kofler

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

#ifndef INSERT_MODEL_RELOC_H
#define INSERT_MODEL_RELOC_H

#include "../../generic.h"
#include "../../data.h"

// *** Generic ***

// A type for a function implementing a reloc model. SourceSection is
// the section whose relocs are to be determined. If Count is true,
// the function must return the number of items; otherwise it must
// return the location of the next item in SourceSection, or -1 if
// there is no next item. NextItem may never be NULL; but the data it
// points to is NULL in the first call. If it is NULL after a call,
// this means that no more items follow.
// If some user data is optional, then the list model will usually
// fill it with information about the current item.
typedef OFFSET (LIST_MODEL) (SECTION *SourceSection, void **NextItem, void *UserData, BOOLEAN Count, BOOLEAN Emit);

// Get the number of items from the LIST_MODEL passed to the
// function via the Model parameter, for a particular source section.
COUNT GetSectionItemCount (LIST_MODEL *Model, SECTION *SourceSection, void *UserData);

// Get the number of items from the LIST_MODEL passed to the
// function via the Model parameter. If SourceSection is NULL, loop
// through all sections in the program.
COUNT GetItemCount (LIST_MODEL *Model, PROGRAM *Program, SECTION *SourceSection, void *UserData);

// *** Relocs ***

// User data for RelocListModel.
typedef struct {
	SECTION *TargetSection;  // If non-NULL, identifies the section the relocs must point to.
} RELOC_USER_DATA;

// A LIST_MODEL for relocs. This list model will return all relocs
// in SourceSection (that point to TargetSection if it is not NULL).
OFFSET RelocListModel (SECTION *SourceSection, RELOC **NextItem, RELOC_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit);

// *** ROM Calls ***

// A structured type for a used ROM call.
typedef struct {
	COUNT RelocCount;  // The number of references to this ROM call.
} ROM_CALL_FUNCTION_DATA;

// User data for ROMCallListModel.
typedef struct {
	ROM_CALL_FUNCTION_DATA *ROMFunctions;  // Information about the ROM calls used in the program, as an array with the ROM call number as the index.
	COUNT ROMFunctionCount;                // Number of used ROM calls.
	OFFSET CurFunction;                    // Current ROM call number for which to count/return references (if >= 0).
} ROM_CALL_USER_DATA;

// A LIST_MODEL for ROM calls. This list model will return all ROM calls
// in SourceSection (that point to CurFunction if it is >= 0).
OFFSET ROMCallListModel (SECTION *SourceSection, ROM_CALL **NextItem, ROM_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit);

// *** RAM Calls ***

// Available types of RAM calls.
typedef enum {RT_RAM_CALL_4 = 0, RT_EXTRA_RAM_4 = 1, RT_RAM_CALL_2 = 2, RT_EXTRA_RAM_2 = 3, RT_ALL_TYPES = -1, RT_UNSUPPORTED = -2} RAM_CALL_TYPE;
#define RAM_CALL_TYPE_COUNT 4
#define GetRAMCallType(RAMCall) ((RAMCall)->Size == 4 ? ((RAMCall)->ExtraRAMAddr ? RT_EXTRA_RAM_4 : RT_RAM_CALL_4) : (RAMCall)->Size == 2 ? ((RAMCall)->ExtraRAMAddr ? RT_EXTRA_RAM_2 : RT_RAM_CALL_2) : RT_UNSUPPORTED)

// A structured type for a used RAM call (with a specific type).
typedef struct {
	COUNT RelocCount;  // The number of references to this RAM call.
} RAM_CALL_FUNCTION_DATA;

// A structured type for a RAM call type, containing all RAM calls available for it.
typedef struct {
	RAM_CALL_FUNCTION_DATA *Functions;  // Information about the RAM calls used with this type, as an array with the RAM call number as the index.
	COUNT FunctionCount;                // The number of RAM calls used in this type.
} RAM_CALL_TYPE_DATA;

// User data for RAMCallListModel.
typedef struct {
	RAM_CALL_TYPE_DATA RAMTypes[RAM_CALL_TYPE_COUNT];  // Information about the RAM calls used in the program, sorted by type.
	COUNT RAMFunctionCount;                            // The total number of RAM calls used in the program.
	RAM_CALL_TYPE CurType;                             // Current RAM call type to count/return, or RT_ALL_TYPES.
	OFFSET CurFunction;                                // Current RAM call number for which to count/return references (if >= 0).
} RAM_CALL_USER_DATA;

// A LIST_MODEL for RAM calls. This list model will return all ROM calls
// in SourceSection (of type CurType if it is not RT_ALL_TYPES, that point
// to CurFunction if it is >= 0).
OFFSET RAMCallListModel (SECTION *SourceSection, RAM_CALL **NextItem, RAM_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit);

// A structured type for an imported function in a library.
typedef struct {
	COUNT RelocCount;  // The number of references to this library export.
} LIB_FUNCTION_DATA;

// Extra information about each library.
typedef struct {
	LIBRARY *Lib;                  // The library this data belongs to.
	LIB_FUNCTION_DATA *Functions;  // Information about the library exports used in the program.
	COUNT FunctionCount;           // The number of library exports used.
} LIB_DATA;

// User data for LibCallListModel.
typedef struct {
	COUNT LibCount;          // The number of libraries in the Libs field.
	LIB_DATA *Libs;          // An array of all libraries used in the program, with additional information.
	COUNT UsedLibCount;      // The number of libraries actually referenced.
	COUNT LibFunctionCount;  // The total number of library exports used in the program.
	LIBRARY *CurLibrary;     // Pointer to the library of the current item (output only).
	OFFSET CurLib;           // Current library (from Libs) for which to count/return references (if >= 0).
	OFFSET CurFunction;      // Current library export for which to count/return references (if >= 0).
} LIB_CALL_USER_DATA;

// A LIST_MODEL for library calls. This list model will return all library
// calls in SourceSection (that point to CurFunction in CurLib if they are
// >= 0).
OFFSET LibCallListModel (SECTION *SourceSection, LIB_CALL **NextItem, LIB_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit);

#endif
