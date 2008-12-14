/* list.c: List model definitions

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

#include "list.h"

#include "../../integers.h"
#include "../../manip.h"
#include "../../special.h"

#include <stdlib.h>
#include <string.h>

// Get the number of items from the LIST_MODEL passed to the
// function via the Model parameter, for a particular source section.
COUNT GetSectionItemCount (LIST_MODEL *Model, SECTION *SourceSection, void *UserData)
{
	void *NextItem = NULL;
	return Model (SourceSection, &NextItem, UserData, TRUE, FALSE);
}

// Get the number of items from the LIST_MODEL passed to the
// function via the Model parameter. If SourceSection is NULL, loop
// through all sections in the program.
COUNT GetItemCount (LIST_MODEL *Model, PROGRAM *Program, SECTION *SourceSection, void *UserData)
{
	COUNT GetCurrentItemCount (void)
	{
		return GetSectionItemCount (Model, SourceSection, UserData);
	}
	
	if (SourceSection)
		return GetCurrentItemCount ();
	else
	{
		COUNT Result = 0;
		for_each (SourceSection, Program->Sections)
			Result += GetCurrentItemCount ();
		return Result;
	}
}

// A LIST_MODEL for relocs. This list model will return all relocs
// in SourceSection (that point to TargetSection if it is not NULL).
OFFSET RelocListModel (SECTION *SourceSection, RELOC **NextItem, RELOC_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit)
{
	COUNT Counter = 0;
	RELOC *CurReloc;
	
	// If no next item is specified yet, this must be the first call to this model
	// with this SourceSection. The caller cannot fill *NextItem because it does
	// not know how to get the first item.
	if (!(*NextItem))
		*NextItem = GetFirst (SourceSection->Relocs);
	
	// Loop until we are done with counting or can return the next reloc that is
	// important for us.
	while ((CurReloc = *NextItem))
	{
		*NextItem = GetNext (CurReloc);
		
		// Returning a location < 0 would be fatal, since it means we are done.
		if ((CurReloc->Location >= 0) && (CurReloc->Location + CurReloc->Size <= SourceSection->Size))
		{
			// Ignore relocs which we will not emit.
			if (!(CurReloc->Relative || CurReloc->Target.Builtin || CurReloc->Parent->DebuggingInfoType || (CurReloc->Target.Symbol && CurReloc->Target.Symbol->Parent->DebuggingInfoType)))
			{
				// We can only emit 4-byte absolute relocs.
				// Everything else would need to be specified in the user data structure.
				if (CurReloc->Size == 4)
				{
					// If a target section is specified, use only relocs that explicitly point to it.
					// Otherwise, use all relocs that do not explicitly point to a handled section.
					if (UserData->TargetSection ? (CurReloc->Target.Symbol && (CurReloc->Target.Symbol->Parent == UserData->TargetSection)) : (!(CurReloc->Target.Symbol && (CurReloc->Target.Symbol->Parent->Handled))))
					{
						// When counting, simply increment the counter.
						if (Count)
							Counter++;
						// Otherwise, return the location.
						else
						{
							// If Emit is true, we must emit the fixed/target offset of the reloc
							// into the section contents.
							if (Emit)
							{
								// Change CurReloc to be relative to the section or program.
								SetRelocRelation (CurReloc, UserData->TargetSection, 0);
								// We won't see this reloc any more,
								// so increase the statistics.
								SourceSection->Relocs.EmittedCount++;
								SourceSection->Parent->OptimizeInfo->RelocCount++;
							}
							// Return the location.
							return CurReloc->Location;
						}
					}
				}
			}
		}
		// Output an error message only when counting, so we get it only once.
		else if (Count)
			Error (SourceSection->FileName, "Ignoring reloc outside of section.");
	}
	
	// We have reached the end of the section.
	// Return the number of items when counting.
	if (Count)
		return Counter;
	else
		return -1;
}

// A LIST_MODEL for ROM calls. This list model will return all ROM calls
// in SourceSection (that point to CurFunction if it is >= 0).
OFFSET ROMCallListModel (SECTION *SourceSection, ROM_CALL **NextItem, ROM_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit)
{
	COUNT Counter = 0;
	ROM_CALL_FUNCTION_DATA *ROMFunctions = UserData->ROMFunctions;
	OFFSET CurFunction = UserData->CurFunction;
	ROM_CALL *CurROMCall;
	
	if (!Count)
	{
		// After this enumeration, ROM calls are supported and do not need any further treatment.
		SourceSection->ROMCalls.Handled = TRUE;
	}
	
	// If no next item is specified yet, this must be the first call to this model
	// with this SourceSection and CurFunction. The caller cannot fill *NextItem
	// because it does not know how to get the first item.
	if (!(*NextItem))
		*NextItem = GetFirst (SourceSection->ROMCalls);
	
	// Loop until we are done with counting or can return the next ROM call that is
	// important for us.
	while ((CurROMCall = *NextItem))
	{
		*NextItem = GetNext (CurROMCall);
		
		// Returning a location < 0 would be fatal, since it means we are done.
		if ((CurROMCall->Location >= 0) && (CurROMCall->Location + CurROMCall->Size <= SourceSection->Size))
		{
			// Only proceed if we want this ROM call number.
			if ((CurFunction < 0) || (CurROMCall->Number == CurFunction))
			{
				// We can only emit 4-byte ROM calls.
				// Everything else would need to be specified in the user data structure.
				if (CurROMCall->Size == 4)
				{
					// When counting, increment the main counter and all counters
					// from the user data structure.
					if (Count)
					{
						Counter++;
						if (!(ROMFunctions[CurROMCall->Number].RelocCount++))
							UserData->ROMFunctionCount++;
					}
					// Otherwise, return the location.
					else
					{
						// If Emit is true, we must emit the fixed offset of the ROM call
						// into the section contents.
						if (Emit)
						{
							// Write our target offset into the section data.
							if (SourceSection->Data)
								WriteTI4 (*((TI4 *) (SourceSection->Data + CurROMCall->Location)), CurROMCall->FixedOffset);
						}
						// If we want to get all ROM calls, we need information about
						// this ROM call.
						// Otherwise, this does not have any effect.
						UserData->CurFunction = CurROMCall->Number;
						// Return the location.
						return CurROMCall->Location;
					}
				}
				// Output an error message only when counting, so we get it only once.
				else if (Count)
					Error (GetFileName (SourceSection, CurROMCall->Location), "Cannot emit %ld byte ROM call 0x%lX.", (long) CurROMCall->Size, (long) CurROMCall->Number);
			}
		}
		// Output an error message only when counting, so we get it only once.
		else if (Count)
			Error (SourceSection->FileName, "Ignoring ROM call outside of section.");
	}
	
	// We have reached the end of the section.
	// Return the number of items when counting.
	if (Count)
		return Counter;
	else
		return -1;
}

// A LIST_MODEL for RAM calls. This list model will return all ROM calls
// in SourceSection (of type CurType if it is not RT_ALL_TYPES, that point
// to CurFunction if it is >= 0).
OFFSET RAMCallListModel (SECTION *SourceSection, RAM_CALL **NextItem, RAM_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit)
{
	COUNT Counter = 0;
	RAM_CALL_TYPE_DATA *RAMTypes = UserData->RAMTypes;
	RAM_CALL_TYPE CurType = UserData->CurType;
	OFFSET CurFunction = UserData->CurFunction;
	RAM_CALL *CurRAMCall;
	
	if (!Count)
	{
		// After this enumeration, RAM calls are supported and do not need any further treatment.
		SourceSection->RAMCalls.Handled = TRUE;
	}
	
	// If no next item is specified yet, this must be the first call to this model
	// with this SourceSection and CurFunction. The caller cannot fill *NextItem
	// because it does not know how to get the first item.
	if (!(*NextItem))
		*NextItem = GetFirst (SourceSection->RAMCalls);
	
	// Loop until we are done with counting or can return the next RAM call that is
	// important for us.
	while ((CurRAMCall = *NextItem))
	{
		*NextItem = GetNext (CurRAMCall);
		
		// Returning a location < 0 would be fatal, since it means we are done.
		if ((CurRAMCall->Location >= 0) && (CurRAMCall->Location + CurRAMCall->Size <= SourceSection->Size))
		{
			// Only proceed if we want this RAM call number.
			if ((CurFunction < 0) || (CurRAMCall->Number == CurFunction))
			{
				// Check if the type is valid.
				RAM_CALL_TYPE ThisType = GetRAMCallType (CurRAMCall);
				if (ThisType >= 0)
				{
					// Only proceed if we want this RAM call type.
					if ((CurType < 0) || (ThisType == CurType))
					{
						// When counting, increment the main counter and all counters
						// from the user data structure.
						if (Count)
						{
							Counter++;
							if (!(RAMTypes[ThisType].Functions[CurRAMCall->Number].RelocCount++))
							{
								RAMTypes[ThisType].FunctionCount++;
								UserData->RAMFunctionCount++;
							}
						}
						// Otherwise, return the location.
						else
						{
							// If Emit is true, we must emit the fixed offset of the RAM call
							// into the section contents.
							if (Emit)
							{
								// Write our target offset into the section data.
								if (SourceSection->Data)
									WriteTI (SourceSection->Data + CurRAMCall->Location, CurRAMCall->Size, CurRAMCall->FixedOffset, TRUE, FALSE);
							}
							// If we want to get all RAM calls, we need information about
							// this RAM call.
							// Otherwise, this does not have any effect.
							UserData->CurType = ThisType;
							UserData->CurFunction = CurRAMCall->Number;
							// Return the location.
							return CurRAMCall->Location;
						}
					}
				}
				// Output an error message only when counting, so we get it only once.
				else if (Count)
					Error (GetFileName (SourceSection, 0), "Cannot emit %ld byte RAM call 0x%lX.", (long) CurRAMCall->Size, (long) CurRAMCall->Number);
			}
		}
		// Output an error message only when counting, so we get it only once.
		else if (Count)
			Error (SourceSection->FileName, "Ignoring RAM call outside of section.");
	}
	
	// We have reached the end of the section.
	// Return the number of items when counting.
	if (Count)
		return Counter;
	else
		return -1;
}

// A LIST_MODEL for library calls. This list model will return all library
// calls in SourceSection (that point to CurFunction in CurLib if they are
// >= 0).
OFFSET LibCallListModel (SECTION *SourceSection, LIB_CALL **NextItem, LIB_CALL_USER_DATA *UserData, BOOLEAN Count, BOOLEAN Emit)
{
	COUNT Counter = 0;
	COUNT LibCount = UserData->LibCount;
	LIB_DATA *Libs = UserData->Libs;
	OFFSET CurLib = UserData->CurLib;
	OFFSET CurFunction = UserData->CurFunction;
	LIB_CALL *CurLibCall;
	
	if (!Count)
	{
		// After this enumeration, library calls are supported and do not need any further treatment.
		SourceSection->LibCalls.Handled = TRUE;
	}
	
	// If no next item is specified yet, this must be the first call to this model
	// with this SourceSection and CurFunction. The caller cannot fill *NextItem
	// because it does not know how to get the first item.
	if (!(*NextItem))
		*NextItem = GetFirst (SourceSection->LibCalls);
	
	// Loop until we are done with counting or can return the next library call that is
	// important for us.
	while ((CurLibCall = *NextItem))
	{
		*NextItem = GetNext (CurLibCall);
		
		// Returning a location < 0 would be fatal, since it means we are done.
		if ((CurLibCall->Location >= 0) && (CurLibCall->Location + CurLibCall->Size <= SourceSection->Size))
		{
			// Only proceed if we want this library and export.
			if ((CurLib < 0) || ((CurLibCall->Library == Libs[CurLib].Lib) && ((CurFunction < 0) || (CurLibCall->Number == CurFunction))))
			{
				// We can only emit 4-byte library calls.
				// Everything else would need to be specified in the user data structure.
				if (CurLibCall->Size == 4)
				{
					// When counting, increment the main counter and all counters
					// from the user data structure.
					if (Count)
					{
						Counter++;
						
						{
							OFFSET ThisLib;
							
							// Look for the correct library information.
							// If CurLib >= 0, we already know the correct one.
							for (ThisLib = (CurLib >= 0 ? CurLib : 0); ThisLib < LibCount; ThisLib++)
							{
								if (Libs[ThisLib].Lib == CurLibCall->Library)
								{
									if (!(Libs[ThisLib].Functions[CurLibCall->Number].RelocCount++))
									{
										UserData->LibFunctionCount++;
										if (!(Libs[ThisLib].FunctionCount++))
											UserData->UsedLibCount++;
									}
									
									break;
								}
								
								if (CurLib >= 0)
									break;
							}
						}
					}
					// Otherwise, return the location.
					else
					{
						// If Emit is true, we must emit the fixed offset of the library call
						// into the section contents.
						if (Emit)
						{
							// Write our target offset into the section data.
							if (SourceSection->Data)
								WriteTI4 (*((TI4 *) (SourceSection->Data + CurLibCall->Location)), CurLibCall->FixedOffset);
						}
						
						// If we want to get all library calls, we need information about
						// this library call.
						// Otherwise, this does not have any effect.
						UserData->CurLibrary = CurLibCall->Library;
						UserData->CurFunction = CurLibCall->Number;
						
						return CurLibCall->Location;
					}
				}
				// Output an error message only when counting, so we get it only once.
				else if (Count)
					Error (GetFileName (SourceSection, CurLibCall->Location), "Cannot emit %ld byte library call to `%s', function 0x%lX.", (long) CurLibCall->Size, CurLibCall->Library->Name, (long) CurLibCall->Number);
			}
		}
		// Output an error message only when counting, so we get it only once.
		else if (Count)
			Error (SourceSection->FileName, "Ignoring library call outside of section.");
	}
	
	// We have reached the end of the section.
	// Return the number of items when counting.
	if (Count)
		return Counter;
	else
		return -1;
}
