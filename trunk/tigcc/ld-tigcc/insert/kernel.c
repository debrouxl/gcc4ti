/* kernel.c: Routines to handle automatic insertion of section contents in kernel format

   Copyright (C) 2003 Sebastian Reichelt
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

#include "kernel.h"

#include "../integers.h"
#include "../manip.h"
#include "../special.h"
#include "model/list.h"

#include <stdlib.h>
#include <string.h>

// Get the size needed to emit a reloc table in kernel format for the
// items enumerated in the list model specified by Model. SourceSection
// may be NULL.
static SIZE GetKernelFormatRelocSize (LIST_MODEL *Model, PROGRAM *Program, SECTION *SourceSection, void *UserData)
{
	// We simply need 2 bytes per item, and 2 additional bytes at the end.
	return (2 * (GetItemCount (Model, Program, SourceSection, UserData)) + 2);
}

// Emit a reloc table in kernel format for the items enumerated in the
// list model specified by Model. SourceSection may be NULL.
static BOOLEAN EmitKernelFormatRelocs (LIST_MODEL *Model, PROGRAM *Program, SECTION *SourceSection, void *UserData, SECTION *Section, I1 **NewData)
{
	SECTION *CurSection;
	void *NextItem = NULL;
	OFFSET Offset;
	
	// For each section (or just for SourceSection)...
	for (CurSection = (SourceSection ? : GetFirst (Program->Sections)); CurSection; CurSection = GetNext (CurSection))
	{
		// For each item in the list...
		do {
			// Output the target offset, and get the reloc's location.
			Offset = Model (CurSection, &NextItem, UserData, FALSE, TRUE);
			// A return value < 0 indicates that there were no more items.
			if (Offset >= 0)
			{
				// Create a new reloc at this location, pointing to the
				// location of the list item.
				if (!(CreateProgramRelativeReloc (Section, NewData, CurSection, Offset, 2)))
					return FALSE;
			}
		} while (NextItem);
		
		// If we only want the items of one section, we have finished.
		if (SourceSection)
			break;
	}
	
	// Output the two zero bytes.
	*NewData += 2;
	return TRUE;
}

// Append relocation entries in the format required by kernels. If
// TargetSection is NULL, insert all relocation entries that point to
// unhandled sections. Otherwise, insert all relocation entries pointing
// to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertKernelRelocs (SECTION *Section, SECTION *TargetSection)
{
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	RELOC_USER_DATA UserData = {TargetSection};
	
	// If a target section is specified, it is essential now.
	if (TargetSection)
		TargetSection->Essential = TRUE;
	else
		// Do code optimizations now, since this might reduce the number
		// of relocs. If a target section was specified, this is pointless,
		// as relocs into a separate section can never be optimized away.
		FixCode (Program);
	
	{
		// Allocate space for the relocs.
		I1 *NewData = AllocateSpaceInSection (Section, GetKernelFormatRelocSize ((LIST_MODEL *) RelocListModel, Program, NULL, &UserData));
		
		// Apply the format documented in _kernel_program_header.s.
		if (NewData)
		{
			// Insert the relocs.
			if (!(EmitKernelFormatRelocs ((LIST_MODEL *) RelocListModel, Program, NULL, &UserData, Section, &NewData)))
				return FALSE;
		}
		else
			return FALSE;
	}
	
	// If a target section was specified, and its Handled flag was not
	// set yet, now setting it is probably correct.
	if (TargetSection)
		TargetSection->Handled = TRUE;
	
	return TRUE;
}

// Append relocation entries to TargetSection in the format required by
// kernels. If TargetSection is NULL, AlwaysTerminate specifies whether
// to insert the two terminating zero bytes anyway.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertKernelSectionRefs (SECTION *Section, SECTION *TargetSection, BOOLEAN AlwaysTerminate)
{
	// If the target section exists, this is the same format as for kernels.
	// However, if none exists, we need to output the terminating zero
	// bytes anyway. However, we cannot use InsertKernelRelocs in this
	// case because the TargetSection parameter will be NULL.
	if (TargetSection)
		return InsertKernelRelocs (Section, TargetSection);
	else if (AlwaysTerminate)
		// Insert empty relocation table.
		return (AllocateSpaceInSection (Section, 2) != NULL);
	else
		return TRUE;
}

// Append ROM calls in the format required by kernels.
BOOLEAN InsertKernelROMCalls (SECTION *Section)
{
	BOOLEAN Result = TRUE;
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	ROM_CALL_USER_DATA UserData = {NULL, 0, -1};
	
	// Allocate space for dynamic user data.
	if (!(UserData.ROMFunctions = calloc (Program->HighestROMCall + 1, sizeof (ROM_CALL_FUNCTION_DATA))))
	{
		Error (NULL, "Out of memory while inserting ROM calls.");
		return FALSE;
	}
	
	{
		// Go through all ROM calls and increment the appropriate counters.
		COUNT ROMRelocCount = GetItemCount ((LIST_MODEL *) ROMCallListModel, Program, NULL, &UserData);
		
		// If no ROM calls are used, do not output anything.
		if (ROMRelocCount > 0)
		{
			// Allocate space for the ROM call numbers and references.
			I1 *NewData = AllocateSpaceInSection (Section, (4 * UserData.ROMFunctionCount) + (2 * ROMRelocCount) + 2);
			
			// Apply the format documented in _kernel_program_header.s.
			if (NewData)
			{
				// Output the number of functions used.
				WriteTI2 (*((TI2 *) NewData), UserData.ROMFunctionCount - 1);
				NewData += 2;
				
				for (UserData.CurFunction = 0; UserData.CurFunction <= Program->HighestROMCall; UserData.CurFunction++)
				{
					if (UserData.ROMFunctions[UserData.CurFunction].RelocCount)
					{
						// Output the current function number.
						WriteTI2 (*((TI2 *) NewData), UserData.CurFunction);
						NewData += 2;
						
						// Emit all ROM calls using this function.
						if (!(EmitKernelFormatRelocs ((LIST_MODEL *) ROMCallListModel, Program, NULL, &UserData, Section, &NewData)))
						{
							Result = FALSE;
							break;
						}
					}
				}
			}
			else
				Result = FALSE;
		}
	}
	
	// Free the extra information.
	free (UserData.ROMFunctions);
	
	return Result;
}

// Append RAM calls in the format required by kernels.
BOOLEAN InsertKernelRAMCalls (SECTION *Section)
{
	BOOLEAN Result = TRUE;
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	RAM_CALL_USER_DATA UserData = {{{NULL, 0}, {NULL, 0}, {NULL, 0}, {NULL, 0}}, 0, RT_ALL_TYPES, -1};
	
	// Allocate space for dynamic user data.
	RAM_CALL_TYPE_DATA *CurType = UserData.RAMTypes;
	RAM_CALL_TYPE LoopType;
	for (LoopType = 0; LoopType < RAM_CALL_TYPE_COUNT; LoopType++, CurType++)
	{
		if (!(CurType->Functions = calloc (Program->HighestRAMCall + 1, sizeof (RAM_CALL_FUNCTION_DATA))))
		{
			Error (NULL, "Out of memory while inserting RAM calls.");
			Result = FALSE;
			break;
		}
	}
	
	if (Result)
	{
		// Go through all RAM calls and increment the appropriate counters.
		COUNT RAMRelocCount = GetItemCount ((LIST_MODEL *) RAMCallListModel, Program, NULL, &UserData);
		
		// If no RAM calls are used, do not output anything.
		if (RAMRelocCount)
		{
			// Allocate space for the RAM call numbers and references.
			I1 *NewData = AllocateSpaceInSection (Section, (4 * UserData.RAMFunctionCount) + (2 * RAMRelocCount) + 2);
			
			// Apply the format documented in _kernel_program_header.s.
			if (NewData)
			{
				// Output the number of functions used.
				WriteTI2 (*((TI2 *) NewData), UserData.RAMFunctionCount - 1);
				NewData += 2;
				
				// For each possible RAM call type...
				CurType = UserData.RAMTypes;
				for (UserData.CurType = 0; UserData.CurType < RAM_CALL_TYPE_COUNT; UserData.CurType++, CurType++)
				{
					// Check if the type has been used.
					if (CurType->FunctionCount)
					{
						// Gather information about the type.
						SIZE TypeSize = ((UserData.CurType == RT_RAM_CALL_2) || (UserData.CurType == RT_EXTRA_RAM_2) ? 2 : 4);
						BOOLEAN TypeExtraRAM = (UserData.CurType == RT_EXTRA_RAM_4) || (UserData.CurType == RT_EXTRA_RAM_2);
						I2 TypeFlags = 0;
						
						if (TypeSize < 4)
							TypeFlags |= (1 << 15);
						if (TypeExtraRAM)
							TypeFlags |= (1 << 14);
						
						// For each possible RAM function number...
						for (UserData.CurFunction = 0; UserData.CurFunction <= Program->HighestRAMCall; UserData.CurFunction++)
						{
							// Check if a RAM function of this type and number has been used.
							if (CurType->Functions[UserData.CurFunction].RelocCount)
							{
								// Output the type and number.
								if (NewData - Section->Data + 2 <= Section->Size)
									WriteTI2 (*((TI2 *) NewData), UserData.CurFunction | TypeFlags);
								NewData += 2;
								// Emit all RAM calls using this function.
								if (!(EmitKernelFormatRelocs ((LIST_MODEL *) RAMCallListModel, Program, NULL, &UserData, Section, &NewData)))
								{
									Result = FALSE;
									break;
								}
							}
						}
					}
					
					if (!Result)
						break;
				}
			}
			else
				Result = FALSE;
		}
	}
	
	// Free the extra information.
	for (UserData.CurType = 0; UserData.CurType < RAM_CALL_TYPE_COUNT; UserData.CurType++)
	{
		RAM_CALL_FUNCTION_DATA *Functions = UserData.RAMTypes[UserData.CurType].Functions;
		if (Functions)
			free (Functions);
	}
	
	return Result;
}

// Append library calls in the format required by kernels.
BOOLEAN InsertKernelLibraries (SECTION *Section)
{
	BOOLEAN Result = TRUE;
	PROGRAM *Program = Section->Parent;
	
	// Do not output anything if no libraries are referenced.
	if (!(IsEmpty (Program->Libraries)))
	{
		// Initialize user data for list model.
		LIB_CALL_USER_DATA UserData = {0, NULL, 0, 0, NULL, -1, -1};
		
		if (InitializeLibCallUserData (&UserData, Program))
		{
			// Go through all library calls and increment the appropriate counters.
			COUNT LibRelocCount = GetItemCount ((LIST_MODEL *) LibCallListModel, Program, NULL, &UserData);
			
			// Do not output anything if no libraries are referenced.
			if (LibRelocCount)
			{
				// Allocate space for the libraries and relocs.
				I1 *NewData = AllocateSpaceInSection (Section, (12 * UserData.UsedLibCount) + (4 * UserData.LibFunctionCount) + (2 * LibRelocCount));
				
				// Apply the format documented in _kernel_program_header.s.
				if (NewData)
				{
					// For each referenced library...
					LIB_DATA *CurLib = UserData.Libs;
					for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
					{
						if (CurLib->FunctionCount)
						{
							// Output its name.
							if (strlen (CurLib->Lib->Name) > 8)
								Warning (Section->FileName, "Library name `%s' too long; cutting off at 8th character.", CurLib->Lib->Name);
							strncpy (NewData, CurLib->Lib->Name, 8);
							WriteTI1 (*((TI1 *) (NewData + 9)), CurLib->Lib->Version);
							NewData += 10;
						}
					}
					
					// For each referenced library...
					CurLib = UserData.Libs;
					for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
					{
						if (CurLib->FunctionCount)
						{
							// Output the number of exports used.
							WriteTI2 (*((TI2 *) NewData), CurLib->FunctionCount - 1);
							NewData += 2;
							
							// For each referenced export...
							for (UserData.CurFunction = 0; UserData.CurFunction <= CurLib->Lib->Highest; UserData.CurFunction++)
							{
								if (CurLib->Functions[UserData.CurFunction].RelocCount)
								{
									// Output the export number.
									if (NewData - Section->Data + 2 <= Section->Size)
										WriteTI2 (*((TI2 *) NewData), UserData.CurFunction);
									NewData += 2;
									
									// Emit all library calls using this export.
									if (!(EmitKernelFormatRelocs ((LIST_MODEL *) LibCallListModel, Program, NULL, &UserData, Section, &NewData)))
									{
										Result = FALSE;
										break;
									}
								}
							}
						}
					}
				}
				else
					Result = FALSE;
			}
		}
		else
			Result = FALSE;
		
		// Free the extra information.
		FinalizeLibCallUserData (&UserData);
	}
	
	return Result;
}

#ifdef FARGO_SUPPORT
// Append library calls in the format required by Fargo v0.2.0.
BOOLEAN InsertFargo020Libraries (SECTION *Section)
{
	BOOLEAN Result = TRUE;
	PROGRAM *Program = Section->Parent;
	
	// Do not output anything if no libraries are referenced.
	if (!(IsEmpty (Program->Libraries)))
	{
		// Initialize user data for list model.
		LIB_CALL_USER_DATA UserData = {0, NULL, 0, 0, NULL, -1, -1};
		
		if (InitializeLibCallUserData (&UserData, Program))
		{
			// Go through all library calls and increment the appropriate counters.
			COUNT LibRelocCount = GetItemCount ((LIST_MODEL *) LibCallListModel, Program, NULL, &UserData);
			
			// Do not output anything if no libraries are referenced.
			if (LibRelocCount)
			{
				// Calculate the space needed for the library references.
				SIZE RelocSize = (4 * UserData.UsedLibCount) + (4 * UserData.LibFunctionCount) + (2 * LibRelocCount) + 2;
				
				// Calculate the space needed for the library names.
				SIZE SizeToAllocate = RelocSize;
				LIB_DATA *CurLib = UserData.Libs;
				for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
				{
					if (CurLib->FunctionCount)
						SizeToAllocate += strlen (CurLib->Lib->Name) + 1;
				}
				// Pad to an even size.
				if (SizeToAllocate & 1)
					SizeToAllocate++;
				
				// Allocate space for the libraries and relocs.
				{
					I1 *NewData = AllocateSpaceInSection (Section, SizeToAllocate);
					
					// Apply the format documented in _fargo_program_header.s.
					if (NewData)
					{
						I1 *LibName = NewData + RelocSize;
						
						// For each referenced library...
						CurLib = UserData.Libs;
						for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
						{
							if (CurLib->FunctionCount)
							{
								// Output its name.
								strcpy (LibName, CurLib->Lib->Name);
								// Create a new reloc at the current location, pointing to
								// the location of the library name.
								{
									RELOC *Reloc = CreateProgramRelativeReloc (Section, &NewData, Section, LibName - Section->Data, 2);
									if (!Reloc)
									{
										Result = FALSE;
										break;
									}
								}
								LibName += strlen (CurLib->Lib->Name) + 1;
								
								// Output the references to it.
								{
									// For each referenced export...
									LIB_FUNCTION_DATA *CurFunction = CurLib->Functions;
									for (UserData.CurFunction = 0; UserData.CurFunction <= CurLib->Lib->Highest; UserData.CurFunction++, CurFunction++)
									{
										if (CurFunction->RelocCount)
										{
											// Output its number.
											if (NewData - Section->Data + 2 <= Section->Size)
												WriteTI2 (*((TI2 *) NewData), UserData.CurFunction + 1);
											NewData += 2;
											
											// Emit all library calls using this export.
											if (!(EmitKernelFormatRelocs ((LIST_MODEL *) LibCallListModel, Program, NULL, &UserData, Section, &NewData)))
											{
												Result = FALSE;
												break;
											}
										}
									}
									
									if (!Result)
										break;
								}
								
								NewData += 2;
							}
						}
					}
					else
						Result = FALSE;
				}
			}
		}
		else
			Result = FALSE;
		
		// Free the extra information.
		FinalizeLibCallUserData (&UserData);
	}
	
	return Result;
}
#endif /* FARGO_SUPPORT */

// Append exported symbols in the format required by kernels. If
// TrailingZeroBytes is FALSE, use the Fargo v0.2.x format without
// trailing zero bytes.
BOOLEAN InsertKernelExports (SECTION *Section, BOOLEAN TrailingZeroBytes)
{
	PROGRAM *Program = Section->Parent;
	
	// Export Counter.
	COUNT ExportCount = 0;
	
	// Loop Variables.
	SECTION *CurSection;
	SYMBOL *CurSymbol;
	
	// Get the number of exports.
	// The number of exports is equal to the highest export number + 1.
	for_each (CurSection, Program->Sections)
	{
		for_each (CurSymbol, CurSection->Symbols)
		{
			if (CurSymbol->Exported)
			{
				OFFSET ExportNumber = GetExportNumber (CurSymbol->Name);
				if ((ExportNumber >= 0) && (ExportCount < ExportNumber + 1))
					ExportCount = ExportNumber + 1;
			}
		}
	}
	
	if (ExportCount || TrailingZeroBytes)
	{
		// Allocate space for the exports.
		I1 *NewData = AllocateSpaceInSection (Section, (2 * ExportCount) + (TrailingZeroBytes ? 2 : 0));
		
		// Apply the format documented in _kernel_program_header.s.
		if (NewData)
		{
			// For each section...
			for_each (CurSection, Program->Sections)
			{
				// For each symbol...
				for_each (CurSymbol, CurSection->Symbols)
				{
					// Check if it is exported.
					if (CurSymbol->Exported)
					{
						OFFSET ExportNumber = GetExportNumber (CurSymbol->Name);
						if (ExportNumber >= 0)
						{
							// Create a new reloc at the appropriate location,
							// pointing to the exported symbol.
							RELOC *Reloc = calloc (1, sizeof (RELOC));
							if (Reloc)
							{
								Reloc->Parent = Section;
								Reloc->Location = NewData - Section->Data + 2 * ExportNumber;
								Reloc->Size = 2;
								Reloc->Target.Symbol = CurSymbol;
								Reloc->Target.SymbolName = CurSymbol->Name;
								SetRelocProgramRelative (Reloc);
								InsertReloc (Section, Reloc);
							}
							else
							{
								Error (NULL, "Out of memory while inserting exports.");
								return FALSE;
							}
						}
					}
				}
			}
		}
		else
			return FALSE;
	}
	
	return TRUE;
}
