/* comprrlc.c: Routines for compressed relocation tables

   Copyright (C) 2003-2005 Kevin Kofler
   Portions copyright (C) 2002-2003 Sebastian Reichelt

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

#include "comprrlc.h"

#include "../integers.h"
#include "../special.h"
#include "../manip.h"
#include "model/list.h"

#include <stdlib.h>
#include <string.h>

// Append a new I1 to the section. Return TRUE on success, FALSE on failure.
static BOOLEAN AppendI1ToSection (SECTION *Section, I1 NewI1)
{
	TI1 *NewSpace = (TI1 *) AllocateSpaceInSection (Section, 1);
	if (!NewSpace)
		return FALSE;
	
	WriteTI1 (*NewSpace, NewI1);
	return TRUE;
}

// Append a new I2 to the section. Return TRUE on success, FALSE on failure.
static BOOLEAN AppendI2ToSection (SECTION *Section, I2 NewI2)
{
	TI2 *NewSpace = (TI2 *) AllocateSpaceInSection (Section, 2);
	if (!NewSpace)
		return FALSE;

	WriteTI2 (*NewSpace, NewI2);
	return TRUE;
}

// Emit a compressed reloc nibble buffer. Return TRUE on success, FALSE on
// failure.
static BOOLEAN EmitCompressedRelocNibbles (SECTION *Section, I4 NibbleCount, I1 *NibbleBuffer)
{
	I4 CurrentNibble;
	I4 ByteCount = (NibbleCount + 1) >> 1; // number of bytes emitted as nibbles
	
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) return FALSE;})
#define AppendNibbles(n1, n2) AppendI1 (((n1) << 4) + (n2))
	
	if (NibbleCount < 3)
	{
		// Too few entries for nibbles, emit bytes.
		for (CurrentNibble = 0; CurrentNibble < NibbleCount; CurrentNibble++)
			AppendI1 (NibbleBuffer[CurrentNibble] + 1);
		return TRUE;
	}
	
	// Emit the first nibble pair.
	AppendNibbles (ByteCount + 6, *NibbleBuffer);
	
	// Emit the remaining nibble pairs.
	for (CurrentNibble = 1; CurrentNibble < NibbleCount - 1; CurrentNibble += 2)
		AppendNibbles (NibbleBuffer[CurrentNibble], NibbleBuffer[CurrentNibble + 1]);
	
	// If the number of nibbles is even, emit the last one as a byte.
	if (!(NibbleCount & 1))
		AppendI1 (NibbleBuffer[NibbleCount - 1] + 1);
	
#undef AppendI1
#undef AppendNibbles
	
	return TRUE;
}

// Emit a compressed reloc. If Offset is -1, end the relocation table. Return
// TRUE on success, FALSE on failure.
static BOOLEAN EmitCompressedReloc (SECTION *Section, OFFSET Offset)
{
// Maximum number of nibbles in a nibble group.
#define MAX_NIBBLES 9
	
	static I4 NibbleCount = 0;
	static I1 NibbleBuffer[MAX_NIBBLES] = {};
	
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) return FALSE;})
#define AppendI2(i2) ({if (!AppendI2ToSection (Section, (i2))) return FALSE;})
#define EmitNibbles() ({if (!EmitCompressedRelocNibbles (Section, NibbleCount, NibbleBuffer)) return FALSE; NibbleCount = 0;})
	
	if (Offset == -1)
	{
		// End the relocation table.
		EmitNibbles();
		AppendI1(0);
	}
	else
	{
Continue:
		if (Offset < 16)
		{
			// We have a nibble. Check if we have room left in the buffer.
			if (NibbleCount == MAX_NIBBLES)
			{
				// Room full. Emit the nibbles in the buffer.
				EmitNibbles();
			}
			// Put the nibble into the buffer.
			NibbleBuffer[NibbleCount++] = Offset;
		}
		else
		{
			// We have a byte or a word. Emit the nibbles in the buffer.
			EmitNibbles();
			
			if (Offset < 127)
			{
				// We have a byte.
				AppendI1 (Offset + 1);
			}
			else if (Offset < 16510)
			{
				// We have a word.
				AppendI2 (Offset + 49025);
			}
			else
			{
				// We have an 0xFFFF word and an additional offset.
				AppendI2 (65535);
				Offset -= 16510;
				goto Continue;
			}
		}
	}
	
#undef AppendI1
#undef AppendI2
#undef EmitNibbles
#undef MAX_NIBBLES
	
	return TRUE;
}

// Emit a compressed reloc given the true offset between the 2 relocs. Compute
// the offset as used in the compressed reloc table or give an error message if
// it isn't representable. Call EmitCompressedReloc with the computed offset.
// Return TRUE on success, FALSE on failure.
static BOOLEAN EmitCompressedRelocFromActualOffset (SECTION *Section, OFFSET Offset)
{
	if ((Offset > 0) && (Offset & 1))
	{
		Error (Section->FileName, "Odd offset `%ld' between 2 absolute relocs. Even offset needed.", (long) Offset);
		return FALSE;
	}

	if (Offset < 4)
	{
		Error (Section->FileName, (Offset >= 0) ? "Cannot emit overlapping absolute relocs (offset `0x%lx', need offset >=4)."
		                                        : "Invalid internal reloc sorting order (offset `-0x%lx', need positive offset).",
		       (Offset >= 0) ? (long)Offset : -(long)Offset);
		return FALSE;
	}
	
	return EmitCompressedReloc (Section, (Offset - 4) >> 1);
}

// Emit a reloc table in compressed format for the items enumerated in the
// list model specified by Model.
static BOOLEAN EmitCompressedFormatRelocs (LIST_MODEL *Model, SECTION *SourceSection, const LOCATION *SourceBase, void *UserData, SECTION *Section)
{
	if (!SourceSection)
		SourceSection = Section;
	
	SourceSection->Frozen = TRUE;
	
	{
		void *NextItem = NULL;
		OFFSET Offset;
		
		// Output the first item in the list...
		// Output the target offset, and get the reloc's location.
		Offset = Model (SourceSection, &NextItem, UserData, FALSE, TRUE);
		
		// A return value < 0 indicates that the list was empty.
		if (Offset >= 0)
		{
			OFFSET BaseAddress = GetLocationOffset (SourceSection, SourceBase);
			
			if (BaseAddress & 1)
			{
				Error (Section->FileName, "Invalid base address `0x%lx'. Even base address needed.", (long) BaseAddress);
				return FALSE;
			}
			
			if (Offset & 1)
			{
				Error (Section->FileName, "Odd offset between base address `0x%lx' and first absolute reloc `0x%lx'. Even offset needed.", (long) BaseAddress, (long) Offset);
				return FALSE;
			}
			
			if (Offset < BaseAddress)
			{
				Error (Section->FileName, "Cannot emit absolute reloc located at `0x%lx' before base address.", (long) Offset);
				return FALSE;
			}
			
			// Emit the reloc.
			if (!(EmitCompressedReloc (Section, (Offset - BaseAddress) >> 1)))
				return FALSE;
			
	 		// Output the remaining items in the list...
			while (NextItem)
			{
				// Save the previous offset.
				OFFSET LastOffset = Offset;
				
				// Output the target offset, and get the reloc's location.
				Offset = Model (SourceSection, &NextItem, UserData, FALSE, TRUE);
				
				// A negative offset means the list ends here.
				if (Offset >= 0)
				{
					// Emit the reloc.
					if (!(EmitCompressedRelocFromActualOffset (Section, Offset - LastOffset)))
						return FALSE;
				}
			}
		}
		
		return EmitCompressedReloc (Section, -1);
	}
}

// Append relocation entries in the format required by the TIGCCLIB relocation
// code. If TargetSection is NULL, append all relocation entries that point to
// unhandled sections. Otherwise, append all relocation entries pointing to this
// section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertCompressedRelocs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	// Initialize user data for list model.
	RELOC_USER_DATA UserData = {TargetSection};
	
	// Do code optimizations now, since this might reduce the number
	// of relocs. This is useful even if a target section was specified
	// because we can't fix the code anymore after the section is frozen.
	// This could even cause us to emit invalid code (bra +0).
	FixCode (Section->Parent);

	// If a target section is specified, it is essential now, and it may
	// not be modified any more.
	if (TargetSection)
		TargetSection->Frozen = TargetSection->Essential = TRUE;
	
	// Apply the format documented in _compressed_format_relocs.s.
	if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) RelocListModel, MergedSection, Reference, &UserData, Section)))
		return FALSE;
	
	// If a target section was specified, and its Handled flag was not
	// set yet, now setting it is probably correct.
	if (TargetSection)
		TargetSection->Handled = TRUE;
	
	return TRUE;
}

// Append relocation entries in the format required by the TIGCCLIB relocation
// code, using InsertCompressedRelocs. If TargetSection is NULL, output an empty
// relocation table. Otherwise, append all relocation entries pointing to this
// section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertCompressedSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	if (TargetSection)
		return InsertCompressedRelocs (Section, TargetSection, MergedSection, Reference);
	else
		return (AllocateSpaceInSection (Section, 1) != NULL);
}

// Append ROM calls in the format required by the TIGCCLIB relocation code.
BOOLEAN InsertCompressedROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	ROM_CALL_USER_DATA UserData = {NULL, 0, -1};
	
	// Allocate space for dynamic user data.
	if (!(UserData.ROMFunctions = calloc (Program->HighestROMCall + 1, sizeof (ROM_CALL_FUNCTION_DATA))))
	{
		Error (NULL, "Out of memory while inserting ROM calls with compressed relocs.");
		return FALSE;
	}
	
	{
		BOOLEAN Result = TRUE;
		
		// Go through all ROM calls and increment the appropriate counters.
		COUNT ROMRelocCount = GetSectionItemCount ((LIST_MODEL *) ROMCallListModel, MergedSection ? : Section, &UserData);
		
		// If no ROM calls are used, do not output anything but the final null-terminator.
		if (ROMRelocCount > 0)
		{
			// Apply the format documented in _compressed_format_rom_calls.s.
			OFFSET LastFunction = -1;
			
			for (UserData.CurFunction = 0; UserData.CurFunction <= Program->HighestROMCall; UserData.CurFunction++)
			{
				if (UserData.ROMFunctions[UserData.CurFunction].RelocCount)
				{
					// Emit the function number as a compressed offset.
					// Yes, the offset is intentionally integrated into the table that follows!
					OFFSET FunctionOffset = UserData.CurFunction - (LastFunction + 1);
					if (!(EmitCompressedReloc (Section, FunctionOffset)))
					{
						Result = FALSE;
						break;
					}
					LastFunction = UserData.CurFunction;
						
					// Emit all ROM calls using this function.
					if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) ROMCallListModel, MergedSection, Reference, &UserData, Section)))
					{
						Result = FALSE;
						break;
					}
				}
			}
		}
		
		// Output the final null-terminator.
		if (!(AppendI1ToSection (Section, 0)))
			Result = FALSE;
		
		// Free the extra information.
		free (UserData.ROMFunctions);
		
		return Result;
	}
}

// Emit a compressed index in PreOs format. Return TRUE on success, FALSE on
// failure. This is NOT the same format as the compressed relocs!
static BOOLEAN EmitPreOsCompressedIndex (SECTION *Section, COUNT LastIndex, COUNT Index)
{
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) return FALSE;})
#define AppendI2(i2) ({if (!AppendI2ToSection (Section, (i2))) return FALSE;})
	
	OFFSET Offset = Index - (LastIndex + 1);
	
	if (Offset < 0)
	{
		// The computed offset must be nonnegative (positive or zero).
		Error (NULL, "Invalid index sequence during PreOs compressed relocs.");
		return FALSE;
	}
	else
	{
		if (Offset <= 253)
		{
			// Offsets in the range 0 .. 253 are encoded as a single byte.
			AppendI1 (Offset);
		}
		else if (Offset <= (254 + 255))
		{
			// Offsets in the range 254+0 .. 254+255 are encoded as 2 bytes.
			AppendI1 (254);
			AppendI1 (Offset - 254);
		}
		else
		{
			// For offsets larger than 254+255, we don't encode the offset, but
			// the actual index, in a 3-byte encoding.
			AppendI1 (255);
			AppendI2 (Index);
		}
	}
	
#undef AppendI1
#undef AppendI2
	
	return TRUE;
}

// Append the section size and relocs to the specified section in the
// format required by PreOS.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertPreOSSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	if (TargetSection)
	{
		// Initialize user data for list model.
		RELOC_USER_DATA UserData = {TargetSection};
		
		// Target size. Round up to the next multiple of 4, and output the size
		// divided by 4.
		if (!(AppendI2ToSection (Section, (TargetSection->Size + 3) >> 2)))
			return FALSE;
		
		// Apply the format documented in _preos_program_header.s.
		return EmitCompressedFormatRelocs ((LIST_MODEL *) RelocListModel, MergedSection, Reference, &UserData, Section);
	}	
	else
	{
		// Target size:
		return (AllocateSpaceInSection (Section, 2) != NULL);
		// The empty relocation table can be omitted.
	}	
}

// Append ROM calls in the format required by PreOs.
BOOLEAN InsertPreOsROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	ROM_CALL_USER_DATA UserData = {NULL, 0, -1};
	
	// Allocate space for dynamic user data.
	if (!(UserData.ROMFunctions = calloc (Program->HighestROMCall + 1, sizeof (ROM_CALL_FUNCTION_DATA))))
	{
		Error (NULL, "Out of memory while inserting ROM calls with compressed relocs.");
		return FALSE;
	}
	
	{
		BOOLEAN Result = TRUE;
		
#define EmitIndex(oldidx,idx) ({if (!EmitPreOsCompressedIndex (Section, (oldidx), (idx))) { Result = FALSE; goto ROMCallsOutOfMem; } })
		
		// Go through all ROM calls and increment the appropriate counters.
		COUNT ROMRelocCount = GetItemCount ((LIST_MODEL *) ROMCallListModel, Program, NULL, &UserData);
		
		// If no ROM calls are used, output an empty table.
		if (!ROMRelocCount)
		{
			EmitIndex (-1, 0);
		}
		else
		{
			// Apply the format documented in _preos_program_header.s.
			OFFSET LastFunction = -1;
			
			// Output the number of functions used.
			EmitIndex (-1, UserData.ROMFunctionCount);
			
			for (UserData.CurFunction = 0; UserData.CurFunction <= Program->HighestROMCall; UserData.CurFunction++)
			{
				if (UserData.ROMFunctions[UserData.CurFunction].RelocCount)
				{
					// Output the current function number.
					EmitIndex (LastFunction, UserData.CurFunction);
					LastFunction = UserData.CurFunction;
					
					// Emit all ROM calls using this function.
					if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) ROMCallListModel, MergedSection, Reference, &UserData, Section)))
					{
						Result = FALSE;
						goto ROMCallsOutOfMem;
					}
				}
			}
		}
		
#undef EmitIndex
		
ROMCallsOutOfMem:
		// Free the extra information.
		free (UserData.ROMFunctions);
		
		return FALSE;
	}
}

// Append RAM calls in the format required by PreOs.
BOOLEAN InsertPreOsRAMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	BOOLEAN Result = TRUE;
	
	// Initialize user data for list model.
	RAM_CALL_USER_DATA UserData = {{{NULL, 0}, {NULL, 0}, {NULL, 0}, {NULL, 0}}, 0, RT_ALL_TYPES, -1};
	
	// Allocate space for dynamic user data.
	RAM_CALL_TYPE_DATA *CurType = UserData.RAMTypes;
	RAM_CALL_TYPE LoopType;
	for (LoopType = 0; LoopType < RAM_CALL_TYPE_COUNT; LoopType++, CurType++)
	{
		if (!(CurType->Functions = calloc (Program->HighestRAMCall + 1, sizeof (RAM_CALL_FUNCTION_DATA))))
		{
			Error (NULL, "Out of memory while inserting RAM calls with compressed relocs.");
			Result = FALSE;
			goto RAMCallsOutOfMem;
		}
	}
	
	{
		
#define EmitIndex(oldidx,idx) ({if (!EmitPreOsCompressedIndex (Section, (oldidx), (idx))) { Result = FALSE; goto RAMCallsOutOfMem; } })
		
		// Go through all RAM calls and increment the appropriate counters.
		COUNT RAMRelocCount = GetItemCount ((LIST_MODEL *) RAMCallListModel, Program, NULL, &UserData);
		
		// If no RAM calls are used, output an empty table.
		if (!RAMRelocCount)
		{
			EmitIndex (-1, 0);
		}
		else
		{
			// Apply the format documented in _preos_program_header.s.
			OFFSET LastFunction = -1;
			
			// Output the number of functions used.
			EmitIndex (-1, UserData.RAMFunctionCount);
			
			// For each possible RAM call type...
			for (UserData.CurType = 0; UserData.CurType < RAM_CALL_TYPE_COUNT; UserData.CurType++)
			{
				// Check if the type has been used.
				if (UserData.RAMTypes[UserData.CurType].FunctionCount)
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
						if (UserData.RAMTypes[UserData.CurType].Functions[UserData.CurFunction].RelocCount)
						{
							// Output the type and number.
							COUNT RAMCallNumber = UserData.CurFunction | TypeFlags;
							EmitIndex (LastFunction, RAMCallNumber);
							LastFunction = RAMCallNumber;
							
							// Emit all RAM calls using this function.
							if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) RAMCallListModel, MergedSection, Reference, &UserData, Section)))
							{
								Result = FALSE;
								goto RAMCallsOutOfMem;
							}
						}
					}
				}
			}
		}
		
#undef EmitIndex
		
RAMCallsOutOfMem:
		// Free the extra information.
		for (UserData.CurType = 0; UserData.CurType < RAM_CALL_TYPE_COUNT; UserData.CurType++)
		{
			RAM_CALL_FUNCTION_DATA *Functions = UserData.RAMTypes[UserData.CurType].Functions;
			if (Functions)
				free (Functions);
		}
	}
	
	return Result;
}

// Append libraries and library calls in the format required by PreOs.
BOOLEAN InsertPreOsLibraries (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	
	// Output an empty table if no libraries are referenced.
	if (IsEmpty (Program->Libraries))
		return (AppendI1ToSection (Section, 0)); // library count
	else
	{
		BOOLEAN Result = TRUE;
		
		// Initialize user data for list model.
		LIB_CALL_USER_DATA UserData = {0, NULL, 0, 0, NULL, -1, -1};
		
		if (InitializeLibCallUserData (&UserData, Program))
		{
			
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) { Result = FALSE; goto PreOsLibsOutOfMem; } })
#define EmitIndex(oldidx,idx) ({if (!EmitPreOsCompressedIndex (Section, (oldidx), (idx))) { Result = FALSE; goto PreOsLibsOutOfMem; } })
			
			// Go through all library calls and increment the appropriate counters.
			COUNT LibRelocCount = GetItemCount ((LIST_MODEL *) LibCallListModel, Program, MergedSection, &UserData);
			
			// Output an empty table if no libraries are referenced.
			if (!LibRelocCount)
			{
				AppendI1 (0); // library count
			}
			else
			{
				// Pointer to the inserted data.
				I1 *NewData;
				
				// The number of requested libraries.
				COUNT LibCount = UserData.UsedLibCount;
				AppendI1 (LibCount);
				
				// Apply the format documented in _preos_program_header.s.
				
				// Allocate space for the library names.
				NewData = AllocateSpaceInSection (Section, 10 * UserData.UsedLibCount);
				
				// Now handle the library names.
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
				}
				else
				{
					Error (NULL, "Out of memory while inserting libraries with compressed relocs.");
					Result = FALSE;
					goto PreOsLibsOutOfMem;
				}
				
				{
					// For each referenced library...
					LIB_DATA *CurLib = UserData.Libs;
					for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
					{
						if (CurLib->FunctionCount)
						{
							OFFSET LastFunction = -1;
							
							// Output the number of exports used.
							EmitIndex (-1, CurLib->FunctionCount - 1);
							
							// For each referenced export...
							for (UserData.CurFunction = 0; UserData.CurFunction <= CurLib->Lib->Highest; UserData.CurFunction++)
							{
								if (CurLib->Functions[UserData.CurFunction].RelocCount)
								{
									// Output the export number.
									EmitIndex (LastFunction, UserData.CurFunction);
									LastFunction = UserData.CurFunction;
									
									// Emit all library calls using this export.
									if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) LibCallListModel, MergedSection, Reference, &UserData, Section)))
									{
										Result = FALSE;
										goto PreOsLibsOutOfMem;
									}
								}
							}
						}
					}
				}
			}
		}
		else
			Result = FALSE;
		
PreOsLibsOutOfMem:
		// Free the extra information.
		FinalizeLibCallUserData (&UserData);
		
#undef AppendI1
#undef EmitIndex
		
		return Result;
	}
}

// The reference address used for the compressed relocation tables in PreOs.
#define PREOS_BASE_ADDR 36
	
// Append import tables and relocation entries in the format required by PreOs.
// In order: library imports, ROM calls, RAM calls, relocs, BSS references. All
// in one stream, pointed to by a single pointer.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertPreOsCompressedTables (SECTION *Section)
{
	PROGRAM *Program = Section->Parent;
	SECTION *MergedSection = Program->MainSection;
	LOCATION Reference = {Program->EntryPoint.Symbol, Program->EntryPoint.SymbolName, Program->EntryPoint.Offset + PREOS_BASE_ADDR, FALSE};
	
	// FIXME: Remove as soon as specification is not preliminary any more.
	Warning (NULL, "Encoding relocation tables according to a PRELIMINARY specification!");
	
	return (InsertPreOsLibraries   (Section,                      MergedSection, &Reference)
	     && InsertPreOsROMCalls    (Section,                      MergedSection, &Reference)
	     && InsertPreOsRAMCalls    (Section,                      MergedSection, &Reference)
	     && InsertCompressedRelocs (Section, NULL,                MergedSection, &Reference)
	     && InsertPreOSSectionRefs (Section, Program->BSSSection, MergedSection, &Reference));
}

#ifdef FARGO_SUPPORT

// Append BSS relocation entries in the format required by Fargo 0.2.1. Append
// all relocation entries pointing to the BSS section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertFargo021SectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	if (TargetSection)
	{
		// Initialize user data for list model.
		RELOC_USER_DATA UserData = {TargetSection};
		
		if (!(AppendI2ToSection (Section, TargetSection->Size)))
			return FALSE;
		
		// Apply the format documented in _fargo021_program_header.s.
		return EmitCompressedFormatRelocs ((LIST_MODEL *) RelocListModel, MergedSection, Reference, &UserData, Section);
	}	
	else
		return (AllocateSpaceInSection (Section, 2) != NULL);
}

// Append library calls in the format required by Fargo 0.2.1.
BOOLEAN InsertFargo021Libraries (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	
	// Output an empty table if no libraries are referenced.
	if (IsEmpty (Program->Libraries))
		return (AllocateSpaceInSection (Section, 2) != NULL);
	else
	{
		BOOLEAN Result = TRUE;
		
		// Initialize user data for list model.
		LIB_CALL_USER_DATA UserData = {0, NULL, 0, 0, NULL, -1, -1};
		
		if (InitializeLibCallUserData (&UserData, Program))
		{
			// Go through all library calls and increment the appropriate counters.
			COUNT LibRelocCount = GetItemCount ((LIST_MODEL *) LibCallListModel, Program, NULL, &UserData);
			
			// Output an empty table if no libraries are referenced.
			if (!LibRelocCount)
			{
				if (!(AllocateSpaceInSection (Section, 2)))
					Result = FALSE;
			}
			else
			{
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) { Result = FALSE; goto Fargo021LibsOutOfMem; } })
#define AppendI2(i2) ({if (!AppendI2ToSection (Section, (i2))) { Result = FALSE; goto Fargo021LibsOutOfMem; } })
				
				// Pointer to the inserted data.
				I1 *NewData;
				SIZE SizeToAllocate;
				I1 *LibName;
				
				// The number of requested libraries.
				COUNT LibCount = UserData.UsedLibCount;
				
				// The placeholder for the library names.
				OFFSET LibNamesLocation;
				
				AppendI2 (LibCount);
				
				LibNamesLocation = Section->Size;
				AppendI2 (0);
							
				// Apply the format documented in _fargo021_program_header.s.
				
				{
					// For each referenced library...
					LIB_DATA *CurLib = UserData.Libs;
					for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
					{
						if (CurLib->FunctionCount)
						{
							OFFSET LastFunction = -1;
							
							// For each referenced export...
							for (UserData.CurFunction = 0; UserData.CurFunction <= CurLib->Lib->Highest; UserData.CurFunction++)
							{
								if (CurLib->Functions[UserData.CurFunction].RelocCount)
								{
									// Output the export number.
									OFFSET FunctionOffset = UserData.CurFunction - LastFunction;
									// Emit the compressed offset. This is NOT the same
									// format as the compressed relocs!
									if (FunctionOffset < 128)
										AppendI1 (FunctionOffset);
									else
										AppendI2 (0x8000 + FunctionOffset);
									LastFunction = UserData.CurFunction;
									
									// Emit all library calls using this export.
									if (!(EmitCompressedFormatRelocs ((LIST_MODEL *) LibCallListModel, MergedSection, Reference, &UserData, Section)))
									{
										Result = FALSE;
										goto Fargo021LibsOutOfMem;
									}
								}
							}
							
							AppendI1 (0);
						}
					}
					
					// Add the redundant null-terminator.
					AppendI1 (0);
					
					// Allocate space for the library names.
					SizeToAllocate = 0;
					// For each referenced library...
					CurLib = UserData.Libs;
					for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
					{
						if (CurLib->FunctionCount)
							SizeToAllocate += strlen (CurLib->Lib->Name) + 1;
					}
					NewData = AllocateSpaceInSection (Section, SizeToAllocate);
					LibName = NewData;
					
					// Now handle the library names.
					if (NewData)
					{
						// Create a new reloc at the given location, pointing to
						// the location of the library names.
						{
							I1 *LibNamePlaceholder = Section->Data + LibNamesLocation;
							RELOC *Reloc = CreateProgramRelativeReloc (Section, &LibNamePlaceholder, Section, LibName - Section->Data, 2);
							if (!Reloc)
							{
								Error (NULL, "Out of memory while inserting libraries with compressed relocs.");
								Result = FALSE;
								goto Fargo021LibsOutOfMem;
							}
						}
						
						// For each referenced library...
						CurLib = UserData.Libs;
						for (UserData.CurLib = 0; UserData.CurLib < UserData.LibCount; UserData.CurLib++, CurLib++)
						{
							if (CurLib->FunctionCount)
							{
								// Output its name.
								strcpy (LibName, CurLib->Lib->Name);
								LibName += strlen (CurLib->Lib->Name) + 1;
							}
						}
					}
					else
					{
						Error (NULL, "Out of memory while inserting libraries with compressed relocs.");
						Result = FALSE;
					}
				}
			}
		}
		else
			Result = FALSE;
		
Fargo021LibsOutOfMem:
		// Free the extra information.
		FinalizeLibCallUserData (&UserData);
		
		return Result;
	}
#undef AppendI1
#undef AppendI2
}

#endif /* FARGO_SUPPORT */


// Emit an mlink-format compressed reloc. If Offset is -1, end the relocation
// table. Return TRUE on success, FALSE on failure.
static BOOLEAN EmitMlinkReloc (SECTION *Section, OFFSET Offset)
{
#define AppendI1(i1) ({if (!AppendI1ToSection (Section, (i1))) return FALSE;})
#define AppendI2(i2) ({if (!AppendI2ToSection (Section, (i2))) return FALSE;})
	
	if (Offset == -1)
	{
		// End the relocation table.
		AppendI1(0);
	}
	else
	{
		if (Offset < 128)
		{
			// We have 1 byte.
			AppendI1 (Offset + 0x80);
		}
		else if (Offset < 16384)
		{
			// We have 2 bytes.
			AppendI1 (Offset >> 7);
			AppendI1 ((Offset & 0x7F) + 0x80);
		}
		else if (Offset < 2097152)
		{
			// We have 3 bytes.
			AppendI1 (Offset >> 14);
			AppendI1 ((Offset >> 7) & 0x7F);
			AppendI1 ((Offset & 0x7F) + 0x80);
		}
		else
			Error (Section->FileName, "Offset `%ld' for mlink-format reloc doesn't fit into 3 bytes.", (long) Offset);
	}
	
#undef AppendI1
#undef AppendI2
	
	return TRUE;
}

// Emit an mlink-format compressed reloc given the true offset between the 2
// relocs. Compute the offset as used in the compressed reloc table or give
// an error message if it isn't representable. Call EmitMlinkReloc with the
// computed offset. Return TRUE on success, FALSE on failure.
static BOOLEAN EmitMlinkRelocFromActualOffset (SECTION *Section, OFFSET Offset)
{
	if ((Offset > 0) && (Offset & 1))
	{
		Error (Section->FileName, "Odd offset `%ld' between 2 absolute relocs. Even offset needed.", (long) Offset);
		return FALSE;
	}

	if (Offset < 0)
	{
		Error (Section->FileName, "Invalid internal reloc sorting order (offset `-0x%lx', need positive offset).",
		       -(long)Offset);
		return FALSE;
	}
	
	return EmitMlinkReloc (Section, Offset >> 1);
}

// Emit a reloc table in mlink compressed format for the items enumerated in the
// list model specified by Model.
static BOOLEAN EmitMlinkFormatRelocs (LIST_MODEL *Model, SECTION *SourceSection, const LOCATION *SourceBase, void *UserData, SECTION *Section)
{
	if (!SourceSection)
		SourceSection = Section;
	
	SourceSection->Frozen = TRUE;
	
	{
		void *NextItem = NULL;
		OFFSET Offset;
		
		// Output the first item in the list...
		// Output the target offset, and get the reloc's location.
		Offset = Model (SourceSection, &NextItem, UserData, FALSE, TRUE);
		
		// A return value < 0 indicates that the list was empty.
		if (Offset >= 0)
		{
			OFFSET BaseAddress = GetLocationOffset (SourceSection, SourceBase);
			
			if (BaseAddress & 1)
			{
				Error (Section->FileName, "Invalid base address `0x%lx'. Even base address needed.", (long) BaseAddress);
				return FALSE;
			}
			
			if (Offset & 1)
			{
				Error (Section->FileName, "Odd offset between base address `0x%lx' and first absolute reloc `0x%lx'. Even offset needed.", (long) BaseAddress, (long) Offset);
				return FALSE;
			}
			
			if (Offset < BaseAddress)
			{
				Error (Section->FileName, "Cannot emit absolute reloc located at `0x%lx' before base address.", (long) Offset);
				return FALSE;
			}
			
			// Emit the reloc.
			if (!(EmitMlinkReloc (Section, (Offset - BaseAddress) >> 1)))
				return FALSE;
			
	 		// Output the remaining items in the list...
			while (NextItem)
			{
				// Save the previous offset.
				OFFSET LastOffset = Offset;
				
				// Output the target offset, and get the reloc's location.
				Offset = Model (SourceSection, &NextItem, UserData, FALSE, TRUE);
				
				// A negative offset means the list ends here.
				if (Offset >= 0)
				{
					// Emit the reloc.
					if (!(EmitMlinkRelocFromActualOffset (Section, Offset - LastOffset)))
						return FALSE;
				}
			}
		}
		
		return EmitMlinkReloc (Section, -1);
	}
}

// Append mlink-style relocation entries in the format required by the TIGCCLIB
// relocation code. If TargetSection is NULL, append all relocation entries that
// point to unhandled sections. Otherwise, append all relocation entries
// pointing to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertMlinkRelocs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	// Initialize user data for list model.
	RELOC_USER_DATA UserData = {TargetSection};
	
	// Do code optimizations now, since this might reduce the number
	// of relocs. This is useful even if a target section was specified
	// because we can't fix the code anymore after the section is frozen.
	// This could even cause us to emit invalid code (bra +0).
	FixCode (Section->Parent);

	// If a target section is specified, it is essential now, and it may
	// not be modified any more.
	if (TargetSection)
		TargetSection->Frozen = TargetSection->Essential = TRUE;
	
	// Apply the format documented in _mlink_format_relocs.s.
	if (!(EmitMlinkFormatRelocs ((LIST_MODEL *) RelocListModel, MergedSection, Reference, &UserData, Section)))
		return FALSE;
	
	// If a target section was specified, and its Handled flag was not
	// set yet, now setting it is probably correct.
	if (TargetSection)
		TargetSection->Handled = TRUE;
	
	return TRUE;
}

// Append mlink-style relocation entries in the format required by the TIGCCLIB
// relocation code, using InsertMlinkRelocs. If TargetSection is NULL, output
// an empty relocation table. Otherwise, append all relocation entries pointing
// to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertMlinkSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference)
{
	if (TargetSection)
		return InsertMlinkRelocs (Section, TargetSection, MergedSection, Reference);
	else
		return (AllocateSpaceInSection (Section, 1) != NULL);
}

// Append ROM calls in the mlink-style format required by the TIGCCLIB
// relocation code.
BOOLEAN InsertMlinkROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference)
{
	PROGRAM *Program = Section->Parent;
	
	// Initialize user data for list model.
	ROM_CALL_USER_DATA UserData = {NULL, 0, -1};
	
	// Allocate space for dynamic user data.
	if (!(UserData.ROMFunctions = calloc (Program->HighestROMCall + 1, sizeof (ROM_CALL_FUNCTION_DATA))))
	{
		Error (NULL, "Out of memory while inserting ROM calls with mlink-format relocs.");
		return FALSE;
	}
	
	{
		BOOLEAN Result = TRUE;
		
		// Go through all ROM calls and increment the appropriate counters.
		COUNT ROMRelocCount = GetSectionItemCount ((LIST_MODEL *) ROMCallListModel, MergedSection ? : Section, &UserData);
		
		// If no ROM calls are used, do not output anything but the final null-terminator.
		if (ROMRelocCount > 0)
		{
			// Apply the format documented in _mlink_format_rom_calls.s.
			OFFSET LastFunction = 0;
			
			for (UserData.CurFunction = 0; UserData.CurFunction <= Program->HighestROMCall; UserData.CurFunction++)
			{
				if (UserData.ROMFunctions[UserData.CurFunction].RelocCount)
				{
					// Emit the function number as a mlink-format compressed offset.
					OFFSET FunctionOffset = UserData.CurFunction - LastFunction;
					if (!(EmitMlinkReloc (Section, FunctionOffset)))
					{
						Result = FALSE;
						break;
					}
					LastFunction = UserData.CurFunction;
						
					// Emit all ROM calls using this function.
					if (!(EmitMlinkFormatRelocs ((LIST_MODEL *) ROMCallListModel, MergedSection, Reference, &UserData, Section)))
					{
						Result = FALSE;
						break;
					}
				}
			}
		}
		
		// Output the final null-terminator.
		if (!(AppendI1ToSection (Section, 0)))
			Result = FALSE;
		
		// Free the extra information.
		free (UserData.ROMFunctions);
		
		return Result;
	}
}

