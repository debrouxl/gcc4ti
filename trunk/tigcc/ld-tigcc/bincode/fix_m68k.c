/* fix_m68k.c: Routines for M68000 code fixup

   Copyright (C) 2003 Sebastian Reichelt
   Copyright (C) 2003-2004 Kevin Kofler

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

#include "fix_m68k.h"

#include "m68k.h"
#include "fix_tios.h"
#include "fix_emu.h"
#include "cutrange.h"
#include "../manip.h"

#include <stdlib.h>

// Apply generic code fixes and optimizations to a section.
void M68kFixCode (SECTION *Section)
{
	M68kFixCodePreMerge (Section, NULL, 0);
}

// Fix and optionally optimize code executable on the M68k processor family,
// for two sections which are to be merged.
// Src may be NULL.
// If DestSize is nonzero, it specifies a fixed size for the destination
// section, which will not change even when cutting ranges from it.
void M68kFixCodePreMerge (SECTION *Dest, SECTION *Src, SIZE DestSize)
{
	if (Dest->Code && (!(Dest->Frozen)) && (!(Dest->Parent->Frozen)))
	{
		OPTIMIZE_INFO *OptimizeInfo = Dest->Parent->OptimizeInfo;
		SIZE OrigSize = Dest->Size;
		
		RELOC *Reloc, *NextReloc;
		
		// For each reloc...
		for (Reloc = (DestSize ? GetFirst (Dest->Relocs) : GetLast (Dest->Relocs)); Reloc; Reloc = NextReloc)
		{
			NextReloc = (DestSize ? GetNext (Reloc) : GetPrev (Reloc));
			
			// Completely ignore builtin relocs. Also ignore relation-relative
			// relocs, since the only relative relocs we can optimize further
			// are branches, and those are never relation-relative.
			// Ignore relocs which are not in code segments.
			if ((!(Reloc->Target.Builtin || Reloc->Relation || Reloc->Unoptimizable)) && IsCodeRange (Dest, Reloc->Location, Reloc->Location + Reloc->Size))
			{
				// We can only fix or optimize a reloc whose target we know.
				if (Reloc->Target.Symbol && ((Reloc->Target.Symbol->Parent == Dest) || (Reloc->Target.Symbol->Parent == Src)))
				{
					OFFSET TargetDistance;
					// Get the distance of the target symbol, provided it is in the
					// same section.
					if (Reloc->Target.Symbol->Parent == Dest)
						TargetDistance = GetLocationOffset (Dest, &(Reloc->Target));
					// If it is in the next section, add the section size.
					// This is not 100% correct, since we might pad the section, but
					// currently there is no reason to add padding to code. If we get
					// a slightly wrong distance, we will probably get away with it
					// anyway.
					else
						TargetDistance = (DestSize ? : Dest->Size) + GetLocationOffset (Src, &(Reloc->Target));
					// Add the fixed offset because it needs to be added to the reloc
					// target. Subtract the location of the reloc.
					TargetDistance += Reloc->FixedOffset - Reloc->Location;
					
					// Fix and possibly optimize or remove the reloc.
					M68kFixReloc (Reloc, TargetDistance, OptimizeInfo);
				}
			}
		}
		
		if (Dest->Size < OrigSize)
			FinalizeRangeCutting (Dest);
	}
}

// Cut the range between Start and End (not including End) if that is
// permitted; otherwise fill it with NOPs. Start and End are expected
// to be at even addresses. It is also assumed that IsBinaryDataRange
// has been called on the range (without an exception, or with an
// exception that has since been removed).
static void M68kCutOrFillRange (SECTION *Section, OFFSET Start, OFFSET End, OPTIMIZE_INFO *OptimizeInfo)
{
	if (End > Start)
	{
		BOOLEAN SectionEnd = End >= Section->Size;
		
		// Make sure Start is not outside of the section data;
		if (Start < 0)
			Start = 0;
		// Make sure End is not outside of the section data;
		if (SectionEnd)
			End = Section->Size;
		
		// If the range is at the end of the section, shrink the section.
		if (SectionEnd && CanShrinkSection (Section, Start, NULL))
		{
			// Look for a short branch to the end of the section. GNU as
			// does this to simulate long conditional branches.
			BOOLEAN BranchFound = FALSE;
			const I1 *Data = Section->Data;
			
			if (Data)
			{
				SIZE Size = Section->Size;
				OFFSET CurPos;
				
				for (CurPos = Size - 4; (CurPos >= 0) && (CurPos >= Size - 8); CurPos -= 2)
				{
					if (((Data [CurPos + 0] & M68K_Bcc_MASK_0) == M68K_Bcc_S_0) && (Data [CurPos + 1] == Size - (CurPos + 2)))
					{
						BranchFound = TRUE;
						break;
					}
				}
			}
			
			if (!BranchFound)
			{
				// No such branch was found, so we can (hopefully) safely cut
				// the section.
				CutSection (Section, Start);
				return;
			}
		}
		// If the range is in the middle of the section, try to cut it out.
		else
		{
			if (CanCutRange (Section, Start, End))
			{
				SIZE Length = End - Start;
				OptimizeInfo->CutRangesResult += Length;
				
				if (OptimizeInfo->CutRanges)
				{
					CutRange (Section, Start, End);
					OptimizeInfo->NearAssemblyResult -= Length;
					return;
				}
			}
		}
		
		// If we cannot cut the range for some reason, fill it with NOPs.
		{
			I1 *Data = Section->Data;
			if (Data)
			{
				OFFSET CurPos;
				for (CurPos = Start; CurPos + 1 < End; CurPos += 2)
				{
					Data [CurPos + 0] = M68K_NOP_0;
					Data [CurPos + 1] = M68K_NOP_1;
				}
			}
		}
	}
}

// Fix and possibly optimize a given relocation entry.
// TargetDistance is the estimated (signed) distance of the relocation target, including the
// fixed offset. OptimizeInfo contains information about what to optimize.
// The reloc may be removed during the process.
void M68kFixReloc (RELOC *Reloc, OFFSET TargetDistance, OPTIMIZE_INFO *OptimizeInfo)
{
	SECTION *Section = Reloc->Parent;
	I1 *Data = Section->Data;
	OFFSET RelocLocation = Reloc->Location;
	OFFSET OpcodeLocation;
	I1 *Opcode;
	
	if (!Data)
		return;
	
	if (Reloc->Unoptimizable)
		return;

	// Be careful with relocs at the beginning of the section; they might
	// cause segmentation faults when we try to determine the opcode.
	// Usually, ignore all relocs whose location is less than 2.
	// As a special exception, allow 1-byte relocs with a location of 1.
	if (!((Reloc->Location >= 2) || ((Reloc->Size == 1) && (Reloc->Location == 1))))
		return;
	
	// Optimize a subroutine branch followed by an RTS.
	if (OptimizeInfo->OptimizeReturns)
	{
		OFFSET RelocEnd = RelocLocation + Reloc->Size;
		OFFSET RTSEnd = RelocEnd + 2;
		
		if ((RTSEnd <= Section->Size) && (Data [RelocEnd + 0] == M68K_RTS_0) && (Data [RelocEnd + 1] == M68K_RTS_1))
		{
			OFFSET OpcodeLocation;
			I1 *Opcode;
			BOOLEAN Optimized = FALSE;
			
			if (Reloc->Relative)
			{
				switch (Reloc->Size)
				{
					// Optimize 1-byte relative relocs.
					case 1:
						OpcodeLocation = RelocLocation - 1;
						Opcode = Data + OpcodeLocation;
						
						// Check if the reloc belongs to a BSR.S, and that there
						// is nothing in our way.
						if ((Opcode [0] == M68K_BSR_S_0) && (IsBinaryDataRange (Section, OpcodeLocation, RTSEnd, Reloc)))
						{
							// Optimize it into a BRA.S.
							Opcode [0] = M68K_BRA_S_0;
							Optimized = TRUE;
						}
						
						break;
					
					// Optimize 2-byte relative relocs.
					case 2:
						OpcodeLocation = RelocLocation - 2;
						Opcode = Data + OpcodeLocation;
						
						// Check if the reloc belongs to a BSR.W, and that there
						// is nothing in our way.
						if ((Opcode [0] == M68K_BSR_W_0) && (Opcode [1] == M68K_BSR_W_1) && (IsBinaryDataRange (Section, OpcodeLocation, RTSEnd, Reloc)))
						{
							// Optimize it into a JMP.
							Opcode [0] = M68K_BRA_W_0;
							Opcode [1] = M68K_BRA_W_1;
							Optimized = TRUE;
						}
						
						break;
				}
			}
			else
			{
				// Optimize 4-byte absolute relocs.
				if (Reloc->Size == 4)
				{
					OpcodeLocation = RelocLocation - 2;
					Opcode = Data + OpcodeLocation;
					
					// Check if the reloc belongs to a JSR, and that there is
					// nothing in our way.
					if ((Opcode [0] == M68K_JSR_0) && (Opcode [1] == M68K_JSR_1) && (IsBinaryDataRange (Section, OpcodeLocation, RTSEnd, Reloc)))
					{
						// Optimize it into a JMP.
						Opcode [0] = M68K_JMP_0;
						Opcode [1] = M68K_JMP_1;
						Optimized = TRUE;
					}
				}
			}
			
			// Remove the RTS.
			if (Optimized)
				M68kCutOrFillRange (Section, RelocEnd, RTSEnd, OptimizeInfo);
		}
	}
	
	// Only branch fixes and optimizations make sense for relative relocs.
	if (Reloc->Relative)
	{
		// Check if it is a 1-byte relative reloc at the end of the
		// section, pointing to the next instruction. This means that
		// the value will probably be 0, which is invalid for 1-byte
		// branches.
		if ((Reloc->Size == 1) && (TargetDistance == 0))
		{
			OpcodeLocation = RelocLocation - 1;
			Opcode = Data + OpcodeLocation;
			
			// Check whether it belongs to a branch. If it does, it
			// is invalid.
			if (((Opcode [0] & M68K_Bcc_MASK_0) == M68K_Bcc_S_0) && (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 2, Reloc)))
			{
				// If it is a BSR.S, removing it would modify the
				// semantics. Instead, insert a NOP at the end of the
				// section (if this is really the end of the section).
				if (Opcode [0] == M68K_BSR_S_0)
				{
					if (OpcodeLocation + 2 == Section->Size)
					{
						// Allocate two bytes at the end of the section.
						I1 *Space = AllocateSpaceInSection (Section, 2);
						
						if (Space)
						{
							Space [0] = M68K_NOP_0;
							Space [1] = M68K_NOP_1;
						}
					}
				}
				else
				{
					// Delete the reloc.
					FreeReloc (Reloc);
					
					// Cut or fill the gained space.
					M68kCutOrFillRange (Section, OpcodeLocation, OpcodeLocation + 2, OptimizeInfo);
					
					// Since the reloc has been removed, return from the function.
					return;
				}
			}
		}
		else if (OptimizeInfo->OptimizeBranches && (Reloc->Size == 2))
		{
			OpcodeLocation = RelocLocation - 2;
			Opcode = Data + OpcodeLocation;
			
			// Check whether the reloc belongs to a branch.
			if (((Opcode [0] & M68K_Bcc_MASK_0) == M68K_Bcc_W_0) && (Opcode [1] == M68K_Bcc_W_1) && (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 4, Reloc)))
			{
				// Check whether it can be removed.
				if ((TargetDistance == 2) && (Opcode [0] != M68K_BSR_W_0))
				{
					// Delete the reloc.
					FreeReloc (Reloc);
					
					// Cut or fill the gained space.
					M68kCutOrFillRange (Section, OpcodeLocation, OpcodeLocation + 4, OptimizeInfo);
					
					// Since the reloc has been removed, return from the function.
					return;
				}
				// Check whether it is near enough for a Bcc.S.
				else if ((TargetDistance != 2) && (M68K_REL_OK (TargetDistance, 1)))
				{
					// Optimize it into a Bcc.S.
					Opcode [0] = M68K_Bcc_S_0 | (Data [OpcodeLocation] & (~M68K_Bcc_MASK_0));
					Opcode [1] = 0;
					// Change the reloc to 1-byte relative.
					Reloc->Size = 1;
					// Adjust the location.
					Reloc->Location--;
					RelocLocation--;
					// Adjust the offset. A short branch always
					// uses the next instruction as a reference.
					Reloc->FixedOffset--;
					
					// Cut or fill the gained space.
					M68kCutOrFillRange (Section, OpcodeLocation + 2, OpcodeLocation + 4, OptimizeInfo);
				}
			}
		}
	}
	// Other than that, only change 4-byte absolute relocs.
	else if (Reloc->Size == 4)
	{
		// Most opcodes are two bytes long, and the reloc follows
		// immediately.
		OpcodeLocation = RelocLocation - 2;
		
		// Safety check before accessing the section data.
		if (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 6, Reloc))
		{
			Opcode = Data + OpcodeLocation;
			
			{
				// *** Branch Optimization ***
				
				// Try to optimize a 4-byte absolute branch into a 2-byte or 1-byte
				// relative one.
				// Check whether the reloc belongs to a branch.
				BOOLEAN IsJMP = ((Opcode [0] == M68K_JMP_0) && (Opcode [1] == M68K_JMP_1));
				BOOLEAN IsJSR = ((Opcode [0] == M68K_JSR_0) && (Opcode [1] == M68K_JSR_1));
				
				if ((IsJMP || IsJSR))
				{
					// Check whether it can be removed.
					if (OptimizeInfo->OptimizeBranches && (TargetDistance == 4) && IsJMP)
					{
						// Delete the reloc.
						FreeReloc (Reloc);
						
						// Cut or fill the gained space.
						M68kCutOrFillRange (Section, OpcodeLocation, OpcodeLocation + 6, OptimizeInfo);
						
						OptimizeInfo->OptimizeBranchesResult++;
					}
					// Check whether it is near enough for a BRA.S or BSR.S.
					else if (OptimizeInfo->OptimizeBranches && (TargetDistance != 4) && (M68K_REL_OK (TargetDistance, 1)))
					{
						// Optimize it into a BRA.S or BSR.S.
						Opcode [0] = (IsJSR ? M68K_BSR_S_0 : M68K_BRA_S_0);
						Opcode [1] = 0;
						// Change the reloc to 1-byte relative.
						Reloc->Relative = TRUE;
						Reloc->Size = 1;
						// Adjust the location.
						Reloc->Location--;
						// Adjust the offset. A short branch always
						// uses the next instruction as a reference.
						Reloc->FixedOffset--;
						
						// Cut or fill the gained space.
						M68kCutOrFillRange (Section, OpcodeLocation + 2, OpcodeLocation + 6, OptimizeInfo);
						
						OptimizeInfo->OptimizeBranchesResult++;
					}
					// Treat it as a normal absolute to relative optimization.
					else
					{
						BOOLEAN BranchOptimized = FALSE;
						
						// Check if the target is near enough for a BRA/BSR.
						if (M68K_REL_OK (TargetDistance, 2))
						{
							OptimizeInfo->OptimizeBranchesResult++;
							if (OptimizeInfo->NearAssemblyResult >= 0)
								OptimizeInfo->NearAssemblyResult += 2;
							
							if (OptimizeInfo->OptimizeBranches)
							{
								// Optimize it into a BRA.W or BSR.W.
								Opcode [0] = (IsJSR ? M68K_BSR_W_0 : M68K_BRA_W_0);
								Opcode [1] = (IsJSR ? M68K_BSR_W_1 : M68K_BRA_W_1);
								// Change the reloc to 2-byte relative.
								Reloc->Relative = TRUE;
								Reloc->Size = 2;
								
								// Cut or fill the gained space.
								M68kCutOrFillRange (Section, OpcodeLocation + 4, OpcodeLocation + 6, OptimizeInfo);
								
								// Do not try to do anything else with this branch.
								BranchOptimized = TRUE;
							}
						}
						else
						{
							// The target is not near enough. This means that near
							// assembly is not possible.
							OptimizeInfo->NearAssemblyResult = -1;
						}
						
						if (!BranchOptimized)
						{
							// Optimize F-Line jumps if desired.
							OptimizeInfo->UseFLineJumpsResult++;
							if (OptimizeInfo->Use4ByteFLineJumps)
								M68kEmuMakeFLineJump (Reloc, Opcode, IsJSR);
							else if (OptimizeInfo->UseFLineJumps)
								M68kTIOSMakeFLineJump (Reloc, Opcode, IsJSR);
							else
								return;
							
							// Cut or fill the gained space.
							M68kCutOrFillRange (Section, Reloc->Location + Reloc->Size, OpcodeLocation + 6, OptimizeInfo);
						}
					}
				}
				// Not a branch.
				else
				{
					BOOLEAN Optimized = FALSE;
					
					// Check if the target is near enough.
					if (M68K_REL_OK (TargetDistance, 2))
					{
						// *** Move Optimization ***
						
						// Optimize LEA(.L) var.L,reg into
						// LEA(.L) var.W(%PC),reg.
						if ((((Opcode [0] & M68K_LEA_ABS_MASK_0) == M68K_LEA_ABS_0) && ((Opcode [1] & M68K_LEA_ABS_MASK_1) == M68K_LEA_ABS_1))
						// Optimize PEA(.L) var.L into PEA(.L) var.W(%PC).
						 || ((Opcode [0] == M68K_PEA_ABS_0) && (Opcode [1] == M68K_PEA_ABS_1))
						// Optimize MOVE.x var.L,reg/(reg)/(reg)+ into
						// MOVE.x var.W(%PC),reg/(reg)/(reg)+.
						 || (((Opcode [0] & M68K_MOVE_ABS_REG_MASK_0) == M68K_MOVE_ABS_REG_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_MASK_1) == M68K_MOVE_ABS_REG_1)
						  && (!((Opcode [0] & M68K_MOVE_ABS_REG_INV_0_MASK_0) == M68K_MOVE_ABS_REG_INV_0_0))
						  && (!(((Opcode [0] & M68K_MOVE_ABS_REG_INV_1_MASK_0) == M68K_MOVE_ABS_REG_INV_1_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_INV_1_MASK_1) == M68K_MOVE_ABS_REG_INV_1_1))))
						// Optimize MOVE.x var.L,-(reg) into
						// MOVE.x var.W(%PC),-(reg).
						 || (((Opcode [0] & M68K_MOVE_ABS_PREDEC_MASK_0) == M68K_MOVE_ABS_PREDEC_0) && ((Opcode [1] & M68K_MOVE_ABS_PREDEC_MASK_1) == M68K_MOVE_ABS_PREDEC_1)
						  && (!((Opcode [0] & M68K_MOVE_ABS_PREDEC_INV_0_MASK_0) == M68K_MOVE_ABS_PREDEC_INV_0_0))))
						{
							OptimizeInfo->OptimizeMovesResult++;
							if (OptimizeInfo->NearAssemblyResult >= 0)
								OptimizeInfo->NearAssemblyResult += 2;
							
							if (OptimizeInfo->OptimizeMoves)
							{
								// Turn the opcode into a pc-relative one.
								M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
								
								// Do everything else later.
								Optimized = TRUE;
							}
						}
						
						// *** Test Optimization ***
						
						// Optimize CMP.x var.L,reg into CMP.x var.W(%PC),reg.
						else if ((((Opcode [0] & M68K_CMP_ABS_REG_MASK_0) == M68K_CMP_ABS_REG_0) && ((Opcode [1] & M68K_CMP_ABS_REG_MASK_1) == M68K_CMP_ABS_REG_1)
						       && (!((Opcode [1] & M68K_CMP_ABS_REG_INV_0_MASK_1) == M68K_CMP_ABS_REG_INV_0_1)))
						// Optimize BTST reg,var.L into BTST reg,var.W(%PC).
						      || (((Opcode [0] & M68K_BTST_REG_ABS_MASK_0) == M68K_BTST_REG_ABS_0) && ((Opcode [1] & M68K_BTST_REG_ABS_MASK_1) == M68K_BTST_REG_ABS_1)))
						{
							OptimizeInfo->OptimizeTestsResult++;
							if (OptimizeInfo->NearAssemblyResult >= 0)
								OptimizeInfo->NearAssemblyResult += 2;
							
							if (OptimizeInfo->OptimizeTests)
							{
								// Turn the opcode into a pc-relative one.
								M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
								
								// Do everything else later.
								Optimized = TRUE;
							}
						}
						
						// *** Calculation Optimization ***
						
						// Optimize ADD/SUB.x var.L,reg into
						// ADD/SUB.x var.W(%PC),reg.
						else if ((((Opcode [0] & M68K_ADDSUB_ABS_REG_0_MASK_0) == M68K_ADDSUB_ABS_REG_0_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_0_MASK_1) == M68K_ADDSUB_ABS_REG_0_1))
						      || (((Opcode [0] & M68K_ADDSUB_ABS_REG_1_MASK_0) == M68K_ADDSUB_ABS_REG_1_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_1_MASK_1) == M68K_ADDSUB_ABS_REG_1_1))
						// Optimize MUL/DIV.x var.L,reg into
						// MUL/DIV.x var.W(%PC),reg.
						      || (((Opcode [0] & M68K_MULDIV_ABS_REG_MASK_0) == M68K_MULDIV_ABS_REG_0) && ((Opcode [1] & M68K_MULDIV_ABS_REG_MASK_1) == M68K_MULDIV_ABS_REG_1))
						// Optimize AND/OR.x var.L,reg into
						// AND/OR.x var.W(%PC),reg.
						      || (((Opcode [0] & M68K_ANDOR_ABS_REG_MASK_0) == M68K_ANDOR_ABS_REG_0) && ((Opcode [1] & M68K_ANDOR_ABS_REG_MASK_1) == M68K_ANDOR_ABS_REG_1)))
						{
							OptimizeInfo->OptimizeCalcsResult++;
							if (OptimizeInfo->NearAssemblyResult >= 0)
								OptimizeInfo->NearAssemblyResult += 2;
							
							if (OptimizeInfo->OptimizeCalcs)
							{
								// Turn the opcode into a pc-relative one.
								M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
								
								// Do everything else later.
								Optimized = TRUE;
							}
						}
					}
					
					if (Optimized)
					{
						// Change the reloc to 2-byte relative.
						Reloc->Relative = TRUE;
						Reloc->Size = 2;
						
						// Cut or fill the gained space.
						M68kCutOrFillRange (Section, OpcodeLocation + 4, OpcodeLocation + 6, OptimizeInfo);
					}
					else
					{
						// Check if the target is near enough.
						if (M68K_REL_OK (TargetDistance, 2))
						{
							if (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 8, Reloc))
							{
								// Optimize MOVE.x var.L,ofs(reg) into
								// MOVE.x var.W(%PC),ofs(reg)
								// We cannot handle this above because of the
								// offset, which comes after the reloc.
								if (((Opcode [0] & M68K_MOVE_ABS_OFSREG_MASK_0) == M68K_MOVE_ABS_OFSREG_0) && ((Opcode [1] & M68K_MOVE_ABS_OFSREG_MASK_1) == M68K_MOVE_ABS_OFSREG_1)
								 && (!((Opcode [0] & M68K_MOVE_ABS_OFSREG_INV_0_MASK_0) == M68K_MOVE_ABS_OFSREG_INV_0_0)))
								{
									OptimizeInfo->OptimizeMovesResult++;
									if (OptimizeInfo->NearAssemblyResult >= 0)
										OptimizeInfo->NearAssemblyResult += 2;
									
									if (OptimizeInfo->OptimizeMoves)
									{
										// Turn the opcode into a pc-relative one.
										M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
										
										// Move the offset to the correct place.
										Opcode [4] = Opcode [6];
										Opcode [5] = Opcode [7];
										
										// Change the reloc to 2-byte relative.
										Reloc->Relative = TRUE;
										Reloc->Size = 2;
										
										// Cut or fill the gained space.
										M68kCutOrFillRange (Section, OpcodeLocation + 6, OpcodeLocation + 8, OptimizeInfo);
									}
								}
							}
							
							{
								OpcodeLocation = RelocLocation - 4;
								Opcode = Data + OpcodeLocation;
								
								if (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 8, Reloc))
								{
									// Optimize MOVEM.x var.L,regs into
									// MOVEM var.W(%PC),regs.
									if (((Opcode [0] & M68K_MOVEM_ABS_REGS_MASK_0) == M68K_MOVEM_ABS_REGS_0) && ((Opcode [1] & M68K_MOVEM_ABS_REGS_MASK_1) == M68K_MOVEM_ABS_REGS_1))
									{
										OptimizeInfo->OptimizeMovesResult++;
										if (OptimizeInfo->NearAssemblyResult >= 0)
											OptimizeInfo->NearAssemblyResult += 2;
										
										if (OptimizeInfo->OptimizeMoves)
										{
											// Turn the opcode into a pc-relative one.
											M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
											
											// Change the reloc to 2-byte relative.
											Reloc->Relative = TRUE;
											Reloc->Size = 2;
											
											// Cut or fill the gained space.
											M68kCutOrFillRange (Section, OpcodeLocation + 6, OpcodeLocation + 8, OptimizeInfo);
										}
									}
									// Optimize BTST #num,var.L into
									// BTST #num,var.W(%PC).
									else if ((Opcode [0] == M68K_BTST_IMM_ABS_0) && (Opcode [1] == M68K_BTST_IMM_ABS_1) && (Opcode [2] == M68K_BTST_IMM_ABS_2))
									{
										OptimizeInfo->OptimizeTestsResult++;
										if (OptimizeInfo->NearAssemblyResult >= 0)
											OptimizeInfo->NearAssemblyResult += 2;
										
										if (OptimizeInfo->OptimizeTests)
										{
											// Turn the opcode into a pc-relative one.
											M68K_MAKE_REL_OPCODE_1 (Opcode [1]);
											
											// Change the reloc to 2-byte relative.
											Reloc->Relative = TRUE;
											Reloc->Size = 2;
											
											// Cut or fill the gained space.
											M68kCutOrFillRange (Section, OpcodeLocation + 6, OpcodeLocation + 8, OptimizeInfo);
										}
									}
								}
							}
						}
						else
						{
							// The target is not near enough. This means that near
							// assembly is not possible.
							OptimizeInfo->NearAssemblyResult = -1;
						}
					}
				}
			}
		}
	}
}

// Checks if a specific reloc might be optimizable. This is currently
// limited to 4-bytes absolute relocs because that is the only case
// this is needed for.
BOOLEAN M68kIsRelocOptimizable (const RELOC *Reloc)
{
	if (Reloc->Unoptimizable) return FALSE;

	if (Reloc->Size == 4 && (!(Reloc->Relative)))
	{
		const SECTION *Section = Reloc->Parent;
		
		// Most opcodes are two bytes long, and the reloc follows
		// immediately.
		OFFSET OpcodeLocation = Reloc->Location - 2;
		
		// Safety check before accessing the section data.
		if (IsBinaryDataRange (Section, OpcodeLocation, OpcodeLocation + 6, Reloc))
		{
			const I1 *Opcode = Section->Data + OpcodeLocation;
			// *** Branch Optimization ***
			if (((Opcode [0] == M68K_JMP_0) && (Opcode [1] == M68K_JMP_1))
			 || ((Opcode [0] == M68K_JSR_0) && (Opcode [1] == M68K_JSR_1))
			// *** Move Optimization ***
			 || (((Opcode [0] & M68K_LEA_ABS_MASK_0) == M68K_LEA_ABS_0) && ((Opcode [1] & M68K_LEA_ABS_MASK_1) == M68K_LEA_ABS_1))
			 || ((Opcode [0] == M68K_PEA_ABS_0) && (Opcode [1] == M68K_PEA_ABS_1))
			 || (((Opcode [0] & M68K_MOVE_ABS_REG_MASK_0) == M68K_MOVE_ABS_REG_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_MASK_1) == M68K_MOVE_ABS_REG_1)
			  && (!((Opcode [0] & M68K_MOVE_ABS_REG_INV_0_MASK_0) == M68K_MOVE_ABS_REG_INV_0_0))
			  && (!(((Opcode [0] & M68K_MOVE_ABS_REG_INV_1_MASK_0) == M68K_MOVE_ABS_REG_INV_1_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_INV_1_MASK_1) == M68K_MOVE_ABS_REG_INV_1_1))))
			 || (((Opcode [0] & M68K_MOVE_ABS_PREDEC_MASK_0) == M68K_MOVE_ABS_PREDEC_0) && ((Opcode [1] & M68K_MOVE_ABS_PREDEC_MASK_1) == M68K_MOVE_ABS_PREDEC_1)
			  && (!((Opcode [0] & M68K_MOVE_ABS_PREDEC_INV_0_MASK_0) == M68K_MOVE_ABS_PREDEC_INV_0_0)))
			 || (((Opcode [0] & M68K_MOVE_ABS_OFSREG_MASK_0) == M68K_MOVE_ABS_OFSREG_0) && ((Opcode [1] & M68K_MOVE_ABS_OFSREG_MASK_1) == M68K_MOVE_ABS_OFSREG_1)
			  && (!((Opcode [0] & M68K_MOVE_ABS_OFSREG_INV_0_MASK_0) == M68K_MOVE_ABS_OFSREG_INV_0_0)))
			 || (((Opcode [0] & M68K_MOVEM_ABS_REGS_MASK_0) == M68K_MOVEM_ABS_REGS_0) && ((Opcode [1] & M68K_MOVEM_ABS_REGS_MASK_1) == M68K_MOVEM_ABS_REGS_1))
			// *** Test Optimization ***
			 || (((Opcode [0] & M68K_CMP_ABS_REG_MASK_0) == M68K_CMP_ABS_REG_0) && ((Opcode [1] & M68K_CMP_ABS_REG_MASK_1) == M68K_CMP_ABS_REG_1)
			  && (!((Opcode [1] & M68K_CMP_ABS_REG_INV_0_MASK_1) == M68K_CMP_ABS_REG_INV_0_1)))
			 || (((Opcode [0] & M68K_BTST_REG_ABS_MASK_0) == M68K_BTST_REG_ABS_0) && ((Opcode [1] & M68K_BTST_REG_ABS_MASK_1) == M68K_BTST_REG_ABS_1))
			 || ((Opcode [0] == M68K_BTST_IMM_ABS_0) && (Opcode [1] == M68K_BTST_IMM_ABS_1) && (Opcode [2] == M68K_BTST_IMM_ABS_2))
			// *** Calculation Optimization ***
			 || (((Opcode [0] & M68K_ADDSUB_ABS_REG_0_MASK_0) == M68K_ADDSUB_ABS_REG_0_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_0_MASK_1) == M68K_ADDSUB_ABS_REG_0_1))
			 || (((Opcode [0] & M68K_ADDSUB_ABS_REG_1_MASK_0) == M68K_ADDSUB_ABS_REG_1_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_1_MASK_1) == M68K_ADDSUB_ABS_REG_1_1))
			 || (((Opcode [0] & M68K_MULDIV_ABS_REG_MASK_0) == M68K_MULDIV_ABS_REG_0) && ((Opcode [1] & M68K_MULDIV_ABS_REG_MASK_1) == M68K_MULDIV_ABS_REG_1))
			 || (((Opcode [0] & M68K_ANDOR_ABS_REG_MASK_0) == M68K_ANDOR_ABS_REG_0) && ((Opcode [1] & M68K_ANDOR_ABS_REG_MASK_1) == M68K_ANDOR_ABS_REG_1)))
				return TRUE;
		}
	}
	
	return FALSE;
}

// If the section ends with exactly one NOP instruction, remove the NOP.
void M68kRemoveTrailingNOP (SECTION *Section)
{
	I1 *Data = Section->Data;
	SIZE Size = Section->Size;
	
	// Validate basic circumstances.
	if (Data && (Size >= 4)
	// Check for NOP in last instruction.
     && (Data [Size - 2] == M68K_NOP_0) && (Data [Size - 1] == M68K_NOP_1) && (IsBinaryDataRange (Section, Size - 2, Size, NULL))
    // Check for NOP in previous instruction (which would mean that we
	// actually want the NOPs to be there).
	 && (!((Data [Size - 4] == M68K_NOP_0) && (Data [Size - 3] == M68K_NOP_1)))
	// Check if something prevents us from shrinking the section.
	 && (CanShrinkSection (Section, Size - 2, NULL)))
     	// Cut the NOP away.
    	CutSection (Section, Size - 2);
}

// Fix and return the target offset of a reloc.
// In particular, for 1-byte relative relocs, the target offset is
// increased by 1, so the the reloc points to the symbol and not to
// the symbol minus 1.
OFFSET M68kFixTargetOffset (OFFSET Offset, SIZE RelocSize, BOOLEAN RelocRelative)
{
	if (RelocRelative && (RelocSize == 1))
		return Offset + 1;
	else
		return Offset;
}

#define SHORT_IMPORTANCE 2048

// Called by M68kGetSectionRelationship; see below.
static COUNT M68kGetRelocImportance (const RELOC *Reloc, OFFSET Offset)
{
	SECTION *Section = Reloc->Parent;
	
	if (Reloc->Unoptimizable) return 0;

	// Byte offsets are useful only for jumps or branches, so detect
	// them.
	if (M68K_REL_OK (Offset, 1))
	{
		OFFSET RelocLocation = Reloc->Location;
		OFFSET OpcodeLocation = RelocLocation - 2;
		I1 *Opcode = Section->Data + OpcodeLocation;
		if (Reloc->Size == 4)
		{
			// Check whether the reloc belongs to a branch.
			BOOLEAN IsJMP = ((Opcode [0] == M68K_JMP_0)
			                 && (Opcode [1] == M68K_JMP_1));
			BOOLEAN IsJSR = ((Opcode [0] == M68K_JSR_0)
			                 && (Opcode [1] == M68K_JSR_1));
			
			if ((IsJMP || IsJSR)
			    && IsBinaryDataRange (Section,
			                          OpcodeLocation,
			                          OpcodeLocation + 6, Reloc))
			{
				return ((Offset == 4 && IsJMP) ? SHORT_IMPORTANCE * 2
				                               : SHORT_IMPORTANCE);
			}
		}
		else if (Reloc->Size == 2)
		{
			// Check whether the reloc belongs to a branch.
			if ((Opcode [0] & M68K_Bcc_MASK_0) == M68K_Bcc_W_0
			    && Opcode [1] == M68K_Bcc_W_1
			    && IsBinaryDataRange (Section,
			                          OpcodeLocation,
			                          OpcodeLocation + 4, Reloc))
			{
				return (Offset == 2 && !(Opcode [0] == M68K_BSR_W_0
				                         && Opcode [1] == M68K_BSR_W_1)) ? SHORT_IMPORTANCE * 2
				                                                         : SHORT_IMPORTANCE;
			}
		}
	}
	
	// Everything else is not optimizable.
	return 0;
}

// Determines the amount of relationship between the two sections, for the
// situation that Section2 might be put just behind Section1. A reference
// that could potentially be short gets 2048 points; a reference that could
// potentially be removed gets twice as much.
// The effect is that a section with one potentially short reference can
// be at most 2048 bytes long for being inserted immediately by reordering,
// and so on.
COUNT M68kGetSectionRelationship (const SECTION *Section1, const SECTION *Section2)
{
	COUNT Result = 0;
	
	const RELOC *Reloc;
	for_each (Reloc, Section1->Relocs)
	{
		const SYMBOL *TargetSymbol = Reloc->Target.Symbol;
		if (TargetSymbol && TargetSymbol->Parent == Section2)
			Result += M68kGetRelocImportance (Reloc, Section1->Size - Reloc->Location + TargetSymbol->Location + Reloc->Target.Offset + Reloc->FixedOffset);
	}
	for_each (Reloc, Section2->Relocs)
	{
		const SYMBOL *TargetSymbol = Reloc->Target.Symbol;
		if (TargetSymbol && TargetSymbol->Parent == Section1)
			Result += M68kGetRelocImportance (Reloc, TargetSymbol->Location + Reloc->Target.Offset + Reloc->FixedOffset - Section1->Size - Reloc->Location);
	}
	
	return Result;
}

// Compute an estimate of how important it is to put the section containing this
// reloc next during local section reordering.
// Here are the estimates used:
// 0-byte branches save 6 bytes and 1 reloc and cannot be deferred -> 512 points
// 2-byte branches save 4 bytes and 1 reloc and can rarely be deferred -> 256
// PC-relative references save 2 bytes and 1 reloc. They can be deferred based
// on how far the accumulated distance is. We compute between 0 and 32 points
// based on the offset, with the formula: (offset^2>>25)+2.
COUNT M68kComputeRelocGoodness(OFFSET Offset, RELOC *Reloc)
{
	SECTION *Section = Reloc->Parent;
	
	if (Reloc->Unoptimizable) return 0;

	// Byte offsets are useful only for jumps or branches, so detect
	// them.
	if (M68K_REL_OK (Offset, 1))
	{
		OFFSET RelocLocation = Reloc->Location;
		OFFSET OpcodeLocation = RelocLocation - 2;
		I1 *Opcode = Section->Data + OpcodeLocation;
		if (Reloc->Size == 4)
		{
			// Check whether the reloc belongs to a branch.
			BOOLEAN IsJMP = ((Opcode [0] == M68K_JMP_0)
			                 && (Opcode [1] == M68K_JMP_1));
			BOOLEAN IsJSR = ((Opcode [0] == M68K_JSR_0)
			                 && (Opcode [1] == M68K_JSR_1));
			
			if ((IsJMP || IsJSR)
			    && IsBinaryDataRange (Section,
			                          OpcodeLocation,
			                          OpcodeLocation + 6, Reloc))
			{
				return (Offset == 4 && IsJMP) ? 512 : 256;
			}
		}
		else if (Reloc->Size == 2)
		{
			// Check whether the reloc belongs to a branch.
			if ((Opcode [0] & M68K_Bcc_MASK_0) == M68K_Bcc_W_0
			    && Opcode [1] == M68K_Bcc_W_1
			    && IsBinaryDataRange (Section,
			                          OpcodeLocation,
			                          OpcodeLocation + 4, Reloc))
			{
				return (Offset == 2 && !(Opcode [0] == M68K_BSR_W_0
				                         && Opcode [1] == M68K_BSR_W_1)) ? 512
				                                                         : 256;
			}
		}
	}
	// Word offsets are useful everywhere where a PC-relative reference is
	// possible. So look for those places.
	if (M68K_REL_OK (Offset, 2))
	{
		OFFSET RelocLocation = Reloc->Location;
		OFFSET OpcodeLocation = RelocLocation - 2;
		I1 *Opcode = Section->Data + OpcodeLocation;
		// Safety check before accessing the section data.
		if (Reloc->Size == 4 && IsBinaryDataRange (Section, OpcodeLocation,
		                                           OpcodeLocation + 6, Reloc))
		{
			// Check whether the reloc belongs to a branch.
			if (((Opcode [0] == M68K_JMP_0) && (Opcode [1] == M68K_JMP_1))
			    || ((Opcode [0] == M68K_JSR_0) && (Opcode [1] == M68K_JSR_1))
			// Optimize LEA(.L) var.L,reg into
			// LEA(.L) var.W(%PC),reg.
			    || (((Opcode [0] & M68K_LEA_ABS_MASK_0) == M68K_LEA_ABS_0) && ((Opcode [1] & M68K_LEA_ABS_MASK_1) == M68K_LEA_ABS_1))
			// Optimize PEA(.L) var.L into PEA(.L) var.W(%PC).
			    || ((Opcode [0] == M68K_PEA_ABS_0) && (Opcode [1] == M68K_PEA_ABS_1))
			// Optimize MOVE.x var.L,reg/(reg)/(reg)+ into
			// MOVE.x var.W(%PC),reg/(reg)/(reg)+.
			    || (((Opcode [0] & M68K_MOVE_ABS_REG_MASK_0) == M68K_MOVE_ABS_REG_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_MASK_1) == M68K_MOVE_ABS_REG_1)
			        && (!((Opcode [0] & M68K_MOVE_ABS_REG_INV_0_MASK_0) == M68K_MOVE_ABS_REG_INV_0_0))
			        && (!(((Opcode [0] & M68K_MOVE_ABS_REG_INV_1_MASK_0) == M68K_MOVE_ABS_REG_INV_1_0) && ((Opcode [1] & M68K_MOVE_ABS_REG_INV_1_MASK_1) == M68K_MOVE_ABS_REG_INV_1_1))))
			// Optimize MOVE.x var.L,-(reg) into
			// MOVE.x var.W(%PC),-(reg).
			    || (((Opcode [0] & M68K_MOVE_ABS_PREDEC_MASK_0) == M68K_MOVE_ABS_PREDEC_0) && ((Opcode [1] & M68K_MOVE_ABS_PREDEC_MASK_1) == M68K_MOVE_ABS_PREDEC_1)
			        && (!((Opcode [0] & M68K_MOVE_ABS_PREDEC_INV_0_MASK_0) == M68K_MOVE_ABS_PREDEC_INV_0_0)))
			// Optimize CMP.x var.L,reg into CMP.x var.W(%PC),reg.
			    || (((Opcode [0] & M68K_CMP_ABS_REG_MASK_0) == M68K_CMP_ABS_REG_0) && ((Opcode [1] & M68K_CMP_ABS_REG_MASK_1) == M68K_CMP_ABS_REG_1)
			        && (!((Opcode [1] & M68K_CMP_ABS_REG_INV_0_MASK_1) == M68K_CMP_ABS_REG_INV_0_1)))
			// Optimize BTST reg,var.L into BTST reg,var.W(%PC).
			    || (((Opcode [0] & M68K_BTST_REG_ABS_MASK_0) == M68K_BTST_REG_ABS_0) && ((Opcode [1] & M68K_BTST_REG_ABS_MASK_1) == M68K_BTST_REG_ABS_1))
			// Optimize ADD/SUB.x var.L,reg into
			// ADD/SUB.x var.W(%PC),reg.
			    || (((Opcode [0] & M68K_ADDSUB_ABS_REG_0_MASK_0) == M68K_ADDSUB_ABS_REG_0_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_0_MASK_1) == M68K_ADDSUB_ABS_REG_0_1))
			        || (((Opcode [0] & M68K_ADDSUB_ABS_REG_1_MASK_0) == M68K_ADDSUB_ABS_REG_1_0) && ((Opcode [1] & M68K_ADDSUB_ABS_REG_1_MASK_1) == M68K_ADDSUB_ABS_REG_1_1))
			// Optimize MUL/DIV.x var.L,reg into
			// MUL/DIV.x var.W(%PC),reg.
			    || (((Opcode [0] & M68K_MULDIV_ABS_REG_MASK_0) == M68K_MULDIV_ABS_REG_0) && ((Opcode [1] & M68K_MULDIV_ABS_REG_MASK_1) == M68K_MULDIV_ABS_REG_1))
			// Optimize AND/OR.x var.L,reg into
			// AND/OR.x var.W(%PC),reg.
			    || (((Opcode [0] & M68K_ANDOR_ABS_REG_MASK_0) == M68K_ANDOR_ABS_REG_0) && ((Opcode [1] & M68K_ANDOR_ABS_REG_MASK_1) == M68K_ANDOR_ABS_REG_1)))
			{
				return ((Offset * Offset) >> 25);
			}
		}
	}
	// Everything else is not optimizable, so ignore it for section reordering.
	return 0;
}

