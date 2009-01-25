/* fix_m68k.h: Routines for M68000 code fixup

   Copyright (C) 2003 Sebastian Reichelt
   Copyright (C) 2004 Kevin Kofler

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

#ifndef BINCODE_FIX_M68K_H
#define BINCODE_FIX_M68K_H

#include "../generic.h"
#include "../data.h"

// The alignment needed for sections with M68k code or data.
#define M68K_SECTION_ALIGNMENT 2

// Apply generic code fixes and optimizations to a section.
void M68kFixCode (SECTION *Section);

// Fix and optionally optimize code executable on the M68k processor family,
// for two sections which are to be merged.
// Src may be NULL.
// If DestSize is nonzero, it specifies a fixed size for the destination
// section, which will not change even when cutting ranges from it.
void M68kFixCodePreMerge (SECTION *Dest, SECTION *Src, SIZE DestSize);

// If the section ends with exactly one NOP instruction, remove the NOP.
void M68kRemoveTrailingNOP (SECTION *Section);

// Fix and possibly optimize a given relocation entry.
// TargetDistance is the estimated (signed) distance of the relocation target,
// including the fixed offset. OptimizeInfo contains information about what to
// optimize.
// The reloc may be removed during the process.
void M68kFixReloc (RELOC *Reloc, OFFSET TargetDistance, OPTIMIZE_INFO *OptimizeInfo);

// Checks if a specific reloc might be optimizable. This is currently
// limited to 4-bytes absolute relocs because that is the only case
// this is needed for.
BOOLEAN M68kIsRelocOptimizable (const RELOC *Reloc);

// Fix and return the target offset of a reloc.
// In particular, for 1-byte relative relocs, the target offset is increased
// by 1, so the the reloc points to the symbol and not to the symbol minus 1.
OFFSET M68kFixTargetOffset (OFFSET Offset, SIZE RelocSize, BOOLEAN RelocRelative);

// Determines the amount of relationship between the two sections, for the
// situation that Section2 might be put just behind Section1. A reference
// that could potentially be short gets 2048 points; a reference that could
// potentially be removed gets twice as much.
// The effect is that a section with one potentially short reference can
// be at most 2048 bytes long for being inserted immediately by reordering,
// and so on.
COUNT M68kGetSectionRelationship (const SECTION *Section1, const SECTION *Section2);

// Compute an estimate of how important it is to put the section containing this
// reloc next during local section reordering.
// Here are the estimates used:
// 0-byte branches save 6 bytes and 1 reloc and cannot be deferred -> 512 points
// 2-byte branches save 4 bytes and 1 reloc and can rarely be deferred -> 256
// PC-relative references save 2 bytes and 1 reloc. They can be deferred based
// on how far the accumulated distance is. We compute between 0 and 32 points
// based on the offset, with the formula: (offset^2>>25)+2.
COUNT M68kComputeRelocGoodness(OFFSET Offset, RELOC *Reloc);

#endif
