/* cutrange.h: Routines for range cutting

   Copyright (C) 2003 Kevin Kofler

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

#ifndef BINCODE_CUTRANGE_H
#define BINCODE_CUTRANGE_H

#include "../generic.h"
#include "../data.h"

// Check whether the segments which overlap with the interval [Start,End)
// allow range-cutting.
BOOLEAN CanCutRange (SECTION *Section, OFFSET Start, OFFSET End);

// Cut the specified range (the interval [Start,End)) out of
// the specified section. All items within this range are removed. All relocs
// refering to targets or relations behind End (taking Symbol and Offset,
// but NOT FixedOffset, into account) are adjusted.
void CutRange (SECTION *Section, OFFSET Start, OFFSET End);

// Do the cleanup needed after cutting ranges in a section.
void FinalizeRangeCutting (SECTION *Section);

#endif
