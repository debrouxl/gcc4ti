/* constmrg.h: Routines to merge constants

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

#ifndef CONSTMRG_H
#define CONSTMRG_H

#include "generic.h"
#include "data.h"

// Merge constants (including strings) in sections marked Mergeable to avoid
// duplication. A constant is considered to be an interval enclosed within 2
// label groups, where a label group is the equivalence class formed by the
// symbols at a given offset. A constant is said unaligned if the section
// containing it also has the Unaligned flag set, and aligned otherwise. The
// value of a constant is said to be the contents of the interval defining it.
// The algorithm used is a prefix merge algorithm:
// * If the value of a constant A is a prefix of the value of a constant B, and
//   A is unaligned and/or B is aligned, then A is identified with B. 
// * If the value of a constant A is a strict prefix of the value of a constant
//   B, A is aligned and B is unaligned, then B is split into a new aligned
//   section, and A is identified with the so-created B.
// Identification works the following way: The label group defining A is moved
// into the label group defining B. Then, if A was constituting an entire
// section, the section is deleted. Otherwise, A is removed using the
// range-cutting API.
// The linker doesn't look for identical constants in the same section. It is
// the compiler's job to merge constants in this case.
void MergeConstants (PROGRAM *Program);

#endif

