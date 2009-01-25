/* kernel.h: Routines to handle automatic insertion of section contents in kernel format

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

#ifndef INSERT_KERNEL_H
#define INSERT_KERNEL_H

#include "../generic.h"
#include "ins_def.h"
#include "../data.h"
#include "model/list.h"

// Append relocation entries in the format required by kernels. If
// TargetSection is NULL, insert all relocation entries that point to
// unhandled sections. Otherwise, insert all relocation entries pointing
// to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertKernelRelocs (SECTION *Section, SECTION *TargetSection);

// Append relocation entries to TargetSection in the format required by
// kernels. If TargetSection is NULL, AlwaysTerminate specifies whether
// to insert the two terminating zero bytes anyway.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertKernelSectionRefs (SECTION *Section, SECTION *TargetSection, BOOLEAN AlwaysTerminate);

// Append ROM calls in the format required by kernels.
BOOLEAN InsertKernelROMCalls (SECTION *Section);

// Append RAM calls in the format required by kernels.
BOOLEAN InsertKernelRAMCalls (SECTION *Section);

// Append library calls in the format required by kernels.
BOOLEAN InsertKernelLibraries (SECTION *Section);

#ifdef FARGO_SUPPORT
// Append library calls in the format required by Fargo v0.2.0.
BOOLEAN InsertFargo020Libraries (SECTION *Section);
#endif /* FARGO_SUPPORT */

// Append exported symbols in the format required by kernels. If
// TrailingZeroBytes is FALSE, use the Fargo v0.2.x format without
// trailing zero bytes.
BOOLEAN InsertKernelExports (SECTION *Section, BOOLEAN TrailingZeroBytes);

#endif
