/* comprrlc.h: Routines for compressed relocation tables

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

#ifndef INSERT_COMPRRLC_H
#define INSERT_COMPRRLC_H

#include "../generic.h"
#include "ins_def.h"
#include "../data.h"
#include "model/list.h"

// Append relocation entries in the format required by the TIGCCLIB relocation
// code. If TargetSection is NULL, append all relocation entries that point to
// unhandled sections. Otherwise, append all relocation entries pointing to this
// section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertCompressedRelocs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append relocation entries in the format required by the TIGCCLIB relocation
// code, using InsertCompressedRelocs. If TargetSection is NULL, output an empty
// relocation table. Otherwise, append all relocation entries pointing to this
// section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertCompressedSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append ROM calls in the format required by the TIGCCLIB relocation code.
BOOLEAN InsertCompressedROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);

// Append the section size and relocs to the specified section in the
// format required by PreOS.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertPreOSSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append ROM calls in the format required by PreOs.
BOOLEAN InsertPreOsROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);

// Append RAM calls in the format required by PreOs.
BOOLEAN InsertPreOsRAMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);

// Append libraries and library calls in the format required by PreOs.
BOOLEAN InsertPreOsLibraries (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);

// Append import tables and relocation entries in the format required by PreOs.
// In order: library imports, ROM calls, RAM calls, relocs, BSS references. All
// in one stream, pointed to by a single pointer.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertPreOsCompressedTables (SECTION *Section);

#ifdef FARGO_SUPPORT
// Append the section size and section relocation entries in the format
// required by Fargo 0.2.1.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertFargo021SectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append library calls in the format required by Fargo 0.2.1.
BOOLEAN InsertFargo021Libraries (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);
#endif /* FARGO_SUPPORT */

// Append mlink-style relocation entries in the format required by the TIGCCLIB
// relocation code. If TargetSection is NULL, append all relocation entries that
// point to unhandled sections. Otherwise, append all relocation entries
// pointing to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertMlinkRelocs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append mlink-style relocation entries in the format required by the TIGCCLIB
// relocation code, using InsertMlinkRelocs. If TargetSection is NULL, output
// an empty relocation table. Otherwise, append all relocation entries pointing
// to this section.
// Warning: Inserting relocs is special: Since the relocs are changed
// during the process, they can be inserted only once.
BOOLEAN InsertMlinkSectionRefs (SECTION *Section, SECTION *TargetSection, SECTION *MergedSection, const LOCATION *Reference);

// Append ROM calls in the mlink-style format required by the TIGCCLIB
// relocation code.
BOOLEAN InsertMlinkROMCalls (SECTION *Section, SECTION *MergedSection, const LOCATION *Reference);

#endif
