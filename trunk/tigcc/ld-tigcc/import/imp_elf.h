/* imp_elf.h: Routines to import an ELF file

   Copyright (C) 2024 Peter Lafreniere <peter@n8pjl.ca> 

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

#ifndef IMP_ELF_H
#define IMP_ELF_H

#include "../generic.h"

#ifdef ELF_SUPPORT

#include "../data.h"

// Import a m68k-elf file into the internal data structures.
BOOLEAN ImportELFFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName);

#endif /* ELF_SUPPORT */

#endif /* IMP_ELF_H */
