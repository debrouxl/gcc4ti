/* fix_tios.h: Routines for TIOS-specific M68000 code fixup

   Copyright (C) 2003 Kevin Kofler
   Copyright (C) 2003 Sebastian Reichelt

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

#ifndef BINCODE_FIX_M68K_TIOS_H
#define BINCODE_FIX_M68K_TIOS_H

#include "../generic.h"
#include "../data.h"

// Turn an M68000 absolute JRA/JSR to an F-Line jump.
void M68kTIOSMakeFLineJump (RELOC *Reloc, I1 *Opcode, BOOLEAN IsJSR);

#endif
