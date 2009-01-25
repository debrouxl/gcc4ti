/* fix_tios.c: Routines for TIOS-specific M68000 code fixup

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

#include "fix_tios.h"

#include "tios.h"

// Turn an M68000 absolute JRA/JSR to an F-Line jump.
void M68kTIOSMakeFLineJump (RELOC *Reloc, I1 *Opcode, BOOLEAN IsJSR)
{
	if ((Reloc->Size == 4) && (!(Reloc->Relative)))
	{
		// Optimize the JRA or JSR into an F-Line BRA.L or BSR.L.
		Opcode [0] = (IsJSR ? M68K_TIOS_BSR_L_0 : M68K_TIOS_BRA_L_0);
		Opcode [1] = (IsJSR ? M68K_TIOS_BSR_L_1 : M68K_TIOS_BRA_L_1);
		
		// Change the reloc to 4-byte relative.
		Reloc->Relative = TRUE;
	}
}
