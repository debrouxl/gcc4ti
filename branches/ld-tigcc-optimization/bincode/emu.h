/* emu.h: Definitions for emulator-specific M68000 code fixup

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

#ifndef BINCODE_M68K_EMU_H
#define BINCODE_M68K_EMU_H

#include "../generic.h"

// F-Line opcode of the program-relative JMP.W branch (own emulator only).
#define M68K_EMU_JMP_W_0 0xFF
#define M68K_EMU_JMP_W_1 0xEF

// F-Line opcode of the program-relative JSR.W branch (own emulator only).
#define M68K_EMU_JSR_W_0 0xFF
#define M68K_EMU_JSR_W_1 0xEE

#endif
