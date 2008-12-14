/* m68k.h: Definitions for M68000 code fixup

   Copyright (C) 2003 Sebastian Reichelt
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

#ifndef BINCODE_M68K_H
#define BINCODE_M68K_H

#include "../generic.h"

// *** Generic Definitions ***

// Maximum distance for a relative reloc of the specified size.
#define M68K_REL_MAX_2   0x7FFF
#define M68K_REL_MIN_2 (-0x8000)
#define M68K_REL_MAX_1     0x7F
#define M68K_REL_MIN_1   (-0x7F)
#define M68K_REL_MAX   M68K_REL_MAX_2
#define M68K_REL_MIN   M68K_REL_MIN_2

#define M68K_REL_OK(Distance,Size) (((Distance) >= M68K_REL_MIN_##Size) && ((Distance) <= M68K_REL_MAX_##Size))

// Make a relative opcode out of an absolute one.
#define M68K_MAKE_REL_OPCODE_1(Opcode_1) ((Opcode_1)++)

// *** General Branches ***

// Opcode of the unconditional absolute JMP instruction.
#define M68K_JMP_0 0x4E
#define M68K_JMP_1 0xF9

#define M68K_BRA_W_0 0x60
#define M68K_BRA_W_1 0x00

#define M68K_BRA_S_0 0x60

// Opcode of the Bcc.W instruction family (including BRA.W and BSR.W).
#define M68K_Bcc_W_0 0x60
#define M68K_Bcc_W_1 0x00
// Opcode of the Bcc.S instruction family (including BRA.S and BSR.S).
#define M68K_Bcc_S_0 0x60
// Mask for the Bcc instruction family.
#define M68K_Bcc_MASK_0 0xF0

// *** Subroutine Branches ***

// Opcode of the absolute JSR instruction.
#define M68K_JSR_0 0x4E
#define M68K_JSR_1 0xB9

// Opcode of the BSR.W instruction.
#define M68K_BSR_W_0 0x61
#define M68K_BSR_W_1 0x00
// Opcode of the BSR.S instruction.
#define M68K_BSR_S_0 0x61

// *** Move Instructions ***

// Opcode of MOVE.x var.L,reg/(reg)/(reg)+.
#define M68K_MOVE_ABS_REG_0 0x00
#define M68K_MOVE_ABS_REG_1 0x39
// Mask for MOVE.x var.L,reg/(reg)/(reg)+: 11000001 00111111.
#define M68K_MOVE_ABS_REG_MASK_0 0xC1
#define M68K_MOVE_ABS_REG_MASK_1 0x3F
// Invalid instructions for MOVE.x var.L,reg/(reg)/(reg)+.
// Everything starting with 0 on the first hex number is not a MOVE (0x0???).
#define M68K_MOVE_ABS_REG_INV_0_0 0x00
#define M68K_MOVE_ABS_REG_INV_0_MASK_0 0xF0
// MOVE.B var.L,reg cannot use address registers for reg (0x1?7?).
#define M68K_MOVE_ABS_REG_INV_1_0 0x10
#define M68K_MOVE_ABS_REG_INV_1_1 0x70
#define M68K_MOVE_ABS_REG_INV_1_MASK_0 0xF0
#define M68K_MOVE_ABS_REG_INV_1_MASK_1 0xF0

// Opcode of MOVE.x var.L,-(reg).
#define M68K_MOVE_ABS_PREDEC_0 0x01
#define M68K_MOVE_ABS_PREDEC_1 0x39
// Mask for MOVE.x var.L,-(reg): 11000001 11111111.
#define M68K_MOVE_ABS_PREDEC_MASK_0 0xC1
#define M68K_MOVE_ABS_PREDEC_MASK_1 0xFF
// Invalid instructions for MOVE.x var.L,-(reg).
// Everything starting with 0 on the first hex number is not a MOVE (0x0???).
#define M68K_MOVE_ABS_PREDEC_INV_0_0 0x00
#define M68K_MOVE_ABS_PREDEC_INV_0_MASK_0 0xF0

// Opcode of MOVE.x var.L,ofs(reg).
#define M68K_MOVE_ABS_OFSREG_0 0x01
#define M68K_MOVE_ABS_OFSREG_1 0x79
// Mask for MOVE.x var.L,ofs(reg): 11000001 11111111.
#define M68K_MOVE_ABS_OFSREG_MASK_0 0xC1
#define M68K_MOVE_ABS_OFSREG_MASK_1 0xFF
// Invalid instructions for MOVE.x var.L,ofs(reg).
// Everything starting with 0 on the first hex number is not a MOVE (0x0???).
#define M68K_MOVE_ABS_OFSREG_INV_0_0 0x00
#define M68K_MOVE_ABS_OFSREG_INV_0_MASK_0 0xF0

// Opcode of MOVEM.x var.L,regs
#define M68K_MOVEM_ABS_REGS_0 0x4C
#define M68K_MOVEM_ABS_REGS_1 0xB9
// Mask for MOVEM.x var.L,regs: 11111111 10111111.
#define M68K_MOVEM_ABS_REGS_MASK_0 0xFF
#define M68K_MOVEM_ABS_REGS_MASK_1 0xBF

// Opcode of LEA(.L) var.L,reg.
#define M68K_LEA_ABS_0 0x41
#define M68K_LEA_ABS_1 0xF9
// Mask for LEA(.L) var.L,reg: 11110001 11111111.
#define M68K_LEA_ABS_MASK_0 0xF1
#define M68K_LEA_ABS_MASK_1 0xFF

// Opcode of PEA(.L) var.L.
#define M68K_PEA_ABS_0 0x48
#define M68K_PEA_ABS_1 0x79

// *** Test Instructions ***

// Opcode of CMP.x var.L,reg.
#define M68K_CMP_ABS_REG_0 0xB0
#define M68K_CMP_ABS_REG_1 0x39
// Mask for CMP.x var.L,reg: 11110001 00111111.
#define M68K_CMP_ABS_REG_MASK_0 0xF1
#define M68K_CMP_ABS_REG_MASK_1 0x3F
// The second byte cannot start with F (0x??F?).
#define M68K_CMP_ABS_REG_INV_0_1 0xF0
#define M68K_CMP_ABS_REG_INV_0_MASK_1 0xF0

// Opcode of BTST reg,var.L.
#define M68K_BTST_REG_ABS_0 0x01
#define M68K_BTST_REG_ABS_1 0x39
// Mask for BTST reg,var.L: 11110001 11111111.
#define M68K_BTST_REG_ABS_MASK_0 0xF1
#define M68K_BTST_REG_ABS_MASK_1 0xFF

// Opcode of BTST #num,var.L.
#define M68K_BTST_IMM_ABS_0 0x08
#define M68K_BTST_IMM_ABS_1 0x39
#define M68K_BTST_IMM_ABS_2 0x00

// *** Calculation Instructions ***

// 1st Opcode of ADD/SUB.x var.L,reg.
#define M68K_ADDSUB_ABS_REG_0_0 0x90
#define M68K_ADDSUB_ABS_REG_0_1 0x39
// 1st Mask for ADD/SUB.x var.L,reg: 10110001 00111111.
#define M68K_ADDSUB_ABS_REG_0_MASK_0 0xB1
#define M68K_ADDSUB_ABS_REG_0_MASK_1 0x3F

// 2nd Opcode of ADD/SUB.x var.L,reg.
#define M68K_ADDSUB_ABS_REG_1_0 0x91
#define M68K_ADDSUB_ABS_REG_1_1 0xF9
// 2nd Mask for ADD/SUB.x var.L,reg: 10110001 11111111.
#define M68K_ADDSUB_ABS_REG_1_MASK_0 0xB1
#define M68K_ADDSUB_ABS_REG_1_MASK_1 0xFF

// Opcode of MULx/DIVx.x var.L,reg.
#define M68K_MULDIV_ABS_REG_0 0x80
#define M68K_MULDIV_ABS_REG_1 0xF9
// Mask for MULx/DIVx.x var.L,reg.: 10110000 11111111.
#define M68K_MULDIV_ABS_REG_MASK_0 0xB0
#define M68K_MULDIV_ABS_REG_MASK_1 0xFF

// Opcode of AND/OR.x var.L,reg.
#define M68K_ANDOR_ABS_REG_0 0x80
#define M68K_ANDOR_ABS_REG_1 0x39
// Mask for AND/OR.x var.L,reg.: 10110001 00111111.
#define M68K_ANDOR_ABS_REG_MASK_0 0xB1
#define M68K_ANDOR_ABS_REG_MASK_1 0x3F
// The second byte cannot start with F (0x??F?). Otherwise it is a MULx/DIVx.
#define M68K_ANDOR_ABS_REG_INV_0_1 0xF0
#define M68K_ANDOR_ABS_REG_INV_0_MASK_1 0xF0

// *** Other Instructions ***

// Opcode of the NOP instruction.
#define M68K_NOP_0 0x4E
#define M68K_NOP_1 0x71

// Opcode of the RTS instruction.
#define M68K_RTS_0 0x4E
#define M68K_RTS_1 0x75

#endif
