/* Hey EMACS -*- linux-c -*- */
/* $Id: fpudasm.c 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Liévin
 *  Copyright (c) 2001-2003, Romain Liévin
 *  Copyright (c) 2003, Julien Blache
 *  Copyright (c) 2004, Romain Liévin
 *  Copyright (c) 2005, Romain Liévin
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*
    A pseudo-FPU disassembler.
	Many informations comes from the TI's BCD artihmetic package:
	<ftp://ftp.ti.com/pub/graph-ti/calc-apps/89/asm/exec.inc>
*/

#include <stdio.h>
#include <string.h>
#include <stdint.h>

typedef struct
{
	uint16_t	code;
	const char*	name;
} TUPLE;

// 6 chars max
TUPLE operators[9] = {
	{ 0x0000, "FCMP" }, { 0x1000, "FADD" }, { 0x2000, "FDIV" }, { 0x3000, "FMUL" }, 
	{ 0x4000, "FSUB" }, { 0x5000, "FINTRZ" }, { 0x6000, "FMOVE" }, { 0x7000, "FNEG" },
	{ 0x8000, "FTST" },
};

// 7 chars max
TUPLE sizes[6] = {
	{ 0x0000, "BYTE"}, { 0x0200, "WORD"}, { 0x0400, "LONG"},
	{ 0x0600, "SINGLE"}, { 0x0800, "DOUBLE"}, { 0x0a00, "UNSGNED"}, 
};

// 11 chars max
TUPLE srcs[21] = {
	{ 0x0000, "FP0"}, { 0x0010, "FP1"}, { 0x0020, "FP2"}, { 0x0030, "FP3"}, 
	{ 0x0040, "FP4"}, { 0x0050, "FP5"}, { 0x0060, "FP6"}, { 0x0070, "FP7"}, 
	{ 0x0080, "D0"}, { 0x0090, "D1"}, { 0x00a0, "D2"}, { 0x00b0, "D3"}, 
	{ 0x00c0, "D4"}, { 0x00d0, "D5"}, { 0x00e0, "D6"}, { 0x00f0, "D7"}, 
	{ 0x0100, "IMMED_LONG"}, { 0x0110, "IMMED_SHORT"}, { 0x0120, "FRAME_OFF"},
	{ 0x0130, "EFFECT_ADDR"}, { 0x0140, "IMMED_ZERO"},
};

// 11 chars max
TUPLE dsts[11] = {
	{ 0x0000, "R0"}, { 0x0001, "R1"}, { 0x0002, "R2"}, { 0x0003, "R3"}, 
	{ 0x0004, "R4"}, { 0x0005, "R5"}, { 0x0006, "R6"}, { 0x0007, "R7"}, 
	{ 0x0008, "FRAME_OFF"}, { 0x0009, "EFFECT_ADDR"}, { 0x000a, "RETURN_REG"}, 
};

#define GET_OPERATOR(x)		((x) & 0xf000)
#define GET_SIZE(x)			((x) & 0x0e00)
#define GET_SRC(x)			((x) & 0x01f0)
#define GET_DST(x)			((x) & 0x000f)

/*
; BCD arithmetic package

***************************************************************************
*                        OPERATOR / OPERAND WORD                          *
*                                                                         *
*    | 15| 14| 13| 12| 11| 10| 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |    *
*    +---------------+-----------+-------------------+---------------+    *
*    |   OPERATOR    |   SIZE    |    SRC OPERAND    |  DEST OPERAND |    *
*    +---------------+-----------+-------------------+---------------+    *
*        FCMP   0      BYTE    0     FP0          0    R0 (FP or D) 0     *
*        FADD   1      WORD    1      |                 |                 *
*        FDIV   2      LONG    2     FP7          7    R7 (FP or D) 7     *
*        FMUL   3      SINGLE  3     D0           8    FRAME_OFF    8     *
*        FSUB   4      DOUBLE  4      |                EFFECT_ADDR  9     *
*        FINTRZ 5      UNSGNED 5     D7          15    RETURN_REG  10     *
*        FMOVE  6                    IMMED_LONG  16                       *
*        FNEG   7                    IMMED_SHORT 17                       *
*        FTST   8                    FRAME_OFF   18                       *
*                                    EFFECT_ADDR 19                       *
*                                    IMMED_ZERO  20                       *
***************************************************************************
bcdCmp          = 0x0000
bcdAdd          = 0x1000
bcdDiv          = 0x2000
bcdMul          = 0x3000
bcdSub          = 0x4000
bcdIntz         = 0x5000
bcdMove         = 0x6000
bcdNeg          = 0x7000
bcdTst          = 0x8000

bcdByte         = 0x0000
bcdWord         = 0x0200
bcdLong         = 0x0400
bcdSingle       = 0x0600
bcdDouble       = 0x0800
bcdUnsigned     = 0x0A00

; Source operand
bcdFP0          = 0x0000
bcdFP1          = 0x0010
bcdFP2          = 0x0020
bcdFP3          = 0x0030
bcdFP4          = 0x0040
bcdFP5          = 0x0050
bcdFP6          = 0x0060
bcdFP7          = 0x0070
bcdD0           = 0x0080
bcdD1           = 0x0090
bcdD2           = 0x00A0
bcdD3           = 0x00B0
bcdD4           = 0x00C0
bcdD5           = 0x00D0
bcdD6           = 0x00E0
bcdD7           = 0x00F0
bcdLongImm      = 0x0100
bcdShortImm     = 0x0110
bcdFrameSrc     = 0x0120
bcdAbsSrc       = 0x0130
bcdZeroImm      = 0x0140

; Destination operand
bcdR0           = 0x0000
bcdR1           = 0x0001
bcdR2           = 0x0002
bcdR3           = 0x0003
bcdR4           = 0x0004
bcdR5           = 0x0005
bcdR6           = 0x0006
bcdR7           = 0x0007
bcdFrameDest    = 0x0008
bcdAbsDest      = 0x0009
bcdRetReg       = 0x000A
*/

/*
	Input: FPU opcode in 'code'
	Output: FPU disassembled in 'buf'. sizeof(buf) >= 6+1+7+1+11+1+11 = 38
*/
int DasmFPU(uint16_t code, char *buf)
{
	int	operator = GET_OPERATOR(code);
	int size = GET_SIZE(code);
	int src = GET_SRC(code);
	int dst = GET_DST(code);
	int idx[4] = { 0 };
	int i;
	int j = 0;

	for(i = 0; i < (int)(sizeof(operators) / sizeof(TUPLE)); i++)
	{
		if(operators[i].code == operator)
		{
			idx[j++] = i;
			break;
		}
	}

	for(i = 0; i < (int)(sizeof(sizes) / sizeof(TUPLE)); i++)
	{
		if(sizes[i].code == size)
		{
			idx[j++] = i;
			break;
		}
	}
	
	for(i = 0; i < (int)(sizeof(srcs) / sizeof(TUPLE)); i++)
	{
		if(srcs[i].code == src)
		{
			idx[j++] = i;
			break;
		}
	}

	for(i = 0; i < (int)(sizeof(dsts) / sizeof(TUPLE)); i++)
	{
		if(dsts[i].code == dst)
		{
			idx[j++] = i;
			break;
		}
	}

	sprintf(buf, "%s.%s %s,%s", 
		operators[idx[0]].name, sizes[idx[1]].name,
		srcs[idx[2]].name, dsts[idx[3]].name);

	return 0;
}
