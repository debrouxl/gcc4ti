/* Hey EMACS -*- linux-c -*- */
/* $Id: hwpm.c 2385 2007-03-12 21:04:20Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Lievin
 *  Copyright (c) 2001-2003, Romain Lievin
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
	This module manages Hardware Parameter Block. This is a 'C' structure like this:

	typedef struct {
	unsigned short len;					// length of parameter block
	unsigned long hardwareID;			// 1 = TI-92 Plus, 3 = TI-89
	unsigned long hardwareRevision;		// hardware revision number
	unsigned long bootMajor;			// boot code version number
	unsigned long bootRevision;			// boot code revision number
	unsigned long bootBuild;			// boot code build number
	unsigned long gateArray;			// gate array version number
	unsigned long physDisplayBitsWide;	// display width
	unsigned long physDisplayBitsTall;	// display height
	unsigned long LCDBitsWide;			// visible display width
	unsigned long LCDBitsTall;			// visible display height
	} HARDWARE_PARM_BLOCK;
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h> //memset

#include "hwpm.h"
#include "timem.h"

#define _(x) (x)
#define tiemu_info(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))

/* -- */

/*
    Read hardware parameter block from image.
*/
int ti68k_get_hw_param_block(uint8_t *rom_data, uint8_t rom_base, HW_PARM_BLOCK *s)
{
	int i = 0;
    uint32_t addr;

    addr = rd_long(&rom_data[0x104]);
    addr &= 0x000fffff;

    memset(s, 0, sizeof(HW_PARM_BLOCK));
    s->len = rd_word(&(rom_data[addr+0]));
	if(s->len > 2+(4*i++))
		s->hardwareID = rd_long(&(rom_data[addr+2]));
	if(s->len > 2+(4*i++))
		s->hardwareRevision = rd_long(&(rom_data[addr+6]));
	if(s->len > 2+(4*i++))
		s->bootMajor = rd_long(&(rom_data[addr+10]));
	if(s->len > 2+(4*i++))
		s->bootRevision = rd_long(&(rom_data[addr+14]));
	if(s->len > 2+(4*i++))
		s->bootBuild = rd_long(&(rom_data[addr+18]));
	if(s->len > 2+(4*i++))
		s->gateArray = rd_long(&(rom_data[addr+22]));
	if(s->len > 2+(4*i++))
		s->physDisplayBitsWide = rd_long(&(rom_data[addr+26]));
	if(s->len > 2+(4*i++))
		s->physDisplayBitsTall = rd_long(&(rom_data[addr+30]));
	if(s->len > 2+(4*i++))
		s->LCDBitsWide = rd_long(&(rom_data[addr+34]));
	if(s->len > 2+(4*i++))
		s->LCDBitsTall = rd_long(&(rom_data[addr+38]));

    if((s->hardwareID == HWID_V200) && (rom_base == 0x40))
    {
        tiemu_info(_("/* Detected V200 patched ROM (ExtendeD): emulated as TI92+ by changing the hwID from 8 to 1. */"));
        s->hardwareID = HWID_TI92P;
    }

	if((s->hardwareID == HWID_TI89T) && (rom_base == 0x20))
    {
        tiemu_info(_("/* Detected TI89 Titanium patched ROM (ExtendeD): emulated as TI89 by changing the hwID from 9 to 3. */"));
        s->hardwareID = HWID_TI89;
    }

    return 0;
}

/*
    Write hardware parameter block into image.
*/
int ti68k_put_hw_param_block(uint8_t *rom_data, uint8_t rom_base, const HW_PARM_BLOCK *s)
{
	int i = 0;
    uint32_t addr = 0x108;

	wr_long(&rom_data[0x104], (rom_base << 16) || addr);
	wr_word(&(rom_data[addr+0]), s->len);

	if(s->len > 2+(4*i++))
	    wr_long(&(rom_data[addr+2]), s->hardwareID);
	if(s->len > 2+(4*i++))
		wr_long(&(rom_data[addr+6]), s->hardwareRevision);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+10]), s->bootMajor);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+14]), s->bootRevision);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+18]), s->bootBuild);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+22]), s->gateArray);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+26]), s->physDisplayBitsWide);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+30]), s->physDisplayBitsTall);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+34]), s->LCDBitsWide);
	if(s->len > 2+(4*i++))
		 wr_long(&(rom_data[addr+38]), s->LCDBitsTall);

    return 0;
}
