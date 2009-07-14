/* Hey EMACS -*- linux-c -*- */
/* $Id: mem89tm.c 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Liévin
 *  Copyright (c) 2001-2003, Romain Liévin
 *  Copyright (c) 2003, Julien Blache
 *  Copyright (c) 2004, Romain Liévin
 *  Copyright (c) 2005, Romain Liévin, Kevin Kofler
 *  Copyright (c) 2007, Kevin Kofler
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
    Memory management: TI89 Titanium without any HW protection
	Some values may be hard-coded for performance reasons !
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "mem.h"
#include "mem89tm.h"
#include "ti68k_def.h"
#include "mem_size.h"
#include "main.h"

// 000000-03ffff : RAM (256 KB), not mirrored
// 100000-1fffff : unused
// 200000-2fffff : image of 0x000000 (ghost, not mirrored)
// 300000-3fffff : unused
// 400000-4fffff : image of 0x000000 (ghost, not mirrored)
// 500000-5fffff : unused
// 600000-6fffff : memory mapped I/O (all HW)
// 700000-7fffff : memory mapped I/O (HW2, HW3), non ghost'ed
// 800000-8fffff : ROM (TI89 Titanium)
// 900000-9fffff : idem
// a00000-afffff : idem
// b00000-bfffff : idem
// c00000-cfffff : unused
// d00000-dfffff :	 ...
// e00000-efffff :   ...
// d00000-ffffff : unused

int ti89t_mem_init(void)
{
	// set mappers
	mem_get_byte_ptr = ti89t_get_byte;
	mem_get_word_ptr = ti89t_get_word;
	mem_get_long_ptr = ti89t_get_long;

	mem_get_real_addr_ptr = ti89t_get_real_addr;

    return 0;
}

uint8_t* ti89t_get_real_addr(uint32_t adr)
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x03ffff) ||
	   IN_BOUNDS(0x200000, adr, 0x23ffff) ||
	   IN_BOUNDS(0x400000, adr, 0x43ffff))
	{
		return get_p(ram, adr, 0x03ffff);
	}

	// FLASH access
    else if(IN_BOUNDS(0x800000, adr, 0xbfffff))			
	{
		return get_p(rom, adr, ROM_SIZE_TI89T - 1);
	}

	return unused;
}

uint32_t ti89t_get_long(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x03ffff) ||
	   IN_BOUNDS(0x200000, adr, 0x23ffff) ||
	   IN_BOUNDS(0x400000, adr, 0x43ffff))
	{
		return get_l(ram, adr, 0x03ffff);
	}

	// FLASH access
    else if(IN_BOUNDS(0x800000, adr, 0xbfffff))			
	{
		return get_l(rom, adr, ROM_SIZE_TI89T - 1);
	}

    return 0x14141414;
}

uint16_t ti89t_get_word(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x03ffff) ||
	   IN_BOUNDS(0x200000, adr, 0x23ffff) ||
	   IN_BOUNDS(0x400000, adr, 0x43ffff))
	{
		return get_w(ram, adr, 0x03ffff);
	}

	// FLASH access
    else if(IN_BOUNDS(0x800000, adr, 0xbfffff))			
	{
		return get_w(rom, adr, ROM_SIZE_TI89T - 1);
	}

    return 0x1414;
}

uint8_t ti89t_get_byte(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x03ffff) ||
	   IN_BOUNDS(0x200000, adr, 0x23ffff) ||
	   IN_BOUNDS(0x400000, adr, 0x43ffff))
	{
		return get_b(ram, adr, 0x03ffff);
	}

	// FLASH access
    else if(IN_BOUNDS(0x800000, adr, 0xbfffff))			
	{
		return get_b(rom, adr, ROM_SIZE_TI89T - 1);
	}

    return 0x14;
}
