/* Hey EMACS -*- linux-c -*- */
/* $Id: mem92.c 2428 2007-04-04 17:05:38Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Liévin
 *  Copyright (c) 2001-2003, Romain Liévin
 *  Copyright (c) 2003, Julien Blache
 *  Copyright (c) 2004, Romain Liévin
 *  Copyright (c) 2005, Romain Liévin
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
    Memory management: TI92 ROM v1.x & v2.x
	Some values may be hard-coded for performance reasons !
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "mem.h"
#include "mem92.h"
#include "ti68k_def.h"
#include "mem_size.h"
#include "main.h"

// 000000-1fffff : RAM (128 or 256 KB)
// 200000-3fffff : internal ROM or unused (1.x: 1 MB)
// 400000-5fffff : external ROM or unused (1.x: 1 MB or 2.x: 2 MB)
// 600000-6fffff : memory mapped I/O
// 700000-ffffff : unused

int ti92_mem_init(void)
{
	// set mappers
	mem_get_byte_ptr = ti92_get_byte;
	mem_get_word_ptr = ti92_get_word;
	mem_get_long_ptr = ti92_get_long;

	mem_get_real_addr_ptr = ti92_get_real_addr;

    return 0;
}

uint8_t* ti92_get_real_addr(uint32_t adr)
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_p(ram, adr, ram_size - 1);
	}

    // PROM access
	else if(IN_RANGE(adr, rom_base, 2*MB))
	{
		return get_p(rom, adr, rom_size - 1);
	}

    return unused;
}

uint32_t ti92_get_long(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_l(ram, adr, ram_size - 1);
	}

    // PROM access
	else if(IN_RANGE(adr, rom_base, 2*MB))
	{
		return get_l(rom, adr, rom_size - 1);
	}

    return 0x14141414;
}

uint16_t ti92_get_word(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_w(ram, adr, ram_size - 1);
	}

    // PROM access
	else if(IN_RANGE(adr, rom_base, 2*MB))
	{
		return get_w(rom, adr, rom_size - 1);
	}

    return 0x1414;
}

uint8_t ti92_get_byte(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_b(ram, adr, ram_size - 1);
	}

    // PROM access
	else if(IN_RANGE(adr, rom_base, 2*MB))
	{
		return get_b(rom, adr, rom_size - 1);
	}

    return 0x14;
}
