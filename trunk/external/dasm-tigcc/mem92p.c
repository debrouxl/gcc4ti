/* Hey EMACS -*- linux-c -*- */
/* $Id: mem92p.c 2428 2007-04-04 17:05:38Z roms $ */

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
    Memory management: TI92+ FLASH without Hardware Protection
	Some values may be hard-coded for performance reasons !
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "mem.h"
#include "mem92p.h"
#include "ti68k_def.h"
#include "mem_size.h"
#include "main.h"

// 000000-0fffff : RAM (256 KB)
// 100000-1fffff :
// 200000-2fffff : mirror of FLASH (HW2)
// 300000-3fffff : 
// 400000-4fffff : external FLASH (2 MB)
// 500000-5fffff :
// 600000-6fffff : memory mapped I/O (all HW)
// 700000-7fffff : memory mapped I/O (HW2)
// 800000-8fffff : unused
// 900000-9fffff :	 ... 
// a00000-afffff : 
// b00000-bfffff : 
// c00000-cfffff : 
// d00000-dfffff :
// e00000-efffff :   ...
// d00000-ffffff : unused

int ti92p_mem_init(void)
{
	// set mappers
	mem_get_byte_ptr = ti92p_get_byte;
	mem_get_word_ptr = ti92p_get_word;
	mem_get_long_ptr = ti92p_get_long;

	mem_get_real_addr_ptr = ti92p_get_real_addr;
  
    return 0;
}

uint8_t* ti92p_get_real_addr(uint32_t adr)
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_p(ram, adr, RAM_SIZE_TI92P - 1);
	}

    // FLASH access
	else if(IN_BOUNDS(0x200000, adr, 0x5fffff))
	{
		return get_p(rom, adr, ROM_SIZE_TI92P - 1);
	}
	
	return unused;
}

uint32_t ti92p_get_long(uint32_t adr) 
{
	// RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_l(ram, adr, RAM_SIZE_TI92P - 1);
	}

    // FLASH access
	else if(IN_BOUNDS(0x200000, adr, 0x5fffff))
	{
		return get_l(rom, adr, ROM_SIZE_TI92P - 1);
	}
	
    return 0x14141414;
}

uint16_t ti92p_get_word(uint32_t adr) 
{
    // RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_w(ram, adr, RAM_SIZE_TI92P - 1);
	}

    // FLASH access
	else if(IN_BOUNDS(0x200000, adr, 0x5fffff))
	{
		return get_w(rom, adr, ROM_SIZE_TI92P - 1);
	}
	
    return 0x1414;
}

uint8_t ti92p_get_byte(uint32_t adr) 
{    
    // RAM access
	if(IN_BOUNDS(0x000000, adr, 0x1fffff))
	{
		return get_b(ram, adr, RAM_SIZE_TI92P - 1);
	}

    // FLASH access
	else if(IN_BOUNDS(0x200000, adr, 0x5fffff))
	{
		return get_b(rom, adr, ROM_SIZE_TI92P - 1);
	}
	
    return 0x14;
}
