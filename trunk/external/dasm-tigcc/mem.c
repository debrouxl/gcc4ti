/* Hey EMACS -*- linux-c -*- */
/* $Id: mem.c 2268 2006-11-06 17:18:51Z roms $ */

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
    Memory management: RAM, PROM/FLASH, I/O ports and bkpts
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "main.h"
#include "mem.h"
#include "ti68k_def.h"
#include "mem89.h"
#include "mem92.h"
#include "mem92p.h"
#include "mem89tm.h"
#include "memv2.h"

// 000000-0fffff : RAM (128 or 256 KB)
// 100000-1fffff : 
// 200000-2fffff : internal ROM (TI92, TI89, V200) or unused
// 300000-3fffff : idem
// 400000-4fffff : external ROM (TI92, TI92-II, TI92+) or unused
// 500000-5fffff : idem
// 600000-6fffff : memory mapped I/O (all HW)
// 700000-7fffff : memory mapped I/O (HW2, HW3)
// 800000-8fffff : ROM (TI89 Titanium) or unused
// 900000-9fffff : idem
// a00000-afffff : idem
// b00000-bfffff : idem
// c00000-cfffff : unused
// d00000-dfffff :	 ...
// e00000-efffff :   ...
// d00000-ffffff : unused

static GETBYTE_FUNC	get_byte_ptr;	// set on memXX.c or hwprot.c
static GETWORD_FUNC	get_word_ptr;
static GETLONG_FUNC	get_long_ptr;

GETBYTE_FUNC	mem_get_byte_ptr;	// set by memXX.c:tiXX_mem_init
GETWORD_FUNC	mem_get_word_ptr;
GETLONG_FUNC	mem_get_long_ptr;

REALADR_FUNC	mem_get_real_addr_ptr;

/* Mem init/exit */

int hw_mem_init(void)
{
    // set banks and mappers on per calc basis
    switch(calc_type)
    {
    case TI92:  ti92_mem_init();  break;
    case TI92p: ti92p_mem_init(); break;
    case TI89:  ti89_mem_init();  break;
    case V200:  v200_mem_init();  break;
    case TI89t: ti89t_mem_init(); break;
    default: break;
    }

		get_byte_ptr = mem_get_byte_ptr;
		get_word_ptr = mem_get_word_ptr;
		get_long_ptr = mem_get_long_ptr;

    return 0;
}

uint8_t* hw_get_real_address(uint32_t adr)
{
	return mem_get_real_addr_ptr(adr);
}

uint32_t hw_get_long(uint32_t adr) 
{
    adr &= 0xFFFFFF;
	return get_long_ptr(adr);
}

uint16_t hw_get_word(uint32_t adr) 
{
    adr &= 0xFFFFFF;
	return get_word_ptr(adr);
}

uint8_t hw_get_byte(uint32_t adr) 
{
    adr &= 0xFFFFFF;
	return get_byte_ptr(adr);
}

uint8_t hw_get_byte_noexcept(uint32_t adr) 
{
    adr &= 0xFFFFFF;
	return get_byte_ptr(adr);
}
