/* Hey EMACS -*- linux-c -*- */
/* $Id: romcalls.c 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Liévin
 *  Copyright (c) 2001-2003, Romain Liévin
 *  Copyright (c) 2003, Julien Blache
 *  Copyright (c) 2004, Romain Liévin
 *  Copyright (c) 2005, Romain Liévin
 *  Copyright (c) 2006-2007, Kevin Kofler
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
    Symbols (ROM calls address and names)
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "romcalls.h"
#include "images.h"
#include "ti68k_def.h"
#include "timem.h"
#include "main.h"

static int loaded = 0;		// loaded
static int addr_loaded = 0;	// addresses loaded

static ROM_CALL table[NMAX_ROMCALLS] = {
/* Generated from romcalls.txt by replacing:
   ([^:]*):(.*)
   with:
   {0x\1,0,"\2"},
   in Kate. */
#include "romcalls.inc"
};	// list by id
static int size;


/* =========== */

/*
	Retrieve base address of ROM calls table and size.
*/
void romcalls_get_table_infos(uint32_t *base, uint32_t *size)
{
	*base = *size = 0;

	if(calc_type == TI92)
		return;

	*base = rd_long(&rom[0x12000 + 0x88 + 0xC8]);
	*size = rd_long(&rom[((*base-4) & 0x0fffff)]);
}

/*
	Given a ROM call ID, retrieve address of ROM call.
*/
void romcalls_get_symbol_address(int id, uint32_t *addr)
{
	uint32_t base;

	base = rd_long(&rom[0x12000 + 0x88 + 0xC8]);
	*addr = rd_long(&rom[(base & 0x0fffff) + 4*id]); 
}

/* =========== */

/*
	Fill the addr field from ROM calls located in FLASH. Don't touch other fields !
	And construct list from ROM call table.
*/
static int merge_from_flash(void)
{
	uint32_t addr;
	int i;

	romcalls_get_table_infos(&addr, (uint32_t *)&size);
	if(size == 0)
		return -1;

	for(i = 0; i < size; i++)
	{
		if(table[i].name == NULL)
			table[i].name = strdup("unknown");

		table[i].addr = rd_long(&rom[(addr & 0x0fffff) + (i << 2)]); 
	}
	
	addr_loaded = 1;

	return 0;
}

/*
	Load ROM calls from file and FLASH and merge.
	Return value:
	 0 if successful
	-1 if error
	-3 if TI92
*/
int romcalls_load(int noflash)
{
	// check whether parsing is possible
	if(calc_type == TI92) return -3;

	if(!noflash && merge_from_flash())
		return -1;

	loaded = !0;

    return 0;
}

int romcalls_is_loaded(void)
{
	return loaded;
}

/* =========== */

// cache last search (disasm)
static int last_id = 0;	

// returns id or -1
int romcalls_is_addr(uint32_t addr)
{
	int i;

	if(!addr_loaded)	return -1;

	for(i = 0; i < size; i++)
	{
		if(addr == table[i].addr)
			return last_id = i;
	}

	return -1;
}

const char* romcalls_get_name(int id)
{
	if(!loaded)	return "not loaded";
	return table[id].name;
}

uint32_t romcalls_get_addr(int id)
{
	return table[id].addr;
}
