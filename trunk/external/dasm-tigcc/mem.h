/* Hey EMACS -*- linux-c -*- */
/* $Id: main.c 245 2004-05-23 20:45:43Z roms $ */

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

#ifndef __TI68K_MEMORY__
#define __TI68K_MEMORY__

#include "stdint.h"

/* Typedefs */

typedef uint8_t  (*GETBYTE_FUNC) (uint32_t);
typedef uint16_t (*GETWORD_FUNC) (uint32_t);
typedef uint32_t (*GETLONG_FUNC) (uint32_t);

typedef uint8_t* (*REALADR_FUNC) (uint32_t addr);

extern GETBYTE_FUNC	mem_get_byte_ptr;
extern GETWORD_FUNC	mem_get_word_ptr;
extern GETLONG_FUNC	mem_get_long_ptr;

extern REALADR_FUNC mem_get_real_addr_ptr;

/* Functions */

int hw_mem_init(void);

// defs similar to UAE's memory.h (interface)
extern uint8_t  hw_get_byte_noexcept(uint32_t addr);
extern uint8_t  hw_get_byte(uint32_t addr);
extern uint16_t hw_get_word(uint32_t addr);
extern uint32_t hw_get_long(uint32_t addr);

extern uint8_t* hw_get_real_address(uint32_t addr);

/* Useful macros for memory access */

#define IN_BOUNDS(a,v,b)	(((v) >= (a)) && ((v) <= (b)))
#define IN_RANGE(v,b,r)		(((v) >= (b)) && ((v) <= ((b) + ((r)-1))))

#define get_b(ptr,adr,mask)	(ptr[(adr) & (mask)])
#define get_w(ptr,adr,mask)	((uint16_t) ((get_b(ptr,adr,mask) <<  8) | get_b(ptr,(adr)+1,mask)))
#define get_l(ptr,adr,mask)	((uint32_t)	((get_w(ptr,adr,mask) << 16) | get_w(ptr,(adr)+2,mask)))

#define get_p(ptr,adr,mask)  ((ptr) + ((adr) & (mask)))

#endif
