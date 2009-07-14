/* Hey EMACS -*- linux-c -*- */
/* $Id: mem_size.h 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000, Thomas Corvazier, Romain Lievin
 *  Copyright (c) 2001-2002, Romain Lievin, Julien Blache
 *  Copyright (c) 2003-2004, Romain Liévin
 *  Copyright (c) 2005, Romain Liévin, Kevin Kofler
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


#ifndef __TI68K_MEMSIZE__
#define __TI68K_MEMSIZE__

/*
	Size of some common memory devices.
	0 means not supported & used by the hand-held.
*/

#define RAM_SIZE_TI92_I		(128*KB)
#define RAM_SIZE_TI92_II	(256*KB)
#define RAM_SIZE_TI89		(256*KB)
#define RAM_SIZE_TI92P		(256*KB)
#define RAM_SIZE_V200		(256*KB)
#define RAM_SIZE_TI89T		(256*KB)

#define ROM_SIZE_TI92_I		(1*MB)
#define ROM_SIZE_TI92_II	(2*MB)
#define ROM_SIZE_TI89		(2*MB)
#define ROM_SIZE_TI92P		(2*MB)
#define ROM_SIZE_V200		(4*MB)
#define ROM_SIZE_TI89T		(4*MB)

#define ROM_BASE_TI92_I		(0)	// 0x200000 or 0x400000
#define ROM_BASE_TI92_II	0x400000
#define ROM_BASE_TI89		0x200000
#define ROM_BASE_TI92P		0x400000
#define ROM_BASE_V200		0x200000
#define ROM_BASE_TI89T		0x800000

#define IO1_SIZE_TI92_I		32
#define IO1_SIZE_TI92_II	32
#define IO1_SIZE_TI89		32
#define IO1_SIZE_TI92P		32
#define IO1_SIZE_V200		32
#define IO1_SIZE_TI89T		32

#define IO2_SIZE_TI92_I		0
#define IO2_SIZE_TI92_II	0
#define IO2_SIZE_TI89		32
#define IO2_SIZE_TI92P		32
#define IO2_SIZE_V200		32
#define IO2_SIZE_TI89T		256	// seems to be 128 instead of 64 ?!

#define IO3_SIZE_TI92_I		0
#define IO3_SIZE_TI92_II	0
#define IO3_SIZE_TI89		0
#define IO3_SIZE_TI92P		0
#define IO3_SIZE_V200		0
#define IO3_SIZE_TI89T		256

#endif
