/* Hey EMACS -*- linux-c -*- */
/* $Id: ti68k_def.h 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000, Thomas Corvazier, Romain Lievin
 *  Copyright (c) 2001-2002, Romain Liévin, Julien Blache
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


#ifndef __TI68K_DEFS__
#define __TI68K_DEFS__

#ifdef __cplusplus
extern "C" {
#endif

/* Constants */

#define MAXCHARS	256

#define TI92 		(1 << 0)
#define TI89 		(1 << 1)
#define TI92p	 	(1 << 2)
#define V200		(1 << 3)
#define TI89t		(1 << 4)
#define CALC_MAX    TI89t
  
#define EXTERNAL	0
#define INTERNAL 	1

#define EPROM_ROM	0
#define FLASH_ROM 	2

#define KB			(1024)
#define MB			(1024*KB)

#define HW1			1
#define HW2			2
#define HW3         3
#define HW4         4

#define LCDMEM_W	240		// LCD _memory_ height
#define LCDMEM_H	128		// LCD _memory_ height

#ifdef __cplusplus
}
#endif

#endif
