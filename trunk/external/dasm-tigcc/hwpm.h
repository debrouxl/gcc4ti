/* Hey EMACS -*- linux-c -*- */
/* $Id: hwpm.h 2268 2006-11-06 17:18:51Z roms $ */

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

#ifndef __TI68K_HWPM__
#define __TI68K_HWPM__

#include <stdint.h>

/*
  Definitions
*/

// Hardware parameter block from TIGCC documentation
// Exists only on FLASH calculators
typedef struct {
	uint16_t  len;                 /* length of parameter block    */
    uint32_t  hardwareID;          /* 1 = TI-92 Plus, 3 = TI-89    */
    uint32_t  hardwareRevision;    /* hardware revision number     */
    uint32_t  bootMajor;           /* boot code version number     */
    uint32_t  bootRevision;        /* boot code revision number    */
    uint32_t  bootBuild;           /* boot code build number       */
    uint32_t  gateArray;           /* gate array version number    */
    uint32_t  physDisplayBitsWide; /* display width                */
    uint32_t  physDisplayBitsTall; /* display height               */
    uint32_t  LCDBitsWide;         /* visible display width        */
    uint32_t  LCDBitsTall;         /* visible display height       */
} HW_PARM_BLOCK;

// Possible values if hardwareID field
#define HWID_TI92P  1
#define HWID_TI89   3
#define HWID_V200   8
#define HWID_TI89T  9

/*
	Functions
*/

int ti68k_get_hw_param_block(uint8_t *rom_data, uint8_t rom_base, HW_PARM_BLOCK *block);
int ti68k_put_hw_param_block(uint8_t *rom_data, uint8_t rom_base, const HW_PARM_BLOCK *s);

#endif
