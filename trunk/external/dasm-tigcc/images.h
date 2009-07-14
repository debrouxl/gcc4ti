/* Hey EMACS -*- linux-c -*- */
/* $Id: images.h 2268 2006-11-06 17:18:51Z roms $ */

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

#ifndef __TI68K_IMAGES__
#define __TI68K_IMAGES__

#include <stdint.h>

/*
  Definitions
*/

#define IMG_SIGN	"TiEmu img v2.00"
#define IMG_REV		2	// increase this number when changing the structure below

// Please update the docs/TiEmu_img_format.txt documentation when making changes
// on the structure below

// dc = don't care for rom/tib
typedef struct
{
	char	signature[16];	// "TiEmu img v2.00" (dc)
    long    revision;       // structure revision (compatibility)
	long	header_size;	// size of this structure and offset to pure data (dc)

	char	calc_type;		// calculator type
	char	version[5];		// firmware revision
	char	flash;			// EPROM or FLASH
	char	has_boot;		// FLASH upgrade does not have boot
	long	size;			// size of pure data
	char	hw_type;		// hw1 or hw2
    uint8_t rom_base;       // ROM base address (MSB)

    char    fill[0x40-42];  // round up struct to 0x40 bytes
	char*	data;			// pure data (temporary use, 8 bytes)
} IMG_INFO;

/*
	Functions
*/

int ti68k_is_a_rom_file(const char *filename);
int ti68k_is_a_tib_file(const char *filename);
int ti68k_is_a_img_file(const char *filename);

int ti68k_get_rom_infos(const char *filename, IMG_INFO *rom, int preload);
int ti68k_get_tib_infos(const char *filename, IMG_INFO *tib, int preload);
int ti68k_get_img_infos(const char *filename, IMG_INFO *img);

int ti68k_convert_tib_to_image(IMG_INFO *src, unsigned char *dst);

int ti68k_load_image(const char *filename, IMG_INFO *img);

#endif
