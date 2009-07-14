/* Hey EMACS -*- linux-c -*- */
/* $Id: macros.h 2450 2006-06-07 20:03:00Z roms $ */

/*  libtifiles - Ti File Format library, a part of the TiLP project
 *  Copyright (C) 1999-2006  Romain Liévin
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef __TIEMU_MACROS__
#define __TIEMU_MACROS__

#include <stdint.h>

// extract a word from a longword
# define LSW(l) (uint16_t) ((l) & 0x0000FFFF)
# define MSW(l) (uint16_t)(((l) & 0xFFFF0000) >> 16)

// extract a byte from a word
# define LSB(w) (uint8_t) ((w) & 0x00FF)
# define MSB(w) (uint8_t)(((w) & 0xFF00) >> 8)

// extract a nibble from a byte
# define LSN(b)  ((b) & 0x0F)
# define MSN(b) (((b) & 0xF0) >> 4)

// convert 2 nibbles into a BCD byte
# define BCD(b) (10*MSN(b)+LSN(b))

#endif
