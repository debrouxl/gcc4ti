/* tiosupgd.h: Definitions for TIOS upgrade files

   Copyright (C) 2004 Sebastian Reichelt

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#ifndef TIOS_UPGRADE_H
#define TIOS_UPGRADE_H

// *** Constants ***

#define MAX_TIOS_UPGRADE_FILE_SIZE_OLD 0x1EDF72
#define MAX_TIOS_UPGRADE_FILE_SIZE_NEW (MAX_TIOS_UPGRADE_FILE_SIZE_OLD + 0x200000)

// *** File Mapping Definitions ***

typedef struct ATTRIBUTE_PACKED {
	TI4 CheckSum;         // Encrypted checksum.
	TI2 SignatureHeader;  // 02 0D
	TI1 SignatureType;    // 40
	TI1 Signature[64];    // Signature, encrypted using TI's private key.
} TIOS_UPGRADE_CALC_FOOTER;

#endif
