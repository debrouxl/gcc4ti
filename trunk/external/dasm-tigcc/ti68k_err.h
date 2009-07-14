/* Hey EMACS -*- linux-c -*- */
/* $Id: ti68k_err.h 2268 2006-11-06 17:18:51Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000, Thomas Corvazier, Romain Lievin
 *  Copyright (c) 2001-2002, Romain Lievin, Julien Blache
 *  Copyright (c) 2003-2004, Romain Liévin
 *  Copyright (c) 2005-2006, Romain Liévin, Kevin Kofler
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

#ifndef __TI68K_ERRCODES__
#define __TI68K_ERRCODES__

/* Error codes ERR_...  */
/* Error codes must begin at 768 up to 1023 */

// New codes
#define ERR_NONE				0
#define ERR_CANT_OPEN			768
#define ERR_INVALID_IMAGE		770
#define ERR_INVALID_UPGRADE		771
#define ERR_NO_IMAGE			772
#define ERR_INVALID_ROM_SIZE	774
#define ERR_NOT_TI_FILE			775
#define ERR_MALLOC				776
#define ERR_CANT_OPEN_DIR		777
#define ERR_CANT_UPGRADE		778
#define	ERR_INVALID_ROM			779

#define ERR_CANT_OPEN_STATE		780
#define ERR_REVISION_MATCH		781
#define ERR_HEADER_MATCH		782
#define ERR_STATE_MATCH			783

#endif
