/* Hey EMACS -*- linux-c -*- */
/* $Id: files9x.h 3056 2006-11-06 17:12:50Z roms $ */

/*  libtifiles - file format library, a part of the TiLP project
 *  Copyright (C) 1999-2005  Romain Liévin
 *  Copyright (C) 2007 Kevin Kofler
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

#ifndef __TIFILES_FILES9X_H__
#define __TIFILES_FILES9X_H__

#include "tifiles.h"

/* Structures */

typedef FlashContent	Ti9xFlash;

/* Functions */

// reading
int ti9x_file_read_flash(const char *filename, Ti9xFlash *content);

#endif
