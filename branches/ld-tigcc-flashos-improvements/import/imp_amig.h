/* imp_amig.h: Routines to import an AmigaOS-hunks file

   Copyright (C) 2002-2003 Kevin Kofler

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

#ifndef IMP_AMIG_H
#define IMP_AMIG_H

#include "../generic.h"

#ifdef AMIGAOS_SUPPORT

#include "../data.h"

// Import a AmigaOS-hunks file into the internal data structures.
BOOLEAN ImportAmigaOSFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName);

#endif /* AMIGAOS_SUPPORT */

#endif
