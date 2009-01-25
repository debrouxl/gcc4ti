/* imp_coff.h: Routines to import a COFF file

   Copyright (C) 2003 Sebastian Reichelt

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

#ifndef IMP_COFF_H
#define IMP_COFF_H

#include "../../generic.h"

#ifdef COFF_SUPPORT

#include "../data.h"

// Import the exported symbols of a COFF file.
BOOLEAN ArImportCOFFFile (OBJECT_FILE *ObjectFile);

#endif /* COFF_SUPPORT */

#endif
