/* import.c: Routines for object file imports

   Copyright (C) 2002-2003 Sebastian Reichelt

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

#include "import.h"

#ifdef COFF_SUPPORT
#include "../formats/coff.h"
#include "imp_coff.h"
#endif /* COFF_SUPPORT */

#ifdef AMIGAOS_SUPPORT
#include "../formats/amigaos.h"
#include "imp_amig.h"
#endif /* AMIGAOS_SUPPORT */

// Import an object file with an arbitrary format.
BOOLEAN ImportObjectFile (PROGRAM *Program, const I1 *File, SIZE FileSize, const char *FileName)
{
#ifdef COFF_SUPPORT
	if (IsCOFFFile (File, FileSize))
		return (ImportCOFFFile (Program, File, FileSize, FileName));
	else
#endif /* COFF_SUPPORT */
#ifdef AMIGAOS_SUPPORT
	if (IsAmigaOSFile (File, FileSize))
		return (ImportAmigaOSFile (Program, File, FileSize, FileName));
	else
#endif /* AMIGAOS_SUPPORT */
	{
		Error (FileName, "Unknown object file format.");
		return FALSE;
	}
}
