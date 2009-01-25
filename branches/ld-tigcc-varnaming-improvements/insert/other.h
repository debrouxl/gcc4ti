/* other.h: Routines to handle automatic insertion of section contents

   Copyright (C) 2003 Sebastian Reichelt, Kevin Kofler

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

#ifndef INSERT_OTHER_H
#define INSERT_OTHER_H

#include "../generic.h"
#include "ins_def.h"
#include "../data.h"

// Insert exported symbols in the _nostub comment format.
BOOLEAN InsertNostubComments (SECTION *Section);

#ifdef DATA_VAR_SUPPORT
// Insert the name of the data variable specified for the program.
BOOLEAN InsertDataVarName (SECTION *Section);
#endif /* DATA_VAR_SUPPORT */

#endif
