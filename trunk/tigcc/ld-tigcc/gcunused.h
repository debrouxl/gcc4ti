/* gcunused.h: Routines to remove unused sections

   Copyright (C) 2002-2003 Sebastian Reichelt
   Copyright (C) 2003-2005 Kevin Kofler

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

#ifndef GCUNUSED_H
#define GCUNUSED_H

#include "generic.h"
#include "data.h"

// Remove all unused sections.
void RemoveUnusedSections (PROGRAM *Program);

// The following 2 functions are currently needed only for external data
// variable support.
#ifdef DATA_VAR_SUPPORT

// Mark the section containing __main as Referenced. This is a kludge
// compensating for the fact that the startup section referencing __main has not
// been imported at that stage.
void MarkMainSection (PROGRAM *Program);

// Clear the Referenced flag of all sections, in order to be able to run a
// second RemoveUnusedSections pass at a later point.
void ResetReferencedFlags (PROGRAM *Program);

#endif /* DATA_VAR_SUPPORT */

#endif
