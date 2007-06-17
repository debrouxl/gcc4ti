/* pucrunch.h: Pucrunch compression code

   Pucrunch 1997-2005 by Pasi 'Albert' Ojala, a1bert@iki.fi
   Copyright (C) 2000 Thomas Nussbaumer
   Copyright (C) 2007 Kevin Kofler

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

#if defined(TARGET_EMBEDDED) || defined(TARGET_DLL)
#error Pucrunch compression is only supported for the standalone target.
#endif

#include "exp_def.h"

#define F_VERBOSE    (1<<0)
#define F_STATS      (1<<1)

int TTPack(int flags, int in_len, unsigned char *in_data, EXP_FILE *out_file);
