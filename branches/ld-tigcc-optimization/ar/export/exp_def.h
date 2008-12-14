/* export.h: Definitions for file exports

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

#ifndef EXPORT_H
#define EXPORT_H

#include "../../generic.h"
#include "../../integers.h"

#include <stdio.h>

#define EXP_FILE FILE
#define ExportWrite(F,P,L,C) (fwrite (P, L, C, F))
#define ExportSeek(F,P) (fseek (F, P, SEEK_SET))
#define ExportFill(F,P,C) ({ I1 __C = (C); while (ftell (F) < (P)) fwrite (&__C, 1, 1, F); })
#define ExportWriteI4(F,D) ({ TI4 __D; WriteI4 (__D, D); ExportWrite (F, &__D, 4, 1); })

#endif
