/* integers.c: Definitions for integers with arbitrary endianness

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

#include "integers.h"

// See integers.h for a description of these functions.

#define DEFINE_INT_SIZE(Size) \
I##Size ReadTI##Size (const TI##Size I) \
{ \
	return (ReadI##Size (I)); \
} \
I##Size ReadHI##Size (const HI##Size I) \
{ \
	return (ReadI##Size (I)); \
} \
void WriteTI##Size##_ (TI##Size *I, I##Size V) \
{ \
	WriteI##Size (*I, V); \
} \
void WriteHI##Size##_ (HI##Size *I, I##Size V) \
{ \
	WriteI##Size (*I, V); \
}
#include "int_def.inc"
#undef DEFINE_INT_SIZE
