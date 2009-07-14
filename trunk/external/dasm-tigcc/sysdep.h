/* Random host-dependent support code.
   Copyright 1995, 1997, 2000 Free Software Foundation, Inc.
   Written by Ken Raeburn.
   Copyright 2007 Kevin Kofler

   This file is part of libopcodes, the opcodes library.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
   MA 02110-1301, USA.  */

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define PTR void *

#ifdef __GNUC__
#define ATTRIBUTE_FPTR_PRINTF_2 __attribute__((__format__(__printf__,2,3)))
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#else
#define ATTRIBUTE_FPTR_PRINTF_2 /**/
#define ATTRIBUTE_UNUSED /**/
#endif

extern void *xmalloc(size_t);
extern void *xcalloc(size_t, size_t);

#define ARCH_m68k
