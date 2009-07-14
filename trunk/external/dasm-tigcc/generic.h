/* generic.h: Header file to be included by all other files

   Copyright (C) 2002-2004 Sebastian Reichelt
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

#ifndef GENERIC_H
#define GENERIC_H

#ifdef __GNUC__
#define ATTRIBUTE_PACKED __attribute__((__packed__))
#define ATTRIBUTE_MAY_ALIAS __attribute__((__may_alias__))
#else
#define ATTRIBUTE_PACKED /**/
#define ATTRIBUTE_MAY_ALIAS /**/
#endif

// Attempt to auto-detect I1, I2, I4, SI1, SI2 and SI4 based on <limits.h>.
// It is a good idea to double-check these definitions on every new system
// you compile on.
// Such a check is also performed at run time (or optimized away as dead
// code).

#include <limits.h>

// Make sure that the character types take exactly 1 byte.
#if UCHAR_MAX != 0xFF
#error Need 1-byte unsigned char type.
#endif /* UCHAR_MAX != 0xFF */
#if SCHAR_MIN != (-0x80) || SCHAR_MAX != 0x7F
#error Need 1-byte signed char type.
#endif /* UCHAR_MAX != 0xFF */

// Unsigned types.
typedef unsigned char I1;

#if USHRT_MAX == 0xFFFF
typedef unsigned short I2;
#elif UINT_MAX == 0xFFFF
typedef unsigned int I2;
#elif ULONG_MAX == 0xFFFF
typedef unsigned long I2;
#else /* no 2-byte unsigned int */
#error No 2-byte unsigned integer type found.
#endif /* 2-byte unsigned int */

#if ULONG_MAX == 0xFFFFFFFF
typedef unsigned long I4;
#elif UINT_MAX == 0xFFFFFFFF
typedef unsigned int I4;
#elif ULONG_LONG_MAX == 0xFFFFFFFF || ULLONG_MAX == 0xFFFFFFFF
typedef unsigned long long I4;
#else /* no 4-byte unsigned int */
#error No 4-byte unsigned integer type found.
#endif /* 4-byte unsigned int */

// Signed types.
typedef signed char SI1;

#if SHRT_MIN == (-0x8000) && SHRT_MAX == 0x7FFF
typedef short SI2;
#elif INT_MIN == (-0x8000) && INT_MAX == 0x7FFF
typedef int SI2;
#elif LONG_MIN == (-0x8000) && LONG_MAX == 0x7FFF
typedef long SI2;
#else /* no 2-byte signed int */
#error No 2-byte signed integer type found.
#endif /* 2-byte signed int */

#if LONG_MIN == (-0x80000000) && LONG_MAX == 0x7FFFFFFF
typedef long SI4;
#elif INT_MIN == (-0x80000000) && INT_MAX == 0x7FFFFFFF
typedef int SI4;
#elif (LONG_LONG_MIN == (-0x80000000) && LONG_LONG_MAX == 0x7FFFFFFF) \
      || (LLONG_MIN == (-0x80000000) && LLONG_MAX == 0x7FFFFFFF)
typedef long long SI4;
#else /* no 4-byte signed int */
#error No 4-byte signed integer type found.
#endif /* 4-byte signed int */

// Maximum used specific-size integers.
#define IMAX  I4
#define SIMAX SI4

// These should be all right in most cases.
typedef int BOOLEAN;
typedef SI4 COUNT;
typedef COUNT SIZE;

#endif
