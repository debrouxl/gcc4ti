/* generic.h: Header file to be included by all other files

   Copyright (C) 2002-2004 Sebastian Reichelt

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

/*

 User-Definable Functionality
==============================

Specify these in the command line for GCC with the '-D...' switch, or define
them in this file.

ENABLE_HELP
  Enable '--help' and '--version' options.
ENABLE_STATS
  Enable the display of program statistics via '--verbose'.
ENABLE_DUMP
  Include support for dumping the contents (via '--dump...' switches in
  executable version).

TARGET_EMBEDDED
  Compile non-executable version which can be linked to a program dynamically
  or statically.
TARGET_DLL
  Compile as a DLL (dynamic link library). This includes TARGET_EMBEDDED.

COFF_SUPPORT
  Support COFF input files.
AMIGAOS_SUPPORT
  Support AmigaOS input files.

TIOS_SUPPORT
  Support the TIOS output target.
NOSTUB_DLL_SUPPORT
  Support the Nostub DLL output target.
FLASH_OS_SUPPORT
  Support the Flash OS output target.
FARGO_SUPPORT
  Support the Fargo output target.

DATA_VAR_SUPPORT
  Support external data variables.

TIOS_FILE_SUPPORT
  Support the TIOS file format if TARGET_EMBEDDED is not defined.
TIOS_UPGRADE_FILE_SUPPORT
  Support the TIOS upgrade file format if TARGET_EMBEDDED is not defined.

*/

#ifndef GENERIC_H
#define GENERIC_H

// This defines the current version of ld-tigcc and ar-tigcc.
#define PROGRAM_VERSION_STRING "1.07"
#define COPYRIGHT_NOTICE_STRING "Copyright (C) 2002-2006 Sebastian Reichelt, Kevin Kofler and Billy Charvet"

// Handling of user-defined functionality dependencies.
#ifdef TARGET_DLL
#define TARGET_EMBEDDED
#endif /* TARGET_DLL */

#define ATTRIBUTE_PACKED __attribute__((__packed__))
#define ATTRIBUTE_UNUSED __attribute__((__unused__))
#define ATTRIBUTE_INTERFACE __attribute__((__cdecl__))

// When compiling an executable, don't do anything with exported functions.
// Otherwise, default to the cdecl calling convention.
// When compiling a DLL, also mark the function as exported.
#ifdef TARGET_DLL
#define ATTRIBUTE_EXPORTED ATTRIBUTE_INTERFACE __declspec(dllexport)
#else /* !TARGET_DLL */
#ifdef TARGET_EMBEDDED
#define ATTRIBUTE_EXPORTED ATTRIBUTE_INTERFACE
#else /* !TARGET_EMBEDDED */
#define ATTRIBUTE_EXPORTED 
#endif /* !TARGET_EMBEDDED */
#endif /* !TARGET_DLL */

// This specifies how a variable-length array is specified (for example via
// '[1]', '[0]', or '[]', depending on the capabilities of the compiler).
#ifdef __GNUC__
#if __GNUC__ >= 3
#define VAR_ARRAY []
#else /* __GNUC__ < 3 */
#define VAR_ARRAY [0]
#endif /* __GNUC__ < 3 */
#else /* !__GNUC__ */
#define VAR_ARRAY [1]
#endif /* !__GNUC__ */

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

// Boolean types.
typedef I1 B1;
typedef I2 B2;
typedef I4 B4;

// These should be all right in most cases.
typedef int BOOLEAN;
typedef SI4 OFFSET;
#define MAX_OFFSET 0x7FFFFFFF
typedef OFFSET FILE_PTR;
typedef SI4 COUNT;
typedef COUNT SIZE;

// These are for setting only! Do not use them to check values.
#define FALSE 0
#define TRUE  (!FALSE)
#define NULL  ((void *) 0)

// Diagnostic messages.
#ifdef TARGET_EMBEDDED
extern void Error_Internal   (const char *FileName, const char *Text);
extern void Warning_Internal (const char *FileName, const char *Text);
#include <stdio.h>
#define Error(FileName,Text...)   ({ char s__[256*4]; sprintf (s__, Text); Error_Internal   (FileName, s__); })
#define Warning(FileName,Text...) ({ char s__[256*4]; sprintf (s__, Text); Warning_Internal (FileName, s__); })
#else /* !TARGET_EMBEDDED */
#include <stdio.h>
#define Error(FileName,Text...)   ({ register const char *filename__ = (FileName); if (filename__) fprintf (stderr, "%s: Error: ",   filename__); else fprintf (stderr, "Error: ");   fprintf (stderr, Text); fprintf (stderr, "\n"); })
#define Warning(FileName,Text...) ({ register const char *filename__ = (FileName); if (filename__) fprintf (stderr, "%s: Warning: ", filename__); else fprintf (stderr, "Warning: "); fprintf (stderr, Text); fprintf (stderr, "\n"); })
#endif /* !TARGET_EMBEDDED */

// Macro to check whether two ranges overlap. The ranges include Start1 and
// Start2, but not End1 and End2.
#define RangesOverlap(Start1,End1,Start2,End2) (((End1) > (Start2)) && ((End2) > (Start1)))

#endif
