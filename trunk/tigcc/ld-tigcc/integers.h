/* integers.h: Definitions for integers with arbitrary endianness

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

#ifndef INTEGERS_H
#define INTEGERS_H

#include "generic.h"

#define TARGET_BIG_ENDIAN

// *** Zero Integer Definitions ***

typedef I1 ZI1;
typedef I2 ZI2;
typedef I4 ZI4;

#define IsZero(I) (!(I))

#ifdef __i386__
// This only works on targets which don't care about alignment.
#define IsZeroI1(I) (IsZero (*((ZI1 *) &(I))))
#define IsZeroI2(I) (IsZero (*((ZI2 *) &(I))))
#define IsZeroI4(I) (IsZero (*((ZI4 *) &(I))))
#else
// Use only bytewise reads for targets which do care about alignment.
#define IsZeroI1(I) (IsZero (ReadTI1 (*(TI1*)&(I))))
#define IsZeroI2(I) (IsZero (ReadTI2 (*(TI2*)&(I))))
#define IsZeroI4(I) (IsZero (ReadTI4 (*(TI4*)&(I))))
#endif

// *** Target Integer Definitions ***

// 1 Byte Target Integer
// Defined as structure to avoid unguarded reading/writing.
typedef struct ATTRIBUTE_PACKED {
	I1 Val;
} TI1;

// 2 Byte Target Integer
typedef struct ATTRIBUTE_PACKED {
#ifdef TARGET_BIG_ENDIAN
	TI1 Hi, Lo;
#else /* !TARGET_BIG_ENDIAN */
	TI1 Lo, Hi;
#endif /* !TARGET_BIG_ENDIAN */
} TI2;

// 4 Byte Target Integer
typedef struct ATTRIBUTE_PACKED {
#ifdef TARGET_BIG_ENDIAN
	TI2 Hi, Lo;
#else /* !TARGET_BIG_ENDIAN */
	TI2 Lo, Hi;
#endif /* !TARGET_BIG_ENDIAN */
} TI4;

// *** Host Integer Definitions ***

// 1 Byte Host Integer
// Defined as structure to avoid unguarded reading/writing.
typedef struct ATTRIBUTE_PACKED {
	unsigned char Val;
} HI1;

// 2 Byte Host Integer
typedef struct ATTRIBUTE_PACKED {
#ifdef HOST_BIG_ENDIAN
	HI1 Hi, Lo;
#else /* !HOST_BIG_ENDIAN */
	HI1 Lo, Hi;
#endif /* !HOST_BIG_ENDIAN */
} HI2;

// 4 Byte Host Integer
typedef struct ATTRIBUTE_PACKED {
#ifdef HOST_BIG_ENDIAN
	HI2 Hi, Lo;
#else /* !HOST_BIG_ENDIAN */
	HI2 Lo, Hi;
#endif /* !HOST_BIG_ENDIAN */
} HI4;

// *** Macros to read arbitrary integers ***

#define ReadI1(I) ((I).Val)
#define ReadI2(I) ((ReadI1 ((I).Hi) << 8)  | ReadI1 ((I).Lo))
#define ReadI4(I) ((ReadI2 ((I).Hi) << 16) | ReadI2 ((I).Lo))

// *** Functions to read integers ***

#define DEFINE_INT_SIZE(size) I##size ReadTI##size (const TI##size I); I##size ReadHI##size (const HI##size I);
#include "int_def.inc"
#undef DEFINE_INT_SIZE

// *** Macros to write arbitrary integers ***

#define WriteI1(I,V) ((I).Val = (V))
#define WriteI2(I,V) ({ register I2 __V2 = (V); WriteI1 ((I).Hi, (__V2) >> 8);  WriteI1 ((I).Lo, (__V2)); })
#define WriteI4(I,V) ({ register I4 __V4 = (V); WriteI2 ((I).Hi, (__V4) >> 16); WriteI2 ((I).Lo, (__V4)); })

// *** Functions to write integers ***

#define DEFINE_INT_SIZE(size) void WriteTI##size##_ (TI##size *I, I##size V); void WriteHI##size##_ (HI##size *I, I##size V);
#include "int_def.inc"
#undef DEFINE_INT_SIZE

#define WriteTI1(I,V) (WriteTI1_ (&(I), (V)))
#define WriteTI2(I,V) (WriteTI2_ (&(I), (V)))
#define WriteTI4(I,V) (WriteTI4_ (&(I), (V)))
#define WriteHI1(I,V) (WriteHI1_ (&(I), (V)))
#define WriteHI2(I,V) (WriteHI2_ (&(I), (V)))
#define WriteHI4(I,V) (WriteHI4_ (&(I), (V)))

// *** Functions to read arbitrary-length target integers ***

// Read an unsigned target integer of specified size from a
// data buffer.
IMAX  ReadTI  (const void *Data, SIZE Size);
// Read a signed target integer of specified size from a
// data buffer.
SIMAX ReadSTI (const void *Data, SIZE Size);

// *** Functions to write arbitrary-length target integers ***

// Write a target integer of specified size to a data buffer.
// The integer may be interpreted as a signed or unsigned
// value based on the AllowSigned and AllowUnsigned parameters.
// If the value does not fit into the contents, FALSE is
// returned.
BOOLEAN WriteTI (void *Data, SIZE Size, SIMAX Value, BOOLEAN AllowSigned, BOOLEAN AllowUnsigned);

// Add a target integer of specified size to a data buffer.
// The integer may be interpreted as a signed or unsigned
// value based on the AllowSigned and AllowUnsigned parameters.
// If the value does not fit into the contents, FALSE is
// returned.
BOOLEAN AddTI (void *Data, SIZE Size, SIMAX Value, BOOLEAN AllowSigned, BOOLEAN AllowUnsigned);

#endif
