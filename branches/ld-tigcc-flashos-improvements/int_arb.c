/* int_arb.c: Functions to read/write arbitrary-size integers

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

IMAX ReadTI (const void *Data, SIZE Size)
{
#ifdef TARGET_BIG_ENDIAN
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
		return (ReadTI##TempSize (*((const TI##TempSize *) (((const I1 *) Data) + Size - TempSize)))); \
	else
#else /* !TARGET_BIG_ENDIAN */
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
		return (ReadTI##TempSize (*((const TI##TempSize *) Data))); \
	else
#endif /* !TARGET_BIG_ENDIAN */
	
#include "int_def.inc"
		return 0;
	
#undef DEFINE_INT_SIZE
}

SIMAX ReadSTI (const void *Data, SIZE Size)
{
#ifdef TARGET_BIG_ENDIAN
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
		return ((SI##TempSize) (ReadTI##TempSize (*((const TI##TempSize *) (((const I1 *) Data) + Size - TempSize))))); \
	else
#else /* !TARGET_BIG_ENDIAN */
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
		return ((SI##TempSize) (ReadTI##TempSize (*((const TI##TempSize *) Data)))); \
	else
#endif /* !TARGET_BIG_ENDIAN */
	
#include "int_def.inc"
		return 0;

#undef DEFINE_INT_SIZE
}

BOOLEAN WriteTI (void *Data, SIZE Size, SIMAX Value, BOOLEAN AllowSigned, BOOLEAN AllowUnsigned)
{
#ifdef TARGET_BIG_ENDIAN
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
	{ \
		if ((AllowSigned && (Value == ((SI##TempSize) Value))) || (AllowUnsigned && (((IMAX) Value) == ((I##TempSize) Value)))) \
		{ \
			WriteTI##TempSize (*((TI##TempSize *) (((I1 *) Data) + Size - TempSize)), Value); \
			return TRUE; \
		} \
	} \
	else
#else /* !TARGET_BIG_ENDIAN */
#define DEFINE_INT_SIZE(TempSize) \
	if (Size >= TempSize) \
	{ \
		if ((AllowSigned && (Value == ((SI##TempSize) Value))) || (AllowUnsigned && (((IMAX) Value) == ((I##TempSize) Value)))) \
		{ \
			WriteTI##TempSize (*((TI##TempSize *) Data), Value); \
			return TRUE; \
		} \
	} \
	else
#endif /* !TARGET_BIG_ENDIAN */
	
#include "int_def.inc"
		return FALSE;
	return FALSE;
	
#undef DEFINE_INT_SIZE
}

BOOLEAN AddTI (void *Data, SIZE Size, SIMAX Value, BOOLEAN AllowSigned, BOOLEAN AllowUnsigned)
{
	return WriteTI (Data, Size, Value + ReadSTI (Data, Size), AllowSigned, AllowUnsigned);
}
