#ifndef __LIMITS
#define __LIMITS

#include <default.h>

#ifdef __CHAR_UNSIGNED__
#define CHAR_MAX 255
#define CHAR_MIN 0
#else
#define CHAR_MAX 127
#define CHAR_MIN (-128)
#endif

#ifdef __INT_SHORT__
#define INT_MAX 0x7FFF
#define INT_MIN ((int)0x8000)
#define UINT_MAX 0xFFFFU
#else
#define INT_MAX 0x7FFFFFFFL
#define INT_MIN ((int)0x80000000L)
#define UINT_MAX 0xFFFFFFFFUL
#endif

/* Begin Auto-Generated Part */
#define CHAR_BIT 8
#define LONG_MAX 0x7FFFFFFFL
#define LONG_MIN ((long)0x80000000L)
#define SCHAR_MAX 127
#define SCHAR_MIN (-128)
#define SHRT_MAX 0x7FFF
#define SHRT_MIN ((short)0x8000)
#define UCHAR_MAX 255
#define ULONG_MAX 0xFFFFFFFFUL
#define USHRT_MAX 0xFFFFU
/* End Auto-Generated Part */

#endif
