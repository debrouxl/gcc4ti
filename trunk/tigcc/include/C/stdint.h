#ifndef __STDINT
#define __STDINT

#include <default.h>

/* Begin Auto-Generated Part */
#define INT_FAST16_MAX (32767)
#define INT_FAST16_MIN (-32767-1)
#define INT_FAST32_MAX (2147483647L)
#define INT_FAST32_MIN (-2147483647L-1)
#define INT_FAST64_MAX (9223372036854775807LL)
#define INT_FAST64_MIN (-9223372036854775807LL-1)
#define INT_FAST8_MAX (32767)
#define INT_FAST8_MIN (-32767-1)
#define INT_LEAST16_MAX (32767)
#define INT_LEAST16_MIN (-32767-1)
#define INT_LEAST32_MAX (2147483647L)
#define INT_LEAST32_MIN (-2147483647L-1)
#define INT_LEAST64_MAX (9223372036854775807LL)
#define INT_LEAST64_MIN (-9223372036854775807LL-1)
#define INT_LEAST8_MAX (127)
#define INT_LEAST8_MIN (-128)
#define INT16_MAX (32767)
#define INT16_MIN (-32767-1)
#define INT32_MAX (2147483647L)
#define INT32_MIN (-2147483647L-1)
#define INT64_MAX (9223372036854775807LL)
#define INT64_MIN (-9223372036854775807LL-1)
#define INT8_MAX (127)
#define INT8_MIN (-128)
#define INTMAX_MAX (9223372036854775807LL)
#define INTMAX_MIN (-9223372036854775807LL-1)
#define INTPTR_MAX (2147483647L)
#define INTPTR_MIN (-2147483647L-1)
#define PTRDIFF_MAX (16777215L)
#define PTRDIFF_MIN (-16777215L)
#define SIZE_MAX (16777215UL)
#define UINT_FAST16_MAX (65535U)
#define UINT_FAST32_MAX (4294967295UL)
#define UINT_FAST64_MAX (18446744073709551615ULL)
#define UINT_FAST8_MAX (65535U)
#define UINT_LEAST16_MAX (65535U)
#define UINT_LEAST32_MAX (4294967295UL)
#define UINT_LEAST64_MAX (18446744073709551615ULL)
#define UINT_LEAST8_MAX (255)
#define UINT16_MAX (65535U)
#define UINT32_MAX (4294967295UL)
#define UINT64_MAX (18446744073709551615ULL)
#define UINT8_MAX (255)
#define UINTMAX_MAX (18446744073709551615ULL)
#define UINTPTR_MAX (4294967295UL)
typedef short int_fast16_t;
typedef long int_fast32_t;
typedef long long int_fast64_t;
typedef short int_fast8_t;
typedef short int int_least16_t;
typedef long int_least32_t;
typedef long long int int_least64_t;
typedef signed char int_least8_t;
typedef short int16_t;
typedef long int32_t;
typedef long long int64_t;
typedef signed char int8_t;
typedef long long intmax_t;
typedef long intptr_t;
typedef unsigned short uint_fast16_t;
typedef unsigned long uint_fast32_t;
typedef unsigned long long uint_fast64_t;
typedef unsigned short uint_fast8_t;
typedef unsigned short uint_least16_t;
typedef unsigned long uint_least32_t;
typedef unsigned long long uint_least64_t;
typedef unsigned char uint_least8_t;
typedef unsigned short uint16_t;
typedef unsigned long uint32_t;
typedef unsigned long long uint64_t;
typedef unsigned char uint8_t;
typedef unsigned long long uintmax_t;
typedef unsigned long uintptr_t;
#define INT16_C(c) c
#define INT32_C(c) c##L
#define INT64_C(c) c##LL
#define INT8_C(c) c
#define INTMAX_C(c) c##LL
#define UINT16_C(c) c##U
#define UINT32_C(c) c##UL
#define UINT64_C(c) c##ULL
#define UINT8_C(c) c
#define UINTMAX_C(c) c##ULL
/* End Auto-Generated Part */

#endif
