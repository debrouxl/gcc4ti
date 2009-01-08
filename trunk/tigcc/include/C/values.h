#ifndef __VALUES
#define __VALUES

#include <default.h>

#ifdef __INT_SHORT__
#define MAXINT 0x7FFF
#define HIBITI 0x8000
#else
#define MAXINT 0x7FFFFFFFL
#define HIBITI 0x80000000L
#endif

/* Begin Auto-Generated Part */
#define _DEXPLEN 15
#define _EXPBASE 10
#define _FEXPLEN 15
#define _IEEE 0
#define BITSPERBYTE 8
#define DMAXEXP 999
#define DMAXPOWTWO 3321
#define DMINEXP (-999)
#define DSIGNIF 64
#define FMAXEXP 999
#define FMAXPOWTWO 3321
#define FMINEXP (-999)
#define FSIGNIF 56
#define HIBITL 0x80000000L
#define HIBITS 0x8000
#define LN_MAXDOUBLE (2303.58509299)
#define LN_MINDOUBLE (-2300.2825079)
#define MAXDOUBLE (9.999999999999999e999)
#define MAXFLOAT (9.999999999999999e999)
#define MAXLONG 0x7FFFFFFFL
#define MAXSHORT 0x7FFF
#define MINDOUBLE (1e-999)
#define MINFLOAT (1e-999)
#ifndef __HAVE_bcd
#define __HAVE_bcd
typedef struct{unsigned short exponent;unsigned long long mantissa;}bcd __attribute__((__may_alias__));
#endif
#ifndef __HAVE_ti_float
#define __HAVE_ti_float
typedef float ti_float;
#endif
/* End Auto-Generated Part */

#endif
