#ifndef __ASSERT
#define __ASSERT

#include <default.h>

#ifndef NDEBUG

extern void __assertion_failed(const char*,const char*,short)__ATTR_LIB_ASM_NORETURN__;

/* Begin Auto-Generated Part */
#define assert(p) ((p)?(void)0:__assertion_failed(#p,__FILE__,__LINE__))
/* End Auto-Generated Part */

#else
#define assert(p) ((void)0)
#endif

#endif
