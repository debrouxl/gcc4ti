#ifndef __MEM
#define __MEM

#include <default.h>

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#define _memset _rom_call(void*,(void*,short,long),27B)
#define memchr _rom_call(void*,(const void*,short,long),273)
#define memcmp _rom_call(short,(const void*,const void*,long),270)
#define memcpy _rom_call(void*,(void*,const void*,long),26A)
#define memmove _rom_call(void*,(void*,const void*,long),26B)
#define memset _rom_call(void*,(void*,short,long),27C)
#if MIN_AMS>=200
#define memucmp _rom_call(short,(const void*,const void*,long),3CC)
#endif
/* End Auto-Generated Part */

#endif
