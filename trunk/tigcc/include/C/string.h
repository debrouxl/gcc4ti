#ifndef __STRING
#define __STRING

#include <default.h>

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#define _memset _rom_call(void*,(void*,short,long),27B)
#define cmpstri _rom_call(short,(const unsigned char*,const unsigned char*),16F)
#define memchr _rom_call(void*,(const void*,short,long),273)
#define memcmp _rom_call(short,(const void*,const void*,long),270)
#define memcpy _rom_call(void*,(void*,const void*,long),26A)
#define memmove _rom_call(void*,(void*,const void*,long),26B)
#define memset _rom_call(void*,(void*,short,long),27C)
#define sprintf _rom_call_attr(short,(char*,const char*,...),__attribute__((__format__(__printf__,2,3))),53)
#define strcat _rom_call(char*,(char*,const char*),26E)
#define strchr _rom_call(char*,(const char*,short),274)
#define strcmp _rom_call(short,(const unsigned char*,const unsigned char*),271)
#define strcpy _rom_call(char*,(char*,const char*),26C)
#define strcspn _rom_call(unsigned long,(const char*,const char*),275)
#define strerror _rom_call(char*,(short),27D)
#define strlen _rom_call(unsigned long,(const char*),27E)
#define strncat _rom_call(char*,(char*,const char*,long),26F)
#define strncmp _rom_call(short,(const unsigned char*,const unsigned char*,long),272)
#define strncpy _rom_call(char*,(char*,const char*,long),26D)
#define strpbrk _rom_call(char*,(const char*,const char*),276)
#define strrchr _rom_call(char*,(const char*,short),277)
#define strspn _rom_call(unsigned long,(const char*,const char*),278)
#define strstr _rom_call(char*,(const char*,const char*),279)
#define strtok _rom_call(char*,(char*,const char*),27A)
#if MIN_AMS>=200
#define memucmp _rom_call(short,(const void*,const void*,long),3CC)
#define stricmp _rom_call(short,(const unsigned char*,const unsigned char*),407)
#endif
/* End Auto-Generated Part */

#endif
