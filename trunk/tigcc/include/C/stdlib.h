#ifndef __STDLIB
#define __STDLIB

#include <default.h>

#ifndef DOORS
#define ldiv(n,d) ({ldiv_t __r;long __n=(n),__d=(d);asm("move.l 0xC8,%%a5;move.l %2,%%d1;move.l %3,%%d0;move.l (%%a5,2720),%%a0;jsr (%%a0);move.l %%d1,%0;move.l %2,%%d1;move.l %3,%%d0;move.l (%%a5,2724),%%a0;jsr (%%a0);move.l %%d1,%1" : "=g"(__r.quot),"=g"(__r.rem) : "g"(__n),"g"(__d) : "a0","a1","a5","d0","d1","d2");__r;})
#else
#define ldiv(n,d) ({ldiv_t __r;long __n=(n),__d=(d);asm("move.l %2,%%d1;move.l %3,%%d0;jsr _ROM_CALL_2A8;move.l %%d1,%0;move.l %2,%%d1;move.l %3,%%d0;jsr _ROM_CALL_2A9;move.l %%d1,%1" : "=g"(__r.quot),"=g"(__r.rem) : "g"(__n),"g"(__d) : "a0","a1","d0","d1","d2");__r;})
#endif

#ifndef NO_EXIT_SUPPORT
extern void __exit(void)__ATTR_LIB_ASM_NORETURN__;
#endif

extern long __randseed;

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#define RAND_MAX 32767
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
typedef CALLBACK void(*atexit_t)(void);
typedef CALLBACK short(*compare_t)(const void*elem1,const void*elem2);
#ifndef __HAVE_div_t
#define __HAVE_div_t
typedef struct{short quot,rem;}div_t;
#endif
#ifndef __HAVE_ldiv_t
#define __HAVE_ldiv_t
typedef struct{long quot,rem;}ldiv_t;
#endif
#define abort() (_rom_call(void,(const char*),E6)("ABNORMAL PROGRAM TERMINATION"),exit(0))
#define abs(x) ({typeof(x) __x = (x); __x >= 0 ? __x : -__x;})
#ifndef __HAVE_alloca
#define __HAVE_alloca
void *alloca(long)__ATTR_GCC__;
#endif
extern short atexit(atexit_t)__ATTR_LIB_ASM__;
extern short atoi(const char*)__ATTR_LIB_C__;
extern long atol(const char*)__ATTR_LIB_C__;
extern void *bsearch(const void*,const void*,short,short,compare_t)__ATTR_LIB_C__;
#ifndef __HAVE_calloc
#define __HAVE_calloc
extern void *calloc(short asm("d0"),short asm("d1"))__ATTR_LIB_ASM__;
#endif
#define div(n,d) ({short __n=(n),__d=(d);div_t __r;__r.quot=__n/__d;__r.rem=__n%__d;__r;})
#define exit(n) ({(n)?_rom_call(void,(short),152)(n):0;__exit();})
#define fabs(x) _tios_float_1(106,x,float)
#define free _rom_call(void,(void*),A3)
#ifndef __HAVE_labs
#define __HAVE_labs
long labs(long)__ATTR_GCC__;
#endif
#define malloc _rom_call(void*,(long),A2)
#define max(a,b) ({typeof(a) __a = (a); typeof(b) __b = (b); (__a > __b) ? __a : __b;})
#define min(a,b) ({typeof(a) __a = (a); typeof(b) __b = (b); (__a < __b) ? __a : __b;})
extern void qsort(void*,short,short,compare_t)__ATTR_LIB_C__;
extern short rand(void)__ATTR_LIB_ASM__;
#define random(x) ((short)((long)(unsigned short)rand()*(unsigned short)(x)/32768))
#define randomize() srand(*(volatile unsigned char*)0x600017)
#ifndef __HAVE_realloc
#define __HAVE_realloc
extern void *realloc(void* asm("a0"),long asm("d0"))__ATTR_LIB_ASM__;
#endif
#define srand(x) (__randseed=(x))
extern long strtol(const char*,char**,short)__ATTR_LIB_C__;
extern unsigned long strtoul(const char*,char**,short)__ATTR_LIB_C__;
#if MIN_AMS>=101
#ifndef __HAVE_atof
#define __HAVE_atof
extern float atof(const char*)__ATTR_LIB_ASM__;
#endif
#endif
/* End Auto-Generated Part */

#endif
