#ifndef __ALLOC
#define __ALLOC

#include <default.h>

/* Begin Auto-Generated Part */
#define H_NULL 0
#define NULL ((void*)0)
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_alloca
#define __HAVE_alloca
void *alloca(long)__ATTR_GCC__;
#endif
extern void *calloc_throw(short asm("d0"),short asm("d1"))__ATTR_LIB_ASM__;
#ifndef __HAVE_calloc
#define __HAVE_calloc
extern void *calloc(short asm("d0"),short asm("d1"))__ATTR_LIB_ASM__;
#endif
#define free _rom_call(void,(void*),A3)
#define FreeHandles _rom_call(short,(void),23B)
#define HeapAlloc _rom_call(HANDLE,(long),90)
#define HeapAllocESTACK _rom_call(HANDLE,(long),91)
#define HeapAllocHigh _rom_call(HANDLE,(long),92)
#define HeapAllocHighThrow _rom_call(HANDLE,(long),94)
#define HeapAllocPtr _rom_call(void*,(long),A2)
extern void *HeapAllocPtrThrow(long asm("a0"))__ATTR_LIB_ASM__;
#define HeapAllocThrow _rom_call(HANDLE,(long),93)
#define HeapAvail _rom_call(unsigned long,(void),8F)
#define HeapCompress _rom_call(void,(void),95)
#define HeapDeref _rom_call(void*,(HANDLE),96)
#define HeapEnd _rom_call(void*,(void),A1)
#define HeapFree _rom_call(void,(HANDLE),97)
#define HeapFreeIndir _rom_call(void,(HANDLE*),98)
#define HeapFreePtr _rom_call(void,(void*),A3)
#define HeapGetHandle _rom_call(HANDLE,(void),239)
#define HeapGetLock _rom_call(short,(HANDLE),9B)
#define HeapLock _rom_call(HANDLE,(HANDLE),9A)
#define HeapMax _rom_call(unsigned long,(void),9C)
#define HeapMoveHigh _rom_call(HANDLE,(HANDLE),A0)
#define HeapPtrToHandle _rom_call(HANDLE,(void*),23A)
#define HeapRealloc _rom_call(HANDLE,(HANDLE,long),9D)
extern HANDLE HeapReallocThrow(HANDLE asm("d0"),long asm("a0"))__ATTR_LIB_ASM__;
#define HeapSize _rom_call(unsigned long,(HANDLE),9E)
#define HeapUnlock _rom_call(HANDLE,(HANDLE),9F)
#define HLock _rom_call(void*,(HANDLE),99)
extern void *malloc_throw(long asm("a0"))__ATTR_LIB_ASM__;
#define malloc _rom_call(void*,(long),A2)
extern void *realloc_throw(void* asm("a0"),long asm("d0"))__ATTR_LIB_ASM__;
#ifndef __HAVE_realloc
#define __HAVE_realloc
extern void *realloc(void* asm("a0"),long asm("d0"))__ATTR_LIB_ASM__;
#endif
#if MIN_AMS>=200
enum HeapWalkCmds{H_WALK_VERIFY=0,H_WALK_STATUS=1,H_WALK_DUMP=2
#if MIN_AMS>=204
,H_WALK_SYM=3
#endif
};
#define HeapShuffle _rom_call(void,(void),474)
#define HeapWalk _rom_call(short,(short),12C)
#endif
/* End Auto-Generated Part */

#endif
