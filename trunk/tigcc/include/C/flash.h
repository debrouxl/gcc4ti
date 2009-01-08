#ifndef __FLASH
#define __FLASH

#include <default.h>

#define __EM_findEmptySlot _rom_call(void*,(long,short),15F)

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
typedef struct{unsigned short len;unsigned long hardwareID;unsigned long hardwareRevision;unsigned long bootMajor;unsigned long bootRevision;unsigned long bootBuild;unsigned long gateArray;unsigned long physDisplayBitsWide;unsigned long physDisplayBitsTall;unsigned long LCDBitsWide;unsigned long LCDBitsTall;}HARDWARE_PARM_BLOCK;
#define EM_abandon _rom_call(void,(HANDLE),15B)
#define EM_blockVerifyErase _rom_call(short,(void*),15D)
#define EM_findEmptySlot(s) (__EM_findEmptySlot((s),0))
#define EM_GC _rom_call(short,(short),160)
#define EM_survey _rom_call(void,(__pulong,__pulong,__pulong,__pulong,__pulong,__pulong),165)
#define EM_write _rom_call(void,(const void*,void*,long),167)
#define FL_addCert _rom_call(unsigned short,(void*,long),169)
#define FL_download _rom_call_attr(void,(long),__attribute__((__noreturn__)),16A)
#define FL_getCert _rom_call(void,(HANDLE*,__pulong,short),16C)
#define FL_getHardwareParmBlock() (MIN_AMS<101&&TIOS_entries<0x3CC?(const void*)"\0\x6\0\0\0\x1":_rom_call(const void*,(void),16B)())
#define FL_getVerNum _rom_call(unsigned short,(void),16D)
#define FL_write _rom_call(void,(const void*,void*,long),171)
#define GetAMSSize() ((*((unsigned long*)(((char*)ROM_base)+0x12002)))+0x49)
#if MIN_AMS>=200
#define FlashMemoryEnd ((unsigned char*const)(_rom_call_addr(43D)))
#define BatTooLowFlash _rom_call(short,(short),434)
#endif
/* End Auto-Generated Part */

#endif
