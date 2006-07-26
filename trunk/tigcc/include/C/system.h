#ifndef __SYSTEM
#define __SYSTEM

#include <default.h>

/* Begin Auto-Generated Part */
#define KB_AUTOREPEAT (1<<11)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
typedef struct{unsigned short Head;unsigned short Tail;unsigned short Size;unsigned short Used;unsigned short Buffer[];}DEF_QUEUE;
#define QUEUE(n) struct{unsigned short Head,Tail,Size,Used,Buffer[n/2];}
typedef CALLBACK void(*Timer_Callback_t)(void);
enum Timers{USER1_TIMER=1,BATT_TIMER=1,APD_TIMER=2,LIO_TIMER=3,CURSOR_TIMER=4,MISC_TIMER=5,USER_TIMER=6
#if MIN_AMS>=204
,BATTERY_TIMER=7
#endif
#if MIN_AMS>=207
,BP_TIMER=8
#endif
};
#define CTypeTable ((MIN_AMS >= 200)?((const unsigned char *)_rom_call_addr(442)):({unsigned char* __CTypeTable; asm volatile ("moveq #8,%%d0; trap #9; move.l %%a0,%0" : "=a"(__CTypeTable) : : "d0","a0"); (const unsigned char *)__CTypeTable; }))
#define OSContrastValue *OSContrastAddress
#define ReleaseDate ((const char*const)(_rom_call_addr_hack(43F,(((MIN_AMS>=101)||(TIOS_entries>0x2AC))?(((const char*const)_rom_call_addr(393))-11):((const char*const)"05/21/1998")),200)))
#define ReleaseVersion ((const char*const)(_rom_call_addr_hack(440,((((MIN_AMS>=101)||(TIOS_entries>0x2AC))?(((const char*const)_rom_call_addr(393))-16):((const char*const)"1.00"))),200)))
#define AB_prodid _rom_call(void,(char*),29D)
#define AB_prodname _rom_call(void,(char*),29E)
#define AB_serno _rom_call(short,(char*),29F)
#define ASM_call(x) ({asm volatile("movem.l %d0-%d7/%a0-%a6,-(%sp)");ASM_fastcall(x);asm volatile("movem.l (%sp)+,%d0-%d7/%a0-%a6");})
#define ASM_fastcall(x) ({__need_in_use_bit;(((void(*)())(x))());})
#define CB_fetchTEXT _rom_call(short,(HANDLE*,__pulong),C2)
#define CB_replaceTEXT _rom_call(short,(char*,long,short),C1)
#define CU_restore _rom_call(void,(short),C3)
#define CU_start _rom_call(short,(void),C4)
#define CU_stop _rom_call(short,(void),C5)
extern void enter_ghost_space(void)__ATTR_LIB_ASM__ __attribute__((deprecated));
#define EX_patch _rom_call(void,(void*,void*),15A)
#define HelpKeys ({__need_in_use_bit;_rom_call(void,(void),181);})
#define idle _rom_call(void,(void),29B)
extern void *kbd_queue(void)__ATTR_LIB_ASM__;
#define NeedStack _rom_call(void,(short),A4)
#define off _rom_call(void,(void),29A)
#define OSCheckBreak _rom_call(short,(void),EC)
#define OSClearBreak _rom_call(void,(void),ED)
#define OSContrastAddress ({unsigned char* __addr; asm volatile ("moveq #4,%%d0; trap #9; move.l %%a0,%0" : "=a"(__addr):: "d0","a0"); __addr; })
#define OSContrastDn() ({_rom_call(void,(void),297) (); asm ("":::"d3","d4");})
#define OSContrastUp() ({_rom_call(void,(void),296) (); asm ("":::"d3","d4");})
#define OSDisableBreak _rom_call(void,(void),EF)
#define OSEnableBreak _rom_call(void,(void),EE)
#define OSFreeTimer _rom_call(short,(short),F1)
#define OSRegisterTimer _rom_call(short,(short,long),F0)
#define OSReset _rom_call(void,(void),294)
#define OSSetSR _rom_call(short,(short),29C)
#define OSTimerCurVal _rom_call(unsigned long,(short),F2)
#define OSTimerExpired _rom_call(short,(short),F3)
#define OSTimerRestart _rom_call(unsigned long,(short),F4)
extern short OSVFreeTimer(short asm("d0"))__ATTR_LIB_ASM__;
extern short OSVRegisterTimer(short asm("d0"),long asm("d3"),Timer_Callback_t asm("d4"))__ATTR_LIB_ASM__;
#define QModeKey _rom_call(short,(short),182)
#define QSysKey _rom_call(short,(short),183)
#define SumStoChkMem _rom_call(short,(void),295)
#define WordInList _rom_call(short,(short,__pushort),184)
#define XR_stringPtr(strno) (AMS_1xx?_rom_call(const char*,(short),293)(strno):_rom_call(const char*,(long),293)(strno))
#if MIN_AMS>=101
#define OSdequeue _rom_call(short,(__pushort,void*),3AA)
#define OSenqueue _rom_call(short,(short,void*),3A9)
#define OSqclear _rom_call(void,(void*),3AD)
#define OSqhead _rom_call(unsigned short,(__pushort,void*),3AC)
#define OSqinquire _rom_call(short,(__pushort,void*),3AB)
#if MIN_AMS>=200
#define CU_cursorState (*((signed short*)(_rom_call_addr(503))))
#define FiftyMsecTick (*((volatile unsigned long*)(_rom_call_addr(4FC))))
#define OSOnBreak (*((unsigned char*)(_rom_call_addr(46E))))
#define AB_getGateArrayVersion _rom_call(unsigned long,(void),15E)
#if MIN_AMS>=202
#define LOC_formatDate _rom_call(void,(const char*,short,short,short,char*),590)
#define LOC_getLocalDateFormat _rom_call(const char*,(void),58F)
#define LOC_localVersionDate _rom_call(char*,(char),591)
#if MIN_AMS>=204
typedef struct{unsigned short len;unsigned char releaseVersionMajor;unsigned char releaseVersionMinor;unsigned short releaseDateYear;unsigned char releaseDateMonth;unsigned char releaseDateDay;}BASECODE_PARM_BLOCK;
#define EX_getBasecodeParmBlock _rom_call(const void*,(void),5DA)
#endif
#endif
#endif
#endif
/* End Auto-Generated Part */

#endif
