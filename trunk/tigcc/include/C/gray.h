#ifndef __GRAY
#define __GRAY

#include <default.h>

/*===========================================================================*/
/*                                                                           */
/* GrayScale-Support v3.50 for TIGCC                                          */
/*                                                                           */
/* compatible with HW1/HW2 on all AMS versions up to 2.05                    */
/*                                                                           */
/*                                                                           */
/* original implementation:       Julien Muchembled (as part of UniversalOS) */
/* NOSTUB and OpenSource version: thomas.nussbaumer@gmx.net (TiCT)           */
/*                                                                           */
/*===========================================================================*/

/*--------------------------------------------------------------------------*/
/* INTERNAL SECTION                                                         */
/*                                                                          */
/* DON'T USE THE INTERNAL VARIABLES DIRECTLY - INSTEAD USE THE MACROS BELOW */
/*--------------------------------------------------------------------------*/

extern void*                   __L_plane;            // pointer to light plane of standard buffer
extern void*                   __D_plane;            // pointer to dark  plane of standard buffer
extern void*                   __L_plane2;           // pointer to light plane of dbuffer
extern void*                   __D_plane2;           // pointer to dark  plane of dbuffer
extern unsigned short          __gray_handle;
extern short                   __gray_hw_type;
extern unsigned short          __gray_dbl_offset;    // offset to active grayscale doublebuffer (only values 0 and 8 are allowed)
extern volatile unsigned long  __switch_cnt;
extern volatile void          *__gray_old_int1_hw1;  // old INT1 handler on HW1
extern volatile void          *__gray_old_int1_hw2;  // old INT1 handler on HW2
extern const char              __gray_version[];     // just for internal maintainance

/*--------------------------------------------------------------------------*/
/* PUBLIC SECTION                                                           */
/*--------------------------------------------------------------------------*/

/* Begin Auto-Generated Part */
#define GRAYDBUFFER_SIZE 7688
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_INT_HANDLER
#define __HAVE_INT_HANDLER
typedef struct __attribute__((__may_alias__)){short foo;}_DEREF_INT_HANDLER,*INT_HANDLER;
#endif
enum GrayModes{GRAY_OFF=0,GRAY_ON=1,GRAY_HW1=1,GRAY_HW2=1};
enum GrayPlanes{LIGHT_PLANE=0,DARK_PLANE=1};
#define GrayAdjust(x) ((void)(*(volatile unsigned char*)0x600013=128-(signed char)(x)))
#define GrayCheckRunning() (!!__gray_handle)
#define IsGrayMode GrayCheckRunning
#define GrayDBufCleanup() ((void)(__gray_dbl_offset=0,__D_plane2=__D_plane,__L_plane2=__L_plane))
#define GrayDBufGetActiveIdx() (!!__gray_dbl_offset)
#define GrayDBufGetActivePlane(x) (GrayDBufGetPlane(GrayDBufGetActiveIdx(),x))
#define GrayDBufGetHiddenIdx() (!__gray_dbl_offset)
#define GrayDBufGetHiddenPlane(x) (GrayDBufGetPlane(GrayDBufGetHiddenIdx(),x))
#define GrayDBufGetPlane(i,x) ((i)?((x)?__D_plane2:__L_plane2):GrayGetPlane(x))
#define GrayDBufInit(p) ({char*aptr=(char*)((((long)p)+7)&0xfffffff8L);__gray_dbl_offset=0;__D_plane2=aptr;(void)(__L_plane2=aptr+3840);})
#define GrayDBufSetActiveAMSPlane(x) GrayDBufSetAMSPlane(GrayDBufGetActiveIdx(),x)
#define GrayDBufSetActiveIdx(i) ((void)(__gray_dbl_offset=((i)?8:0)))
#define GrayDBufSetActiveIdxSync(i) ((void)(GrayWaitNSwitches(1),GrayDBufSetActiveIdx(i)))
#define GrayDBufSetAMSPlane(i,x) (_rom_call(void,(void*,long),1A2)(GrayDBufGetPlane(i,x),0xEF007F))
#define GrayDBufSetHiddenAMSPlane(x) GrayDBufSetAMSPlane(GrayDBufGetHiddenIdx(),x)
#define GrayDBufToggle() ((void)(__gray_dbl_offset=(__gray_dbl_offset?0:8)))
#define GrayDBufToggleSync() ({short __ishw2=_GrayIsRealHW2();if(__ishw2)GrayWaitNSwitches(1);GrayDBufToggle();if(!__ishw2)GrayWaitNSwitches(1);})
#define GrayGetInt1Handler() ((INT_HANDLER)(__gray_hw_type?__gray_old_int1_hw2:__gray_old_int1_hw1))
#define GetGrayInt1Handler GrayGetInt1Handler
#define GrayGetPlane(x) ((x)?__D_plane:__L_plane)
#define GetPlane GrayGetPlane
#define GrayGetSwitchCount() (__switch_cnt)
#define GetGraySwitchCount GrayGetSwitchCount
#define GrayGetVersionString() ((const char*)__gray_version)
#define GrayMode(x) ((x)?GrayOn():({GrayOff();(short)1;}))
extern void GrayOff(void)__ATTR_LIB_ASM__;
extern short GrayOn(void)__ATTR_LIB_ASM__;
extern void GrayOnThrow(void)__ATTR_LIB_ASM__;
#define GraySetAMSPlane(x) (_rom_call(void,(void*,long),1A2)(GrayGetPlane(x),0xEF007F))
#define SetPlane GraySetAMSPlane
#define GraySetInt1Handler(p) ((void)(__gray_hw_type?((INT_HANDLER)__gray_old_int1_hw2=(p)):((INT_HANDLER)__gray_old_int1_hw1=(p))))
#define SetGrayInt1Handler GraySetInt1Handler
#define GraySetSwitchCount(val) (__switch_cnt=(val))
#define SetGraySwitchCount GraySetSwitchCount
#define GrayWaitNSwitches(n) ({unsigned long __w=__switch_cnt+(n);while(__switch_cnt<__w);})
/* End Auto-Generated Part */

#define _GrayIsRealHW2() (__gray_hw_type)

#endif
