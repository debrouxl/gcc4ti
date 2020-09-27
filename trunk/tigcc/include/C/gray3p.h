#ifndef __GRAY3P
#define __GRAY3P

#include <default.h>

/*==============================================================================*/
/*                                                                              */
/* GrayScale-Support v3.57-3P for GCC4TI                                        */
/*                                                                              */
/* compatible with HW1/HW2/HW3/HW4 on all AMS versions up to 3.10               */
/*                                                                              */
/*                                                                              */
/* original implementation:          Julien Muchembled (as part of UniversalOS) */
/* NOSTUB and OpenSource version:    thomas.nussbaumer@gmx.net (TICT)           */
/* 3 plane (7/8 grayscales) version: Kevin@tigcc.ticalc.org ()                  */
/*                                                                              */
/*==============================================================================*/

/*--------------------------------------------------------------------------*/
/* INTERNAL SECTION                                                         */
/*                                                                          */
/* DON'T USE THE INTERNAL VARIABLES DIRECTLY - INSTEAD USE THE MACROS BELOW */
/*--------------------------------------------------------------------------*/

extern void*                   __gray3P_L_plane;          // pointer to light  plane of standard buffer
extern void*                   __gray3P_M_plane;          // pointer to medium plane of standard buffer
extern void*                   __gray3P_D_plane;          // pointer to dark   plane of standard buffer
extern void*                   __gray3P_L_plane2;         // pointer to light  plane of dbuffer
extern void*                   __gray3P_M_plane2;         // pointer to medium plane of dbuffer
extern void*                   __gray3P_D_plane2;         // pointer to dark   plane of dbuffer
extern unsigned short          __gray3P_handle;
extern short                   __gray3P_hw_type;
extern unsigned short          __gray3P_dbl_offset;       // offset to active grayscale doublebuffer (only values 0 and 12 are allowed)
extern volatile unsigned long  __gray3P_switch_cnt;
extern volatile void          *__gray3P_old_int1_handler; // old INT1 handler
extern const char              __gray3P_version[];        // just for internal maintainance

/*--------------------------------------------------------------------------*/
/* PUBLIC SECTION                                                           */
/*--------------------------------------------------------------------------*/

/* Begin not yet Auto-Generated Part */
#define GRAY3PDBUFFER_SIZE 11528
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
enum Gray3PPlanes{LIGHT_PLANE=0,DARK_PLANE=1};
#ifndef __HAVE_INT_HANDLER
#define __HAVE_INT_HANDLER
typedef struct __attribute__((__may_alias__)){short foo;}_DEREF_INT_HANDLER,*INT_HANDLER;
#endif
#define Gray3PAdjust(x) ((void)(*(volatile unsigned char*)0x600013=128-(signed char)(x)))
#define Gray3PCheckRunning() (!!__gray3P_handle)
// No deprecated alias for Gray3PCheckRunning
#define Gray3PDBufCleanup() ((void)(__gray3P_dbl_offset=0,__gray3P_D_plane2=__gray3P_D_plane,__gray3P_M_plane2=__gray3P_M_plane,__gray3P_L_plane2=__gray3P_L_plane))
#define Gray3PDBufGetActiveIdx() (!!__gray3P_dbl_offset)
#define Gray3PDBufGetActivePlane(x) (Gray3PDBufGetPlane(Gray3PDBufGetActiveIdx(),x))
#define Gray3PDBufGetHiddenIdx() (!__gray3P_dbl_offset)
#define Gray3PDBufGetHiddenPlane(x) (Gray3PDBufGetPlane(Gray3PDBufGetHiddenIdx(),x))
#define Gray3PDBufGetPlane(i,x) ((i)?(((void **)(&__gray3P_L_plane2))[(x)]):Gray3PGetPlane(x))
#define Gray3PDBufInit(p) ({char*aptr=(char*)((((long)p)+7)&0xfffffff8L);__gray3P_dbl_offset=0;__gray3P_D_plane2=aptr;__gray3P_M_plane2=aptr+3840;(void)(__gray3P_L_plane2=aptr+7680);})
#define Gray3PDBufSetActiveAMSPlane(x) Gray3PDBufSetAMSPlane(Gray3PDBufGetActiveIdx(),x)
#define Gray3PDBufSetActiveIdx(i) ((void)(__gray3P_dbl_offset=((i)?12:0)))
#define Gray3PDBufSetActiveIdxSync(i) ((void)(Gray3PWaitNSwitches(1),Gray3PDBufSetActiveIdx(i)))
#define Gray3PDBufSetAMSPlane(i,x) (_rom_call(void,(void*,long),1A2)(Gray3PDBufGetPlane(i,x),0xEF007F))
#define Gray3PDBufSetHiddenAMSPlane(x) Gray3PDBufSetAMSPlane(Gray3PDBufGetHiddenIdx(),x)
#define Gray3PDBufToggle() ((void)(__gray3P_dbl_offset=(__gray3P_dbl_offset?0:12)))
#define Gray3PDBufToggleSync() ({short __ishw2=_Gray3PIsRealHW2();if(__ishw2)Gray3PWaitNSwitches(1);Gray3PDBufToggle();if(!__ishw2)Gray3PWaitNSwitches(1);})
#define Gray3PGetInt1Handler() ((INT_HANDLER)(__gray3P_old_int1_handler))
// No deprecated alias for GrayGetInt1Handler
#define Gray3PGetPlane(x) (((void **)(&__gray3P_L_plane))[(x)])
// No deprecated alias for Gray3PGetPlane
#define Gray3PGetSwitchCount() (__gray3P_switch_cnt)
// No deprecated alias for Gray3PGetSwitchCount
#define Gray3PGetVersionString() ((const char*)__gray3P_version)
// No deprecated Gray3PMode
extern void Gray3POff(void)__ATTR_LIB_ASM__;
extern short Gray3POn(register short gray8 asm("d0"))__ATTR_LIB_ASM__;
extern void Gray3POnThrow(register short gray8 asm("d0"))__ATTR_LIB_ASM__;
#define Gray3PSetAMSPlane(x) (_rom_call(void,(void*,long),1A2)(Gray3PGetPlane(x),0xEF007F))
// No deprecated alias for Gray3PSetAMSPlane
#define Gray3PSetInt1Handler(p) ((void)((INT_HANDLER)__gray3P_old_int1_handler=(p)))
// No deprecated alias for Gray3PSetInt1Handler
#define Gray3PSetSwitchCount(val) (__gray3P_switch_cnt=(val))
// No deprecated alias for Gray3PSetSwitchCount
#define Gray3PWaitNSwitches(n) ({unsigned long __w=__gray3P_switch_cnt+(n);while(__gray3P_switch_cnt<__w);})
/* End not yet Auto-Generated Part */

#define _Gray3PIsRealHW2() (__gray3P_hw_type)

#endif
