#ifndef __HOMESCR
#define __HOMESCR

#include <default.h>

typedef void(*__HS_pushEmptyFIFONode__type__)(short)__ATTR_TIOS__;
extern __HS_pushEmptyFIFONode__type__ __get_HS_pushEmptyFIFONode(void)__ATTR_LIB_C__;

/* Begin Auto-Generated Part */
#define H_NULL 0
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_MULTI_EXPR
#define __HAVE_MULTI_EXPR
typedef struct{unsigned short Size;ESQ Expr[];}MULTI_EXPR;
#endif
typedef struct{short ScreenLeft;long ScreenBottom;long XStart;unsigned short Width;unsigned short Height;short Top;HANDLE Expr;short TooLong;short PrettyPrint;unsigned short Exp;unsigned short Fix;}FIFO_ELEMENT;
typedef struct{FIFO_ELEMENT Entry;FIFO_ELEMENT Ans;HANDLE Prev;HANDLE Next;}FIFO_NODE;
#define HomeExecute ({__need_in_use_bit;_rom_call(void,(const char*,short),10E);})
#define HomePushEStack _rom_call(void,(void),10F)
extern void HomeStore(void)__ATTR_LIB_ASM__;
extern void HomeStorePair(HANDLE,HANDLE)__ATTR_LIB_C__;
#define HS_chopFIFO _rom_call(void,(void),23C)
#define HS_countFIFO _rom_call(unsigned short,(void),23D)
#define HS_deleteFIFONode _rom_call(HANDLE,(HANDLE),23E)
#define HS_freeAll _rom_call(void,(void),23F)
#define HS_freeFIFONode _rom_call(void,(HANDLE),240)
#define HS_getAns _rom_call(HANDLE,(short),241)
#define HS_getEntry _rom_call(HANDLE,(short),242)
#define HS_getFIFONode _rom_call(HANDLE,(short),243)
#define HS_newFIFONode _rom_call(HANDLE,(void),245)
#define HS_popEStack _rom_call(HANDLE,(void),244)
#define HS_pushEmptyFIFONode (*(__get_HS_pushEmptyFIFONode()))
#define HS_pushFIFONode _rom_call(void,(HANDLE),246)
#if MIN_AMS>=200
#define HomeAlone _rom_call(unsigned short,(void),506)
#endif
/* End Auto-Generated Part */

#endif
