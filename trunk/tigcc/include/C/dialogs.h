#ifndef __DIALOGS
#define __DIALOGS

#include <default.h>

#define DialogAddScroll(h,x,y,w,g,f,t,p,u,m) DialogAdd(h,0,x,y,3,(short)w,(short)g,(short)f,(short)t,(short)p,(short)u,(short)m)

/* Begin Auto-Generated Part */
#define CENTER (-1)
#define H_NULL 0
#define NULL ((void*)0)
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
#ifndef __HAVE_HSym
#define __HAVE_HSym
typedef struct{HANDLE folder;unsigned short offset;}HSym;
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_Buttons
#define __HAVE_Buttons
enum Buttons{BT_NONE,BT_OK,BT_SAVE,BT_YES,BT_CANCEL,BT_NO,BT_GOTO};
#endif
typedef CALLBACK short(*DialogNew_t)(short x,long y);
#define Dialog ({__need_in_use_bit;_rom_call(short,(void*,short,short,char*,__pshort),30);})
#define DialogAdd _rom_call(HANDLE,(HANDLE,short,short,short,short,...),33)
#define DialogAddPulldown(h,x,y,t,p,i) DialogAdd(h,2,x,y,14,(const char*)(t),(short)(p),(short)(i))
#define DialogAddRequest(h,x,y,t,o,m,w) DialogAdd(h,0,x,y,2,(const char*)(t),(short)(o),(short)(m),(short)(w))
#define DialogAddText(h,x,y,t) DialogAdd(h,0,x,y,7,(const char*)(t))
#define DialogAddTitle(h,t,l,r) DialogAdd(h,0,0,0,8,(const char*)(t),(short)(l),(short)(r))
#define DialogDo ({__need_in_use_bit;_rom_call(short,(HANDLE,short,short,char*,__pshort),32);})
#define DialogNew _rom_call(HANDLE,(short,short,DialogNew_t),34)
#define DialogNewSimple(w,h) DialogNew(w,h,NoCallBack)
#define DlgMessage ({__need_in_use_bit;_rom_call(short,(const char*,const char*,short,short),1B4);})
extern short NoCallBack(short,long)__ATTR_TIOS_CALLBACK__;
#define VarNew ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,...),28E);})
#define VarOpen ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,...),28C);})
#define VarSaveAs ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,const char*,...),28D);})
/* End Auto-Generated Part */

#endif
