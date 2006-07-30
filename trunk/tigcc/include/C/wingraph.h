#ifndef __WINGRAPH
#define __WINGRAPH

#include <default.h>

/* Begin Auto-Generated Part */
#define BITMAP_HDR_SIZE 4
#define NULL ((void*)0)
#ifndef __HAVE_Attrs
#define __HAVE_Attrs
enum Attrs{A_REVERSE,A_NORMAL,A_XOR,A_SHADED,A_REPLACE,A_OR,A_AND,A_THICK1,A_SHADE_V,A_SHADE_H,A_SHADE_NS,A_SHADE_PS};
#endif
#ifndef __HAVE_BITMAP
#define __HAVE_BITMAP
typedef struct{unsigned short NumRows,NumCols;unsigned char Data[];}BITMAP;
#endif
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_BoxAttrs
#define __HAVE_BoxAttrs
enum BoxAttrs{B_NORMAL=0x10,B_ROUNDED=0x20,B_DOUBLE=0x40,B_CUT=0x80};
#endif
#ifndef __HAVE_Buttons
#define __HAVE_Buttons
enum Buttons{BT_NONE=0,BT_OK=1,BT_SAVE=2,BT_YES=3,BT_CANCEL=4,BT_NO=5,BT_GOTO=6};
#endif
#ifndef __HAVE_Fonts
#define __HAVE_Fonts
enum Fonts{F_4x6,F_6x8,F_8x10};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_ICON
#define __HAVE_ICON
typedef struct{unsigned short i[16];}ICON;
#endif
#ifndef __HAVE_pICON
#define __HAVE_pICON
typedef unsigned short*pICON;
#endif
#ifndef __HAVE_SCR_COORDS
#define __HAVE_SCR_COORDS
typedef unsigned char SCR_COORDS;
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_WIN_COORDS
#define __HAVE_WIN_COORDS
typedef short WIN_COORDS;
#endif
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
#ifndef __HAVE_WINDOW_AMS1
#define __HAVE_WINDOW_AMS1
typedef struct WindowStruct_AMS1{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;}WINDOW_AMS1;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_WinFlags
#define __HAVE_WinFlags
enum WinFlags{WF_SYS_ALLOC=0x0001,WF_STEAL_MEM=0x0002,WF_DONT_REALLOC=0x0004,WF_ROUNDEDBORDER=0x0008,WF_SAVE_SCR=0x0010,WF_DUP_SCR=0x0020,WF_TTY=0x0040,WF_ACTIVE=0x0080,WF_NOBORDER=0x0100,WF_NOBOLD=0x0200,WF_DUP_ON=0x0400,WF_VIRTUAL=0x0800,WF_TITLE=0x1000,WF_DIRTY=0x2000,WF_TRY_SAVE_SCR=0x4010,WF_VISIBLE=0x8000};
#endif
#define DeskTop ((WINDOW*const)(((long*)(_rom_call_addr(0)))+1))
#define FirstWindow (*((WINDOW**)(_rom_call_addr(0))))
#define DrawStaticButton _rom_call(void,(WINDOW*,short,short),35)
#define DrawWinBorder _rom_call(void,(WINDOW*,SCR_RECT*),27)
#define MakeWinRect _rom_call(WIN_RECT*,(short,short,short,short),2C)
#define RectWinToScr _rom_call(SCR_RECT*,(const SCR_RECT*,const WIN_RECT*,SCR_RECT*),2A)
#define RectWinToWin _rom_call(WIN_RECT*,(const SCR_RECT*,WIN_RECT*),29)
#define WinActivate _rom_call(void,(WINDOW*),1)
#define WinAttr _rom_call(short,(WINDOW*,short),2)
#define WinBackground _rom_call(void,(WINDOW*,short),4)
#define WinBackupToScr _rom_call(void,(WINDOW*),3)
#define WinBegin _rom_call(void,(WINDOW*),5)
#define WinBitmapGet _rom_call(short,(WINDOW*,const WIN_RECT*,void*),6)
#define WinBitmapPut _rom_call(void,(WINDOW*,short,short,void*,short),7)
#define WinBitmapSize _rom_call(unsigned short,(WINDOW*,const WIN_RECT*),8)
#define WinChar _rom_call(void,(WINDOW*,char),A)
#define WinCharXY _rom_call(void,(WINDOW*,short,short,char,short),9)
#define WinClose _rom_call(void,(WINDOW*),B)
#define WinClr _rom_call(void,(WINDOW*),C)
#define WinDeactivate _rom_call(void,(WINDOW*),D)
#define WinDupStat _rom_call(short,(WINDOW*,short),E)
#define WinEllipse _rom_call(void,(WINDOW*,short,short,short,short),F)
#define WinEnd(w) ((void)0)
#define WinFill _rom_call(void,(WINDOW*,const WIN_RECT*,short),10)
#define WinFillLines2 _rom_call(void,(WINDOW*,const WIN_RECT*,const WIN_RECT*,short),11)
#define WinFillTriangle _rom_call(void,(WINDOW*,short,short,short,short,short,short,short),12)
#define WinFont _rom_call(void,(WINDOW*,short),13)
#define WinGetCursor _rom_call(void,(WINDOW*,__pshort,__pshort),14)
#define WinHeight _rom_call(short,(WINDOW*),292)
#define WinHide _rom_call(void,(WINDOW*),15)
#define WinHome _rom_call(void,(WINDOW*),16)
#define WinLine _rom_call(void,(WINDOW*,const WIN_RECT*),17)
#define WinLineNC _rom_call(void,(WINDOW*,const WIN_RECT*),18)
#define WinLineRel _rom_call(void,(WINDOW*,short,short),1A)
#define WinLineTo _rom_call(void,(WINDOW*,short,short),19)
#define WinMoveCursor _rom_call(void,(WINDOW*,short,short),1B)
#define WinMoveRel _rom_call(void,(WINDOW*,short,short),1D)
#define WinMoveTo _rom_call(void,(WINDOW*,short,short),1C)
#define WinOpen _rom_call(short,(WINDOW*,const WIN_RECT*,short,...),1E)
#define WinPixGet _rom_call(short,(WINDOW*,short,short),1F)
#define WinPixSet _rom_call(void,(WINDOW*,short,short),20)
#define WinRect _rom_call(void,(WINDOW*,const WIN_RECT*,short),21)
#define WinRemove _rom_call_hack(void,(WINDOW*,short),414,(((((unsigned char*)_rom_call_addr(B)+14))+*((signed short*)_rom_call_addr(B)+7))),200)
#define WinReOpen _rom_call(short,(WINDOW*,const WIN_RECT*,short,...),22)
#define WinScrollH _rom_call(void,(WINDOW*,const WIN_RECT*,short),23)
#define WinScrollV _rom_call(void,(WINDOW*,const WIN_RECT*,short),24)
#define WinSetCursor(w,x,y) (void)((w)->CursorX=x,(w)->CursorY=y)
#define WinShow(w) (void)((w)->Flags|=WF_VISIBLE)
#define WinStr _rom_call(void,(WINDOW*,const char*),25)
#define WinStrXY _rom_call(void,(WINDOW*,short,short,const char*),26)
#define WinWidth _rom_call(short,(WINDOW*),291)
#if MIN_AMS>=200
#define BitmapSizeExt _rom_call(unsigned short,(const WIN_RECT*),3ED)
#define RectWinToScrExt _rom_call(SCR_RECT*,(const SCR_RECT*,const WIN_RECT*,SCR_RECT*),415)
#define SetWinClip _rom_call(void,(WINDOW*,SCR_RECT*),42E)
#define WinBeginPaint _rom_call(void,(WINDOW*),500)
#define WinBitmapSizeExt _rom_call(unsigned short,(WINDOW*,const WIN_RECT*),411)
#define WinEndPaint _rom_call(void,(WINDOW*),501)
#define WinLineExt _rom_call(void,(WINDOW*,const WIN_RECT*),47C)
#define WinToScr _rom_call(SCR_RECT*,(const WIN_RECT*,SCR_RECT*),42F)
#if MIN_AMS>=204
enum winWriteFlags{WWF_DRAW=1,WWF_WRAP_ON_COMMAS=2
#if MIN_AMS>=207
,WWF_WRAP_BACK_TO_ZERO=4
#endif
};
#define WinStrXYWrap _rom_call(short,(WINDOW*,WIN_COORDS,WIN_COORDS,const char*,short),5DB)
#endif
#endif
/* End Auto-Generated Part */

#endif
