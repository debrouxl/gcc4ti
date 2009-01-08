#ifndef __GRAPH
#define __GRAPH

#include <default.h>

/* Begin Auto-Generated Part */
#define BITMAP_HDR_SIZE 4
#define LCD_MEM ((void*)0x4C00)
#define LCD_SIZE 3840
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_WIN_COORDS
#define __HAVE_WIN_COORDS
typedef short WIN_COORDS;
#endif
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
#ifndef __HAVE_Attrs
#define __HAVE_Attrs
enum Attrs{A_REVERSE,A_NORMAL,A_XOR,A_SHADED,A_REPLACE,A_OR,A_AND,A_THICK1,A_SHADE_V,A_SHADE_H,A_SHADE_NS,A_SHADE_PS};
#endif
#ifndef __HAVE_BITMAP
#define __HAVE_BITMAP
typedef struct{unsigned short NumRows,NumCols;unsigned char Data[];}BITMAP;
#endif
#ifndef __HAVE_BoxAttrs
#define __HAVE_BoxAttrs
enum BoxAttrs{B_NORMAL=0x10,B_ROUNDED=0x20,B_DOUBLE=0x40,B_CUT=0x80};
#endif
#ifndef __HAVE_Fonts
#define __HAVE_Fonts
enum Fonts{F_4x6,F_6x8,F_8x10};
#endif
#ifndef __HAVE_ICON
#define __HAVE_ICON
typedef struct{unsigned short i[16];}ICON;
#endif
typedef char LCD_BUFFER[LCD_SIZE];
typedef struct{unsigned char Count;struct{signed char Attr,x0,y0,x1,y1;}Data[];}MULTI_LINE;
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
#define ScrRect ((SCR_RECT*const)(_rom_call_addr(2F)))
#define BitmapGet _rom_call(void,(const SCR_RECT*,void*),185)
#define BitmapInit _rom_call(void,(const SCR_RECT*,void*),186)
#define BitmapPut _rom_call(void,(short,short,const void*,const SCR_RECT*,short),187)
#define BitmapSize _rom_call(unsigned short,(const SCR_RECT*),188)
#define ClrScr _rom_call(void,(void),19E)
#define ClearScreen ClrScr
#define DisplayOff() (pokeIO_bclr(0x600015,0))
#define DisplayOn() (pokeIO_bset(0x600015,0))
#define DrawChar _rom_call(void,(short,short,char,short),1A4)
#define DrawClipChar _rom_call(void,(short,short,short,const SCR_RECT*,short),191)
#define DrawClipEllipse _rom_call(void,(short,short,short,short,const SCR_RECT*,short),192)
#define DrawClipLine _rom_call(void,(const WIN_RECT*,const SCR_RECT*,short),193)
#define DrawClipPix _rom_call(void,(short,short),194)
#define DrawClipRect _rom_call(void,(const WIN_RECT*,const SCR_RECT*,short),195)
#define DrawFkey _rom_call(void,(short,short,short,short),1A5)
#define DrawIcon _rom_call(void,(short,short,const void*,short),1A6)
#define DrawLine _rom_call(void,(short,short,short,short,short),1A7)
#define DrawMultiLines _rom_call(void,(short,short,const void*),196)
#define DrawPix _rom_call(void,(short,short,short),1A8)
#define DrawStr _rom_call(void,(short,short,const char*,short),1A9)
#define DrawStrXY DrawStr
#define DrawStrWidth _rom_call(short,(const char*,short),197)
#define FillLines2 _rom_call(void,(const WIN_RECT*,const WIN_RECT*,const SCR_RECT*,short),199)
#define FillTriangle _rom_call(void,(short,short,short,short,short,short,const SCR_RECT*,short),198)
#define FontCharWidth _rom_call(short,(short),190)
#define FontGetSys _rom_call(unsigned char,(void),18E)
#define FontSetSys _rom_call(unsigned char,(short),18F)
#define GetPix _rom_call(short,(short,short),19F)
#define LCD_restore(b) ((void)(_rom_call(void,(),26A)(LCD_MEM,(const void*)(b),(long)LCD_SIZE)))
#define LCD_save(b) ((void)(_rom_call(void,(),26A)((void*)(b),LCD_MEM,(long)LCD_SIZE)))
#define LineTo _rom_call(void,(short,short),19C)
#define DrawTo LineTo
#define MakeWinRect _rom_call(WIN_RECT*,(short,short,short,short),2C)
#define MoveTo _rom_call(void,(short,short),19D)
#define PortRestore _rom_call(void,(void),1A3)
#define PortSet _rom_call(void,(void*,short,short),1A2)
#define QScrRectOverlap _rom_call(short,(const SCR_RECT*,const SCR_RECT*),18D)
#define RestoreScrState _rom_call(void,(const void*),1A1)
#define SaveScrState _rom_call(void,(void*),1A0)
#define ScrRectFill _rom_call(void,(const SCR_RECT*,const SCR_RECT*,short),189)
#define ScrRectOverlap _rom_call(short,(const SCR_RECT*,const SCR_RECT*,SCR_RECT*),18A)
#define ScrRectScroll _rom_call(void,(const SCR_RECT*,const SCR_RECT*,short,short),18B)
#define ScrRectShift _rom_call(void,(const SCR_RECT*,const SCR_RECT*,short,short),18C)
#define ScrToHome _rom_call(SCR_RECT*,(SCR_RECT*),2E)
#define ScrToWin _rom_call(WIN_RECT*,(const SCR_RECT*),2D)
#define SetCurAttr _rom_call(short,(short),19A)
#define SetCurClip _rom_call(void,(const SCR_RECT*),19B)
#if MIN_AMS>=200
#define CalcBitmapSize _rom_call(unsigned short,(BITMAP*),3EF)
#define ClientToScr _rom_call(void,(const SCR_RECT*,const SCR_RECT*,SCR_RECT*),42C)
#define DrawStrWidthP _rom_call(unsigned short,(const char*,short,short),3EE)
#define MakeScrRect _rom_call(SCR_RECT*,(short,short,short,short,SCR_RECT*),42D)
#endif
/* End Auto-Generated Part */

#define ScreenClear ClrScr

#endif
