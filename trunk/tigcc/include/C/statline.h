#ifndef __STATLINE
#define __STATLINE

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_ST_ACTIVITIES
#define __HAVE_ST_ACTIVITIES
enum ST_ACTIVITIES{ACTIVITY_IDLE,ACTIVITY_BUSY,ACTIVITY_PAUSED,ACTIVITY_NORMAL};
#endif
enum ST_FLAGS{ST_IDLE=0,ST_BUSY=1,ST_PAUSE=2,ST_CLEAR=3,ST_NORMAL=3,ST_NOTHING=0,ST_BATT=1,ST_BATT_DARK=2,ST_BATT_OK=0,ST_BATT_LOW=1,ST_BATT_REPLACE=2,ST_RAD=0,ST_DEG=1,ST_FUNC=0,ST_PAR=2,ST_POL=3,ST_SEQ=4,ST_3D=5,ST_DE=6,ST_NONE=0,ST_2ND=1,ST_SHIFT=2,ST_DIAMOND=4,ST_ALPHA=8,ST_SH_A_LOCK=16,ST_A_LOCK=32,ST_AUTO=0,ST_EXACT=1,ST_APPROX=2};
#define ST_angle _rom_call(void,(short),E0)
#define ST_batt _rom_call(void,(short),E1)
#define ST_busy _rom_call(void,(short),E2)
#define ST_eraseHelp _rom_call(short,(void),E3)
#define ST_folder _rom_call(void,(const char*),E4)
#define ST_graph _rom_call(void,(short),E5)
#define ST_helpMsg _rom_call(void,(const char*),E6)
#define ST_showHelp ST_helpMsg
#define ST_modKey _rom_call(void,(short),E7)
#define ST_precision _rom_call(void,(short),E8)
#define ST_readOnly _rom_call(void,(short),E9)
#define ST_refDsp _rom_call(void,(short),EB)
#define ST_stack _rom_call(void,(short,short),EA)
#if MIN_AMS>=200
typedef struct{WINDOW*w;WIN_RECT rect;long value;long low,high;long logwidth;short physwidth;}ST_PROGRESS_BAR;
#define ST_progressBar _rom_call(void,(ST_PROGRESS_BAR*,long,long),4D8)
#define ST_progressDismiss _rom_call(void,(ST_PROGRESS_BAR*),4DB)
#define ST_progressIncrement _rom_call(void,(ST_PROGRESS_BAR*,long),4DA)
#define ST_progressUpdate _rom_call(void,(ST_PROGRESS_BAR*,long),4D9)
#endif
/* End Auto-Generated Part */

#endif
