#ifndef __EVENTS
#define __EVENTS

#include <default.h>

/* Begin Auto-Generated Part */
#define MO_option ((unsigned short*const)(_rom_call_addr_hack(447,(((unsigned short*const)(long)*(short*)((char*)MO_currentOptions+6))),200)))
#define ModeSettings ((MO_OPTIONS*const)(_rom_call_addr_hack(447,(((MO_OPTIONS*const)(long)*(short*)((char*)MO_currentOptions+6))),200)))
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_HANDLE
#define __HAVE_HANDLE
typedef unsigned short HANDLE;
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
#ifndef __HAVE_SCR_STATE
#define __HAVE_SCR_STATE
typedef struct{void*ScrAddr;unsigned char XMax,YMax;short CurFont,CurAttr,CurX,CurY;SCR_RECT CurClip;}SCR_STATE;
#endif
#ifndef __HAVE_ST_ACTIVITIES
#define __HAVE_ST_ACTIVITIES
enum ST_ACTIVITIES{ACTIVITY_IDLE,ACTIVITY_BUSY,ACTIVITY_PAUSED,ACTIVITY_NORMAL};
#endif
#ifndef __HAVE_SystemDataTypes
#define __HAVE_SystemDataTypes
enum SystemDataTypes{SDT_EXPR=0,SDT_LIST=1,SDT_MAT=2,SDT_FUNC=3,SDT_PRGM=4,SDT_PIC=5,SDT_STR=6,SDT_TEXT=7,SDT_GDB=8,SDT_DATA=9,SDT_FIG=10,SDT_MAC=11,SDT_OTH=12,SDT_SYS=13,SDT_ALL=14,SDT_ASM=15};
#endif
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_WinFlags
#define __HAVE_WinFlags
enum WinFlags{WF_SYS_ALLOC=0x0001,WF_STEAL_MEM=0x0002,WF_DONT_REALLOC=0x0004,WF_ROUNDEDBORDER=0x0008,WF_SAVE_SCR=0x0010,WF_DUP_SCR=0x0020,WF_TTY=0x0040,WF_ACTIVE=0x0080,WF_NOBORDER=0x0100,WF_NOBOLD=0x0200,WF_DUP_ON=0x0400,WF_VIRTUAL=0x0800,WF_TITLE=0x1000,WF_DIRTY=0x2000,WF_TRY_SAVE_SCR=0x4010,WF_VISIBLE=0x8000};
#endif
#ifndef __HAVE_EVENT
#define __HAVE_EVENT
typedef struct EventStruct{unsigned short Type;unsigned short RunningApp;unsigned short Side;unsigned short StatusFlags;union{WINDOW*w;WIN_RECT*r;char*pasteText;HANDLE hPasteText;struct{unsigned short Mod;unsigned short Code;}Key;}extra;unsigned char StartType;}EVENT;
#endif
typedef void(*EVENT_HANDLER)(EVENT*)CALLBACK;
enum EventIDs{CM_NOTHING=0x0,CM_LAST_STRING_NUMBER=0x4ff,CM_FIRST_APP_ID=0x500,CM_FIRST_SYS_ID=0x700,CM_IDLE=0x700,CM_NULL=0x700,CM_INIT=0x701,CM_STARTTASK=0x702,CM_START=0x702,CM_ACTIVATE=0x703,CM_FOCUS=0x704,CM_UNFOCUS=0x705,CM_DEACTIVATE=0x706,CM_ENDTASK=0x707,CM_QUIT=0x707,CM_START_CURRENT=0x708,CM_RESTART=0x708,CM_APD=0x709,CM_OFF=0x70A,CM_ON=0x70B,CM_INSTALL=0x70C,CM_UNINSTALL=0x70D,CM_PACK=0x70E,CM_UNPACK=0x70F,CM_KEYPRESS=0x710,CM_KEY_PRESS=0x710,CM_MENU_CUT=0x720,CM_CUT=0x720,CM_MENU_COPY=0x721,CM_COPY=0x721,CM_MENU_PASTE=0x722,CM_PASTE=0x722,CM_STRING=0x723,CM_PASTE_STRING=0x723,CM_HSTRING=0x724,CM_PASTE_HANDLE=0x724,CM_DEL=0x725,CM_DELETE=0x725,CM_CLR=0x726,CM_CLEAR=0x726,CM_MENU_CLEAR=0x727,CM_CLEAR_ALL=0x727,CM_MENU_FIND=0x728,CM_FIND=0x728,CM_INSERT=0x730,CM_TOGGLE_INSERT=0x730,CM_BLINK=0x740,CM_CURSOR_FLASH=0x740,CM_STORE=0x750,CM_STO=0x750,CM_RECALL=0x751,CM_RCL=0x751,CM_WPAINT=0x760,CM_MENU_OPEN=0x770,CM_OPEN=0x770,CM_MENU_SAVE=0x771,CM_SAVE_AS=0x771,CM_MENU_NEW=0x772,CM_NEW=0x772,CM_MENU_FORMAT=0x773,CM_FORMAT=0x773,CM_MENU_ABOUT=0x774,CM_ABOUT=0x774,CM_MODE_CHANGE=0x780,CM_SWITCH_GRAPH=0x781,CM_DEFAULTS=0x782,CM_GEOMETRY=0x7C0,CM_OPEN_SYSDATA=0x7C0,CM_FIRST_APP_STRING=0x800};
typedef struct{unsigned short CurrentFolder;unsigned short SplitScreen;unsigned short NumGraphs;unsigned short Graph1;unsigned short Graph2;unsigned short Split1App;unsigned short Split2App;unsigned short SplitRatio;unsigned short Angle;unsigned short ExactApprox;unsigned short Fix;unsigned short Exp;unsigned short Vector;unsigned short Complex;unsigned short Pretty;unsigned short Base;unsigned short UnitSystem;unsigned short CustomUnits;}MO_OPTIONS;
typedef enum MoNotifyFlags{MO_NOTIFY_FOLDER=0x0001,MO_NOTIFY_GRAPH_COUNT=0x0002,MO_NOTIFY_GRAPH_TYPE_1=0x0004,MO_NOTIFY_GRAPH_TYPE_2=0x0008,MO_NOTIFY_SPLIT=0x0010,MO_NOTIFY_ANGLE=0x0020,MO_NOTIFY_PRECISION=0x0040,MO_NOTIFY_FIX=0x0080,MO_NOTIFY_NUMBER_FORMAT=0x0100,MO_NOTIFY_VECTOR_FORMAT=0x0200,MO_NOTIFY_COMPLEX_FORMAT=0x0400,MO_NOTIFY_PRETTY_PRINT=0x0800,MO_NOTIFY_UNIT_SYSTEM=0x1000,MO_NOTIFY_BASE=0x2000
#if MIN_AMS>=200
,MO_NOTIFY_LANGUAGE=0x4000
#endif
#if MIN_AMS>=207
,MO_NOTIFY_DESKTOP=0x8000
#endif
}MoNotifyFlags;
enum PaintFlags{PAINTING_ENABLED=0,PAINTING_SUSPENDED=2};
enum StandardTaskIDs{AP_NULL=-3,AP_RUNNING=-2,AP_CURRENT=-1};
enum StartTypes{AP_START_CURRENT=0,__AP_START_NEW_2=1,__AP_START_OPEN_2=2,__AP_START_ERROR_2=3,__AP_START_NEW_1=0x10,__AP_START_OPEN_1=0x20,__AP_START_ERROR_1=0x30};
#define EV_hook (*((EVENT_HANDLER*)(_rom_call_addr(2A3))))
#define ABT_dialog _rom_call(void,(void),10D)
#define CAT_dialog _rom_call(void,(void),125)
#define EV_captureEvents _rom_call(EVENT_HANDLER,(EVENT_HANDLER),C6)
#define EV_centralDispatcher ({__need_in_use_bit;_rom_call(void,(void),156);})
#define EV_clearPasteString _rom_call(void,(void),C7)
#define EV_defaultHandler ({__need_in_use_bit;_rom_call(void,(EVENT*),157);})
#define EV_eventLoop ({__need_in_use_bit;_rom_call(void,(void),158);})
extern short EV_getAppID(const char* asm("a0"))__ATTR_LIB_ASM__;
#define EV_getc ({__need_in_use_bit;_rom_call(unsigned short,(short,EVENT*),C8);})
#define EV_getSplitRect _rom_call(WIN_RECT*,(short),C9)
#define EV_notifySwitchGraph _rom_call(void,(void),CA)
#define EV_paintOneWindow _rom_call(short,(void),CB)
#define EV_paintWindows _rom_call(void,(void),CC)
#define EV_registerMenu _rom_call(void,(void*),159)
#define EV_restorePainting _rom_call(short,(short),CD)
#define EV_sendEvent _rom_call(void,(short,EVENT*),CE)
#define EV_sendEventSide _rom_call(void,(short,EVENT*,short),CF)
#define EV_sendString _rom_call(void,(short),D0)
#define EV_setCmdCheck _rom_call(void,(short,short),D1)
#define EV_setCmdState _rom_call(void,(short,short),D2)
#define EV_setFKeyState _rom_call(void,(short,short,short),D3)
#define EV_startApp _rom_call(void,(HANDLE,short),D4)
#define EV_startSide _rom_call(void,(__pshort,HANDLE,short),D5)
#define EV_startTask _rom_call(void,(short),D6)
#define EV_suspendPainting _rom_call(short,(void),D7)
#define EV_switch _rom_call(void,(void),D8)
#define handleRclKey ({__need_in_use_bit;_rom_call(void,(short),14F);})
#define MO_currentOptions _rom_call(void,(void),D9)
#define MO_defaults ({__need_in_use_bit;_rom_call(void,(void),DA);})
#define MO_digestOptions ({__need_in_use_bit;_rom_call(void,(short),DB);})
#define MO_isMultigraphTask _rom_call(short,(short),DC)
#define MO_modeDialog ({__need_in_use_bit;_rom_call(void,(void),DD);})
#define MO_notifyModeChange _rom_call(void,(short),DE)
#define MO_sendQuit _rom_call(void,(short,short),DF)
#if MIN_AMS>=200
#define handleVarLinkKey ({__need_in_use_bit;_rom_call(void,(short),129);})
#endif
/* End Auto-Generated Part */

#define AP_START_NEW (AMS_1xx?__AP_START_NEW_1:__AP_START_NEW_2)
#define AP_START_OPEN (AMS_1xx?__AP_START_OPEN_1:__AP_START_OPEN_2)
#define AP_START_ERROR (AMS_1xx?__AP_START_ERROR_1:__AP_START_ERROR_2)

#endif
