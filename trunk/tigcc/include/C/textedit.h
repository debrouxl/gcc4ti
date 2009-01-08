#ifndef __TEXTEDIT
#define __TEXTEDIT

#include <default.h>

/* Begin Auto-Generated Part */
#define TE_FAR_RIGHT (0xFFFF)
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
#ifndef __HAVE_WIN_RECT
#define __HAVE_WIN_RECT
typedef struct{short x0,y0,x1,y1;}WIN_RECT;
#endif
#ifndef __HAVE_WINDOW
#define __HAVE_WINDOW
typedef struct WindowStruct{unsigned short Flags;unsigned char CurFont;unsigned char CurAttr;unsigned char Background;short TaskId;short CurX,CurY;short CursorX,CursorY;SCR_RECT Client;SCR_RECT Window;SCR_RECT Clip;SCR_RECT Port;unsigned short DupScr;struct WindowStruct*Next;char*Title;SCR_STATE savedScrState;unsigned char Reserved[16];}WINDOW;
#endif
#ifndef __HAVE_EVENT
#define __HAVE_EVENT
typedef struct EventStruct{unsigned short Type;unsigned short RunningApp;unsigned short Side;unsigned short StatusFlags;union{WINDOW*w;WIN_RECT*r;char*pasteText;HANDLE hPasteText;struct{unsigned short Mod;unsigned short Code;}Key;}extra;unsigned char StartType;}EVENT;
#endif
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
typedef enum ETE_FLAGS{TE_WRAP=0x0001,TE_COLON=0x0002,TE_COMMANDS=0x0006,TE_MORE_ARROWS=0x0008,TE_MORE_ELLIPSES=0x0018,TE_SELECT=0x0020,TE_CURSOR=0x0040,TE_FIXED_LENGTH=0x0080,TE_CHANGED=0x0100,TE_FOCUSED=0x0200,TE_AUTO_ANS=0x0400,TE_READ_ONLY=0x0800}TE_FLAGS;
typedef struct TextEditStruct{WINDOW*Parent;unsigned short ReadOnly;WIN_RECT Rect;unsigned short BufSize;unsigned short CurSize;unsigned short CursorOffset;unsigned short StartOffset;union{unsigned short SelStart;unsigned short PreChars;};unsigned short CharWidth;unsigned short CharHeight;unsigned short LineNum;unsigned short CursorX;unsigned short Flags;union{HANDLE h;const char*p;}Text;}TEXT_EDIT;
#define TE_checkSlack _rom_call(void,(TEXT_EDIT*),A6)
#define TE_close _rom_call(void,(TEXT_EDIT*),A5)
#define TE_empty _rom_call(void,(TEXT_EDIT*),A7)
#define TE_focus _rom_call(short,(TEXT_EDIT*),A8)
#define TE_handleEvent ({__need_in_use_bit;_rom_call(short,(TEXT_EDIT*,EVENT*),A9);})
#define TE_indicateReadOnly _rom_call(void,(TEXT_EDIT*),AA)
#define TE_isBlank _rom_call(short,(TEXT_EDIT*),AB)
#define TE_open _rom_call(short,(TEXT_EDIT*,WINDOW*,WIN_RECT*,HANDLE,short,short,short),AC)
#define TE_openFixed _rom_call(short,(TEXT_EDIT*,WINDOW*,WIN_RECT*,char*,short,short),AD)
#define TE_pasteText ({__need_in_use_bit;_rom_call(void,(TEXT_EDIT*,const char*,long),AE);})
#define TE_reopen _rom_call(void,(TEXT_EDIT*,short),AF)
#define TE_reopenPlain _rom_call(void,(TEXT_EDIT*,short),B0)
#define TE_select _rom_call(void,(TEXT_EDIT*,short,short),B1)
#define TE_shrinkWrap _rom_call(HANDLE,(TEXT_EDIT*),B2)
#define TE_unfocus _rom_call(short,(TEXT_EDIT*),B3)
#define TE_updateCommand _rom_call(void,(TEXT_EDIT*,char),B4)
/* End Auto-Generated Part */

#endif
