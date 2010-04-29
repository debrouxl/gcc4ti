#ifndef __DIALOGS
#define __DIALOGS

#include <default.h>

#define DialogAddScroll(h,x,y,w,g,f,t,p,u,m) DialogAdd(h,0,x,y,3,(short)w,(short)g,(short)f,(short)t,(short)p,(short)u,(short)m)

/* Begin Auto-Generated Part */
#define CENTER (-1)
enum DialogMessages{DB_CONTINUE=1,DB_MEMFULL=-1,DB_QACTIVE=-2
#if MIN_AMS>=200
,DB_REDRAW=-3
#endif
,DB_REDRAW_AND_CONTINUE=-4,DB_UNKNOWN=-5,DB_GET_EDIT_HANDLE=-6,DB_GET_TITLE=-7,DB_EXIT=-8};
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
enum Buttons{BT_NONE=0,BT_OK=1,BT_SAVE=2,BT_YES=3,BT_CANCEL=4,BT_NO=5,BT_GOTO=6};
#endif
typedef CALLBACK short(*Dialog_Callback_t)(short Message,long Value);
#define DialogNew_t Dialog_Callback_t
typedef CALLBACK HANDLE(*Dialog_GetHandle_t)(short ID);
typedef struct{unsigned char Type;unsigned char Flags;unsigned char x0,y0;union{struct{void*Menu;unsigned char MaxMenuWidth;}dMenu;struct{short oText;void*PopUp;unsigned short oIndex;}dPopUp;struct{short oText;HANDLE(*GetPopUp)(unsigned short);unsigned short oIndex;}dDynPopUp;struct{short oText;HANDLE hPopUp;unsigned short dummy;unsigned short oIndex;}dHPopUp;struct DEditType{short oText;unsigned short bOffset;unsigned short Flen;unsigned char Dlen;}dEdit;struct{short oText;}dText;struct{short oText;unsigned char lButton,rButton;}dHeader;struct{short oIcon;}dIcon;struct{unsigned char x1,y1;unsigned char Index0,Index1;unsigned char NumDspFields,TotNumFields;unsigned char FieldHeight;}dScrollR;struct{unsigned short xFlags[4];}dFlags;}f;}DIALOG_ITEM;
#define DIALOG_ITEMS DIALOG_ITEM
typedef struct{unsigned short TextOffset;unsigned short NumItems;unsigned char Width,Height;Dialog_Callback_t CallBack;DIALOG_ITEM Fields[];}DIALOG;
enum DialogFlags{DF_TAB_ELLIPSES=0x01
#if MIN_AMS>=200
,DF_MAX_MENU_WIDTH=0x01
#endif
,DF_SCROLLABLE=0x02,DF_CLR_ON_REDRAW=0x04
#if MIN_AMS>=200
,DF_TAB_SPACES=0x04,DF_OWNER_DRAW=0x08,DF_POPUP_RADIO=0x20
#endif
,DF_SCREEN_SAVE=0x40,DF_SKIP=0x80};
enum DialogTypes{D_END=0,D_MENU=1,D_EDIT_FIELD=2,D_SCROLL_REGION=3,D_OPTION=4,D_RADIO=5,D_BUTTON=6,D_TEXT=7,D_HEADER=8,D_POPUP=10,D_DYNPOPUP=11,D_HEDIT=12,D_DYNHEADER=13,D_HPOPUP=14,D_XFLAGS=15};
typedef struct{DIALOG_ITEM*Item;WINDOW*pW;}OWNER_DRAW_STRUCT;
#define Dialog ({__need_in_use_bit;_rom_call(short,(DIALOG*,short,short,char*,__pshort),30);})
#define DialogAdd _rom_call(HANDLE,(HANDLE,short,short,short,short,...),33)
#define DialogAddDynamicRequest(h,f,x,y,l,w) DialogAdd(h,f,x,y,D_HEDIT,(char*)(l),(short)(w))
#define DialogAddPulldown(h,x,y,t,p,i) DialogAdd(h,2,x,y,14,(const char*)(t),(short)(p),(short)(i))
#define DialogAddPulldownEx(h,f,x,y,t,p,i) DialogAdd(h,f,x,y,D_HPOPUP,(char*)(t),(short)(p),(short)(i))
#define DialogAddRequest(h,x,y,t,o,m,w) DialogAdd(h,0,x,y,2,(const char*)(t),(short)(o),(short)(m),(short)(w))
#define DialogAddRequestEx(h,f,x,y,t,o,m,w) DialogAdd(h,f,x,y,D_EDIT_FIELD,(char*)(t),(short)(o),(short)(m),(short)(w))
#define DialogAddScrollRegion(h,f,x,y,w,g,z,t,p,u,m) DialogAdd(h,f,x,y,D_SCROLL_REGION,(short)(w),(short)(g),(short)(z),(short)(t),(short)(p),(short)(u),(short)(m))
#define DialogAddStaticPulldown(h,f,x,y,l,p,i) DialogAdd(h,f,x,y,D_POPUP,(char*)(l),(void*)(p),(unsigned short)(i))
#define DialogAddText(h,x,y,t) DialogAdd(h,0,x,y,7,(const char*)(t))
#define DialogAddTextEx(h,f,x,y,t) DialogAdd(h,f,x,y,D_TEXT,(char*)(t))
#define DialogAddTitle(h,t,l,r) DialogAdd(h,0,0,0,8,(const char*)(t),(short)(l),(short)(r))
#define DialogAddTitleEx(h,f,t,l,r) DialogAdd(h,f,0,0,D_HEADER,(char*)(t),(short)(l),(short)(r))
#define DialogDo ({__need_in_use_bit;_rom_call(short,(HANDLE,short,short,char*,__pshort),32);})
#define DialogNew _rom_call(HANDLE,(short,short,Dialog_Callback_t),34)
#define DialogNewSimple(w,h) DialogNew(w,h,NoCallBack)
#define DlgMessage ({__need_in_use_bit;_rom_call(short,(const char*,const char*,short,short),1B4);})
#define HI_WORD(val) ((unsigned short)((val)>>16L));
#define LO_WORD(val) ((unsigned short)(val))
extern short NoCallBack(short,long)__ATTR_TIOS_CALLBACK__;
#define VarNew ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,...),28E);})
#define VarOpen ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,...),28C);})
#define VarSaveAs ({__need_in_use_bit;_rom_call(HSym,(const ESQ*,const char*,...),28D);})
#define SIZED_DIALOG(NumbItems,StrLen) struct{unsigned short TextOffset;unsigned short NumItems;unsigned char Width,Height;Dialog_Callback_t Callback;DIALOG_ITEM Fields[(NumbItems)+1];char String[StrLen];}
#if MIN_AMS>=200
enum DialogXFlags{XF_ALLOW_VARLINK=0x0001,XF_TE_REPAINT=0x0002,XF_NO_ALPHA_LOCK=0x0004,XF_VARLINK_SELECT_ONLY=0x8001};
#define DialogAddDynamicPulldown(h,f,x,y,l,g,i) DialogAdd((h),(f),(x),(y),D_DYNPOPUP,(const char*)(l),(short*)(g),(short)(i))
#define DialogAddMenu(h,f,x,y,m,w) DialogAdd(h,f,x,y,D_MENU,(void*)(m),(unsigned short)(w))
#define DialogAddXFlags(h,f,u,v,w,x) DialogAdd(h,f,0,0,D_XFLAGS,(unsigned short)(u),(unsigned short)(v),(unsigned short)(w),(unsigned short)(x))
#endif
/* End Auto-Generated Part */

#endif
