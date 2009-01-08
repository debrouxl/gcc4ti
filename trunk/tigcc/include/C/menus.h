#ifndef __MENUS
#define __MENUS

#include <default.h>

/* These constants are needed for use in "asm" statements */
asm(".set MT_TEXT,0x8000");
asm(".set MT_XREF,0x9000");
asm(".set MT_ICON,0xA000");
asm(".set MT_CASCADE,0x4000");

/* Begin Auto-Generated Part */
#define CENTER (-1)
#define H_NULL 0
#define NULL ((void*)0)
#ifndef __HAVE_BITMAP
#define __HAVE_BITMAP
typedef struct{unsigned short NumRows,NumCols;unsigned char Data[];}BITMAP;
#endif
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_CommonKeys
#define __HAVE_CommonKeys
enum CommonKeys{KEY_F1=268,KEY_F2=269,KEY_F3=270,KEY_F4=271,KEY_F5=272,KEY_F6=273,KEY_F7=274,KEY_F8=275,KEY_ESC=264,KEY_QUIT=4360,KEY_APPS=265,KEY_SWITCH=4361,KEY_MODE=266,KEY_BACKSPACE=257,KEY_INS=4353,KEY_CLEAR=263,KEY_VARLNK=4141,KEY_CHAR=4139,KEY_ENTER=13,KEY_ENTRY=4109,KEY_STO=258,KEY_RCL=4354,KEY_SIGN=173,KEY_MATH=4149,KEY_MEM=4150,KEY_ON=267,KEY_OFF=4363};
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
enum DynMenuFlags{DMF_TEXT=0x0001,DMF_ICON=0x0002,DMF_BITMAP=0x0004
#if MIN_AMS>=200
,DMF_CHILD_SUB=0x1000
#endif
,DMF_CHILD=0x2000,DMF_TOP_SUB=0x4000,DMF_TOP=0x8000};
enum ItemTypes{MT_TEXT=0x8000,MT_XREF=0x9000,MT_ICON=0xA000,MT_CASCADE=0x4000};
enum MenuBeginFlags{MBF_WITHICON=0x01,MBF_REDEF=0x02,MBF_SYS_ALLOC=0x04
#if MIN_AMS>=200
,MBF_MAX_MENU_WIDTH=0x08,MBF_STRIKEOUT=0x10,MBF_HMENU=0x20,MBF_NO_DRAWTOP=0x40
#endif
};
enum MenuCheckCmds{MC_CHECK=0
#if MIN_AMS>=200
,MC_UNCHECK=1
#endif
,MC_STATUS=2,MC_FLIP=3};
enum MenuFlagsEnum{MF_POPUP=0x0001,MF_TOOLBOX=0x0002,MF_NONSEQ=0x0004,MF_ICON_TITLE=0x0008,MF_TEXT_TITLE=0x0010,MF_NO_NUMS=0x0020,MF_NO_UNAMED=0x0040,MF_DYN_POPUP=0x0080,MF_ALT_ICONS=0x0100,MF_BITMAP_TITLE=0x0200,MF_ERROR=0x0800,MF_ICONS_OVERLAP=0x1000,MF_TITLE=0x0218};
enum MenuKeyValues{M_NOITEM=0,M_NOTMENUKEY=-2};
enum VCFPFlags{VCFP_ALL=0x01,VCFP_SKIP_CURDIR=0x02};
#define MenuAddIcon _rom_call(HANDLE,(HANDLE,short,const void*,short,short),42)
#define MenuAddText _rom_call(HANDLE,(HANDLE,short,const char*,short,short),41)
#define MenuBegin _rom_call(HANDLE,(const void*,short,short,short,...),36)
#define MenuCheck _rom_call(short,(HANDLE,short,short),37)
#define MenuEnd _rom_call(void,(HANDLE),38)
#define MenuGetTopRedef _rom_call(short,(HANDLE,short),40)
#define MenuKey ({__need_in_use_bit;_rom_call(short,(HANDLE,short),39);})
#define MenuNew _rom_call(HANDLE,(short,short,short),43)
#define MenuOn _rom_call(void,(HANDLE),3A)
#define MenuPopup ({__need_in_use_bit;_rom_call(unsigned short,(const void*,short,short,short),3B);})
#define MenuSubStat _rom_call(void,(HANDLE,short,short),3C)
#define MenuTopRedef _rom_call(void,(HANDLE,short,short),3F)
#define MenuTopSelect _rom_call(void,(HANDLE,short),3E)
#define MenuTopStat _rom_call(void,(HANDLE,short,short),3D)
#define MenuUpdate _rom_call(void,(void),49)
#define PopupAddText _rom_call(HANDLE,(HANDLE,short,const char*,short),44)
#define PopupClear _rom_call(HANDLE,(HANDLE),46)
#define PopupDo ({__need_in_use_bit;_rom_call(short,(HANDLE,short,short,short),47);})
#define PopupNew _rom_call(HANDLE,(const char*,short),45)
#define PopupText _rom_call(const char*,(HANDLE,short),48)
#define VarCreateFolderPopup _rom_call(HANDLE,(__pushort,short),28F)
#if MIN_AMS>=200
#define DynMenuAdd _rom_call(HANDLE,(HANDLE,short,const void*,short,short),3F1)
#define DynMenuChange _rom_call(HANDLE,(HANDLE,short,const void*,short),3F0)
#define MenuFlags _rom_call(short,(HANDLE),3F4)
#define MenuItemDef _rom_call(void*,(HANDLE,short,__pushort),3F3)
#define MenuLoad _rom_call(HANDLE,(const void*,short),3F2)
#define MenuOff _rom_call(void,(HANDLE),419)
#define PopupBegin _rom_call(HANDLE,(HANDLE,short),3F5)
#define PopupBeginDo ({__need_in_use_bit;_rom_call(short,(HANDLE,short,short,short),3F6);})
#define QMenuTopSelect _rom_call(unsigned short,(HANDLE),41A)
#if MIN_AMS>=202
#define FKeyI_H _rom_call(short,(HANDLE,short),592)
#endif
#endif
/* End Auto-Generated Part */

#endif
