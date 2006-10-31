#ifndef __KBD
#define __KBD

#include <default.h>

/* We have to use this internal macro since _keytest needs to be able to
   accept single parameters (in particular, the RR_... constant pairs). */
#define __keytest(row,col) (!!(_rowread_inverted(1<<(row))&(1<<(col))))

/* If you can read the macro below, you must be a really good C programmer.
   But I hope I can make it understandable:
   __current_row and __current_rowread_result are defined by BEGIN_KEYTEST
   and initialized to dummy values.
   The definition is sort of the same as the one above, but
   _rowread_inverted (1 << (row))
   is replaced with
   (__current_row == row ? __current_rowread_result : (__current_row = row, __current_rowread_result = _rowread_inverted (1 << (row))))
   So,
   if the row which was read the last time this macro was called equals the
   one that is to be read now,
     then use the last result in place of _rowread_inverted (1 << (row)),
   otherwise
     set __current_row,
     set __current_rowread_result by calling _rowread_inverted,
     and also return this value
     (see the section in the doc about the ',' operator). */
#define __keytest_optimized(row,col) (!!((__current_row==row?__current_rowread_result:(__current_row=row,__current_rowread_result=_rowread_inverted(1<<(row))))&(1<<(col))))

/* Begin Auto-Generated Part */
#define KB_AUTOREPEAT (1<<11)
#define NULL ((void*)0)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_SCR_RECT
#define __HAVE_SCR_RECT
typedef union{struct{unsigned char x0,y0,x1,y1;}xy;unsigned long l;}SCR_RECT;
#endif
enum Arrows{ARROW_UP=1,ARROW_LEFT=2,ARROW_DOWN=3,ARROW_RIGHT=4};
#ifndef __HAVE_CommonKeys
#define __HAVE_CommonKeys
enum CommonKeys{KEY_F1=268,KEY_F2=269,KEY_F3=270,KEY_F4=271,KEY_F5=272,KEY_F6=273,KEY_F7=274,KEY_F8=275,KEY_ESC=264,KEY_QUIT=4360,KEY_APPS=265,KEY_SWITCH=4361,KEY_MODE=266,KEY_BACKSPACE=257,KEY_INS=4353,KEY_CLEAR=263,KEY_VARLNK=4141,KEY_CHAR=4139,KEY_ENTER=13,KEY_ENTRY=4109,KEY_STO=258,KEY_RCL=4354,KEY_SIGN=173,KEY_MATH=4149,KEY_MEM=4150,KEY_ON=267,KEY_OFF=4363};
#endif
enum GKeyFlags{GKF_NORMAL=0,GKF_MODAL=1,GKF_REPUSH_KEY=2,GKF_ACCEPT=4,GKF_SYS=8,GKF_NO_EVS=16};
enum StatKeys{STAT_2ND=1,STAT_DIAMOND=2,STAT_SHIFT=3,STAT_HAND=4};
#define OSFastArrows (*((unsigned char*)(_rom_call_addr_hack_concat(0x15C,_ROM_CALL_15C,((unsigned char*)((unsigned long)(*(((short*)(_rom_call_addr(51)))+0x80)))),200,0x508))))
#define _keytest_optimized(rowcol...) (__keytest_optimized(rowcol))
#define _keytest(rowcol...) (__keytest(rowcol))
#define _rowread_internal(row) (~(_rowread(row)))
#define _rowread_inverted(row) (_rowread(~((short)(row))))
extern unsigned short _rowread(short asm("d0"))__ATTR_LIB_ASM__;
#define GKeyDown ({__need_in_use_bit;_rom_call(short,(void),17F);})
#define GKeyFlush ({__need_in_use_bit;_rom_call(void,(void),180);})
#define GKeyIn ({__need_in_use_bit;_rom_call(short,(SCR_RECT*,short),17E);})
#define kbhit ({__need_in_use_bit;_rom_call(short,(void),52);})
#define ngetchx ({__need_in_use_bit;_rom_call(short,(void),51);})
#define OSGetStatKeys _rom_call(short,(void),299)
#define OSInitBetweenKeyDelay(rate) ({short __oldRate=_OSInitBetweenKeyDelay(48);(__oldRate*48-1)/_OSInitBetweenKeyDelay((rate))+1;})
#define OSInitKeyInitDelay _rom_call(short,(short),248)
#define pushkey ({__need_in_use_bit;_rom_call(void,(short),50);})
#define BEGIN_KEYTEST {register short __current_row=RR_NO_KEY;register unsigned short __current_rowread_result=0;
#define END_KEYTEST }
#if MIN_AMS>=200
#define alphaLockOff _rom_call(void,(unsigned char*),482)
#define alphaLockOn _rom_call(void,(unsigned char*),481)
#define GetAlphaStatus _rom_call(unsigned char,(void),164)
#define KeyYesOrNo _rom_call(short,(short),3EB)
#define restoreAlphaLock _rom_call(void,(unsigned char*),483)
#define SetAlphaStatus _rom_call(void,(char),163)
#endif
/* End Auto-Generated Part */

#define _OSInitBetweenKeyDelay _rom_call(short,(short),249)

#endif
