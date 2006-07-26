#ifndef __COMPAT
#define __COMPAT

#include <default.h>

/* Begin Auto-Generated Part */
#define KEY_DIAMOND (PSEUDO_CONST_KBD(16384,8192))
#define KEY_DOWN (PSEUDO_CONST_KBD(340,344))
#define KEY_DOWNLEFT (PSEUDO_CONST_KBD(342,345))
#define KEY_LEFT (PSEUDO_CONST_KBD(338,337))
#define KEY_OFF2 (PSEUDO_CONST_KBD(16651,8459))
#define KEY_RIGHT (PSEUDO_CONST_KBD(344,340))
#define KEY_SHIFT (PSEUDO_CONST_KBD(8192,16384))
#define KEY_UP (PSEUDO_CONST_KBD(337,338))
#define KEY_UPRIGHT (PSEUDO_CONST_KBD(345,342))
#define LCD_HEIGHT (PSEUDO_CONST_SCREEN(100,128))
#define LCD_LINE_BYTES (PSEUDO_CONST_SCREEN(20,30))
#define LCD_WIDTH (PSEUDO_CONST_SCREEN(160,240))
#define RR_0 (PSEUDO_CONST_KBD(4,9)),(PSEUDO_CONST_KBD(0,5))
#define RR_1 (PSEUDO_CONST_KBD(4,1)),(PSEUDO_CONST_KBD(1,5))
#define RR_2 (PSEUDO_CONST_KBD(3,1)),(PSEUDO_CONST_KBD(1,6))
#define RR_3 (PSEUDO_CONST_KBD(2,1)),(PSEUDO_CONST_KBD(1,7))
#define RR_4 (PSEUDO_CONST_KBD(4,2)),(PSEUDO_CONST_KBD(2,5))
#define RR_5 (PSEUDO_CONST_KBD(3,2)),(PSEUDO_CONST_KBD(2,6))
#define RR_6 2,(PSEUDO_CONST_KBD(2,7))
#define RR_7 (PSEUDO_CONST_KBD(4,3)),(PSEUDO_CONST_KBD(3,5))
#define RR_8 3,(PSEUDO_CONST_KBD(3,6))
#define RR_9 (PSEUDO_CONST_KBD(2,3)),(PSEUDO_CONST_KBD(3,7))
#define RR_2ND 0,(PSEUDO_CONST_KBD(4,0))
#define RR_A (PSEUDO_CONST_KBD(RR_NO_KEY,9)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_ALPHA (PSEUDO_CONST_KBD(0,RR_NO_KEY)),(PSEUDO_CONST_KBD(7,RR_NO_KEY))
#define RR_APPS (PSEUDO_CONST_KBD(5,7)),(PSEUDO_CONST_KBD(0,6))
#define RR_B (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_BCKSPC (PSEUDO_CONST_KBD(2,8)),(PSEUDO_CONST_KBD(6,0))
#define RR_C (PSEUDO_CONST_KBD(RR_NO_KEY,3)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_CATALOG (PSEUDO_CONST_KBD(3,RR_NO_KEY)),(PSEUDO_CONST_KBD(6,RR_NO_KEY))
#define RR_CLEAR (PSEUDO_CONST_KBD(1,7)),(PSEUDO_CONST_KBD(6,5))
#define RR_COMMA (PSEUDO_CONST_KBD(2,4)),(PSEUDO_CONST_KBD(4,7))
#define RR_COS (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,6))
#define RR_D (PSEUDO_CONST_KBD(RR_NO_KEY,2)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_DIAMOND 0,(PSEUDO_CONST_KBD(6,1))
#define RR_DIVIDE (PSEUDO_CONST_KBD(1,5)),(PSEUDO_CONST_KBD(4,0))
#define RR_DOT (PSEUDO_CONST_KBD(3,9)),(PSEUDO_CONST_KBD(0,6))
#define RR_DOWN 0,(PSEUDO_CONST_KBD(2,7))
#define RR_E (PSEUDO_CONST_KBD(RR_NO_KEY,2)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_EE (PSEUDO_CONST_KBD(5,RR_NO_KEY)),(PSEUDO_CONST_KBD(2,RR_NO_KEY))
#define RR_ENTER1 (PSEUDO_CONST_KBD(1,9)),(PSEUDO_CONST_KBD(0,1))
#define RR_ENTER2 (PSEUDO_CONST_KBD(1,6)),(PSEUDO_CONST_KBD(0,6))
#define RR_ENTER RR_ENTER1
#define RR_EQUALS (PSEUDO_CONST_KBD(5,7)),(PSEUDO_CONST_KBD(4,0))
#define RR_ESC (PSEUDO_CONST_KBD(6,8)),(PSEUDO_CONST_KBD(0,6))
#define RR_F1 (PSEUDO_CONST_KBD(5,6)),(PSEUDO_CONST_KBD(7,4))
#define RR_F2 4,(PSEUDO_CONST_KBD(7,4))
#define RR_F3 (PSEUDO_CONST_KBD(3,2)),(PSEUDO_CONST_KBD(7,4))
#define RR_F4 (PSEUDO_CONST_KBD(2,9)),(PSEUDO_CONST_KBD(7,4))
#define RR_F5 (PSEUDO_CONST_KBD(1,7)),(PSEUDO_CONST_KBD(7,4))
#define RR_F6 (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,4))
#define RR_F7 (PSEUDO_CONST_KBD(RR_NO_KEY,3)),(PSEUDO_CONST_KBD(RR_NO_KEY,4))
#define RR_F8 (PSEUDO_CONST_KBD(RR_NO_KEY,1)),(PSEUDO_CONST_KBD(RR_NO_KEY,4))
#define RR_F (PSEUDO_CONST_KBD(RR_NO_KEY,3)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_G (PSEUDO_CONST_KBD(RR_NO_KEY,4)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_H (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_HAND (PSEUDO_CONST_KBD(RR_NO_KEY,0)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_HOME (PSEUDO_CONST_KBD(5,RR_NO_KEY)),(PSEUDO_CONST_KBD(6,RR_NO_KEY))
#define RR_I (PSEUDO_CONST_KBD(RR_NO_KEY,7)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_J (PSEUDO_CONST_KBD(RR_NO_KEY,6)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_K (PSEUDO_CONST_KBD(RR_NO_KEY,7)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_L (PSEUDO_CONST_KBD(RR_NO_KEY,8)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_LEFT 0,(PSEUDO_CONST_KBD(1,4))
#define RR_LN (PSEUDO_CONST_KBD(RR_NO_KEY,6)),(PSEUDO_CONST_KBD(RR_NO_KEY,5))
#define RR_M (PSEUDO_CONST_KBD(RR_NO_KEY,7)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_MINUS (PSEUDO_CONST_KBD(1,9)),(PSEUDO_CONST_KBD(2,0))
#define RR_MODE (PSEUDO_CONST_KBD(4,8)),(PSEUDO_CONST_KBD(6,5))
#define RR_MULTIPLY (PSEUDO_CONST_KBD(1,7)),(PSEUDO_CONST_KBD(3,7))
#define RR_N (PSEUDO_CONST_KBD(RR_NO_KEY,6)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_NEGATE (PSEUDO_CONST_KBD(2,9)),(PSEUDO_CONST_KBD(0,7))
#define RR_NO_KEY 0xF
#define RR_O (PSEUDO_CONST_KBD(RR_NO_KEY,8)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_P (PSEUDO_CONST_KBD(RR_NO_KEY,6)),(PSEUDO_CONST_KBD(RR_NO_KEY,7))
#define RR_PAREN_CLOSE (PSEUDO_CONST_KBD(3,4)),(PSEUDO_CONST_KBD(4,6))
#define RR_PAREN_OPEN 4,(PSEUDO_CONST_KBD(4,5))
#define RR_PLUS (PSEUDO_CONST_KBD(1,8)),(PSEUDO_CONST_KBD(1,4))
#define RR_POWER (PSEUDO_CONST_KBD(1,6)),(PSEUDO_CONST_KBD(5,0))
#define RR_Q (PSEUDO_CONST_KBD(RR_NO_KEY,9)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_R (PSEUDO_CONST_KBD(RR_NO_KEY,3)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_RIGHT 0,(PSEUDO_CONST_KBD(3,6))
#define RR_S (PSEUDO_CONST_KBD(RR_NO_KEY,1)),(PSEUDO_CONST_KBD(RR_NO_KEY,2))
#define RR_SHIFT 0,(PSEUDO_CONST_KBD(5,2))
#define RR_SIN (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,5))
#define RR_SPACE (PSEUDO_CONST_KBD(RR_NO_KEY,4)),(PSEUDO_CONST_KBD(RR_NO_KEY,0))
#define RR_STORE (PSEUDO_CONST_KBD(5,3)),(PSEUDO_CONST_KBD(1,0))
#define RR_T (PSEUDO_CONST_KBD(2,4)),(PSEUDO_CONST_KBD(5,3))
#define RR_TAN (PSEUDO_CONST_KBD(RR_NO_KEY,5)),(PSEUDO_CONST_KBD(RR_NO_KEY,7))
#define RR_THETA (PSEUDO_CONST_KBD(RR_NO_KEY,8)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_U (PSEUDO_CONST_KBD(RR_NO_KEY,6)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_UP 0,(PSEUDO_CONST_KBD(0,5))
#define RR_V (PSEUDO_CONST_KBD(RR_NO_KEY,4)),(PSEUDO_CONST_KBD(RR_NO_KEY,1))
#define RR_W (PSEUDO_CONST_KBD(RR_NO_KEY,1)),(PSEUDO_CONST_KBD(RR_NO_KEY,3))
#define RR_WITH (PSEUDO_CONST_KBD(5,RR_NO_KEY)),(PSEUDO_CONST_KBD(3,RR_NO_KEY))
#define RR_X (PSEUDO_CONST_KBD(5,2)),(PSEUDO_CONST_KBD(5,1))
#define RR_Y (PSEUDO_CONST_KBD(4,5)),(PSEUDO_CONST_KBD(5,3))
#define RR_Z (PSEUDO_CONST_KBD(3,1)),(PSEUDO_CONST_KBD(5,1))
#define TI89_CLASSIC (TI89&&HW_VERSION<3)
#define TI89_TITANIUM (TI89&&HW_VERSION>=3)
#define TI89 (CALCULATOR==0)
#define TI92PLUS (CALCULATOR==1)
#define V200 (CALCULATOR==3)
#define PSEUDO_CONST_CALC(calc92,calc89,calc92plus,calcv200) (CALCULATOR?((CALCULATOR==1)?(calc92plus):(calcv200)):(calc89))
#define PSEUDO_CONST_KBD(kbd89,kbd92) PSEUDO_CONST_CALC(kbd92,kbd89,kbd92,kbd92)
#define PSEUDO_CONST_SCREEN(screen89,screen92) PSEUDO_CONST_CALC(screen92,screen89,screen92,screen92)
/* End Auto-Generated Part */

#ifdef DOORS
#define _CALCULATOR _ram_call (0,const unsigned char*)
#define HW_VERSION ((unsigned short)(_CALCULATOR[1]))
#else /* !DOORS */
unsigned short __get_hw_version(void) __attribute__((const));
#define HW_VERSION (__get_hw_version())
#endif /* !DOORS */

#if defined (_TI89_ONLY)
#define ROM_base ((void*)(((unsigned long)__jmp_tbl)&0xE00000))
#define CALCULATOR 0
#elif defined (_TI92PLUS_ONLY)
#define ROM_base ((void*)0x400000)
#define CALCULATOR 1
#elif defined (_V200_ONLY)
#define ROM_base ((void*)(((unsigned long)__jmp_tbl)&0xE00000))
#define CALCULATOR 3
#else /* not only one calculator */

#ifdef DOORS

#define ROM_base _ram_call (3,const void*)
/* PreOs 0.70 says CALCULATOR is -1 on the Titanium. We don't. */
#define CALCULATOR ((signed char)_CALCULATOR[0]>0?_CALCULATOR[0]:0)

#undef LCD_WIDTH
#define LCD_WIDTH _ram_call (1,unsigned long)
#undef LCD_HEIGHT
#define LCD_HEIGHT _ram_call (2,unsigned long)
#undef LCD_LINE_BYTES
#define LCD_LINE_BYTES _ram_call (4,unsigned long)

#undef KEY_LEFT
#define KEY_LEFT _ram_call (5,unsigned long)
#undef KEY_RIGHT
#define KEY_RIGHT _ram_call (6,unsigned long)
#undef KEY_UP
#define KEY_UP _ram_call (7,unsigned long)
#undef KEY_DOWN
#define KEY_DOWN _ram_call (8,unsigned long)
#undef KEY_UPRIGHT
#define KEY_UPRIGHT _ram_call (9,unsigned long)
#undef KEY_DOWNLEFT
#define KEY_DOWNLEFT _ram_call (A,unsigned long)
#undef KEY_DIAMOND
#define KEY_DIAMOND _ram_call (B,unsigned long)
#undef KEY_SHIFT
#define KEY_SHIFT _ram_call (D,unsigned long)

#else /* !DOORS */

#define ROM_base ((void*)(((unsigned long)__jmp_tbl)&0xE00000))

#ifdef NO_CALC_DETECT

#ifdef USE_V200
#define CALCULATOR (ROM_base==(void*)0x400000?1:(((unsigned char*)(_rom_call_addr(2F)))[2]>=200?3:0))
#else
#define CALCULATOR (ROM_base==(void*)0x400000)
#endif

#else /* !NO_CALC_DETECT */

extern const short __calculator;
#ifdef USE_TI89
#define CALCULATOR (__calculator)
#else /* !USE_TI89 */
/* Special optimization: __calculator cannot be 0 */
#define CALCULATOR (__calculator==3?3:1)
#endif /* !USE_TI89 */

#endif /* !NO_CALC_DETECT */

#endif /* !DOORS */

#ifdef OPTIMIZE_CALC_CONSTS
#undef PSEUDO_CONST_CALC
#define PSEUDO_CONST_CALC(calc92,calc89,calc92plus,calcv200) ({extern void __ld_calc_const_##calc92##_##calc89##_##calc92plus##_##calcv200;(unsigned short)&__ld_calc_const_##calc92##_##calc89##_##calc92plus##_##calcv200;})
#endif /* OPTIMIZE_CALC_CONSTS */

#endif /* not only one calculator */

#pragma GCC poison TI92
//#define TI92 (CALCULATOR==2)

#endif
