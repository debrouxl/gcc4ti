#ifndef __BASOP
#define __BASOP

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_ESQ
#define __HAVE_ESQ
typedef unsigned char ESQ;
#endif
#ifndef __HAVE_CESI
#define __HAVE_CESI
typedef const ESQ*CESI;
#endif
#ifndef __HAVE_ESI
#define __HAVE_ESI
typedef ESQ*ESI;
#endif
#define EStackIndex ESI
#define Quantum ESQ
#ifndef __HAVE_SYM_STR
#define __HAVE_SYM_STR
typedef CESI SYM_STR;
#endif
#if MIN_AMS>=101
#define did_push_to_polar ({__need_in_use_bit;_rom_call(short,(),313);})
#define push_degrees ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),314);})
#define push_indir_name ({__need_in_use_bit;_rom_call(void,(CESI),2B2);})
#define push_list_plus ({__need_in_use_bit;_rom_call(void,(CESI,CESI),3BD);})
#define push_list_times ({__need_in_use_bit;_rom_call(void,(CESI,CESI),3BE);})
#define push_matrix_product ({__need_in_use_bit;_rom_call(void,(CESI,CESI),3C5);})
#define push_pow _rom_call(void,(CESI,CESI),30F)
#define push_to_cylin ({__need_in_use_bit;_rom_call(void,(),328);})
#define push_to_sphere ({__need_in_use_bit;_rom_call(void,(),329);})
#if MIN_AMS>=200
#define push_assignment ({__need_in_use_bit;_rom_call(void,(CESI),4DD);})
#define push_dot_add ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4A0);})
#define push_dot_div ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4CD);})
#define push_dot_mult ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4A1);})
#define push_dot_sub ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4A2);})
#define push_radians ({__need_in_use_bit;_rom_call(void,(CESI),4B3);})
#define push_substitute_no_simplify _rom_call(void,(CESI,CESI,CESI),489)
#if MIN_AMS>=202
#define push_and ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5AB);})
#define push_arg_minus_1 ({__need_in_use_bit;_rom_call(void,(CESI),520);})
#define push_arg_plus_1 ({__need_in_use_bit;_rom_call(void,(CESI),51F);})
#define push_difference ({__need_in_use_bit;_rom_call(void,(CESI,CESI),51A);})
#define push_dot_exponentiate ({__need_in_use_bit;_rom_call(void,(CESI,CESI),596);})
#define push_equals ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A3);})
#define push_exponentiate ({__need_in_use_bit;_rom_call(void,(CESI,CESI),595);})
#define push_factorial ({__need_in_use_bit;_rom_call(void,(CESI),540);})
#define push_greater_than_or_equals ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A7);})
#define push_greater_than ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A5);})
#define push_less_than_or_equals ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A8);})
#define push_less_than ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A6);})
#define push_negate ({__need_in_use_bit;_rom_call(void,(CESI),524);})
#define push_not_equals ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5A4);})
#define push_not ({__need_in_use_bit;_rom_call(void,(CESI),5AA);})
#define push_or _rom_call(void,(CESI,CESI),5AC)
#define push_percent ({__need_in_use_bit;_rom_call(void,(CESI),555);})
#define push_product ({__need_in_use_bit;_rom_call(void,(CESI,CESI),521);})
#define push_ratio ({__need_in_use_bit;_rom_call(void,(CESI,CESI),526);})
#define push_square ({__need_in_use_bit;_rom_call(void,(CESI),52C);})
#define push_sum ({__need_in_use_bit;_rom_call(void,(CESI,CESI),594);})
#if MIN_AMS>=204
#define push_substitute_simplify ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),5B9);})
#define push_substitute_using_such_that ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),5BE);})
#endif
#endif
#endif
#endif
/* End Auto-Generated Part */

#endif
