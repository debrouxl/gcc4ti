#ifndef __BASFUNC
#define __BASFUNC

#include <default.h>

/* Begin Auto-Generated Part */
#define NULL_INDEX ((CESI)0)
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
#define push_getfold _rom_call(void,(void),317)
#define push_getkey ({__need_in_use_bit;_rom_call(void,(void),316);})
#define push_getmode _rom_call(void,(CESI),318)
#define push_gettype ({__need_in_use_bit;_rom_call(void,(CESI),319);})
#define push_instring _rom_call(void,(CESI,CESI,CESI),31A)
#define push_part ({__need_in_use_bit;_rom_call(void,(),31C);})
#define push_pttest ({__need_in_use_bit;_rom_call(void,(CESI,CESI),31D);})
#define push_pxltest ({__need_in_use_bit;_rom_call(void,(CESI,CESI),31E);})
#define push_rand _rom_call(void,(CESI),31F)
#define push_randpoly ({__need_in_use_bit;_rom_call(void,(CESI,CESI),320);})
#define push_setfold ({__need_in_use_bit;_rom_call(void,(CESI),321);})
#define push_setgraph _rom_call(void,(CESI,CESI),322)
#define push_setmode ({__need_in_use_bit;_rom_call(void,(CESI,CESI),323);})
#define push_settable _rom_call(void,(CESI,CESI),324)
#define push_str_to_expr ({__need_in_use_bit;_rom_call(void,(CESI),325);})
#define push_string _rom_call(void,(CESI),326)
#define push_switch ({__need_in_use_bit;_rom_call(void,(CESI),327);})
#if MIN_AMS>=200
#define push_approx ({__need_in_use_bit;_rom_call(void,(CESI),4F7);})
#define push_augment _rom_call(void,(CESI,CESI),496)
#define push_char _rom_call(void,(CESI),497)
#define push_coldim _rom_call(void,(CESI),498)
#define push_colnorm ({__need_in_use_bit;_rom_call(void,(CESI),499);})
#define push_cross_product ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4CE);})
#define push_cumsum ({__need_in_use_bit;_rom_call(void,(CESI),49A);})
#define push_dense_poly_eval ({__need_in_use_bit;_rom_call(void,(ESI,ESI),49C);})
#define push_determinant ({__need_in_use_bit;_rom_call(void,(CESI,CESI),49D);})
#define push_diag _rom_call(void,(CESI),49E)
#define push_dimension _rom_call(void,(CESI),49F)
#define push_dotproduct ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4A3);})
#define push_eigvc ({__need_in_use_bit;_rom_call(void,(CESI),4CF);})
#define push_eigvl ({__need_in_use_bit;_rom_call(void,(CESI),4D0);})
#define push_identity_mat _rom_call(void,(CESI),4A4)
#define push_left _rom_call(void,(CESI,CESI),4A5)
#define push_list_to_mat _rom_call(void,(CESI,CESI),4A6)
#define push_mat_to_list _rom_call(void,(CESI),4A8)
#define push_matnorm ({__need_in_use_bit;_rom_call(void,(CESI),4A7);})
#define push_mean ({__need_in_use_bit;_rom_call(void,(CESI),4A9);})
#define push_median ({__need_in_use_bit;_rom_call(void,(CESI),4AA);})
#define push_mid _rom_call(void,(CESI,CESI,CESI),4AB)
#define push_mrow ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),4AC);})
#define push_mrowadd ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),4AD);})
#define push_newlist _rom_call(void,(CESI),4AE)
#define push_newmat _rom_call(void,(CESI,CESI),4AF)
#define push_ord _rom_call(void,(CESI),4B0)
#define push_prodlist ({__need_in_use_bit;_rom_call(void,(CESI),4B2);})
#define push_randmat _rom_call(void,(CESI,CESI),4B4)
#define push_randnorm ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4B5);})
#define push_red_row_ech ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4B6);})
#define push_right _rom_call(void,(CESI,CESI),4B7)
#define push_rotate ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4B8);})
#define push_round ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4B9);})
#define push_row_echelon ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4BE);})
#define push_rowadd ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),4BA);})
#define push_rowdim ({__need_in_use_bit;_rom_call(void,(CESI),4BB);})
#define push_rownorm ({__need_in_use_bit;_rom_call(void,(CESI),4BC);})
#define push_rowswap _rom_call(void,(CESI,CESI,CESI),4BD)
#define push_sequence ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI,CESI),4BF);})
#define push_shift ({__need_in_use_bit;_rom_call(void,(CESI,CESI),4C0);})
#define push_simult ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),4C1);})
#define push_stddev ({__need_in_use_bit;_rom_call(void,(CESI),4C3);})
#define push_submat _rom_call(void,(CESI,CESI,CESI,CESI,CESI),4C4)
#define push_sumlist ({__need_in_use_bit;_rom_call(void,(CESI),4C5);})
#define push_unitv ({__need_in_use_bit;_rom_call(void,(CESI),4C8);})
#define push_variance ({__need_in_use_bit;_rom_call(void,(CESI),4C9);})
#if MIN_AMS>=202
#define did_push_anti_deriv ({__need_in_use_bit;_rom_call(short,(CESI,CESI,short),5B1);})
#define did_push_series ({__need_in_use_bit;_rom_call(short,(CESI,CESI,CESI,CESI,short),588);})
#define push_1st_derivative ({__need_in_use_bit;_rom_call(void,(CESI,CESI),5AE);})
#define push_abs ({__need_in_use_bit;_rom_call(void,(CESI),543);})
#define push_acos ({__need_in_use_bit;_rom_call(void,(CESI),537);})
#define push_acosh ({__need_in_use_bit;_rom_call(void,(CESI),53E);})
#define push_asin ({__need_in_use_bit;_rom_call(void,(CESI),536);})
#define push_asinh ({__need_in_use_bit;_rom_call(void,(CESI),53D);})
#define push_atan ({__need_in_use_bit;_rom_call(void,(CESI),538);})
#define push_atanh ({__need_in_use_bit;_rom_call(void,(CESI),53F);})
#define push_ceiling ({__need_in_use_bit;_rom_call(void,(CESI),54B);})
#define push_comb ({__need_in_use_bit;_rom_call(void,(CESI,CESI),542);})
#define push_comdenom ({__need_in_use_bit;_rom_call(void,(CESI,CESI),59B);})
#define push_conj ({__need_in_use_bit;_rom_call(void,(CESI),547);})
#define push_cos ({__need_in_use_bit;_rom_call(void,(CESI),533);})
#define push_cosh ({__need_in_use_bit;_rom_call(void,(CESI),53B);})
#define push_csolve ({__need_in_use_bit;_rom_call(void,(CESI,CESI),585);})
#define push_czeros ({__need_in_use_bit;_rom_call(void,(CESI,CESI),587);})
#define push_def_int ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),5B2);})
#define push_denominator ({__need_in_use_bit;_rom_call(void,(CESI),55B);})
#define push_desolve ({__need_in_use_bit;_rom_call(void,(CESI),58B);})
#define push_div_dif_1c ({__need_in_use_bit;_rom_call(void,(ESI,ESI,ESI),58D);})
#define push_div_dif_1f ({__need_in_use_bit;_rom_call(void,(ESI,ESI,ESI),58C);})
#define push_exp ({__need_in_use_bit;_rom_call(void,(CESI),52E);})
#define push_expand ({__need_in_use_bit;_rom_call(void,(CESI,CESI,short),59A);})
#define push_extended_prod ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),5B5);})
#define push_factor ({__need_in_use_bit;_rom_call(void,(CESI,CESI,short),59C);})
#define push_floor ({__need_in_use_bit;_rom_call(void,(CESI),54A);})
#define push_fractional_part ({__need_in_use_bit;_rom_call(void,(CESI),54E);})
#define push_gcd_numbers _rom_call(void,(CESI,CESI),514)
#define push_im ({__need_in_use_bit;_rom_call(void,(CESI),546);})
#define push_integer_gcd _rom_call(void,(CESI,CESI),551)
#define push_integer_lcm ({__need_in_use_bit;_rom_call(void,(CESI,CESI),552);})
#define push_integer_part ({__need_in_use_bit;_rom_call(void,(CESI),54D);})
#define push_integer_quotient ({__need_in_use_bit;_rom_call(void,(CESI,CESI),54F);})
#define push_integer_remainder ({__need_in_use_bit;_rom_call(void,(CESI,CESI),550);})
#define push_is_prime ({__need_in_use_bit;_rom_call(void,(CESI),515);})
#define push_lim ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),5AD);})
#define push_ln ({__need_in_use_bit;_rom_call(void,(CESI),52F);})
#define push_log10 ({__need_in_use_bit;_rom_call(void,(CESI),530);})
#define push_max1 ({__need_in_use_bit;_rom_call(void,(CESI),554);})
#define push_max2 ({__need_in_use_bit;_rom_call(void,(CESI,CESI),599);})
#define push_max ({__need_in_use_bit;_rom_call(void,(CESI,CESI),58A);})
#define push_min1 ({__need_in_use_bit;_rom_call(void,(CESI),553);})
#define push_min2 ({__need_in_use_bit;_rom_call(void,(CESI,CESI),598);})
#define push_min ({__need_in_use_bit;_rom_call(void,(CESI,CESI),589);})
#define push_mod ({__need_in_use_bit;_rom_call(void,(CESI,CESI),54C);})
#define push_nint ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),5B3);})
#define push_nsolve ({__need_in_use_bit;_rom_call(void,(CESI,CESI),583);})
#define push_nth_derivative ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI),5AF);})
#define push_numerator ({__need_in_use_bit;_rom_call(void,(CESI),55A);})
#define push_perm ({__need_in_use_bit;_rom_call(void,(CESI,CESI),541);})
#define push_phase ({__need_in_use_bit;_rom_call(void,(CESI),548);})
#define push_r_cis ({__need_in_use_bit;_rom_call(void,(CESI,CESI),549);})
#define push_re ({__need_in_use_bit;_rom_call(void,(CESI),545);})
#define push_rec_to_angle ({__need_in_use_bit;_rom_call(void,(CESI,CESI),539);})
#define push_sign ({__need_in_use_bit;_rom_call(void,(CESI),544);})
#define push_sin2 ({__need_in_use_bit;_rom_call(void,(CESI,CESI),531);})
#define push_sin ({__need_in_use_bit;_rom_call(void,(CESI),532);})
#define push_sinh ({__need_in_use_bit;_rom_call(void,(CESI),53A);})
#define push_solve ({__need_in_use_bit;_rom_call(void,(CESI,CESI),584);})
#define push_sqrt ({__need_in_use_bit;_rom_call(void,(CESI),52B);})
#define push_summation ({__need_in_use_bit;_rom_call(void,(CESI,CESI,CESI,CESI),5B4);})
#define push_tan ({__need_in_use_bit;_rom_call(void,(CESI),534);})
#define push_tanh ({__need_in_use_bit;_rom_call(void,(CESI),53C);})
#define push_when ({__need_in_use_bit;_rom_call(void,(CESI),57D);})
#define push_zeros ({__need_in_use_bit;_rom_call(void,(CESI,CESI),586);})
#endif
#endif
#endif
/* End Auto-Generated Part */

#define push_subst_no_simp push_substitute_no_simplify
#define push_nSolve push_nsolve

#endif
