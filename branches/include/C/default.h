#ifndef __DEFAULT_H
#define __DEFAULT_H

#define asm __asm__ /* for ANSI mode compiling */

#define _main __main

#define __ATTR_STD__ __attribute__((__stkparm__))
#define __ATTR_STD_NORETURN__ __attribute__((__stkparm__,__noreturn__))
#define CALLBACK __ATTR_STD__
#define __ATTR_TIOS__ __ATTR_STD__
#define __ATTR_TIOS_NORETURN__ __ATTR_STD_NORETURN__
#define __ATTR_TIOS_CALLBACK__ CALLBACK
#define __ATTR_GCC__ 
#define __ATTR_LIB_C__ __attribute__((__regparm__(4)))
#define __ATTR_LIB_ASM__ __ATTR_STD__
#define __ATTR_LIB_ASM_NORETURN__ __ATTR_STD_NORETURN__
#define __ATTR_LIB_CALLBACK_C__ CALLBACK
#define __ATTR_LIB_CALLBACK_ASM__ CALLBACK

#ifndef FARGO

#ifdef USE_TI92P
#define USE_TI92PLUS
#endif

#ifdef USE_TI89
asm (".xdef _ti89");
asm (".xdef _ti89ti");
#if !defined (USE_TI92PLUS) && !defined (USE_V200)
#define _TI89_ONLY
#define _ONE_CALC_ONLY
#endif
#endif

#ifdef USE_TI92PLUS
asm (".xdef _ti92plus");
#if !defined (USE_TI89) && !defined (USE_V200)
#define _TI92PLUS_ONLY
#define _ONE_CALC_ONLY
#endif
#endif

#ifdef USE_V200
asm (".xdef _v200");
#if !defined (USE_TI89) && !defined (USE_TI92PLUS)
#define _V200_ONLY
#define _ONE_CALC_ONLY
#endif
#endif

#if !defined (USE_TI89) && !defined (USE_TI92PLUS) && !defined (USE_V200)
/* The program uses its own symbols to control the linker.
   So support all calcs. */
#define USE_TI89
#define USE_TI92PLUS
#define USE_V200
#endif

#if defined (USE_TI89) && defined (USE_TI92PLUS) && defined (USE_V200)
#define _SUPPORT_ALL_CALCS
#endif

#ifdef _NO_INCLUDE_PATCH
#define _INCLUDE_PATCH(patchname) 
#else
#define _INCLUDE_PATCH(patchname) asm(".xdef __ref_all_"#patchname)
#endif

/* Quick hacks for better archive support. */
#ifdef _GENERIC_ARCHIVE
#ifndef NO_CALC_DETECT
#define NO_CALC_DETECT
#endif
#undef OPTIMIZE_ROM_CALLS
#undef USE_FLINE_ROM_CALLS
#undef USE_FLINE_JUMPS
#undef USE_4_BYTE_FLINE_JUMPS
#undef USE_INTERNAL_FLINE_EMULATOR
#undef KERNEL_FORMAT_RELOCS
#undef KERNEL_FORMAT_ROM_CALLS
#undef KERNEL_FORMAT_BSS
#undef COMPRESSED_FORMAT_RELOCS
#undef COMPRESSED_FORMAT_ROM_CALLS
#undef COMPRESSED_FORMAT_BSS
#undef MLINK_FORMAT_RELOCS
#undef MLINK_FORMAT_ROM_CALLS
#undef MLINK_FORMAT_BSS
#ifndef MIN_AMS
#define MIN_AMS 100
#endif
#else
_INCLUDE_PATCH(__startup_code);
#ifndef MIN_AMS
#define MIN_AMS 101
#endif
#endif

#ifdef EXECUTE_IN_GHOST_SPACE
#if defined(USE_KERNEL) || defined(DOORS)
#error EXECUTE_IN_GHOST_SPACE does not work in kernel mode yet
#endif
_INCLUDE_PATCH(__execute_in_ghost_space);
#endif

#ifndef NO_CALC_DETECT
#if !defined (_SUPPORT_ALL_CALCS)
#define _NEED_CALC_DETECT
 _INCLUDE_PATCH(__detect_calc);
#ifdef _ONE_CALC_ONLY
  _INCLUDE_PATCH(__test_for_specific_calc);
#ifdef USE_TI89
   _INCLUDE_PATCH(__test_for_89);
#endif
#ifdef USE_TI92PLUS
   _INCLUDE_PATCH(__test_for_92p);
#endif
#ifdef USE_V200
   _INCLUDE_PATCH(__test_for_v200);
#endif
#else
  _INCLUDE_PATCH(__test_against_specific_calc);
#ifndef USE_TI89
   _INCLUDE_PATCH(__test_for_89);
#endif
#ifndef USE_TI92PLUS
   _INCLUDE_PATCH(__test_for_92p);
#endif
#ifndef USE_V200
   _INCLUDE_PATCH(__test_for_v200);
#endif
#endif
#elif defined (OPTIMIZE_CALC_CONSTS)
#define _NEED_CALC_DETECT
 _INCLUDE_PATCH(__detect_calc);
 _INCLUDE_PATCH(__test_for_specific_calc);
 _INCLUDE_PATCH(__test_for_any_calc);
#endif
#endif

#if (!defined (NO_AMS_CHECK)) && ((MIN_AMS>101 && (defined (USE_TI89))) || (MIN_AMS>100 && (defined (USE_TI92PLUS))) || (MIN_AMS>206 && (defined (USE_V200))))
#define _NEED_AMS_CHECK
 _INCLUDE_PATCH(__MIN_AMS_required);
#if (MIN_AMS>=300)
  _INCLUDE_PATCH(__MIN_AMS_3_00);
#if (MIN_AMS>300)
#warning Cannot check for AMS versions greater than 3.00
#endif
#elif (MIN_AMS>=209)
  _INCLUDE_PATCH(__MIN_AMS_2_09);
#if (MIN_AMS>209)
#warning Cannot check for AMS versions between 2.09 and 3.00
#endif
#elif (MIN_AMS>=208)
  _INCLUDE_PATCH(__MIN_AMS_2_08);
#elif (MIN_AMS>=207)
  _INCLUDE_PATCH(__MIN_AMS_2_07);
#elif (MIN_AMS>=205)
  _INCLUDE_PATCH(__MIN_AMS_2_05);
#if (MIN_AMS>205)
#warning Cannot check for AMS version 2.06
#endif
#elif (MIN_AMS>=204)
  _INCLUDE_PATCH(__MIN_AMS_2_04);
#elif (MIN_AMS>=203)
  _INCLUDE_PATCH(__MIN_AMS_2_03);
#elif (MIN_AMS>=202)
  _INCLUDE_PATCH(__MIN_AMS_2_02);
#elif (MIN_AMS>=201)
  _INCLUDE_PATCH(__MIN_AMS_2_01);
#elif (MIN_AMS>=200)
  _INCLUDE_PATCH(__MIN_AMS_2_00);
#elif (MIN_AMS>=105)
  _INCLUDE_PATCH(__MIN_AMS_1_05);
#if (MIN_AMS>105)
#warning Cannot check for AMS versions between 1.05 and 2.00
#endif
#elif (MIN_AMS>=101)
  _INCLUDE_PATCH(__MIN_AMS_1_01);
#if (MIN_AMS>101)
#warning Cannot check for AMS versions between 1.01 and 1.05
#endif
#endif
#endif

/* The following auto-generated definitions are preliminary and possibly overridden later by other files. */

#define __rom_call_addr_concat _rom_call_addr_concat

/* Begin Auto-Generated Part */
#define MIN_AMS_MAJOR (MIN_AMS/100)
#define MIN_AMS_MINOR (MIN_AMS%100)
#define __jmp_tbl (*(void***)0xC8)
#define _rom_call_addr_concat(intindex,romindex) (__jmp_tbl[intindex])
#define _rom_call_addr_hack_concat(intindex,romindex,addr,minams,minindex) (MIN_AMS >= (minams) || TIOS_entries > (minindex) ? _rom_call_addr_concat (intindex, romindex) : ({ asm (".xdef __ref_all___reject_unofficial_os"); (void *) (addr); }))
#define _rom_call_addr_hack(index,addr,minams) (_rom_call_addr_hack_concat(0x##index,_ROM_CALL_##index,(addr),(minams),0x##index))
#define _rom_call_addr(index) (_rom_call_addr_concat(0x##index,_ROM_CALL_##index))
#define _rom_call_attr(type,args,attr,index) (*({typedef __ATTR_TIOS__ attr type(*__temp__type__)args;(__temp__type__)(__rom_call_addr_concat(0x##index,_ROM_CALL_##index));}))
#define _rom_call_concat(type,args,intindex,romindex) (*((type(*__ATTR_TIOS__)args)(__rom_call_addr_concat(intindex,romindex))))
#define _rom_call_hack_attr_concat(type,args,attr,intindex,romindex,addr,minams,minindex) (*({ typedef __ATTR_TIOS__ attr type (*__temp__type__hack__) args; (__temp__type__hack__) (MIN_AMS >= (minams) || TIOS_entries > (minindex) ? (const void *) (&(_rom_call_concat (void, (void), (intindex), (romindex)))) : ({ asm (".xdef __ref_all___reject_unofficial_os"); (const void *) (addr); })); }))
#define _rom_call_hack_attr(type,args,attr,index,addr,minams) (_rom_call_hack_attr_concat(type,args,attr,0x##index,_ROM_CALL_##index,(addr),(minams),0x##index))
#define _rom_call_hack(type,args,index,addr,minams) (_rom_call_hack_attr_concat(type,args,,0x##index,_ROM_CALL_##index,(addr),(minams),0x##index))
#define _rom_call(type,args,index) (_rom_call_concat(type,args,0x##index,_ROM_CALL_##index))
#define import_binary(filename,symname) asm(".globl "#symname"\n"#symname":\n\t.incbin \""filename"\"")
/* End Auto-Generated Part */

#define TIOS_entries (*(unsigned long*)(__jmp_tbl-1))

#ifdef UNOFFICIAL_OS_SUPPORT
#undef _rom_call_hack_attr_concat
#define _rom_call_hack_attr_concat(type,args,attr,intindex,romindex,addr,minams) (*({typedef __ATTR_TIOS__ attr type(*__temp__type__hack__)args;(__temp__type__hack__)(MIN_AMS>=(minams)||TIOS_entries>(intindex)?(const void*)(&(_rom_call_concat(void,(void),(intindex),(romindex)))):(const void*)__invalid_rom_call_hack);}))
#undef _rom_call_addr_hack_concat
#define _rom_call_addr_hack_concat(intindex,romindex,addr,minams,minindex) (MIN_AMS>=(minams)||TIOS_entries>(minindex)?_rom_call_addr_concat(intindex,romindex):__invalid_rom_call_hack)
extern void *__invalid_rom_call_hack;
#endif

#ifdef REJECT_UNOFFICIAL_OS
_INCLUDE_PATCH(__reject_unofficial_os);
#endif

#ifdef USE_INTERNAL_FLINE_EMULATOR
_INCLUDE_PATCH(__fline_internal_emulator);
#else
#if defined (USE_4_BYTE_FLINE_JUMPS)
#error You need to define USE_INTERNAL_FLINE_EMULATOR for 4-byte F-Line jumps
#elif defined (USE_FLINE_ROM_CALLS) || defined (USE_FLINE_JUMPS)
#if (MIN_AMS < 204) && !defined (USE_FLINE_EMULATOR)
#error You need to define USE_[INTERNAL_]FLINE_EMULATOR or an appropriate value for MIN_AMS
#endif
#endif
#endif

#ifdef USE_FLINE_ROM_CALLS
_INCLUDE_PATCH(__fline_rom_calls);
asm(".set _F_LINE,0xF800");
#endif
#ifdef USE_4_BYTE_FLINE_JUMPS
_INCLUDE_PATCH(__fline_jumps_4byte);
asm(".xdef __ld_use_4byte_fline_jumps");
#else
#ifdef USE_FLINE_JUMPS
_INCLUDE_PATCH(__fline_jumps);
asm(".xdef __ld_use_fline_jumps");
#endif
#endif

asm(".set _A_LINE,0xA000");

#define __need_in_use_bit _INCLUDE_PATCH(__set_file_in_use_bit);

#ifdef SET_FILE_IN_USE_BIT
__need_in_use_bit;
#endif

#ifndef _GENERIC_ARCHIVE
#ifdef COMPRESSED_FORMAT_DATA_VAR
_INCLUDE_PATCH(__compressed_format_data_var);
#else
#ifdef MLINK_FORMAT_DATA_VAR
_INCLUDE_PATCH(__mlink_format_data_var);
#else
_INCLUDE_PATCH(__kernel_format_data_var);
#endif
#endif
#endif

#if defined (DOORS) || defined (USE_KERNEL)

#if MIN_AMS >= 200
#define AMS_1xx 0
#else
#define AMS_1xx ((_ram_call(14,unsigned short)&0x0F00)==0x100)
#endif
#if MIN_AMS >= 300
#define AMS_2xx 0
#else
#define AMS_2xx ((_ram_call(14,unsigned short)&0x0F00)==0x200)
#endif
#define AMS_3xx ((_ram_call(14,unsigned short)&0x0F00)==0x300)

#else

#if MIN_AMS >= 200
#define AMS_1xx 0
#else
#define AMS_1xx (TIOS_entries<1000)
#endif
#if MIN_AMS >= 300
#define AMS_2xx 0
#define AMS_3xx 1
#else
#define AMS_2xx (!AMS_1xx && TIOS_entries<0x608)
#define AMS_3xx (TIOS_entries>=0x608)
#endif

#endif

#endif

#ifdef OMIT_BSS_INIT
asm(".xdef __ld_omit_bss_init");
#endif

#ifndef __PINT
#define __PINT

#ifdef __INT_SHORT__

typedef union
  {
    short *__sp;
    int *__ip;
#ifndef STRICT_POINTERS
    unsigned short *__usp;
    unsigned int *__uip;
#endif
  } __pshort __attribute__((__transparent_union__));

typedef union
  {
    unsigned short *__sp;
    unsigned int *__ip;
#ifndef STRICT_POINTERS
    short *__ssp;
    int *__sip;
#endif
  } __pushort __attribute__((__transparent_union__));

typedef long *__plong;
typedef unsigned long *__pulong;

typedef union
  {
    const short *__sp;
    const int *__ip;
#ifndef STRICT_POINTERS
    const unsigned short *__usp;
    const unsigned int *__uip;
#endif
  } __cpshort __attribute__((__transparent_union__));

typedef union
  {
    const unsigned short *__sp;
    const unsigned int *__ip;
#ifndef STRICT_POINTERS
    const short *__ssp;
    const int *__sip;
#endif
  } __cpushort __attribute__((__transparent_union__));

typedef const long *__cplong;
typedef const unsigned long *__cpulong;

#else

typedef short *__pshort;
typedef unsigned short *__pushort;

typedef union
  {
    long *__lp;
    int *__ip;
#ifndef STRICT_POINTERS
    unsigned long *__ulp;
    unsigned int *__uip;
#endif
  } __plong __attribute__((__transparent_union__));

typedef union
  {
    unsigned long *__lp;
    unsigned int *__ip;
#ifndef STRICT_POINTERS
    long *__slp;
    int *__sip;
#endif
  } __pulong __attribute__((__transparent_union__));

typedef const short *__cpshort;
typedef const unsigned short *__cpushort;

typedef union
  {
    const long *__lp;
    const int *__ip;
#ifndef STRICT_POINTERS
    const unsigned long *__ulp;
    const unsigned int *__uip;
#endif
  } __cplong __attribute__((__transparent_union__));

typedef union
  {
    const unsigned long *__lp;
    const unsigned int *__ip;
#ifndef STRICT_POINTERS
    const long *__slp;
    const int *__sip;
#endif
  } __cpulong __attribute__((__transparent_union__));

#endif

#endif

extern float __BC()__ATTR_LIB_ASM__;
#define _tios_float_1(f,x,t) ({typedef float(*__temp__type__)(short,t)__ATTR_LIB_ASM__;((__temp__type__)__BC)(4*0x##f,x);})
#define _tios_float_2(f,x,y,t1,t2) ({typedef float(*__temp__type__)(short,t1,t2)__ATTR_LIB_ASM__;((__temp__type__)__BC)(4*0x##f,x,y);})

#if !defined (NOSTUB) && !defined (DOORS) && !defined (FARGO)
#ifdef USE_KERNEL
#include <doors.h>
#else
#include <nostub.h>
#endif
#endif

#endif
