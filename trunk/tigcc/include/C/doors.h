#ifndef DOORS
#define DOORS

#ifdef NOSTUB

#error "doors.h" must not be included in "nostub" mode!
#undef DOORS

#else

#include <default.h>
#define __REDEFINE_ALL_ROM_CALLS
#include <romsymb.h>

/* Begin Auto-Generated Part */
#define _ram_call_addr(ind) (&_RAM_CALL_##ind)
#define _ram_call(ind,type) ((type)(&_RAM_CALL_##ind))
/* End Auto-Generated Part */

extern void
  _RAM_CALL_0,_RAM_CALL_1,_RAM_CALL_2,_RAM_CALL_3,
  _RAM_CALL_4,_RAM_CALL_5,_RAM_CALL_6,_RAM_CALL_7,
  _RAM_CALL_8,_RAM_CALL_9,_RAM_CALL_A,_RAM_CALL_B,
  _RAM_CALL_C,_RAM_CALL_D,_RAM_CALL_E,_RAM_CALL_F,
  _RAM_CALL_10,_RAM_CALL_11,_RAM_CALL_12,_RAM_CALL_13,
  _RAM_CALL_14,_RAM_CALL_15,_RAM_CALL_16,_RAM_CALL_17,
  _RAM_CALL_18,_RAM_CALL_19,_RAM_CALL_1A,_RAM_CALL_1B,
  _RAM_CALL_1C,_RAM_CALL_1D,_RAM_CALL_1E,_RAM_CALL_1F;

#ifndef _GENERIC_ARCHIVE

_INCLUDE_PATCH(__kernel);

#ifdef USE_PREOS_COMPRESSED_TABLES
_INCLUDE_PATCH(__preos_headers);
#endif

#ifdef RETURN_VALUE

#define __str(x) #x             /* A set of nasty hacks with preprocessor */
#define __xstr(x) __str(x)      /* based on some very specific features   */
#define __var(x) x##1
#define __xvar(x) __var(x)

#if __xvar(RETURN_VALUE)
_INCLUDE_PATCH(__doors_retval);
#else
_INCLUDE_PATCH(__doors_retval_var);
asm("\n"
"	/* RETURN_VALUE <variable> Extra Support */\n"
"	.xdef __retval_var_name__\n"
"	.byte 0\n"
"	.ascii \""__xstr(RETURN_VALUE)"\"\n"
"__retval_var_name__:\n"
"	.byte 0\n"
"	.even\n"
"	");
#endif

#undef __str
#undef __xstr
#undef __var
#undef __xvar
    
#endif

#if defined(ENABLE_ERROR_RETURN)
__INCLUDE_PATCH(__special_error_return_support);
#endif

#ifdef COMMENT_STRING
char _comment[] = (COMMENT_STRING);
#endif

#endif

#endif

#endif
