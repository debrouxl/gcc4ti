#ifndef NOSTUB
#define NOSTUB

#ifdef DOORS

#error "nostub.h" must not be included in "Doors" mode!
#undef NOSTUB

#else

#include <default.h>

#ifndef _GENERIC_ARCHIVE

asm (".xdef _tigcc_native");

_INCLUDE_PATCH(__nostub);

#if defined (USE_FLINE_ROM_CALLS) || defined (KERNEL_FORMAT_ROM_CALLS) || defined (COMPRESSED_FORMAT_ROM_CALLS) || defined (MLINK_FORMAT_ROM_CALLS)
#include <romsymb.h>
#ifdef USE_FLINE_ROM_CALLS
// Kernel-mode ROM calls as introduced by romsymb.h should be avoided.
#undef _rom_call_addr
#define _rom_call_addr(index) (__jmp_tbl[0x##index])
#else
#undef OPTIMIZE_ROM_CALLS
#endif
#endif

#ifdef OPTIMIZE_ROM_CALLS
_INCLUDE_PATCH(__optimize_rom_calls);
#undef __jmp_tbl
register void **__jmp_tbl asm("a5");
#endif

#ifdef SAVE_SCREEN
_INCLUDE_PATCH(__save_screen);
#endif

#ifdef KERNEL_FORMAT_RELOCS
_INCLUDE_PATCH(__kernel_format_relocs);
#endif

#ifdef COMPRESSED_FORMAT_RELOCS
_INCLUDE_PATCH(__compressed_format_relocs);
#endif

#ifdef MLINK_FORMAT_RELOCS
_INCLUDE_PATCH(__mlink_format_relocs);
#endif

#ifndef MERGE_BSS
#ifdef COMPRESSED_FORMAT_BSS
_INCLUDE_PATCH(__compressed_format_bss);
#else
#ifdef MLINK_FORMAT_BSS
_INCLUDE_PATCH(__mlink_format_bss);
#else
_INCLUDE_PATCH(__kernel_format_bss);
#endif
#endif
#endif

#ifdef COMPRESSED_FORMAT_ROM_CALLS
_INCLUDE_PATCH(__compressed_format_rom_calls);
#else
#ifdef MLINK_FORMAT_ROM_CALLS
_INCLUDE_PATCH(__mlink_format_rom_calls);
#else
_INCLUDE_PATCH(__kernel_format_rom_calls);
#endif
#endif

#ifdef RETURN_VALUE                /* A RETURN_VALUE option is present */

#define __str(x) #x                /* A set of nasty hacks with preprocessor */
#define __xstr(x) __str(x)         /* based on some very specific features   */
#define __var(x) x##1
#define __xvar(x) __var(x)

#if __xvar(RETURN_VALUE)           /* Check whether RETURN_VALUE is blank */
_INCLUDE_PATCH(__nostub_retval);
#else                              /* A variable is assigned to RETURN_VALUE */
_INCLUDE_PATCH(__nostub_retval_var);
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

#undef __str                       /* Erase temporary macros */
#undef __xstr
#undef __var
#undef __xvar

#endif

#ifdef ENABLE_ERROR_RETURN
_INCLUDE_PATCH(__special_error_return_support);
#if MIN_AMS<200
_INCLUDE_PATCH(__special_error_return_support_ams_1);
#endif
#endif

/* Support for the _nostub data extension (comment etc.) format: */
#define _comment        _nostub_data__0000
#define _program_name   _nostub_data__0001
#define _version_string _nostub_data__0002
#define _version_number _nostub_data__0003
#define _bw_icon        _nostub_data__0004
#define _grayscale_icon _nostub_data__0005
#define _incompat_flags _nostub_data__0006
#define _authors        _nostub_data__0007

#define __TEXTSEC__ __attribute__((__section__(".text")))

#ifdef COMMENT_STRING
__TEXTSEC__ char _comment[] = (COMMENT_STRING);
#endif

#ifdef COMMENT_PROGRAM_NAME
__TEXTSEC__ char _program_name[] = (COMMENT_PROGRAM_NAME);
#endif

#ifdef COMMENT_VERSION_STRING
__TEXTSEC__ char _version_string[] = (COMMENT_VERSION_STRING);
#endif

#ifdef COMMENT_VERSION_NUMBER
__TEXTSEC__ struct __attribute__((__aligned__(2))) {unsigned char major,minor,revision,subrev;} _version_number = {COMMENT_VERSION_NUMBER};
#endif

#ifdef COMMENT_BW_ICON
__TEXTSEC__ short _bw_icon[16] = COMMENT_BW_ICON;
#endif

#ifdef COMMENT_GRAY_ICON
__TEXTSEC__ short _grayscale_icon[2][16] = {COMMENT_GRAY_ICON};
#endif

#ifdef INCOMPAT_CREATES_HANDLES
#define COMMENT_INCOMPAT_CREATES_HANDLES 0x01
#else
#define COMMENT_INCOMPAT_CREATES_HANDLES 0
#endif

#ifdef INCOMPAT_USES_TRAPS
#define COMMENT_INCOMPAT_USES_TRAPS 0x02
#else
#define COMMENT_INCOMPAT_USES_TRAPS 0
#endif

#ifdef INCOMPAT_USES_VECTORS
#define COMMENT_INCOMPAT_USES_VECTORS 0x04
#else
#define COMMENT_INCOMPAT_USES_VECTORS 0
#endif

#ifdef INCOMPAT_USES_EV_HOOK
#define COMMENT_INCOMPAT_USES_EV_HOOK 0x08
#else
#define COMMENT_INCOMPAT_USES_EV_HOOK 0
#endif

#ifdef INCOMPAT_NEEDS_ALL_STACK
#define COMMENT_INCOMPAT_NEEDS_ALL_STACK 0x10
#else
#define COMMENT_INCOMPAT_NEEDS_ALL_STACK 0
#endif

#define COMMENT_INCOMPAT_FLAGS (COMMENT_INCOMPAT_CREATES_HANDLES | COMMENT_INCOMPAT_USES_TRAPS | COMMENT_INCOMPAT_USES_VECTORS | COMMENT_INCOMPAT_USES_EV_HOOK | COMMENT_INCOMPAT_NEEDS_ALL_STACK)

#if COMMENT_INCOMPAT_FLAGS
__TEXTSEC__ unsigned long _incompat_flags = (COMMENT_INCOMPAT_FLAGS);
#endif

#ifdef COMMENT_AUTHORS
__TEXTSEC__ char _authors[] = (COMMENT_AUTHORS);
#endif

#endif

#endif

#endif
