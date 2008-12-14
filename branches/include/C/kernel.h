#ifndef __KERNEL_H__
#define __KERNEL_H__

#if defined(NOSTUB) || defined(KERNEL)
# error "kernel.h" must not be included with tigcclib.h
#endif

#ifdef DOORS
#warning "kernel.h" has more recent capabilities is an updated kernel header. You shouldn't use "doors.h".
#endif

#define KERNEL_NEW

/* If someone uses the main function */
// FIXME: push this, within #ifndef / #else / #endif guard, to default.h.
#define main _main

/* Select target */
// FIXME: merge with default.h.
#ifdef USE_TI89
	short	_ti89;
#endif
#if defined(USE_TI92PLUS) || defined(USE_TI92P)
	short	_ti92plus;
#endif
#ifdef USE_V200
	short	_v200;
#endif
#if !defined(USE_TI92PLUS) && !defined(USE_TI92P) && !defined(USE_TI89) && !defined(USE_V200)
	short	_ti92plus,_ti89,_v200;
#endif

/* Define default value */
// FIXME: use default.h instead of copying & pasting.
#define asm __asm__ 
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

/* Float definition */
// FIXME: use default.h instead of copying & pasting.
extern float __BC()__ATTR_LIB_ASM__;
#define _tios_float_1(f,x,t) ({typedef float(*__temp__type__)(short,t)__ATTR_LIB_ASM__;((__temp__type__)__BC)(4*0x##f,x);})
#define _tios_float_2(f,x,y,t1,t2) ({typedef float(*__temp__type__)(short,t1,t2)__ATTR_LIB_ASM__;((__temp__type__)__BC)(4*0x##f,x,y);})


/* Like in doors.h, #define to make romsymb.h redefine __rom_call_addr_concat */
#define __REDEFINE_ALL_ROM_CALLS
#include <romsymb.h>

// FIXME: use default.h instead of copying & pasting.
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

/* Kernel RAM_CALLS */
extern void
	_RAM_CALL_0,_RAM_CALL_1,_RAM_CALL_2,_RAM_CALL_3,
	_RAM_CALL_4,_RAM_CALL_5,_RAM_CALL_6,_RAM_CALL_7,
	_RAM_CALL_8,_RAM_CALL_9,_RAM_CALL_A,_RAM_CALL_B,
	_RAM_CALL_C,_RAM_CALL_D,_RAM_CALL_E,_RAM_CALL_F,
	_RAM_CALL_10,_RAM_CALL_11,_RAM_CALL_12,_RAM_CALL_13,
	_RAM_CALL_14,_RAM_CALL_21,_RAM_CALL_22,_RAM_CALL_23,
	_RAM_CALL_24,_RAM_CALL_25,_RAM_CALL_26,_RAM_CALL_27,
	_RAM_CALL_28,_RAM_CALL_2D,_RAM_CALL_2E;
#define __RAM_CALL(n,type) ((type)&_RAM_CALL_##n)

/* Define Kernel values -- pass 2 */
// FIXME: merge as much as possible with compat.h. What to do with the few DoorsOS / PreOS incompatibilities ?
#define __CALCULATOR    __RAM_CALL(0, const unsigned char*)
#define CALCULATOR	(__CALCULATOR[0])
#define HW_VERSION	(__CALCULATOR[1])
#define EMULATOR	(__CALCULATOR[3])
#define font_medium	__RAM_CALL(E, const void*)
#define font_small	__RAM_CALL(22, const void*)
#define font_large	__RAM_CALL(23, const void *)
#define	LCD_MEM		__RAM_CALL(21, void*)
#define LCD_WIDTH	__RAM_CALL (1, unsigned long)
#define LCD_HEIGHT	__RAM_CALL (2, unsigned long)
#define LCD_LINE_BYTES	__RAM_CALL (4, unsigned long)
#define LCD_SIZE	__RAM_CALL (C, unsigned long)
#define	ROM_BASE	__RAM_CALL(3, unsigned char *)
#define	RETURN_VALUE	(*(unsigned char **)_RAM_CALL_F = *(unsigned char **)_ROM_CALL_109)
#define Heap		__RAM_CALL(11, void***)
#define FOLDER_LIST_HANDLE __RAM_CALL (12, unsigned long)
#define MainHandle	__RAM_CALL (13, unsigned long)
#define ROM_VERSION	__RAM_CALL (14, unsigned long)
#define kb_globals	__RAM_CALL(10, void*)
#define KEY_PRESSED_FLAG (*(unsigned short*)(kb_globals+0x1C))
#define GETKEY_CODE	(*(unsigned short*)(kb_globals+0x1E))
#define KEY_LEFT	__RAM_CALL(5, unsigned long)
#define KEY_RIGHT	__RAM_CALL(6, unsigned long)
#define KEY_UP		__RAM_CALL(7, unsigned long)
#define KEY_DOWN	__RAM_CALL(8, unsigned long)
#define KEY_UPRIGHT	__RAM_CALL(9, unsigned long)
#define KEY_DOWNLEFT	__RAM_CALL(A, unsigned long)
#define KEY_DIAMOND	__RAM_CALL(B, unsigned long)
#define KEY_SHIFT	__RAM_CALL(D, unsigned long)
#define	ROM_BASE	__RAM_CALL(3, unsigned char *)
#define	GHOST_SPACE	__RAM_CALL(2D, unsigned char *)
#define	KERNEL_SPACE	__RAM_CALL(2E, unsigned char *)

// FIXME: use default.h instead of copying & pasting.
typedef unsigned short *__pushort;	  
typedef short *__pshort;
typedef long *__plong;
typedef unsigned long *__pulong;

// FIXME: use default.h instead of copying & pasting.
#define AMS_1xx ((_ram_call(14,unsigned short)&0x0F00)==0x100)
#define AMS_2xx ((_ram_call(14,unsigned short)&0x0F00)==0x200)
#define AMS_3xx ((_ram_call(14,unsigned short)&0x0F00)==0x300)

/* Define kernel functions */
#define	Idle		_RAM_CALL_15
#define kernel_Idle 	Idle
extern	void	Idle(void);
#define	kernel_Exec	_RAM_CALL_16
extern	short kernel_Exec(HANDLE hd asm("d0"));
#define	kernel_Ptr2Hd	_RAM_CALL_17
extern	HANDLE kernel_Ptr2Hd(void *ptr asm("a0"));
#define	kernel_Hd2Sym	_RAM_CALL_18
extern	SYM_ENTRY *kernel_Hd2Sym(HANDLE hd asm("d0"));
typedef	struct _LibRef LibRef;
#define	kernel_LibsBegin _RAM_CALL_19
LibRef *kernel_LibsBegin(char *libname asm("a0"), unsigned char version asm("d1"));
#define kernel_LibsEnd	_RAM_CALL_1A
void kernel_LibsEnd(LibRef *lib asm("a0"));
#define	kernel_LibsPtr	_RAM_CALL_1C
void *kernel_LibsPtr(LibRef *lib asm("a0"), short function asm("d0"));
#define	kernel_LibsCall	_RAM_CALL_1B
__attribute__((__stkparm__)) unsigned long kernel_LibsCall(LibRef *lib, short function, ...);
#define	kernel_LibsExec	_RAM_CALL_1D
__attribute__((__stkparm__)) void kernel_LibsExec(char *name, short function, char version, ...);
#define	kernel_HdKeep	_RAM_CALL_1E
void	kernel_HdKeep(HANDLE hd asm("d0"));
#define	kernel_ExtractFromPack	_RAM_CALL_1F
HANDLE	kernel_ExtractFromPack(void *pack asm("a5"), short index asm("d0"));
#define	kernel_ExtractFile	_RAM_CALL_20
HANDLE	kernel_ExtractFile(const char *name asm("a2"));
#define	kernel_ExtractFileFromPack	_RAM_CALL_29
HANDLE	kernel_ExtractFileFromPack(HANDLE hd asm("d0"), const char *name asm("a2"));
#define	exit	_RAM_CALL_2A
void	exit(int c asm("d0"));
#define	atexit	_RAM_CALL_2B
int	atexit(void (*func)(void) asm("a0"));
#define kernel_RegisterVector _RAM_CALL_2C
void	kernel_RegisterVector (unsigned short vect asm("d0"), const void *func asm("a0"));
#define kernel_SystemDir _RAM_CALL_2F
extern const char kernel_SystemDir[];

/* Use other TIGCC headers */
#include <alloc.h>
#include <args.h>
#include <asmtypes.h>
#include <assert.h>
#include <bascmd.h>
#include <basfunc.h>
#include <basop.h>
#include <cert.h>
// HEADER NOT INCLUED FOR NOW: compat.h (FIXME)
#include <ctype.h>
// HEADER NOT INCLUED FOR NOW: default.h (FIXME)
#include <dialogs.h>
// HEADER NOT INCLUED: dll.h
// HEADER IS INCOMPATIBLE WITH THIS ONE: doors.h
#include <error.h>
#include <estack.h>
#include <events.h>
#include <files.h>
#include <flash.h> // TODO: merge FL_getHardwareParmBlock definitions.
#include <float.h>
#include <gdraw.h>
#include <graph.h>
#include <graphing.h>
// HEADER NOT INCLUED: gray.h
#include <homescr.h>
#include <intr.h>
#include <kbd.h>
#include <limits.h>
#include <link.h>
#include <mem.h>
#include <menus.h>
// HEADER NOT INCLUED: nostub.h
#include <peekpoke.h>
#include <rsa.h>
#include <setjmp.h>
#include <sprites.h>
#include <statline.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h> // TODO: modify the #ifndef DOORS.
#include <string.h>
#include <system.h>
#include <textedit.h>
#include <timath.h>
#include <unknown.h> // HEADER NOT INCLUED ??
#include <values.h>
#include <vat.h>
//#include <version.h> HEADER NOT INCLUED MAYBE ?
#include <wingraph.h>


/* From tigcclib.h */
/* The following macros are for debugging purposes; don't use them... */

#define __MARK(s) asm(".ascii \"" #s "\"");
#define __HALT asm volatile("0:bra.s 0b")

#define __WHERE_AM_I ({register void *__p; asm("bsr 0f; 0:move.l (%%sp)+,%0":"=g"(__p)); __p;})



/* Define graphlib functions */
asm("graphlib__version02: .xdef graphlib__version02");

#define graphlib_fill graphlib__0000
void	graphlib_fill(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned short width asm("d2"), unsigned short height asm("d3"), unsigned short color asm("d4"));

#define graphlib_put_sprite graphlib__0001
void	graphlib_put_sprite(unsigned short x asm("d0"), unsigned short y asm("d1"), void *sprite asm("a0"));

#define graphlib_put_sprite2 graphlib__000C
void	graphlib_put_sprite2(unsigned short x asm("d0"), unsigned short y asm("d1"), void *sprite asm("a0"), unsigned char *mask asm("a2"));

#define graphlib_put_sprite_mask graphlib__000B
void	graphlib_put_sprite_mask(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned char mask asm("d3"), void *sprite asm("a0"));

#define graphlib_smallbox graphlib__0002
void	graphlib_smallbox(char *title asm("a0"));

#define graphlib_box graphlib__0003
void	graphlib_box(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned short width asm("d2"), unsigned short height asm("d3"), char *title asm("a0"));

#define graphlib_frame graphlib__0004
void	graphlib_frame(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned short width asm("d4"), unsigned short height asm("d5"));

#define graphlib_clrscr graphlib__0005
void	graphlib__clrscr();

#define graphlib_clrscr2 graphlib__0014
void	graphlib__clrscr2();

#define graphlib_vert graphlib__0006
void	graphlib_vert(unsigned short x asm("d0"), unsigned short y1 asm("d1"), unsigned short y2 asm("d2"));

#define graphlib_horiz graphlib__0007
void	graphlib_horiz(unsigned short x1 asm("d0"), unsigned short y asm("d1"), unsigned short x2 asm("d2"), unsigned short color asm("d3"));

#define graphlib_bigbox graphlib__0008
void	graphlib_bigbox(char *title asm("a0"));

#define _graphlib_scrtomem graphlib__0009
void	_graphlib_scrtomem(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned short width asm("d2"), unsigned short height asm("d3"));
#define graphlib_scrtomem(_x, _y, _height, _width) ({register unsigned short __d4 asm("d4"); _graphlib_scrtomem(_x, _y, _height, _width); __d4;})

#define graphlib_memtoscr graphlib__000A
void	graphlib_memtoscr(unsigned short x asm("d0"), unsigned short y asm("d1"), unsigned short width asm("d2"), unsigned short height asm("d3"), HANDLE hd asm("d4"));

#define graphlib_line graphlib__0017
void	graphlib_line(unsigned short x1 asm("d0"), unsigned short y1 asm("d1"), unsigned short x2 asm("d2"), unsigned short y2 asm("d3"), void *screen asm("a0"));

#define graphlib_choosescreen graphlib__000D
extern unsigned short graphlib_choosescreen;

typedef struct {
	unsigned short x1;
	unsigned short y1;
	unsigned short x2;
	unsigned short y2;
	unsigned short x;
	unsigned short y;
	char	*str;
	} dialog_struct;
#define graphlib_show_dialog graphlib__0015
void	graphlib_show_dialog(dialog_struct *d asm("a6"));

#define graphlib_clear_dialog graphlib__0016
void	graphlib_clear_dialog();

#define graphlib_erase_rect graphlib__0018
void	graphlib_erase_rect(void *r);

#define graphlib_frame_rect graphlib__0019
void	graphlib_frame_rect(void *r);

#define graphlib_gray2 graphlib__000E
void	graphlib_gray2();

#define graphlib_gray4 graphlib__000F
void	graphlib_gray4();

#define graphlib_gray7 graphlib__0010
void	graphlib_gray7();

#define graphlib_plane0 graphlib__0011
#define graphlib_plane1 graphlib__0012
#define graphlib_plane2 graphlib__0013
extern unsigned char *graphlib_plane0;
extern unsigned char *graphlib_plane1;
extern unsigned char *graphlib_plane2;

/* Define userlib functions */
asm("userlib__version02: .xdef userlib__version02");

#define	userlib_idle_loop userlib__0000
short	userlib_idle_loop();
#define userlib_random	userlib__0001
short	userlib_random(short limit asm("d0"));
#define userlib_randseed userlib__0002
extern	short	userlib__randseed;
#define userlib_exec userlib__0003
__attribute__((__stkparm__)) short	userlib_exec(HANDLE handle);
#define userlib_inputstr userlib__0006
char	*userlib_inputstr(short x asm("d1"), short y asm("d2"), short maxlen asm("d3"));
#define	userlib_smallmenu userlib__000C
short	userlib_samllmenu(short x asm("d0"), short y asm("d1"), char nbitem asm("d2"), char *str_list asm("a0"));
#define userlib_runprog userlib__0010
short	userlib_runprog(char *progname asm("a0"));

// Hexlib
asm("hexlib__version01: .xdef hexlib__version01");

#define	hexlib_put_char	hexlib__0000
void	hexlib_put_char(long x asm("d2"), long y asm("d1"), long character asm("d0"));
#define hexlib_put_bin	hexlib__0001
void	hexlib_put_bin(long x asm("d2"), long y asm("d1"), long number asm("d0"), long digits asm("d4"));
#define	hexlib_put_hex	hexlib__0002
void	hexlib_put_hex(long x asm("d2"), long y asm("d1"), long number asm("d0"), long digits asm("d4"));

// Shrnklib
asm("shrnklib__version03: .xdef shrnklib__version03");

#define	shrnklib_OpenArchive	shrnklib__0000
HANDLE	shrnklib_OpenArchive(void *archive asm("a0"));
#define	shrnklib_CloseArchive	shrnklib__0001
void	shrnklib_CloseArchive(HANDLE arch_hd asm("d0"));
#define	shrnklib_Extract	shrnklib__0002
void	*shrnklib_Extract(HANDLE arch_hd asm("d0"), short index asm("d1"), void *dest asm("a0"));
#define	shrnklib_Free(ptr) HeapFree(HeapPtrToHandle(ptr))

//Ziplib
asm("ziplib__version01: .xdef ziplib__version01");

#define	ziplib_check_cmem	ziplib__0000
short	ziplib_check_cmem(void *data asm("a0"), unsigned short len asm("d0"));
#define	ziplib_check_emem	ziplib__0001
short	ziplib_check_emem(void *archive asm("a0"));
#define	ziplib_eval_cmem	ziplib__0002
short	ziplib_eval_cmem(void *data asm("a0"), unsigned short len asm("d0"));
#define	ziplib_eval_emem	ziplib__0003
short	ziplib_eval_emem(void *archive asm("a0"));
#define	ziplib_compress	ziplib__0004
short	ziplib_compress(void *data asm("a0"), unsigned short len asm("d0"), void *dest asm("a1"));
#define	ziplib_extract	ziplib__0005
short	ziplib_extract(void *archive asm("a0"), void *dest asm("a1"));
#define	ziplib_zipfile	ziplib__0006
char	ziplib_zipfile(SYM_ENTRY *sym asm("a0"), char comment asm("d0"));
#define	ziplib_iscomp	ziplib__000B
short	ziplib_iscomp(SYM_ENTRY *sym asm("a0"));
#define	_ziplib_tempfile	ziplib__0007
long long _ziplib_tempfile(SYM_ENTRY *sym asm("a0"), char comment asm("d1"));
#define	ziplib_tempfile(sym, comment) ({long long result__ = _ziplib_tempfile(sym, comment); (char)(result__>>32)?-(short)(char)(result__>>32):(short)result__;})
#define ziplib_extract_string	ziplib__0008
void	ziplib_extract_string(void *archive asm("a0"), short arch_index asm("d3"), short str_index asm("d4"), void *dest asm("a1"));
#define ziplib_write_string	ziplib__0009
void	ziplib_write_string(void *archive asm("a0"), short x asm("d0"), short y asm("d1"), short arch_index asm("d3"), short str_index asm("d4"));
#define ziplib_write_string_inv	ziplib__000A
void	ziplib_write_string_inv(void *archive asm("a0"), short x asm("d0"), short y asm("d1"), short arch_index asm("d3"), short str_index asm("d4"));

#endif

