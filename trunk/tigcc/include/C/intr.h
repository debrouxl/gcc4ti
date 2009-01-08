#ifndef __INTR
#define __INTR

#include <default.h>

extern void __attribute__((__may_alias__)) __dummy_handler__;

/* Begin Auto-Generated Part */
#define AUTO_INT_COUNT (LAST_AUTO_INT-FIRST_AUTO_INT+1)
#define FIRST_AUTO_INT 1
#define FIRST_TRAP 0
#define LAST_AUTO_INT 7
#define LAST_TRAP 15
#define TRAP_COUNT (LAST_TRAP-FIRST_TRAP+1)
#ifndef __HAVE_Bool
#define __HAVE_Bool
enum Bool{FALSE,TRUE};
#endif
#ifndef __HAVE_INT_HANDLER
#define __HAVE_INT_HANDLER
typedef struct __attribute__((__may_alias__)){short foo;}_DEREF_INT_HANDLER,*INT_HANDLER;
#endif
enum IntVecs{AUTO_INT_1=0x64,AUTO_INT_2=0x68,AUTO_INT_3=0x6C,AUTO_INT_4=0x70,AUTO_INT_5=0x74,AUTO_INT_6=0x78,AUTO_INT_7=0x7C,TRAP_0=0x80,TRAP_1=0x84,TRAP_2=0x88,TRAP_3=0x8C,TRAP_4=0x90,TRAP_5=0x94,TRAP_6=0x98,TRAP_7=0x9C,TRAP_8=0xA0,TRAP_9=0xA4,TRAP_10=0xA8,TRAP_11=0xAC,TRAP_12=0xB0,TRAP_13=0xB4,TRAP_14=0xB8,TRAP_15=0xBC,INT_VEC_RESET=0x04,INT_VEC_BUS_ERROR=0x08,INT_VEC_ADDRESS_ERROR=0x0C,INT_VEC_ILLEGAL_INSTRUCTION=0x10,INT_VEC_ZERO_DIVIDE=0x14,INT_VEC_CHK_INS=0x18,INT_VEC_TRAPV_INS=0x1C,INT_VEC_PRIVILEGE_VIOLATION=0x20,INT_VEC_TRACE=0x24,INT_VEC_LINE_1010=0x28,INT_VEC_LINE_1111=0x2C,INT_VEC_UNINITIALIZED_INT=0x3C,INT_VEC_SPURIOUS_INT=0x60,INT_VEC_KEY_PRESS=0x68,INT_VEC_LINK=0x70,INT_VEC_ON_KEY_PRESS=0x78,INT_VEC_STACK_OVERFLOW=0x7C,INT_VEC_INT_MASK=0x84,INT_VEC_MANUAL_RESET=0x88,INT_VEC_OFF=0x90,INT_VEC_SELF_TEST=0xA8,INT_VEC_ARCHIVE=0xAC,INT_VEC_ER_THROW=0xBC};
#define AutoInts IntVecs
#define AUTO_INT(IntNo) ((long)(IntNo)*4+0x60)
#define DisablePRG() ({ asm volatile ("bclr #3,0x600015"); })
#define DUMMY_HANDLER ((INT_HANDLER)(&__dummy_handler__))
#define EnablePRG() ({ asm volatile ("bset #3,0x600015"); })
#define ExecuteHandler(h) ({INT_HANDLER __addr=(h); asm("pea 0f;move %%sr,-(%%sp);jra (%0);0:"::"a"(__addr));})
#define GetIntVec(i) (*(INT_HANDLER*)(i))
#define IsPRGEnabled() (!!((*((volatile unsigned char*)0x600015))&0x8))
#define PRG_getRate() (((*((unsigned char*)0x600015))&0x30)>>4)
extern unsigned char PRG_getStart(void)__ATTR_LIB_C__;
#define PRG_getValue() (*((volatile unsigned char*)0x600017))
#define PRG_setRate(x) (*((unsigned char*)0x600015)=(*((unsigned char*)0x600015)&0xCF)|(((x)&0x3)<<4))
#define PRG_setStart(x) (*((volatile unsigned char*)0x600017)=(x))
#define SetIntVec(i,h) ({asm volatile ("bclr.b #2,0x600001");(void) (*(INT_HANDLER*)(i) = (h));asm volatile ("bset.b #2,0x600001");})
#define TRAP(TrapNo) ((long)(TrapNo)*4+0x80)
#define DEFINE_INT_HANDLER(name) extern _DEREF_INT_HANDLER name[]; asm(".xdef __ref_all___custom_int_handlers;"); void __##name##_body__ (void) asm( #name ); __attribute__((__interrupt_handler__)) void __##name##_body__ (void)
/* End Auto-Generated Part */

#endif
