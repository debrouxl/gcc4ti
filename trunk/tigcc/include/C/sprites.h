#ifndef __SPRITES
#define __SPRITES

#include <default.h>

/* Begin Auto-Generated Part */
enum SprtModes{SPRT_XOR,SPRT_OR,SPRT_AND,SPRT_RPLC};
extern void Sprite8(short asm("d0"),short asm("d1"),short asm("d2"),const unsigned char* asm("a0"),void* asm("a1"),short asm("d3"))__ATTR_LIB_ASM__;
extern void Sprite16(short asm("d0"),short asm("d1"),short asm("d2"),__cpushort asm("a0"),void* asm("a1"),short asm("d3"))__ATTR_LIB_ASM__;
extern void Sprite32(short asm("d0"),short asm("d1"),short asm("d2"),__cpulong asm("a0"),void* asm("a1"),short asm("d3"))__ATTR_LIB_ASM__;
/* End Auto-Generated Part */

#endif
