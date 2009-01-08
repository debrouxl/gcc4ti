#ifndef __SETJMP
#define __SETJMP

#include <default.h>

/* Begin Auto-Generated Part */
typedef struct{unsigned long D2,D3,D4,D5,D6,D7;unsigned long A2,A3,A4,A5,A6,A7;unsigned long PC;}JMP_BUF[1];
#define longjmp _rom_call_attr(void,(void*,short),__attribute__((__noreturn__)),267)
#define setjmp _rom_call(short,(void*),266)
/* End Auto-Generated Part */

#define jmp_buf JMP_BUF

#endif
