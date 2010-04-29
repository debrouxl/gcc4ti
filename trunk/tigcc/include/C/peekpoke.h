#ifndef __PEEKPOKE
#define __PEEKPOKE

#include <default.h>

/* Begin Auto-Generated Part */
#define peek_bit(addr,bit) (!!(*((unsigned char*)(long)(addr))&(1<<(bit))))
#define peek_l(addr) (*((unsigned long*)(long)(addr)))
#define peek_w(addr) (*((unsigned short*)(long)(addr)))
#define peek(addr) (*((unsigned char*)(long)(addr)))
#define peekIO_bit(port,bit) (!!(*((volatile unsigned char*)(long)(port))&(1<<(bit))))
#define peekIO_w(port) (*((volatile unsigned short*)(long)(port)))
#define peekIO(port) (*((volatile unsigned char*)(long)(port)))
#define poke_bchg(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bchg.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define poke_bclr(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bclr.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define poke_bset(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bset.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define poke_l(addr,val) (void)(*((unsigned long*)(long)(addr))=(val))
#define poke_w(addr,val) (void)(*((unsigned short*)(long)(addr))=(val))
#define poke(addr,val) (void)(*((unsigned char*)(long)(addr))=(val))
#define pokeIO_bchg(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bchg.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define pokeIO_bclr(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bclr.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define pokeIO_bset(__dest_addr__,__bit_nr__) ({register char *__addr_temp__=(char *)(__dest_addr__);asm("bset.b %1,%0":"+dm"(*__addr_temp__):"di"(__bit_nr__));})
#define pokeIO_w(port,val) (void)(*((volatile unsigned short*)(long)(port))=(val))
#define pokeIO(port,val) (void)(*((volatile unsigned char*)(long)(port))=(val))
#define speek_l(addr) (*((signed long*)(long)(addr)))
#define speek_w(addr) (*((signed short*)(long)(addr)))
#define speek(addr) (*((signed char*)(long)(addr)))
/* End Auto-Generated Part */

#endif
