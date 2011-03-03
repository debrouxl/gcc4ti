#define USE_TI89
#define USE_TI92P
#define USE_V200

#define KERNEL_ROM_CALLS
#define SAVE_SCREEN
#define MIN_AMS 100

#include <tigcclib.h>

//! Number of bits used for the LFSR.
#define NR_BITS (14)

//! Generator for 32-bit LFSR
#define XOR_MASK (0xA3000000)

__attribute__((__stkparm__)) unsigned long long __muldi3_old(unsigned long long x, unsigned long long y);
asm("
|muldi3 routine copyright (C) 2002, Kevin Kofler
	.xdef __muldi3_old

.text
__muldi3_old:
	move.w 18(%a7),%d0
	mulu 4(%a7),%d0
	move.w 16(%a7),%d2
	mulu 6(%a7),%d2
	add.w %d2,%d0
	move.w 14(%a7),%d2
	mulu 8(%a7),%d2
	add.w %d2,%d0
	move.w 12(%a7),%d2
	mulu 10(%a7),%d2
	add.w %d2,%d0
	swap %d0
	clr.w %d0

	move.w 18(%a7),%d2
	mulu 6(%a7),%d2
	add.l %d2,%d0
	move.w 16(%a7),%d2
	mulu 8(%a7),%d2
	add.l %d2,%d0
	move.w 14(%a7),%d2
	mulu 10(%a7),%d2
	add.l %d2,%d0

	move.w 18(%a7),%d1
	mulu 8(%a7),%d1
	swap %d1
	add.w %d1,%d0
	clr.w %d1
	swap %d0
	addx.w %d1,%d0
	swap %d0

	move.w 16(%a7),%d2
	mulu 10(%a7),%d2
	swap %d2
	add.w %d2,%d0
	clr.w %d2
	swap %d0
	addx.w %d2,%d0
	swap %d0
	add.l %d2,%d1
	moveq.l #0,%d2
	addx.l %d2,%d0

	move.w 18(%a7),%d2
	mulu 10(%a7),%d2
	add.l %d2,%d1
	moveq.l #0,%d2
	addx.l %d2,%d0

	rts
");

//! It's a main...
void _main(void) {
    unsigned long count = 0;
    unsigned long mask = XOR_MASK;
    unsigned long seed;

    // If AMS 1.xx and not PedroM, use a poor seed; otherwise, use FiftyMsecTick.
    if (AMS_1xx && *((unsigned short *)0x32) != 0x524F) {
        seed = (unsigned long)(255-peekIO(0x600017));
    }
    else {
        seed = *((volatile unsigned long *)(_rom_call_addr(4FC)));
    }

    for (;;)
    {
        unsigned long a, b, c, d;
        unsigned long long x, y, z1, z2;

        if (seed & 1) seed = (seed>>1) ^ mask;
        else          seed = seed>>1;
        a = seed;

        if (seed & 1) seed = (seed>>1) ^ mask;
        else          seed = seed>>1;
        b = seed;

        if (seed & 1) seed = (seed>>1) ^ mask;
        else          seed = seed>>1;
        c = seed;

        if (seed & 1) seed = (seed>>1) ^ mask;
        else          seed = seed>>1;
        d = seed;

        x = ((unsigned long long)a << 32) | c;
        y = ((unsigned long long)d << 32) | b;
        z1 = __muldi3_old(x, y);
        z2 = x * y;

        //printf("%lu %lX %lX %lX %lX\n", count, a, b, c, d);
        if (z1 != z2) {
            printf("%lu\n%lX\n%lX\n%lX\n%lX\n", count, a, b, c, d);
            ngetchx();
            break;
        }

        count++;
        if ((count & 0x3FF) == 0) {
            printf("%lu\n", count);
        }

        if (_keytest(RR_ESC)) goto end;
    }

end:
    // Debounce.
    asm("move.l #0x27FFF,%%d0; 0: subq.l #1,%%d0; bpl.s 0b" : : : "d0","cc");
    GKeyFlush();
}
