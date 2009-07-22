#define USE_TI89
#define USE_TI92P
#define USE_V200

#define FLINE_ROM_CALLS
#define USE_INTERNAL_FLINE_EMULATOR
#define SAVE_SCREEN
#define MIN_AMS 100

#include <tigcclib.h>

//! Number of main loop iterations.
#define ITERATIONS (1)
//! Enable this define to test a variable sorted item count (for benchmarking with non-power of two sizes).
//#define SCALE

//! Parametrable item type, so as to make the program more generic
#define item_type uint16_t

//! Number of bits used for the LFSR.
#define NR_BITS (14)

//! Number of items
#define ITEM_COUNT (1U << NR_BITS)
//#define ITEM_COUNT ((1U << NR_BITS) + (1U << (NR_BITS - 1)))
//#define ITEM_COUNT ((1U << NR_BITS) + (1U << (NR_BITS - 1)) + (1U << (NR_BITS - 2)))
//#define ITEM_COUNT ((1U << NR_BITS) + (1U << (NR_BITS - 1)) + (1U << (NR_BITS - 2)) + (1U << (NR_BITS - 3)) + (1U << (NR_BITS - 4)))
static item_type items[ITEM_COUNT] = {0};

//! Number of items that get sorted.
#define SORTED_ITEM_COUNT (ITEM_COUNT)
static unsigned short sorted_item_count;
//#define SORTED_ITEM_COUNT ((1U << NR_BITS) - (1U << (NR_BITS - 2)))

//! LFSR generator patterns for 1 -> 15-bit LFSRs.
#if NR_BITS==1
#define XOR_MASK (0x0001)
#elif NR_BITS==2
#define XOR_MASK (0x0003)
#elif NR_BITS==3
#define XOR_MASK (0x0006)
#elif NR_BITS==4
#define XOR_MASK (0x000C)
#elif NR_BITS==5
#define XOR_MASK (0x0014)
#elif NR_BITS==6
#define XOR_MASK (0x0030)
#elif NR_BITS==7
#define XOR_MASK (0x0060)
#elif NR_BITS==8
#define XOR_MASK (0x00B8)
#elif NR_BITS==9
#define XOR_MASK (0x0110)
#elif NR_BITS==10
#define XOR_MASK (0x0240)
#elif NR_BITS==11
#define XOR_MASK (0x0500)
#elif NR_BITS==12
#define XOR_MASK (0x0CA0)
#elif NR_BITS==13
#define XOR_MASK (0x1B00)
#elif NR_BITS==14
#define XOR_MASK (0x3500)
#elif NR_BITS==15
#define XOR_MASK (0x6000)
#else
#error Incorrect number of bits.
#endif

//! Mask for the LFSR seed.
#define SEED_MASK ((1U << NR_BITS) - 1)

//! AUTO_INT_5 ticks
volatile unsigned long count = 0;
//! AUTO_INT_5 handler
DEFINE_INT_HANDLER(CountTicks)
{
    asm volatile("addq.l #1,%0" : "=g"(count));
}

//! Comparison function
CALLBACK short cmp (const void *p1, const void *p2) {
  return (*(item_type*)p1) - (*(item_type*)p2);
}

//! A shell sort with Shell's original gap sequence.
static __ATTR_LIB_C__ void old_qsort(void *list, short num_items, short size, compare_t cmp_func)
{
  unsigned short gap,byte_gap,i,j;                
  char *p,*a,*b,temp;                       
  for (gap=((unsigned short)num_items)>>1; gap>0; gap>>=1)    // Yes, this is not a quicksort,
    {                                                         // but works fast enough...    
      byte_gap=gap*(unsigned short)size;
      for(i=byte_gap; i<((unsigned short)num_items)*(unsigned short)size; i+=size)
        for(p=(char*)list+i-byte_gap; p>=(char*)list; p-= byte_gap)
          {
            a=p; b=p+byte_gap;
            if(cmp_func(a,b)<=0) break;
            for(j=size;j;j--)
              temp=*a, *a++=*b, *b++=temp;
          }
    }
}

//! A shell sort using the best known sequence of gaps.
__ATTR_LIB_C__ void ciura_qsort(void *list, short num_items, short size, compare_t cmp_func);
/*{
    // Best known gap sequence for shell sort, found by Marcin Ciura, and an additional term for larger arrays.
    static const unsigned short step[10] = {1, 4, 10, 23, 57, 132, 301, 701, 1750, 4000};
    unsigned short byte_gap,i,j;
    short k;
    char *p,*a,temp;
    for (k = 9; k >= 0; k--) {
        byte_gap=step[k]*(unsigned short)size;
        for(i=byte_gap; i<((unsigned short)num_items)*(unsigned short)size; i+=size) {
            for(p=(char*)list+i-byte_gap; p>=(char*)list; p-= byte_gap) {
                a=p;
                if(cmp_func(a,a+byte_gap)<=0) break;
                for(j=size;j;j--) {
                    temp=*a, *a=*(a+byte_gap), *(a+byte_gap)=temp; a++;
                }
            }
        }
    }
}*/
asm("
| d3 <- p
| d4 <- a+byte_gap
| d5 <- k, index in table
| d6 <- i
| d7 <- size
| a2 <- cmp_func
| a3 <- byte_gap
| a4 <- list
| a6 <- num_items * size
	.text
	.even
	.globl ciura_qsort
ciura_qsort:
	movem.l %d3-%d7/%a2-%a4/%a6,-(%sp)
	move.l %a0,%a4	;# list, list
	move.w %d1,%d7	;# size, size
	move.l %a1,%a2	;# cmp_func, cmp_func
	moveq #20,%d5
	swap %d5
	move.w .Lsteplast(%pc),%d5	;#, k
	cmp.w #16,%d0	;#, num_items
	bhi.s 4f	;#
	moveq #2,%d5
	swap %d5
	addq.w #1,%d5	;#, k
4:
	mulu.w %d7,%d0	;# size, num_items
	move.w %d0,%a6	;# num_items, num_items.61

6:
	move.w %d5,%d6	;# k, i
	mulu.w %d7,%d6	;# size, i
	move.l %d6,%d0	;# i, byte_gap
	neg.l %d0
	move.l %d0,%a3
	bra.s 7f	;#
8:
	moveq #0,%d0	;# i
	move.w %d6,%d0	;# i, i
	move.l %a4,%d3	;# list, p
	add.l %d0,%d3	;# i, p
	add.l %a3,%d3	;# D.1283, p
	move.l %d3,%d4	;# p, ivtmp.60
	sub.l %a3,%d4	;# D.1283, ivtmp.60
	bra.s 9f	;#
10:
	move.l %d4,-(%sp)	;# ivtmp.60,
	move.l %d3,-(%sp)	;# p,
	jsr (%a2)	;#
	addq.l #8,%sp	;#,
	tst.w %d0	;#
	ble.s 11f	;#
	move.l %d4,%a1	;# ivtmp.60, ivtmp.47
	move.w %d7,%d1	;# size, j
	move.l %d3,%a0	;# p, a
	subq.w #1,%d1	;#
14:
	move.b (%a0),%d0	;#* a, temp
	move.b (%a1),(%a0)+	;#* ivtmp.47,
	move.b %d0,(%a1)+	;# temp,
	dbf %d1,14b	;#, j

	add.l %a3,%d3	;# D.1283, p
	add.l %a3,%d4	;# ivtmp.53, ivtmp.60
9:
	cmp.l %d3,%a4	;# p, list
	bls.s 10b	;#
11:
	add.w %d7,%d6	;# size, i
7:
	cmp.w %a6,%d6	;# num_items.61, i
	bcs.s 8b	;#
	swap %d5
	subq.w #2,%d5
	move.w .Lstep(%pc, %d5.w),%d0
	swap %d5
	move.w %d0,%d5
	bne.s 6b	;#

	movem.l (%sp)+,%d3-%d7/%a2-%a4/%a6
	rts
	.even
.Lstep:
	.word	0
	.word	1
	.word	4
	.word	10
	.word	23
	.word	57
	.word	132
	.word	301
	.word	701
	.word	1750
.Lsteplast:
	.word	4000
");

//! A shell sort with inlined comparison function and hard-coded size.
// NOTE: it would be even faster if rewritten in ASM, using the TIGCCLIB qsort() as
// a starting point, but that would lose the genericity over "item_type".
//
// But even without the rewrite, it still fulfills the purpose of showing that
// if performance is what matters, than the comparison function and the element
// swap SHOULD be inlined !
static __ATTR_LIB_C__ void inline_shellsort(void *list, short num_items, short ignored1, compare_t ignored2)
{
    unsigned short gap,i;
    item_type *p,*a,temp;
    for (gap = 4096; gap > 0; gap = (gap>>1) - (gap>>4)) {
        for(i=gap; i<((unsigned short)num_items); i++) {
            for(p=(item_type*)list+i-gap; p>=(item_type*)list; p -= gap) {
                a=p;
                if(*a <= *(a+gap)) break;
                temp=*a, *a=*(a+gap), *(a+gap)=temp;
            }
        }
    }
}

//! Fill up the array we're going to sort, using a (NR_BITS)-bit LFSR.
static void fill_items(unsigned short seed) {
    unsigned short seq, mask;
    item_type *ptr;
    seq = seed;
    mask = XOR_MASK;
    ptr = items;
    do {
        // ASM block is optimized version of:
        // if (seq & 1) seq = (seq>>1) ^ XOR_MASK;
        // else         seq = seq>>1;
        //
        // Indeed, we can always shift, and get the rightmost bit in cc
        // after shifting.
        asm volatile("lsr.w #1,%0; bcc.s 0f; eor.w %1,%0; 0: " : "=d"(seq) : "d"(mask) : "cc");
        *ptr++ = (item_type)seq;
    }
    while (ptr < (items + sorted_item_count - 1));
    *ptr++ = 0; // Don't forget the zero.
}

//! Check that the arrays are filled with monotonically increasing values.
static unsigned short check_items(void) {
    item_type seq, old_seq;
    item_type *ptr;
    seq = 0;
    old_seq = 0;
    ptr = items;
    while (ptr < (items + sorted_item_count)) {
        seq = *ptr++;
        if (seq < old_seq) {
            return seq;
        }
        old_seq = seq;
    }
    return 0xFFFF;
}

//! Sub-routine for launching one of the sort routines.
static unsigned long run_sort(unsigned short seed, void (* __ATTR_LIB_C__ sort)(void *, short, short, compare_t)) {
#if ITERATIONS == 1
    void *p1 = (void *)0x123456, *p2 = p1, *p3 = p1;
    static const item_type allones = -1;
    static const item_type zero = 0;
    static const item_type onehundredandtwentythree = 123;
#endif
    unsigned short check;
    unsigned short iter;
    unsigned long prev_count, returnedcount;

    prev_count = count;
    while (prev_count == count);
    count = 0;
    for (iter = 0; iter < ITERATIONS; iter++) {
        fill_items(seed);
        sort (items, sorted_item_count, sizeof (items[0]), cmp);
    }
    returnedcount = count;
    check = check_items();
    if(check == 0xFFFF) {
#if ITERATIONS == 1
        p1 = bsearch (&allones, items, sorted_item_count, sizeof (items[0]), cmp);
        p2 = bsearch (&zero, items, sorted_item_count, sizeof (items[0]), cmp);
        p3 = bsearch (&onehundredandtwentythree, items, sorted_item_count, sizeof (items[0]), cmp);
        printf("%lu %lp %lp %lp\n", returnedcount, p1, p2, p3);
#endif
    }
    else {
        printf("FAILED %hu %hu %lu\n", sorted_item_count, check, (unsigned long)(items[check]));
    }

    return returnedcount;
}

//! It's a main...
void _main(void) {
    short rate, start;
    unsigned short seed;
    unsigned long count_old = 0, count_new = 0, count_ciura = 0, count_inlineshell = 0;

    //INT_HANDLER ai1;
    INT_HANDLER ai5;

    // Save the programmable timer settings, set up our interrupt handlers and timer settings.
    rate = PRG_getRate();
    start = PRG_getStart();
    //ai1=GetIntVec(AUTO_INT_1);
    ai5=GetIntVec(AUTO_INT_5);
    //SetIntVec(AUTO_INT_1,DUMMY_HANDLER);
    SetIntVec(AUTO_INT_5,CountTicks);
    OSSetSR(0x0200);

    // If AMS 1.xx and not PedroM, use a poor seed; otherwise, use FiftyMsecTick.
    if (AMS_1xx && *((unsigned short *)0x32) != 0x524F) {
        seed = (unsigned short)(255-peekIO(0x600017));
    }
    else {
        seed = *((volatile unsigned long*)(_rom_call_addr(4FC))) & SEED_MASK;
    }

    sorted_item_count = SORTED_ITEM_COUNT;
#ifdef SCALE
    for (; sorted_item_count > 3; sorted_item_count -= (sorted_item_count >> 2))
#endif
    {
#if ITERATIONS == 1
        printf("0x%lp %hu\n", items, sorted_item_count);
#endif

        PRG_setRate(0);
        PRG_setStart(0xFF);

        count_old = run_sort(seed, old_qsort);
        if (_keytest(RR_ESC)) goto end;

        count_new = run_sort(seed, qsort);
        if (_keytest(RR_ESC)) goto end;

        count_ciura = run_sort(seed, ciura_qsort);
        if (_keytest(RR_ESC)) goto end;

        count_inlineshell = run_sort(seed, inline_shellsort);
        if (_keytest(RR_ESC)) goto end;

        PRG_setRate(rate);
        PRG_setStart(start);
    }

#if ITERATIONS > 1
    printf("%lu\n%lu\n%lu\n%lu\n", count_old, count_new, count_ciura, count_inlineshell);
#endif

end:
    // Restore the programmable timer settings and the interrupt handlers.
    SetIntVec(AUTO_INT_5,ai5);
    //SetIntVec(AUTO_INT_1,ai1);
    asm("move.l #0x27FFF,%%d0; 0: subq.l #1,%%d0; bpl.s 0b" : : : "d0","cc");
    GKeyFlush();
    OSSetSR(0x0000);

    GKeyIn(NULL, 0);
}
