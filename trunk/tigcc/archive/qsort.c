#include <stdlib.h>

// Do not use register a5; callback function might need it.
register long __tbl asm ("a5");

// This is not a quick sort, it's a shell sort.
// For sorting data that has no significant statistical property, on embedded platforms
// without processor caches, the shell sort is one of the very best size/speed tradeoffs.
__ATTR_LIB_C__ void qsort(void *list, short num_items, short size, compare_t cmp_func);

asm("
| d3 <- p
| d4 <- a+byte_gap
| d5 <- k
| d6 <- i
| d7 <- size
| a2 <- cmp_func
| a3 <- byte_gap
| a4 <- list
| a6 <- num_items * size
.text
	.even
	.globl qsort
qsort:
	movem.l %d3-%d7/%a2-%a4/%a6,-(%sp)
	move.l %a0,%a4	;# list, list
	move.w %d1,%d7	;# size, size
	move.l %a1,%a2	;# cmp_func, cmp_func
	move.w #4096,%d5	;#, k
	cmp.w #16,%d0	;#, num_items
	bhi.s .L4	;#
	moveq #1,%d5	;#, k
.L4:
	mulu.w %d7,%d0	;# size, num_items
	move.w %d0,%a6	;# num_items, num_items.61

.L6:
	move.w %d5,%d6	;# k, i
	mulu.w %d7,%d6	;# size, i
	move.l %d6,%d0	;# i, byte_gap
	neg.l %d0
	move.l %d0,%a3
	bra.s .L7	;#
.L8:
	moveq #0,%d0	;# i
	move.w %d6,%d0	;# i, i
	move.l %a4,%d3	;# list, p
	add.l %d0,%d3	;# i, p
	add.l %a3,%d3	;# D.1283, p
	move.l %d3,%d4	;# p, ivtmp.60
	sub.l %a3,%d4	;# D.1283, ivtmp.60
	bra.s .L9	;#
.L10:
	move.l %d4,-(%sp)	;# ivtmp.60,
	move.l %d3,-(%sp)	;# p,
	jsr (%a2)	;#
	addq.l #8,%sp	;#,
	tst.w %d0	;#
	ble.s .L11	;#
	move.l %d4,%a1	;# ivtmp.60, ivtmp.47
	move.w %d7,%d1	;# size, j
	move.l %d3,%a0	;# p, a
	subq.w #1,%d1	;#
.L14:
	move.b (%a0),%d0	;#* a, temp
	move.b (%a1),(%a0)+	;#* ivtmp.47,
	move.b %d0,(%a1)+	;# temp,
	dbf %d1,.L14	;#, j

	add.l %a3,%d3	;# D.1283, p
	add.l %a3,%d4	;# ivtmp.53, ivtmp.60
.L9:
	cmp.l %d3,%a4	;# p, list
	bls.s .L10	;#
.L11:
	add.w %d7,%d6	;# size, i
.L7:
	cmp.w %a6,%d6	;# num_items.61, i
	bcs.s .L8	;#

	lsr.w #1,%d5	;#, tmp59
	move.w %d5,%d0	;# k, tmp60
	lsr.w #3,%d0	;#, tmp60
	sub.w %d0,%d5	;# tmp60, k
	bne.s .L6	;#

	movem.l (%sp)+,%d3-%d7/%a2-%a4/%a6
	rts
");


// The assembly routine above was created using the following C code as a starting point:
/*{
    unsigned short byte_gap,i;
    short j;
    unsigned short k;
    char *p,*a,temp;

    k = ((unsigned short)num_items <= 16) ? 1 : 4096;
    num_items = (unsigned short)num_items * (unsigned short)size;

    for (; k > 0; k = (k>>1) - (k>>4)) {
        byte_gap=k*(unsigned short)size;
        for(i=byte_gap; i<(unsigned short)num_items; i+=size) {
            for(p=(char*)list+i-byte_gap; p>=(char*)list; p-= byte_gap) {
                a=p;
                if(cmp_func(a,a+byte_gap)<=0) break;
                for(j=size;j;j--) {
                    temp=*a; *a=*(a+byte_gap); *(a+byte_gap)=temp; a++;
                }
            }
        }
    }
}*/

// Compiling it with -Os under GCC 4.1.2-tigcc-4 yielded:
/*
	subq.w #8,%sp
	movm.l #0x1f3a,-(%sp)
	move.l %a0,%a4	;# list, list
	move.w %d1,%d7	;# size, size
	move.l %a1,40(%sp)	;# cmp_func, cmp_func
	muls.w %d1,%d0	;# size, num_items
	move.w %d0,%a3	;# num_items, num_items.60
	move.w #4096,%d2	;#, k
	moveq #16,%d0	;#,
	cmp.w %a3,%d0	;# num_items.60,
	jbcs .L18	;#
	moveq #1,%d2	;#, k
	jbra .L18	;#
.L5:
	move.w %d2,%d6	;# k, i
	muls.w %d7,%d6	;# size, i
	move.w %d6,%a6	;# i, byte_gap
	jbra .L6	;#
.L7:
	moveq #0,%d5	;# D.1283
	move.w %a6,%d5	;# byte_gap, D.1283
	moveq #0,%d0	;# i
	move.w %d6,%d0	;# i, i
	move.l %a4,%d3	;# list, p
	add.l %d0,%d3	;# i, p
	sub.l %d5,%d3	;# D.1283, p
	move.l %d5,%d0	;# D.1283,
	neg.l %d0	;#
	move.l %d0,%a2	;#, ivtmp.53
	move.l %d3,%d4	;# p, ivtmp.59
	add.l %d5,%d4	;# D.1283, ivtmp.59
	jbra .L8	;#
.L9:
	move.l %d4,-(%sp)	;# ivtmp.59,
	move.l %d3,-(%sp)	;# p,
	move.l %d2,44(%sp)	;#,
	move.l 48(%sp),%a0	;# cmp_func,
	jbsr (%a0)	;#
	addq.l #8,%sp	;#,
	move.l 36(%sp),%d2	;#,
	tst.w %d0	;#
	jble .L10	;#
	move.l %d4,%a1	;# ivtmp.59, ivtmp.47
	move.w %d7,%d1	;# size, j
	move.l %d3,%a0	;# p, a
	jbra .L12	;#
.L13:
	move.b (%a0),%d0	;#* a, temp
	move.b (%a1),(%a0)+	;#* ivtmp.47,
	move.b %d0,(%a1)+	;# temp,
	subq.w #1,%d1	;#, j
.L12:
	tst.w %d1	;# j
	jbne .L13	;#
	sub.l %d5,%d3	;# D.1283, p
	add.l %a2,%d4	;# ivtmp.53, ivtmp.59
.L8:
	cmp.l %d3,%a4	;# p, list
	jbls .L9	;#
.L10:
	add.w %d7,%d6	;# size, i
.L6:
	cmp.w %a3,%d6	;# num_items.60, i
	jbcs .L7	;#
	move.w %d2,%d1	;# k, tmp58
	lsr.w #1,%d1	;#, tmp58
	move.w %d2,%d0	;# k, tmp59
	lsr.w #4,%d0	;#, tmp59
	move.w %d1,%d2	;# tmp58, k
	sub.w %d0,%d2	;# tmp59, k
.L18:
	tst.w %d2	;# k
	jbne .L5	;#
	movm.l (%sp)+,#0x5cf8
	addq.w #8,%sp
	rts
*/
// In six steps, 30 bytes were saved, yielding the ASM routine at the top of this file.
