/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. [rescinded 22 July 1999]
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdlib.h>

// Do not use register a5; callback function might need it.
register long __tbl asm ("a5");

/*
 * Perform a binary search.
 *
 * The code below is a bit sneaky.  After a comparison fails, we
 * divide the work in half by moving either left or right. If lim
 * is odd, moving left simply involves halving lim: e.g., when lim
 * is 5 we look at item 2, so we change lim to 2 so that we will
 * look at items 0 & 1.  If lim is even, the same applies.  If lim
 * is odd, moving right again involves halving lim, this time moving
 * the base up one item past p: e.g., when lim is 5 we change base
 * to item 3 and make lim 2 so that we will look at items 3 and 4.
 * If lim is even, however, we have to shrink it by one before
 * halving: e.g., when lim is 4, we still looked at item 2, so we
 * have to make lim 3, then halve, obtaining 1, so that we will only
 * look at item 3.
 */
void *bsearch(const void *key asm("a0"), const void *bptr asm("a1"), short n asm("d0"), short w asm("d1"), compare_t cmp_func asm("a2")) __ATTR_LIB_ASM__;
asm("
	.text
	.even
	.globl	bsearch
bsearch:
	movem.l %d3-%d5/%a3-%a4,-(%sp)
	move.l %a0,%d5	;# key, key
	move.w %d1,%d4	;# w, w
	move.l %a1,%a3	;# bptr, base
	move.w %d0,%d3	;# n, lim
	beq.s .L2	;#
.L3:
	move.w %d3,%d0	;# lim, tmp40
	lsr.w #1,%d0	;#, tmp40
	mulu.w %d4,%d0	;# w, tmp41
	lea 0(%a3,%d0.l),%a4	;# base, tmp41, rptr
	move.l %a4,-(%sp)	;# rptr,
	move.l %d5,-(%sp)	;# key,
	jsr (%a2)	;#* cmp_func
	addq.l #8,%sp	;#,
	tst.w %d0	;# rcmp
	beq.s .L4	;#
	ble.s .L5	;#
	lea 0(%a4,%d4.w),%a3	;#, base
	subq.w #1,%d3	;#, lim
.L5:
	lsr.w #1,%d3	;#, lim
	bne.s .L3	;#
.L2:
	suba.l %a4,%a4	;# rptr
.L4:
	move.l %a4,%a0	;# rptr, <result>
	movem.l (%sp)+,%d3-%d5/%a3-%a4
	rts
");

// The assembly routine above was created using the following C code as a starting point:
/*{
  char *base = (char *)bptr;
  unsigned short lim;
  short rcmp;
  void *rptr;

  for (lim = n; lim != 0; lim >>= 1) {
    rptr = base + ((long)(lim >> 1) * (unsigned short)w);
    rcmp = cmp_func(key, rptr);
    if (rcmp == 0)
      return rptr;
    if (rcmp > 0) {  // key > p: move right
      base = (char *)rptr + w;
      lim--;
    } // else move left
  }
  return NULL;
}*/
