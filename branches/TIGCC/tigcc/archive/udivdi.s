|udivdi3 routine copyright (C) 2002, Kevin Kofler
|WARNING: A division by 0 will not cause an exception, but just crash in an
|         infinite loop!

	.xdef __udivdi3

.text
__udivdi3:
	movem.l %d3-%d6,-(%a7)
	move.l 20(%a7),%d4
	move.l 24(%a7),%d5
	move.l 28(%a7),%d2
	move.l 32(%a7),%d3
	moveq.l #0,%d0
	moveq.l #0,%d1

	moveq.l #-1,%d6
.L__udivdi3_shl:
	addq.w #1,%d6
	add.l %d3,%d3
	addx.l %d2,%d2
	bcc.s .L__udivdi3_shl
	roxr.l #1,%d2
	roxr.l #1,%d3

.L__udivdi3_shr:
	add.l %d1,%d1
	addx.l %d0,%d0
	cmp.l %d2,%d4
	bne.s .L__udivdi3_cmp
	cmp.l %d3,%d5
.L__udivdi3_cmp:
	bcs.s .L__udivdi3_skip
	sub.l %d3,%d5
	subx.l %d2,%d4
	addq.l #1,%d1
.L__udivdi3_skip:
	lsr.l #1,%d2
	roxr.l #1,%d3
	dbra.w %d6,.L__udivdi3_shr

	movem.l (%a7)+,%d3-%d6
	rts
