|umoddi3 routine copyright (C) 2002, Kevin Kofler
|WARNING: A division by 0 will not cause an exception, but just crash in an
|         infinite loop!

	.xdef __umoddi3

.text
__umoddi3:
	movea.l %d3,%a0
	move.l 4(%a7),%d0
	move.l 8(%a7),%d1
	move.l 12(%a7),%d2
	move.l 16(%a7),%d3

	movea.l %d4,%a1
	moveq.l #-1,%d4
.L__umoddi3_shl:
	addq.w #1,%d4
	add.l %d3,%d3
	addx.l %d2,%d2
	bcc.s .L__umoddi3_shl
	roxr.l #1,%d2
	roxr.l #1,%d3

.L__umoddi3_shr:
	cmp.l %d2,%d0
	bne.s .L__umoddi3_cmp
	cmp.l %d3,%d1
.L__umoddi3_cmp:
	bcs.s .L__umoddi3_skip
	sub.l %d3,%d1
	subx.l %d2,%d0
.L__umoddi3_skip:
	lsr.l #1,%d2
	roxr.l #1,%d3
	dbra.w %d4,.L__umoddi3_shr

	move.l %a1,%d4
	move.l %a0,%d3
	rts
