|lshrdi3 routine copyright (C) 2002, Kevin Kofler

	.xdef __lshrdi3

.text
__lshrdi3:
	tst.w 12(%a7)
	bne.s .L__lshrdi3_return0
	move.l 4(%a7),%d0
	move.l 8(%a7),%d1
	move.w 14(%a7),%d2
	beq.s .L__lshrdi3_returnn
	subq.w #1,%d2
.L__lshrdi3_loop:
	lsr.l #1,%d0
	roxr.l #1,%d1
	dbra.w %d2,.L__lshrdi3_loop
.L__lshrdi3_returnn:
	rts

.L__lshrdi3_return0:
	moveq.l #0,%d0
	moveq.l #0,%d1
	rts
