|ashrdi3 routine copyright (C) 2002, Kevin Kofler

	.xdef __ashrdi3

.text
__ashrdi3:
	tst.w 12(%a7)
	bne.s .L__ashrdi3_return0
	move.l 4(%a7),%d0
	move.l 8(%a7),%d1
	move.w 14(%a7),%d2
	beq.s .L__ashrdi3_returnn
	subq.w #1,%d2
.L__ashrdi3_loop:
	asr.l #1,%d0
	roxr.l #1,%d1
	dbra.w %d2,.L__ashrdi3_loop
.L__ashrdi3_returnn:
	rts

.L__ashrdi3_return0:
	tst.b 4(%a7)
	blt.s .L__ashrdi3_return_neg1
	moveq.l #0,%d0
	moveq.l #0,%d1
	rts

.L__ashrdi3_return_neg1:
	moveq.l #-1,%d0
	moveq.l #-1,%d1
	rts
