|ashldi3 routine copyright (C) 2002, Kevin Kofler

	.xdef __ashldi3

.text
__ashldi3:
	tst.w 12(%a7)
	bne.s .L__ashldi3_return0
	move.l 4(%a7),%d0
	move.l 8(%a7),%d1
	move.w 14(%a7),%d2
	beq.s .L__ashldi3_returnn
	subq.w #1,%d2
.L__ashldi3_loop:
	add.l %d1,%d1
	addx.l %d0,%d0
	dbra.w %d2,.L__ashldi3_loop
.L__ashldi3_returnn:
	rts

.L__ashldi3_return0:
	moveq.l #0,%d0
	moveq.l #0,%d1
	rts
