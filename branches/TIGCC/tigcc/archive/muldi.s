|muldi3 routine copyright (C) 2002, Kevin Kofler

	.xdef __muldi3

.text
__muldi3:
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
