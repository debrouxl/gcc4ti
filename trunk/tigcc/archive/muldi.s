|muldi3 routine copyright (C) 2002, Kevin Kofler

	.xdef __muldi3

.text
__muldi3:
	lea 18(%sp),%a0
	lea 4(%sp),%a1
	move.w (%a0),%d0  | 18
	mulu (%a1)+,%d0   | 4
	move.w -(%a0),%d2 | 16
	mulu (%a1)+,%d2   | 6
	add.w %d2,%d0
	move.w -(%a0),%d2 | 14
	mulu (%a1)+,%d2   | 8
	add.w %d2,%d0
	move.w -(%a0),%d2 | 12
	mulu (%a1),%d2    | 10
	add.w %d2,%d0
	swap %d0
	clr.w %d0

	addq.l #6,%a0      | 18
	subq.l #4,%a1      | 6


	move.w (%a0),%d2   | 18
	mulu (%a1)+,%d2    | 6
	add.l %d2,%d0
	move.w -(%a0),%d2  | 16
	mulu (%a1)+,%d2    | 8
	add.l %d2,%d0
	move.w -(%a0),%d2  | 14
	mulu (%a1),%d2     | 10
	add.l %d2,%d0

	addq.l #4,%a0
	move.w (%a0),%d1  | 18
	mulu -2(%a1),%d1  | 8
	swap %d1
	add.w %d1,%d0
	clr.w %d1
	swap %d0
	addx.w %d1,%d0
	swap %d0

	move.w -2(%a0),%d2 | 16
	mulu (%a1),%d2     | 10
	swap %d2
	add.w %d2,%d0
	clr.w %d2
	swap %d0
	addx.w %d2,%d0
	swap %d0
	add.l %d2,%d1
	moveq.l #0,%d2
	addx.l %d2,%d0

	move.w (%a0),%d2 | 18
	mulu (%a1),%d2   | 10
	add.l %d2,%d1
	moveq.l #0,%d2
	addx.l %d2,%d0

	rts
