	.xdef __mulsi3

.text
__mulsi3:
	move.l (4,%sp),%d1
	move.l (8,%sp),%d2
	move.l %d2,%d0
	mulu %d1,%d0
	swap %d2
	mulu %d1,%d2
	swap %d1
	mulu (10,%sp),%d1
	add.w %d1,%d2
	swap %d2
	clr.w %d2
	add.l %d2,%d0
	rts
