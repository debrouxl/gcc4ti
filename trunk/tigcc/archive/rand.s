	.xdef rand,__randseed

.text
rand:
	lea __randseed(%pc),%a0
	move.l #0x41C64E6D,%d2
	move.l (%a0),%d1
	move.l %d2,%d0
	mulu %d1,%d0
	swap %d2
	mulu %d1,%d2
	swap %d1
	mulu #0x4E6D,%d1
	add.w %d1,%d2
	swap %d2
	clr.w %d2
	add.l %d2,%d0
	add.l #12345,%d0
	move.l %d0,(%a0)
	lsr.l #8,%d0
	and.w #32767,%d0
	rts
	.even
__randseed:
	.long 1
