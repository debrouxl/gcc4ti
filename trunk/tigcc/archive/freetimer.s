	.xdef OSVFreeTimer

.text
OSVFreeTimer:
	moveq.l #2,%d1
	moveq.l #-1,%d2
	lea.l 0x600001,%a1
	subq.w #1,%d0
	cmp.w %d1,%d0
	bcc.s .L__timer_ffai
	muls.w #12,%d0
	move.l 0x74,%a0
	cmp.l #132133782,(%a0,-4)
	bne.s .L__timer_fok
	move.l %d2,-32(%a0,%d0.w)
	move.l (%a0,-32),%d0
	and.l (%a0,-20),%d0
	addq.l #1,%d0
	bne.s .L__timer_fok
	bclr.b %d1,(%a1)
	move.l (%a0,-8),0x74:w
	bset.b %d1,(%a1)
.L__timer_fok:
	moveq #1,%d0
	rts
.L__timer_ffai:
	clr.w %d0
	rts
