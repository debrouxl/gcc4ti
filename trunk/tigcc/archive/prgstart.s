	.xdef PRG_getStart

.text
PRG_getStart:
	move.w #0x700,%d0
	trap #1
	lea.l 0x600017,%a0
	move.b -2(%a0),%d1
	bset #3,-2(%a0)
.L__PRG_getStart_ne_loop:
	tst.b (%a0)
	bne.s .L__PRG_getStart_ne_loop
.L__PRG_getStart_eq_loop:
	move.b (%a0),%d2
	beq.s .L__PRG_getStart_eq_loop
	btst #3,%d1
	bne.s .L__PRG_getStart_done
	bclr #3,-2(%a0)
.L__PRG_getStart_done:
	trap #1
	move.b %d2,%d0
	rts
