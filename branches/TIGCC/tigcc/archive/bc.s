	.xdef __BC

.text
__BC:
	link %a6,#-10
	move.l (%a6,26),-(%sp)
	move.l (%a6,22),-(%sp)
	move.l (%a6,18),-(%sp)
	move.l (%a6,14),-(%sp)
	move.l (%a6,10),-(%sp)
	move.w (%a6,8),%d0
	move.l 0xC8,%a0
	move.l (%a0,%d0.w),%a0
	jsr (%a0)
	move.l (%a6,-10),%d0
	move.l (%a6,-6),%d1
	move.w (%a6,-2),%d2
	unlk %a6
	rts
