	.xdef __BC

.text
__BC:
	link %a6,#-10
	lea 26(%a6),%a0        | 4, 8
	move.l (%a0),-(%sp)    | 2, 20
	move.l -(%a0),-(%sp)   | 2, 22
	move.l -(%a0),-(%sp)   | 2, 22
	move.l -(%a0),-(%sp)   | 2, 22
	move.l -(%a0),-(%sp)   | 2, 22
	move.w -(%a0),%d0      | 2, 10
	                       | -8 bytes, -6 clocks
	|move.l (%a6,26),-(%sp) | 4, 24
	|move.l (%a6,22),-(%sp) | 4, 24
	|move.l (%a6,18),-(%sp) | 4, 24
	|move.l (%a6,14),-(%sp) | 4, 24
	|move.l (%a6,10),-(%sp) | 4, 24
	|move.w (%a6,8),%d0     | 4, 12
	movea.l 0xC8.w,%a0
	movea.l 0(%a0,%d0.w),%a0
	jsr (%a0)
	movem.l -10(%a6),%d0-%d1
	move.w (%a6,-2),%d2
	unlk %a6
	rts
