	.xdef __floatunssibf

.text
__floatunssibf:
	link.w %a6,#-36
	pea.l 8(%a6)
	move.l 0xC8,%a0
	move.l 0xB5*4(%a0),%a0 /* _bcd_math */
	jsr (%a0)
	.word 0x6B30 /* bcdMove | bcdUnsigned | bcdAbsSrc | bcdR0 */
	move.l -10(%a6),%d0
	move.l -6(%a6),%d1
	move.w -2(%a6),%d2
	unlk %a6
	rts
