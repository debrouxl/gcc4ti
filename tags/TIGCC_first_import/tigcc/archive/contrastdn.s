	.xdef OSContrastDn

.text
OSContrastDn:
	movem.l %d3-%d4,-(%sp)
	move.l 0xC8,%a0
	move.l (%a0,2652),%a0
	jsr (%a0)
	movem.l (%sp)+,%d3-%d4
	rts
