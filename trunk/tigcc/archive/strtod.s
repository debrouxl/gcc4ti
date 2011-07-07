.xdef strtod

	.text
strtod:
	link %a6,#-0x30 | 4
	movea.l 0xC8.w,%a1 | 4
	move.l 0xC(%a6),(%sp) | 4
	bne.s 0f | 2
	pea -0x2C(%a6) | 4
0:
	move.l 8(%a6),-(%sp) | 4
	movea.l 0x4D4*4(%a1),%a1 /* strtod */
	jsr (%a1) | 2
	move.w -(%a6),%d2 | 2
	move.l -(%a6),%d1 | 2
	move.l -(%a6),%d0 | 2
	lea 0xA(%a6),%a6 | 4
	unlk %a6 | 2
	rts | 2
