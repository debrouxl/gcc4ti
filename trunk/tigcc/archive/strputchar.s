| Warning: This routine has the attribute __ATTR_TIOS_CALLBACK__!

	.xdef strputchar

.text
strputchar:
	movea.l (%sp,6),%a1
	movea.l (%a1),%a0
	move.b (%sp,5),(%a0)
	addq.l #1,(%a1)
	rts
