| Warning: This routine has the attribute __ATTR_TIOS__!

	.xdef printf

.text
printf:
	movea.l 0xC8,%a0
	movea.l (%a0,0x14C),%a0 /* vcbprintf */
	lea (%a0,32),%a0
	movea.w (%a0),%a1
	pea (%sp,8)
	move.l (%sp,8),-(%sp)
	clr.l -(%sp)
	pea fputchar
	jsr (%a0.l,%a1)
	lea (%sp,16),%sp
	rts
