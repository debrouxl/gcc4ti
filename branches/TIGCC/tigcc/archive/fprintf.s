| Warning: This routine has the attribute __ATTR_TIOS__!

	.xdef fprintf

.text
fprintf:
	movea.l 0xC8,%a0
	movea.l (%a0,0x14C),%a0 /* vcbprintf */
	lea (%a0,32),%a0
	movea.w (%a0),%a1
	pea (%sp,12)
	move.l (%sp,12),-(%sp)
	move.l (%sp,12),-(%sp)
	pea fputc
	jsr (%a0.l,%a1)
	lea (%sp,16),%sp
	rts
