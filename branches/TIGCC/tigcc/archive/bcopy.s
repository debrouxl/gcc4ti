	.xdef bcopy

.text
bcopy:
	move.w (12,%sp),-(%sp)
	clr.w -(%sp)
	move.l (8,%sp),-(%sp)
	move.l (16,%sp),-(%sp)
	movea.l 0xC8,%a0
	movea.l (%a0,0x26A*4),%a0
	jsr (%a0) /* memcpy */
	lea (%sp,12),%sp
	rts
