	.xdef bzero

.text
bzero:
	move.w (8,%sp),-(%sp)
	clr.l -(%sp)
	move.l (10,%sp),-(%sp)
	movea.l 0xC8,%a0
	movea.l (%a0,0x27C*4),%a0	   
	jsr (%a0) /* memset */
	lea (%sp,10),%sp
	rts
