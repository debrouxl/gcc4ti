	.xdef fputs

.text
.L__fputs_1:
	move.l %a0,(%sp,4)
	move.l (%sp,8),-(%sp)
	move.w %d1,-(%sp)
	jbsr fputc
	addq.l #6,%sp
fputs:
	move.l (%sp,4),%a0
	clr.w %d1
	move.b (%a0)+,%d1
	jbne .L__fputs_1
	rts
