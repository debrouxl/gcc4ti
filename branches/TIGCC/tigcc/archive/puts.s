	.xdef puts

.text
.L__puts_1:
	move.l %a0,(%sp,4)
	move.w %d0,-(%sp)
	jbsr fputchar
	addq.l #2,%sp
puts:
	move.l (%sp,4),%a0
	move.b (%a0)+,%d0
	jbne .L__puts_1
	rts
