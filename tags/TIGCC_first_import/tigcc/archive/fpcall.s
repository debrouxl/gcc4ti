	.xdef __fp_entry_1,__fp_call

.text
__fp_entry_1:
	lea (%sp,24),%a1
__fp_call:
	add.w #728,%d0
	move.l -(%a1),-(%sp)
	move.l -(%a1),-(%sp)
	move.l -(%a1),-(%sp)
	move.l -(%a1),-(%sp)
	move.l -(%a1),-(%sp)
	move.l 0xC8,%a0
	move.l (%a0,%d0.w),%a0
	jsr (%a0)
	lea (%sp,20),%sp
	rts
