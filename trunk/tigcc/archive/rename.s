	.file	"rename.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	rename
rename:
	lea (-104,%sp),%sp
	move.l %a3,-(%sp)
	move.l %a2,-(%sp)
	move.l %a1,%a3
	lea (8,%sp),%a2
	clr.b (%a2)
.L2:
	addq.l #1,%a2
	move.b (%a0)+,%d0
	move.b %d0,(%a2)
	jbne .L2
	move.l %a2,%a1
.L5:
	addq.l #1,%a1
	move.b (%a3)+,%d0
	move.b %d0,(%a1)
	jbne .L5
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l 396(%a0),%a0
	move.l %a1,-(%sp)
	move.l %a2,-(%sp)
	jbsr (%a0)
	addq.l #8,%sp
	subq.w #1,%d0
	move.l (%sp)+,%a2
	move.l (%sp)+,%a3
	lea (104,%sp),%sp
	rts
