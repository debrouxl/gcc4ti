	.file	"unlink.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	unlink
unlink:
	lea (-56,%sp),%sp
	lea (2,%sp),%a1
	clr.b (%a1)
.L2:
	addq.l #1,%a1
	move.b (%a0)+,%d0
	move.b %d0,(%a1)
	jbne .L2
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l 376(%a0),%a0
	move.l %a1,-(%sp)
	jbsr (%a0)
	addq.l #4,%sp
	subq.w #1,%d0
	lea (56,%sp),%sp
	rts
