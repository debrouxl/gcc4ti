	.file	"pushfifonode.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	__get_HS_pushEmptyFIFONode
__get_HS_pushEmptyFIFONode:
	move.l %a3,-(%sp)
	move.l %a2,-(%sp)
	move.l 200.w,%a2
	move.l 1084(%a2),%a1
	lea (256,%a1),%a3
	jbra .L2
.L3:
	cmp.w #20154,(%a1)
	jbne .L4
	addq.l #2,%a1
	move.w (%a1),%a0
	lea (%a1,%a0.l),%a0
	cmp.l 2324(%a2),%a0
	jbne .L9
.L4:
	addq.l #2,%a1
.L2:
	cmp.l %a1,%a3
	jbhi .L3
#APP
	.word _A_LINE+410
#NO_APP
.L9:
	move.l (%sp)+,%a2
	move.l (%sp)+,%a3
	rts
