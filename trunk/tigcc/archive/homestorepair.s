	.file	"homestorepair.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	HomeStorePair
HomeStorePair:
	link.w %a6,#-120
	movm.l #0x1e00,-(%sp)
	move.w %d0,%d5
	move.w %d1,%d6
	move.l 200.w,%a0
	pea -60(%a6)
	move.l 1360(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d3
	addq.l #4,%sp
	move.l 200.w,%a0
	jbeq .L7
	move.w %d5,-(%sp)
	move.l 604(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	cmp.w %d6,%d5
	jbeq .L4
	move.l 200.w,%a0
	move.w %d6,-(%sp)
	move.l 604(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
.L4:
	move.l 200.w,%a0
	move.l 1356(%a0),%a0
	move.w %d3,-(%sp)
	jbra .L8
	.even
.L7:
	move.l 2324(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d3
	move.l 200.w,%a0
	move.l 1364(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	pea -120(%a6)
	move.l 1360(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d4
	addq.l #4,%sp
	move.l 200.w,%a0
	jbne .L5
	move.w %d3,-(%sp)
	move.l 600(%a0),%a0
	jbsr (%a0)
	move.w %d5,16(%a0)
	move.w %d6,42(%a0)
	jbsr __get_HS_pushEmptyFIFONode
	move.w %d3,-(%sp)
	jbsr (%a0)
	move.l 200.w,%a0
	move.l 1364(%a0),%a0
	jbsr (%a0)
	addq.l #4,%sp
	jbra .L1
	.even
.L5:
	move.w %d3,-(%sp)
	move.l 2304(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.l 1356(%a0),%a0
	move.w %d4,-(%sp)
.L8:
	jbsr (%a0)
	.even
.L1:
	movm.l -136(%a6),#0x78
	unlk %a6
	rts
