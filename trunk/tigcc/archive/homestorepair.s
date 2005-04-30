	.file	"homestorepair.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	HomeStorePair
HomeStorePair:
	lea (-60,%sp),%sp
	movm.l #0x1e20,-(%sp)
	move.w %d0,%d5
	move.w %d1,%d6
	move.l 200.w,%a0
	lea (20,%sp),%a2
	move.l %a2,-(%sp)
	move.l 1360(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d3
	addq.l #4,%sp
	move.l 200.w,%a0
	jbne .L2
	move.l 2324(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d3
	move.l 200.w,%a0
	move.l 1364(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.l %a2,-(%sp)
	move.l 1360(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d4
	addq.l #4,%sp
	jbeq .L4
	jbra .L10
.L2:
	move.w %d5,-(%sp)
	move.l 604(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	cmp.w %d6,%d5
	jbeq .L6
	move.l 200.w,%a0
	move.w %d6,-(%sp)
	move.l 604(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
.L6:
	move.l 200.w,%a0
	move.w %d3,-(%sp)
	jbra .L11
.L10:
	move.l 200.w,%a0
	move.w %d3,-(%sp)
	move.l 2304(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.w %d4,-(%sp)
.L11:
	move.l 1356(%a0),%a0
	jbsr (%a0)
.L4:
	move.l 200.w,%a0
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
	movm.l (%sp)+,#0x478
	lea (60,%sp),%sp
	rts
