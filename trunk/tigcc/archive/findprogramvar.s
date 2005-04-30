	.file	"findprogramvar.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	FindProgramVar
FindProgramVar:
	movm.l #0x1c20,-(%sp)
#APP
	bsr 0f
0:move.l (%sp)+,%d4
#NO_APP
	and.l #262143,%d4
	move.l 200.w,%a0
	move.w #2,-(%sp)
	clr.l -(%sp)
	move.l 432(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a2
	addq.l #6,%sp
	jbra .L2
.L3:
	move.w 12(%a2),%d5
	move.l 200.w,%a0
	move.w %d5,-(%sp)
	move.l 600(%a0),%a0
	jbsr (%a0)
	move.l %a0,%d3
	addq.l #2,%sp
	cmp.l %d4,%a0
	jbhi .L4
	move.l 200.w,%a0
	move.w %d5,-(%sp)
	move.l 632(%a0),%a0
	jbsr (%a0)
	add.l %d3,%d0
	addq.l #2,%sp
	cmp.l %d4,%d0
	jbhi .L6
.L4:
	move.l 200.w,%a0
	move.l 436(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a2
.L2:
	cmp.w #0,%a2
	jbne .L3
.L6:
	move.l %a2,%a0
	movm.l (%sp)+,#0x438
	rts
