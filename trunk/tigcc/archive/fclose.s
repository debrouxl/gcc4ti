	.file	"fclose.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fclose
fclose:
	move.l %a2,-(%sp)
	move.l %d3,-(%sp)
	move.l %a0,%a2
	cmp.w #0,%a0
	jbne .L2
	moveq #-1,%d0
	jbra .L4
.L2:
	move.w 10(%a0),%d1
	btst #4,%d1
	sne %d0
	move.b %d0,%d3
	ext.w %d3
	btst #1,%d1
	jbeq .L8
	move.l 200.w,%a1
	move.l 4(%a0),%a0
	move.w (%a0),%d0
	addq.w #2,%d0
	move.w %d0,-(%sp)
	clr.w -(%sp)
	move.w 8(%a2),-(%sp)
	move.l 628(%a1),%a0
	jbsr (%a0)
	addq.l #6,%sp
.L8:
	move.l 200.w,%a0
	move.w 8(%a2),-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.l %a2,-(%sp)
	move.l 652(%a0),%a0
	jbsr (%a0)
	move.w %d3,%d0
	addq.l #6,%sp
.L4:
	move.l (%sp)+,%d3
	move.l (%sp)+,%a2
	rts
