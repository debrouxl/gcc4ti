	.file	"fclose.c"
#NO_APP
	.text
tigcc_compiled.:
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
	moveq.l #-1,%d0
	cmp.w #0,%a0
	jbeq .L1
	move.w 10(%a0),%d0
	move.w %d0,%d3
	and.w #16,%d3
	lsr.w #4,%d3
	neg.w %d3
	and.w #2,%d0
	jbeq .L5
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
.L5:
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
.L1:
	move.l (%sp)+,%d3
	move.l (%sp)+,%a2
	rts
