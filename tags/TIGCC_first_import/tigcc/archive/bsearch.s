	.file	"bsearch.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	bsearch
bsearch:
	movm.l #0x1f38,-(%sp)
	move.l %a0,%a4
	move.l %a1,%a3
	move.w %d1,%d7
	clr.w %d6
	move.w %d0,%d4
	subq.w #1,%d4
.L2:
	move.w %d6,%d3
	add.w %d4,%d3
	lsr.w #1,%d3
	move.w %d3,%d0
	mulu.w %d7,%d0
	move.l %a3,%d5
	add.l %d0,%d5
	move.l %d5,-(%sp)
	move.l %a4,-(%sp)
	jbsr (%a2)
	addq.l #8,%sp
	tst.w %d0
	jble .L5
	move.w %d3,%d6
	addq.w #1,%d6
	jbra .L4
	.even
.L5:
	move.w %d3,%d4
	subq.w #1,%d4
	tst.w %d0
	jblt .L4
	move.l %d5,%d0
	jbra .L1
	.even
.L4:
	cmp.w %d6,%d4
	jbcc .L2
	moveq.l #0,%d0
.L1:
	move.l %d0,%a0
	movm.l (%sp)+,#0x1cf8
	rts
