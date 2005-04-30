	.file	"bsearch.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
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
	move.w %d0,%d5
	subq.w #1,%d5
	clr.w %d6
.L2:
	move.w %d6,%d3
	add.w %d5,%d3
	lsr.w #1,%d3
	move.w %d3,%d0
	mulu.w %d7,%d0
	move.l %a3,%d4
	add.l %d0,%d4
	move.l %d4,-(%sp)
	move.l %a4,-(%sp)
	jbsr (%a2)
	addq.l #8,%sp
	tst.w %d0
	jble .L3
	move.w %d3,%d6
	addq.w #1,%d6
	jbra .L5
.L3:
	tst.w %d0
	jbge .L6
	move.w %d3,%d5
	subq.w #1,%d5
.L5:
	cmp.w %d6,%d5
	jbcc .L2
	moveq #0,%d4
.L6:
	move.l %d4,%a0
	movm.l (%sp)+,#0x1cf8
	rts
