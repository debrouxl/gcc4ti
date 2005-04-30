	.file	"qsort.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	qsort
qsort:
	subq.w #4,%sp
	movm.l #0x1f3a,-(%sp)
	move.l %a0,%a3
	move.w %d1,%d7
	move.l %a1,36(%sp)
	move.w %d0,%d6
	lsr.w #1,%d6
	muls.w %d1,%d0
	move.w %d0,%a6
	jbra .L2
.L3:
	move.w %d6,%d4
	muls.w %d7,%d4
	move.w %d4,%a4
	jbra .L4
.L5:
	moveq #0,%d5
	move.w %a4,%d5
	moveq #0,%d0
	move.w %d4,%d0
	move.l %a3,%d3
	add.l %d0,%d3
	jbra .L16
.L7:
	move.l %d3,%a2
	add.l %d5,%a2
	move.l %a2,-(%sp)
	move.l %d3,-(%sp)
	move.l 44(%sp),%a0
	jbsr (%a0)
	addq.l #8,%sp
	tst.w %d0
	jble .L8
	move.l %d3,%a0
	clr.w %d1
	jbra .L10
.L11:
	move.b (%a0),%d0
	move.b (%a2),(%a0)+
	move.b %d0,(%a2)+
	addq.w #1,%d1
.L10:
	cmp.w %d1,%d7
	jbne .L11
.L16:
	sub.l %d5,%d3
	cmp.l %d3,%a3
	jbls .L7
.L8:
	add.w %d7,%d4
.L4:
	cmp.w %a6,%d4
	jbcs .L5
	lsr.w #1,%d6
.L2:
	tst.w %d6
	jbne .L3
	movm.l (%sp)+,#0x5cf8
	addq.w #4,%sp
	rts
