	.file	"qsort.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	qsort
qsort:
	subq.w #8,%sp
	movm.l #0x1f3a,-(%sp)
	move.l %a0,%a6
	move.w %d0,%a4
	move.w %d1,%d6
	move.l %a1,36(%sp)
	move.w %d0,%d5
	jbra .L24
	.even
.L22:
	move.w %d5,%d4
	muls.w %d6,%d4
	move.w %d4,%d7
	jbra .L6
	.even
.L21:
	clr.l %d0
	move.w %d4,%d0
	move.l %a6,%d3
	add.l %d0,%d3
	jbra .L23
	.even
.L20:
	move.l %d3,%a3
	clr.l %d0
	move.w %d7,%d0
	lea (%a3,%d0.l),%a2
	move.l %a2,-(%sp)
	move.l %d3,-(%sp)
	move.l 44(%sp),%a0
	jbsr (%a0)
	addq.l #8,%sp
	tst.w %d0
	jble .L11
	move.w %d6,%d1
	jbra .L15
	.even
.L19:
	move.b (%a3),%d0
	move.b (%a2),(%a3)+
	move.b %d0,(%a2)+
	subq.w #1,%d1
.L15:
	tst.w %d1
	jbne .L19
.L23:
	clr.l %d0
	move.w %d7,%d0
	sub.l %d0,%d3
	cmp.l %d3,%a6
	jbls .L20
.L11:
	add.w %d6,%d4
.L6:
	move.w %a4,%d0
	muls.w %d6,%d0
	cmp.w %d4,%d0
	jbhi .L21
.L24:
	lsr.w #1,%d5
	jbne .L22
	movm.l (%sp)+,#0x5cf8
	addq.w #8,%sp
	rts
