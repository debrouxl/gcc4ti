	.file	"atol.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	atol
atol:
	move.l %d3,-(%sp)
	move.l %a0,%a1
	clr.w %d3
	moveq.l #0,%d2
	jbra .L2
	.even
.L5:
	addq.l #1,%a1
.L2:
	move.b (%a1),%d0
	cmp.b #32,%d0
	jbeq .L5
	cmp.b #45,%d0
	jbeq .L7
	cmp.b #-83,%d0
	jbne .L6
.L7:
	addq.l #1,%a1
	moveq.l #-1,%d3
	jbra .L10
	.even
.L6:
	cmp.b #43,%d0
	jbne .L10
	addq.l #1,%a1
	jbra .L10
	.even
.L13:
	move.l %d2,%d0
	add.l %d0,%d0
	lsl.l #3,%d2
	move.l %d2,%a0
	add.l %d0,%a0
	clr.l %d0
	move.b %d1,%d0
	lea -48(%a0,%d0.l),%a0
	move.l %a0,%d2
.L10:
	clr.w %d1
	move.b (%a1)+,%d1
	move.w %d1,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	sls %d0
	ext.w %d0
	neg.w %d0
	jbne .L13
	move.l %d2,%d0
	tst.w %d3
	jbeq .L14
	neg.l %d0
.L14:
	move.l (%sp)+,%d3
	rts
