	.file	"atol.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	atol
atol:
	move.l %d3,-(%sp)
	move.l %a0,%a1
	jbra .L2
.L3:
	addq.l #1,%a1
.L2:
	move.b (%a1),%d0
	cmp.b #32,%d0
	jbeq .L3
	cmp.b #45,%d0
	jbeq .L5
	cmp.b #-83,%d0
	jbne .L7
.L5:
	addq.l #1,%a1
	moveq #-1,%d3
	jbra .L8
.L7:
	cmp.b #43,%d0
	jbne .L17
	addq.l #1,%a1
.L17:
	clr.w %d3
.L8:
	moveq #0,%d1
	jbra .L11
.L12:
	move.l %d1,%d0
	add.l %d1,%d0
	lsl.l #3,%d1
	move.l %d1,%a0
	add.l %d0,%a0
	moveq #0,%d0
	move.w %d2,%d0
	lea -48(%a0,%d0.l),%a0
	move.l %a0,%d1
.L11:
	clr.w %d2
	move.b (%a1)+,%d2
	move.w %d2,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	jbls .L12
	tst.w %d3
	jbeq .L14
	neg.l %d1
.L14:
	move.l %d1,%d0
	move.l (%sp)+,%d3
	rts
