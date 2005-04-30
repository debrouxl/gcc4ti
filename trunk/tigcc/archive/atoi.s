	.file	"atoi.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	atoi
atoi:
	move.l %d3,-(%sp)
	jbra .L2
.L3:
	addq.l #1,%a0
.L2:
	move.b (%a0),%d0
	cmp.b #32,%d0
	jbeq .L3
	cmp.b #45,%d0
	jbeq .L5
	cmp.b #-83,%d0
	jbne .L7
.L5:
	addq.l #1,%a0
	moveq #-1,%d3
	jbra .L8
.L7:
	cmp.b #43,%d0
	jbne .L18
	addq.l #1,%a0
.L18:
	clr.w %d3
.L8:
	clr.w %d1
	jbra .L11
.L12:
	muls.w #10,%d1
	add.w %d2,%d1
	add.w #-48,%d1
.L11:
	clr.w %d2
	move.b (%a0)+,%d2
	move.w %d2,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	jbls .L12
	tst.w %d3
	jbeq .L14
	move.w %d1,%d0
	neg.w %d0
	jbra .L16
.L14:
	move.w %d1,%d0
.L16:
	move.l (%sp)+,%d3
	rts
