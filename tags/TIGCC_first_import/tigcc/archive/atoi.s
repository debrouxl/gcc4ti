	.file	"atoi.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	atoi
atoi:
	move.l %d3,-(%sp)
	clr.w %d2
	clr.w %d3
	jbra .L2
	.even
.L5:
	addq.l #1,%a0
.L2:
	move.b (%a0),%d0
	cmp.b #32,%d0
	jbeq .L5
	cmp.b #45,%d0
	jbeq .L7
	cmp.b #-83,%d0
	jbne .L6
.L7:
	addq.l #1,%a0
	moveq.l #-1,%d3
	jbra .L10
	.even
.L6:
	cmp.b #43,%d0
	jbne .L10
	addq.l #1,%a0
	jbra .L10
	.even
.L13:
	muls.w #10,%d2
	add.w %d1,%d2
	add.w #-48,%d2
.L10:
	clr.w %d1
	move.b (%a0)+,%d1
	move.w %d1,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	sls %d0
	ext.w %d0
	neg.w %d0
	jbne .L13
	move.w %d2,%d0
	neg.w %d0
	tst.w %d3
	jbne .L15
	move.w %d2,%d0
.L15:
	move.l (%sp)+,%d3
	rts
