	.file	"ftell.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	ftell
ftell:
	move.w 10(%a0),%d1
	move.w %d1,%d0
	and.w #16,%d0
	moveq.l #-1,%d2
	tst.w %d0
	jbne .L1
	and.w #64,%d1
	move.l (%a0),%d0
	move.l 4(%a0),%d2
	tst.w %d1
	jbeq .L3
	sub.l %d2,%d0
	subq.l #2,%d0
	jbra .L4
	.even
.L3:
	sub.l %d2,%d0
	subq.l #5,%d0
.L4:
	move.l %d0,%d2
.L1:
	move.l %d2,%d0
	rts
