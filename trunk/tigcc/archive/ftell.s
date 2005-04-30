	.file	"ftell.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	ftell
ftell:
	move.w 10(%a0),%d1
	btst #4,%d1
	jbeq .L2
	moveq #-1,%d0
	rts
.L2:
	move.l (%a0),%a1
	sub.l 4(%a0),%a1
	move.l %a1,%a0
	btst #6,%d1
	jbeq .L5
	moveq #2,%d0
	jbra .L7
.L5:
	moveq #5,%d0
.L7:
	sub.l %d0,%a0
	move.l %a0,%d0
	rts
