	.file	"fgets.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fgets
fgets:
	movm.l #0x1830,-(%sp)
	move.l %a0,%d4
	move.w %d0,%d3
	move.l %a1,%a3
	moveq.l #-1,%d1
	move.l %a0,%a2
	jbra .L2
	.even
.L8:
	cmp.w #13,%d1
	jbne .L6
	move.w 10(%a3),%d0
	and.w #64,%d0
	jbne .L6
	moveq.l #10,%d1
.L6:
	move.b %d1,(%a2)+
	cmp.b #10,%d1
	jbeq .L3
.L2:
	subq.w #1,%d3
	tst.w %d3
	jble .L3
	move.l %a3,%a0
	jbsr fgetc
	move.w %d0,%d1
	cmp.w #-1,%d0
	jbne .L8
.L3:
	clr.b (%a2)
	cmp.w #-1,%d1
	jbne .L11
	moveq.l #0,%d0
	cmp.l %a2,%d4
	jbeq .L10
.L11:
	move.l %d4,%d0
.L10:
	move.l %d0,%a0
	movm.l (%sp)+,#0xc18
	rts
