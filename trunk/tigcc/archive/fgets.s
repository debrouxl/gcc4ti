	.file	"fgets.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fgets
fgets:
	movm.l #0x1830,-(%sp)
	move.l %a0,%d3
	move.w %d0,%d4
	move.l %a1,%a3
	move.l %a0,%a2
	moveq #-1,%d0
	jbra .L2
.L3:
	cmp.w #13,%d0
	jbne .L4
	btst #6,11(%a3)
	jbne .L4
	moveq #10,%d0
.L4:
	move.b %d0,(%a2)+
	cmp.b #10,%d0
	jbeq .L8
.L2:
	subq.w #1,%d4
	tst.w %d4
	jble .L8
	move.l %a3,%a0
	jbsr fgetc
	cmp.w #-1,%d0
	jbne .L3
.L8:
	clr.b (%a2)
	cmp.w #-1,%d0
	jbne .L10
	cmp.l %a2,%d3
	sne %d0
	ext.w %d0
	ext.l %d0
	and.l %d0,%d3
.L10:
	move.l %d3,%a0
	movm.l (%sp)+,#0xc18
	rts
