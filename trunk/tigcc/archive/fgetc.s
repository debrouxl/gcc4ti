	.file	"fgetc.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fgetc
fgetc:
	movm.l #0x1820,-(%sp)
	move.l %a0,%a2
	move.w 10(%a0),%d1
	move.w %d1,%d3
	lsr.w #6,%d3
	eor.w #1,%d3
	and.w #1,%d3
	btst #4,%d1
	jbne .L2
	btst #0,%d1
	jbne .L4
	or.w #16,%d1
	move.w %d1,10(%a0)
	jbra .L2
.L4:
	move.w 12(%a0),%d0
	jbge .L7
	clr.w 12(%a0)
	and.w #255,%d0
	jbra .L6
.L7:
	btst #5,%d1
	jbne .L2
	move.l (%a0),%a1
	move.b (%a1)+,%d0
	clr.w %d4
	move.b %d0,%d4
	move.l %a1,(%a0)
	cmp.b #13,%d0
	jbne .L10
	tst.w %d3
	jbeq .L10
	jbsr fgetc
.L10:
	move.l 4(%a2),%a0
	moveq #0,%d0
	move.w (%a0),%d0
	move.l %a0,%d1
	add.l %d0,%d1
	tst.w %d3
	jbeq .L13
	moveq #0,%d0
	jbra .L15
.L13:
	moveq #2,%d0
.L15:
	add.l %d1,%d0
	cmp.l (%a2),%d0
	jbne .L19
	or.w #32,10(%a2)
.L19:
	move.w %d4,%d0
	jbra .L6
.L2:
	moveq #-1,%d0
.L6:
	movm.l (%sp)+,#0x418
	rts
