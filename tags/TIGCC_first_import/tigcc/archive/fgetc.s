	.file	"fgetc.c"
#NO_APP
	.text
tigcc_compiled.:
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
	move.w %d1,%d4
	lsr.w #6,%d4
	eor.w #1,%d4
	and.w #1,%d4
	move.w %d1,%d0
	and.w #16,%d0
	moveq.l #-1,%d2
	tst.w %d0
	jbne .L1
	move.w %d1,%d0
	and.w #1,%d0
	jbne .L3
	or.w #16,%d1
	move.w %d1,10(%a0)
	jbra .L1
	.even
.L3:
	move.w 12(%a0),%d3
	jbge .L4
	clr.w 12(%a0)
	move.w %d3,%d2
	and.w #255,%d2
	jbra .L1
	.even
.L4:
	and.w #32,%d1
	moveq.l #-1,%d2
	tst.w %d1
	jbne .L1
	move.l (%a0),%a1
	clr.w %d3
	move.b (%a1),%d3
	addq.l #1,(%a0)
	cmp.w #13,%d3
	jbne .L6
	tst.w %d4
	jbeq .L6
	jbsr fgetc
.L6:
	move.l 4(%a2),%a0
	clr.l %d0
	move.w (%a0),%d0
	add.l %a0,%d0
	move.l (%a2),%d1
	tst.w %d4
	jbne .L9
	addq.l #2,%d0
.L9:
	cmp.l %d0,%d1
	jbne .L8
	or.w #32,10(%a2)
.L8:
	move.w %d3,%d2
.L1:
	move.w %d2,%d0
	movm.l (%sp)+,#0x418
	rts
