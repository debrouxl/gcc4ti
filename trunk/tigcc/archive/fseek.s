	.file	"fseek.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fseek
fseek:
	movm.l #0x1820,-(%sp)
	move.l %d0,%d3
	move.w %d1,%d4
	move.w 10(%a0),%d1
	move.w %d1,%d2
	and.w #64,%d2
	move.l 4(%a0),%a1
	lea (2,%a1),%a2
	jbne .L3
	lea (5,%a1),%a2
.L3:
	clr.l %d0
	move.w (%a1),%d0
	add.l %d0,%a1
	tst.w %d2
	jbeq .L4
	addq.l #2,%a1
.L4:
	move.l %a2,%d2
	add.l %d3,%d2
	tst.w %d4
	jbeq .L6
	move.l %a1,%d2
	add.l %d3,%d2
	cmp.w #1,%d4
	jbne .L6
	move.l (%a0),%d2
	add.l %d3,%d2
.L6:
	move.w %d1,%d0
	and.w #16,%d0
	moveq.l #-1,%d3
	tst.w %d0
	jbne .L1
	cmp.l %d2,%a2
	jbhi .L11
	cmp.l %d2,%a1
	jbcs .L11
	cmp.w #2,%d4
	jbls .L10
.L11:
	or.w #16,%d1
	move.w %d1,10(%a0)
	moveq.l #-1,%d3
	jbra .L1
	.even
.L10:
	move.l %d2,(%a0)
	clr.w 12(%a0)
	cmp.l %d2,%a1
	jbne .L12
	or.w #32,%d1
	jbra .L14
	.even
.L12:
	and.w #-33,%d1
.L14:
	move.w %d1,10(%a0)
	clr.w %d3
.L1:
	move.w %d3,%d0
	movm.l (%sp)+,#0x418
	rts
