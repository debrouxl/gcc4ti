	.file	"fread.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fread
fread:
	movm.l #0x1f30,-(%sp)
	move.l %a0,%a3
	move.w %d0,%d7
	move.w %d1,%d6
	move.l %a1,%a2
	move.w 10(%a1),%d0
	move.w %d0,%d4
	and.w #64,%d4
	or.w #64,%d0
	move.w %d0,10(%a1)
	clr.w %d5
	jbra .L2
	.even
.L12:
	move.l %a2,%a0
	jbsr fgetc
	tst.w %d0
	jblt .L11
	move.b %d0,(%a3)+
	addq.w #1,%d3
.L6:
	cmp.w %d3,%d7
	jbhi .L12
	addq.w #1,%d5
.L2:
	clr.w %d3
	cmp.w %d5,%d6
	jbhi .L6
.L11:
	or.w #-65,%d4
	and.w %d4,10(%a2)
	move.w %d5,%d0
	movm.l (%sp)+,#0xcf8
	rts
