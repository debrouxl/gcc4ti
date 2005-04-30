	.file	"fread.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fread
fread:
	movm.l #0x1f38,-(%sp)
	move.l %a0,%d4
	move.w %d0,%a4
	move.w %d1,%d7
	move.l %a1,%a3
	move.w 10(%a1),%d0
	move.w %d0,%d3
	and.w #64,%d3
	or.w #64,%d0
	move.w %d0,10(%a1)
	moveq #0,%d5
	jbra .L2
.L3:
	move.l %a3,%a0
	jbsr fgetc
	tst.w %d0
	jblt .L4
	move.b %d0,(%a2)+
.L6:
	move.w %a2,%d0
	sub.w %d4,%d0
	cmp.w %a4,%d0
	jbcs .L3
	addq.l #1,%d5
	move.l %a2,%d4
.L2:
	move.w %d5,%d6
	cmp.w %d5,%d7
	jbls .L4
	move.l %d4,%a2
	jbra .L6
.L4:
	or.w #-65,%d3
	and.w %d3,10(%a3)
	move.w %d6,%d0
	movm.l (%sp)+,#0x1cf8
	rts
