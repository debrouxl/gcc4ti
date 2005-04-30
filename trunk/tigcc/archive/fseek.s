	.file	"fseek.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fseek
fseek:
	movm.l #0x1c00,-(%sp)
	move.l %d0,%d5
	move.w 10(%a0),%d3
	move.w %d3,%d2
	and.w #64,%d2
	move.l 4(%a0),%a1
	jbeq .L2
	moveq #2,%d0
	jbra .L4
.L2:
	moveq #5,%d0
.L4:
	move.l %a1,%d4
	add.l %d0,%d4
	moveq #0,%d0
	move.w (%a1),%d0
	add.l %d0,%a1
	tst.w %d2
	jbeq .L5
	moveq #2,%d0
	jbra .L7
.L5:
	moveq #0,%d0
.L7:
	add.l %d0,%a1
	tst.w %d1
	jbne .L8
	move.l %d4,%d0
	jbra .L10
.L8:
	cmp.w #1,%d1
	jbeq .L11
	move.l %a1,%d0
	jbra .L10
.L11:
	move.l (%a0),%d0
.L10:
	add.l %d5,%d0
	btst #4,%d3
	jbne .L23
	cmp.l %d0,%d4
	jbhi .L16
	cmp.l %d0,%a1
	jbcs .L16
	cmp.w #2,%d1
	jbls .L19
.L16:
	or.w #16,%d3
	move.w %d3,10(%a0)
.L23:
	moveq #-1,%d0
	jbra .L15
.L19:
	move.l %d0,(%a0)
	clr.w 12(%a0)
	move.w 10(%a0),%d1
	cmp.l %d0,%a1
	jbne .L20
	or.w #32,%d1
	jbra .L24
.L20:
	and.w #-33,%d1
.L24:
	move.w %d1,10(%a0)
	clr.w %d0
.L15:
	movm.l (%sp)+,#0x38
	rts
