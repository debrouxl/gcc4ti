	.file	"fputc.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fputc
fputc:
	movm.l #0x1c30,-(%sp)
	move.w 24(%sp),%d4
	move.l 26(%sp),%a3
	move.w 10(%a3),%d1
	move.w %d1,%d3
	lsr.w #6,%d3
	eor.w #1,%d3
	and.w #1,%d3
	move.l 4(%a3),%a2
	btst #4,%d1
	jbne .L32
	btst #1,%d1
	jbne .L5
.L33:
	or.w #16,%d1
	move.w %d1,10(%a3)
.L32:
	moveq #-1,%d0
	jbra .L4
.L5:
	move.l %a2,%d5
	move.w (%a2),%d0
	addq.w #3,%d0
	cmp.w #-16,%d0
	jbhi .L33
	and.l #65535,%d0
	cmp.l 14(%a3),%d0
	jbls .L9
	move.l 200.w,%a0
	move.w 8(%a3),-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	move.l 14(%a3),%a0
	move.w 18(%a3),%d1
	moveq #-16,%d0
	sub.w %d1,%d0
	and.l #65535,%d0
	addq.l #2,%sp
	cmp.l %a0,%d0
	jbcs .L11
	moveq #0,%d0
	move.w %d1,%d0
	add.l %d0,%a0
	move.l %a0,14(%a3)
	jbra .L13
.L11:
	move.l #65520,14(%a3)
.L13:
	move.l 200.w,%a0
	move.l 14(%a3),-(%sp)
	move.w 8(%a3),-(%sp)
	move.l 628(%a0),%a0
	jbsr (%a0)
	addq.l #6,%sp
	tst.w %d0
	jbne .L14
	or.w #16,10(%a3)
	jbra .L32
.L14:
	move.l 200.w,%a0
	move.w 8(%a3),-(%sp)
	move.l 612(%a0),%a0
	jbsr (%a0)
	move.l %a0,4(%a3)
	move.l %a0,%a2
	move.l %a0,%d0
	sub.l %d5,%d0
	add.l %d0,(%a3)
	addq.l #2,%sp
.L9:
	btst #5,11(%a3)
	jbeq .L16
	addq.w #1,(%a2)
.L16:
	cmp.w #10,%d4
	jbne .L18
	tst.w %d3
	jbeq .L18
	moveq #13,%d4
.L18:
	move.l (%a3),%a0
	move.b %d4,(%a0)+
	move.l %a0,(%a3)
	cmp.w #13,%d4
	jbne .L22
	tst.w %d3
	jbeq .L22
	move.l %a3,-(%sp)
	move.w #32,-(%sp)
	jbsr fputc
	addq.l #6,%sp
.L22:
	moveq #0,%d0
	move.w (%a2),%d0
	move.l %a2,%d1
	add.l %d0,%d1
	tst.w %d3
	jbeq .L25
	moveq #0,%d0
	jbra .L27
.L25:
	moveq #2,%d0
.L27:
	move.l %d1,%a0
	add.l %d0,%a0
	cmp.l (%a3),%a0
	jbne .L28
	or.w #32,10(%a3)
	tst.w %d3
	jbeq .L28
	clr.b (%a0)
	move.l (%a3),%a0
	move.b #-32,1(%a0)
.L28:
	move.w %d4,%d0
.L4:
	movm.l (%sp)+,#0xc38
	rts
