	.file	"sprite32.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	Sprite32
Sprite32:
	movm.l #0x1f3e,-(%sp)
	move.w %d2,%a6
	move.l %a0,%a4
	move.w %d3,%d7
	muls.w #30,%d1
	lea (%a1,%d1.w),%a0
	move.w %d0,%d1
	asr.w #3,%d1
	and.l #65534,%d1
	add.l %d1,%a0
	move.w %d0,%d6
	and.w #15,%d6
	moveq #32,%d0
	sub.w %d6,%d0
	move.w %d0,%a5
	lea (4,%a0),%a1
	clr.w %d0
	jbra .L2
.L3:
	move.l (%a4)+,%d1
	moveq #0,%d3
	move.w %d6,%d3
	move.l (%a0),%d2
	moveq #0,%d4
	move.w %a5,%d4
	lea (30,%a0),%a2
	move.w %d0,%d5
	addq.w #1,%d5
	lea (30,%a1),%a3
	cmp.w #2,%d7
	jbne .L4
	not.l %d1
	move.l %d1,%d0
	lsr.l %d3,%d0
	not.l %d0
	and.l %d0,%d2
	move.l %d2,(%a0)
	lsl.l %d4,%d1
	not.l %d1
	and.l %d1,(%a1)
	jbra .L6
.L4:
	move.l %d1,%d0
	lsr.l %d3,%d0
	lsl.l %d4,%d1
	tst.w %d7
	jbne .L7
	eor.l %d0,%d2
	move.l %d2,(%a0)
	eor.l %d1,(%a1)
	jbra .L6
.L7:
	or.l %d0,%d2
	move.l %d2,(%a0)
	or.l %d1,(%a1)
.L6:
	move.l %a2,%a0
	move.w %d5,%d0
	move.l %a3,%a1
.L2:
	cmp.w %a6,%d0
	jbne .L3
	movm.l (%sp)+,#0x7cf8
	rts
