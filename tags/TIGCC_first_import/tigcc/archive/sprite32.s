	.file	"sprite32.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	Sprite32
Sprite32:
	movm.l #0x1e20,-(%sp)
	move.w %d2,%d5
	move.l %a0,%a2
	move.w %d3,%d6
	muls.w #30,%d1
	lea (%a1,%d1.w),%a0
	move.w %d0,%d1
	asr.w #3,%d1
	and.l #65534,%d1
	add.l %d1,%a0
	move.w %d0,%d4
	and.w #15,%d4
	moveq.l #32,%d0
	sub.w %d4,%d0
	move.w %d0,%a1
	jbra .L2
	.even
.L10:
	move.l (%a2)+,%d1
	cmp.w #2,%d6
	jbne .L6
	not.l %d1
	moveq.l #15,%d0
	and.l %d4,%d0
	move.l %d1,%d2
	lsr.l %d0,%d2
	move.l %d2,%d0
	not.l %d0
	and.l %d0,(%a0)
	clr.l %d0
	move.w %a1,%d0
	lsl.l %d0,%d1
	not.l %d1
	and.l %d1,4(%a0)
	jbra .L4
	.even
.L6:
	moveq.l #15,%d0
	and.l %d4,%d0
	move.l %d1,%d2
	lsr.l %d0,%d2
	clr.l %d0
	move.w %a1,%d0
	move.l %d1,%d3
	lsl.l %d0,%d3
	move.l (%a0),%d1
	move.l 4(%a0),%d0
	tst.w %d6
	jbne .L8
	eor.l %d2,%d1
	move.l %d1,(%a0)
	eor.l %d3,%d0
	jbra .L11
	.even
.L8:
	or.l %d2,%d1
	move.l %d1,(%a0)
	or.l %d3,%d0
.L11:
	move.l %d0,4(%a0)
.L4:
	lea (30,%a0),%a0
.L2:
	dbra %d5,.L10
	movm.l (%sp)+,#0x478
	rts
