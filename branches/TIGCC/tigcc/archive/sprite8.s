	.file	"sprite8.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	Sprite8
Sprite8:
	movm.l #0x1c20,-(%sp)
	move.w %d2,%d4
	move.l %a0,%a2
	muls.w #30,%d1
	lea (%a1,%d1.w),%a0
	move.w %d0,%d1
	asr.w #3,%d1
	and.l #65534,%d1
	add.l %d1,%a0
	and.w #15,%d0
	moveq.l #24,%d5
	sub.w %d0,%d5
	jbra .L2
	.even
.L10:
	clr.w %d0
	move.b (%a2)+,%d0
	move.w %d5,%d2
	ext.l %d2
	move.l (%a0),%d1
	lea (30,%a0),%a1
	cmp.w #2,%d3
	jbne .L6
	not.w %d0
	and.l #0xFF,%d0
	lsl.l %d2,%d0
	not.l %d0
	and.l %d0,%d1
	jbra .L11
	.even
.L6:
	and.l #0xFF,%d0
	lsl.l %d2,%d0
	tst.w %d3
	jbne .L8
	eor.l %d0,%d1
	jbra .L11
	.even
.L8:
	or.l %d0,%d1
.L11:
	move.l %d1,(%a0)
	move.l %a1,%a0
.L2:
	dbra %d4,.L10
	movm.l (%sp)+,#0x438
	rts
