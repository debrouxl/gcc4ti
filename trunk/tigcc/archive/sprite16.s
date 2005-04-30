	.file	"sprite16.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	Sprite16
Sprite16:
	movm.l #0x1c30,-(%sp)
	move.w %d2,%d5
	move.l %a0,%a3
	muls.w #30,%d1
	lea (%a1,%d1.w),%a0
	move.w %d0,%d1
	asr.w #3,%d1
	and.l #65534,%d1
	add.l %d1,%a0
	and.w #15,%d0
	moveq #16,%d4
	sub.w %d0,%d4
	sub.l %a1,%a1
	jbra .L2
.L3:
	move.w (%a3)+,%d0
	move.w %d4,%d2
	ext.l %d2
	move.l (%a0),%d1
	lea (30,%a0),%a2
	addq.w #1,%a1
	cmp.w #2,%d3
	jbne .L4
	not.w %d0
	and.l #65535,%d0
	lsl.l %d2,%d0
	not.l %d0
	and.l %d0,%d1
	jbra .L11
.L4:
	and.l #65535,%d0
	lsl.l %d2,%d0
	tst.w %d3
	jbne .L7
	eor.l %d0,%d1
	jbra .L11
.L7:
	or.l %d0,%d1
.L11:
	move.l %d1,(%a0)
	move.l %a2,%a0
.L2:
	cmp.w %a1,%d5
	jbne .L3
	movm.l (%sp)+,#0xc38
	rts
