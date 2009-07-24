
.globl ClipSprite16
.text
.even
ClipSprite16:
	movem.l %d3-%d5,-(%a7)

	tst.w %d1
	bge.s .L__cs16_1
	add.w %d1,%d2
	add.w %d1,%d1
	suba.w %d1,%a0
	moveq #0,%d1
.L__cs16_1:
	move.w %d1,%d5
	add.w %d2,%d1
	sub.w #128,%d1
	ble.s .L__cs16_2
	sub.w %d1,%d2
.L__cs16_2:
	mulu.w #30,%d5
	adda.w %d5,%a1
	move.w %d0,%d1
	not.w %d0
	and.w #15,%d0
	moveq #-1,%d4
	addq.w #1,%d0
	asr.w #4,%d1
	add.w %d1,%d1
	bge.s .L__cs16_3
	cmp.w #-2,%d1
	blt.s .L__cs16_rts
	clr.w %d4
	swap %d4
.L__cs16_3:
	cmp.w #28,%d1
	blt.s .L__cs16_5
	beq.s .L__cs16_4
.L__cs16_rts:
	movem.l (%a7)+,%d3-%d5
	rts
.L__cs16_4:
	clr.w %d4
.L__cs16_5:
	adda.w %d1,%a1

	subq.w #1,%d3
	moveq #0,%d5
	not.w %d5
	lsl.l %d0,%d5
	and.l %d4,%d5
	not.l %d5 |ANDing flag used in RPLC mode
	bra.s .L__cs16_loopentry | 2

.L__cs16_AND:
	not.w %d1
	lsl.l %d0,%d1
	and.l %d4,%d1
	not.l %d1
	and.l %d1,(%a1)

.L__cs16_loop:
	lea 30(%a1),%a1 | 4

.L__cs16_loopentry:
	subq.w #1,%d2
	blt.s .L__cs16_rts

	moveq #0,%d1
	move.w (%a0)+,%d1
	cmp.w #1,%d3
	beq.s .L__cs16_AND
	lsl.l %d0,%d1
	and.l %d4,%d1
	tst.w %d3
	blt.s .L__cs16_XOR
	beq.s .L__cs16_OR
|RPLC
	and.l %d5,(%a1)
.L__cs16_OR:
	or.l %d1,(%a1)
	bra.s .L__cs16_loop
.L__cs16_XOR:
	eor.l %d1,(%a1)
	bra.s .L__cs16_loop
