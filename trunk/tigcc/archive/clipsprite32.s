
.globl ClipSprite32
.text
.even
ClipSprite32:
	movem.l %d3-%d7,-(%a7)

	tst.w %d1
	bge.s .L__cs32_1
	add.w %d1,%d2
	asl.w #2,%d1
	suba.w %d1,%a0
	moveq #0,%d1
.L__cs32_1:
	move.w %d1,%d3
	add.w %d2,%d1
	sub.w #128,%d1
	ble.s .L__cs32_2
	sub.w %d1,%d2
.L__cs32_2:
	move.w %d3,%d1
	mulu.w #30,%d1
	adda.w %d1,%a1
	move.w %d0,%d1
	move.w %d0,%d5
	asr.w #4,%d1
	add.w %d1,%d1
	adda.w %d1,%a1

	and.w #15,%d0
	moveq #16,%d6 | 2
	sub.w %d0,%d6 | 2

	moveq #-1,%d4
	lsl.w %d6,%d4
	moveq #-1,%d3
	lsr.l %d0,%d3

	tst.w %d5
	bge.s .L__cs32_3
	swap %d3
	clr.w %d3
	swap %d3
	cmp.w #-16,%d5
	bge.s .L__cs32_3
	moveq #0,%d3
	cmp.w #-32,%d5
	bgt.s .L__cs32_3
.L__cs32_rts:
	movem.l (%a7)+,%d3-%d7
	rts

.L__cs32_3:
	cmp.w #240-32,%d5
	ble.s .L__cs32_4
	moveq #0,%d4
	cmp.w #240-16,%d5
	blt.s .L__cs32_4
	clr.w %d3
	cmp.w #240,%d5
	bge.s .L__cs32_rts

.L__cs32_4:
	move.l %d3,%d6
	move.w %d4,%d7
	not.l %d6
	not.w %d7
	move.l %d6,.L__cs32_leftmaskdropoff+2
	move.w 2(%a7),%d5
	subq.w #2,%d5
	bne.s .L__cs32_notand
	move.l %d6,%d3
	move.w %d7,%d4
.L__cs32_notand:
	lea -26(%a1),%a1
.L__cs32_loop:
	subq.w #1,%d2
	blt.s .L__cs32_rts
	lea 26(%a1),%a1
	move.l (%a0)+,%d1
	ror.l %d0,%d1
	move.l %d1,%d6
	swap %d6
	tst.w %d5
	beq.s .L__cs32_Am
	and.l %d3,%d1 |left side
	and.w %d4,%d6 |right side
	cmpi.w #-1,%d5
	beq.s .L__cs32_Om
	bgt.s .L__cs32_Rm
	|XOR
	eor.l %d1,(%a1)+
	eor.w %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Rm:	|RPLC
  .L__cs32_leftmaskdropoff:
	and.l #0xDEED,(%a1)
	or.l %d1,(%a1)+
	and.w %d7,(%a1)
	or.w %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Om:	|OR
	or.l %d1,(%a1)+
	or.w %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Am:	|AND
	or.l %d3,%d1 |left side
	or.w %d4,%d6 |right side
	and.l %d1,(%a1)+
	and.w %d6,(%a1)
	bra.s .L__cs32_loop


| This one works, but uses 2 longword operations, where 1 longword + 1 word operations are enough.
/*
	.xdef ClipSprite32

.text
ClipSprite32:
	movem.l %d3-%d7,-(%a7)

	tst.w %d1
	bge.s .L__cs32_nonegY
	add.w %d1,%d2
	asl.w #2,%d1
	suba.w %d1,%a0
	moveq #0,%d1
.L__cs32_nonegY:
	move.w %d1,%d3
	add.w %d2,%d1
	sub.w #128,%d1
	ble.s .L__cs32_nooverY
	sub.w %d1,%d2
.L__cs32_nooverY:
	move.w %d3,%d1
	mulu.w #30,%d1
	adda.w %d1,%a1
	move.w %d0,%d1
	move.w %d0,%d5
	asr.w #4,%d1
	add.w %d1,%d1
	adda.w %d1,%a1

	and.w #15,%d0
	move.w %d0,%d6
	eor.w #31,%d6
	addq.w #1,%d6

	moveq #-1,%d4
	lsl.l %d6,%d4
	moveq #-1,%d3
	lsr.l %d0,%d3

	tst.w %d5
	bge.s .L__cs32_nonegX
	swap %d3
	clr.w %d3
	swap %d3
	cmp.w #-16,%d5
	bge.s .L__cs32_nonegX
	moveq #0,%d3
	cmp.w #-32,%d5
	bgt.s .L__cs32_nonegX
.L__cs32_rts:
	movem.l (%a7)+,%d3-%d7
	rts

.L__cs32_nonegX:
	cmp.w #240-32,%d5
	ble.s .L__cs32_nooverX
	moveq #0,%d4
	cmp.w #240-16,%d5
	blt.s .L__cs32_nooverX
	clr.w %d3
	cmp.w #240,%d5
	bge.s .L__cs32_rts

.L__cs32_nooverX:
	move.l %d3,%d6
	move.l %d4,%d7
	not.l %d6
	not.l %d7
	move.l %d6,.L__cs32_leftmaskdropoff+2
	move.w 4+5*4(%a7),%d5
	subq.w #2,%d5
	bne.s .L__cs32_notand
	move.l %d6,%d3
	move.l %d7,%d4
.L__cs32_notand:
	lea -26(%a1),%a1
.L__cs32_loop:
	subq.w #1,%d2
	blt.s .L__cs32_rts
	lea 26(%a1),%a1
	move.l (%a0)+,%d1
	ror.l %d0,%d1
	move.l %d1,%d6
	tst.w %d5
	beq.s .L__cs32_Am
	and.l %d3,%d1 |left side
	and.l %d4,%d6 |right side
	cmpi.w #-1,%d5
	beq.s .L__cs32_Om
	bgt.s .L__cs32_Rm
	|XOR
	eor.l %d1,(%a1)+
	eor.l %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Rm:	|RPLC
  .L__cs32_leftmaskdropoff:
	and.l #0xDEED,(%a1)
	or.l %d1,(%a1)+
	and.l %d7,(%a1)
	or.l %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Om:	|OR
	or.l %d1,(%a1)+
	or.l %d6,(%a1)
	bra.s .L__cs32_loop
.L__cs32_Am:	|AND
	or.l %d3,%d1 |left side
	or.l %d4,%d6 |right side
	and.l %d1,(%a1)+
	and.l %d6,(%a1)
	bra.s .L__cs32_loop
*/
