
.globl Sprite32
.text
.even
Sprite32:
	movem.l %d3-%d7,-(%a7) | 4
| Compute offset from beginning of plane.
	mulu.w #30,%d1 | 4
	adda.l %d1,%a1 | 2
	move.w %d0,%d1 | 2
	lsr.w #4,%d1 | 2
	add.w %d1,%d1 | 2
	adda.w %d1,%a1 | 2
| d0, d5: shift counts.
	and.w #15,%d0 | 4
	moveq #16,%d5 | 2
	sub.w %d0,%d5 | 2

| d4: drawing mode.
	subq.w #1,%d3 | 2
| d3, d6: masks used by AND and RPLC.
	moveq #-1,%d4 | 2
	lsr.w %d0,%d4 | 2
	move.w %d4,%d6 | 2
	not.l %d4 | 2
	swap %d4 | 2
| Jump to loop entry.
	bra.s .L__s32_loopentry | 2

| AND.
.L__s32_Am:
	or.l %d4,%d1 | 2
	or.w %d6,%d7 | 2
	and.l %d1,(%a1)+ | 2
	and.w %d7,(%a1) | 2

| Next line.
.L__s32_loop:
	lea 26(%a1),%a1 | 4

.L__s32_loopentry:
| Have we finished ?
	subq.w #1,%d2 | 2
	blt.s .L__s32_rts | 2

	move.l (%a0)+,%d1 | 2
	move.w %d1,%d7 | 2
	lsr.l %d0,%d1 | 2
	lsl.w %d5,%d7 | 2
	cmp.w #1,%d3 | 4
	beq.s .L__s32_Am | 2
	tst.w %d3 | 2
	blt.s .L__s32_Xm | 2
	beq.s .L__s32_Om | 2
| RPLC.
	and.l %d4,(%a1) | 2
	and.w %d6,4(%a1) | 2
| OR.
.L__s32_Om:
	or.l %d1,(%a1)+ | 2
	or.w %d7,(%a1) | 2
	bra.s .L__s32_loop | 2

| XOR
.L__s32_Xm:
	eor.l %d1,(%a1)+ | 2
	eor.w %d7,(%a1) | 2
	bra.s .L__s32_loop | 2

| Return
.L__s32_rts:
	movem.l (%a7)+,%d3-%d7 | 4
	rts | 2
