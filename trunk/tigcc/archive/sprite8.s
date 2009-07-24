
.globl Sprite8
.text
.even
Sprite8:
| Compute offset from beginning of plane.
	mulu.w #30,%d1 | 4
	adda.l %d1,%a1 | 2
	move.w %d0,%d1 | 2
	lsr.w #4,%d1 | 2
	add.w %d1,%d1 | 2
	adda.w %d1,%a1 | 2
| d0: shift count.
	moveq #24,%d1 | 2
	and.w #15,%d0 | 4
	sub.w %d0,%d1 | 2

	move.l %d4,-(%a7) | 2
| d3: drawing mode.
	subq.w #1,%d3 | 2
| d4: mask used by AND and RPLC.
	moveq #-1,%d4 | 2
	clr.b %d4 | 2
	rol.l %d1,%d4 | 2
| Jump to loop entry.
	bra.s .L__s8_loopentry | 2

| AND.
.L__s8_Am:
	or.l %d4,%d0 | 2
	and.l %d0,(%a1) | 2

| Next line.
.L__s8_loop:
	lea 30(%a1),%a1 | 4

.L__s8_loopentry:
| Have we finished ?
	subq.w #1,%d2 | 2
	blt.s .L__s8_rts | 2

	moveq #0,%d0 | 2
	move.b (%a0)+,%d0 | 2
	lsl.l %d1,%d0 | 2
	cmp.w #1,%d3 | 4
	beq.s .L__s8_Am | 2
	tst.w %d3 | 2
	blt.s .L__s8_Xm | 2
	beq.s .L__s8_Om | 2
| RPLC.
	and.l %d4,(%a1) | 2

| OR.
.L__s8_Om:
	or.l %d0,(%a1) | 2
	bra.s .L__s8_loop | 2

| XOR
.L__s8_Xm:
	eor.l %d0,(%a1) | 2
	bra.s .L__s8_loop | 2

| Return
.L__s8_rts:
	addq.w #1,%d3 | 2
	move.l (%a7)+,%d4 | 2
	rts | 2
