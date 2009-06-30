
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
	not.w %d0 | 2
	and.w #15,%d0 | 4
	add.w #9,%d0 | 4

	move.w %d4,-(%a7) | 2
	move.l %d3,-(%a7) | 2
| d4: drawing mode.
	move.w 4+6(%a7),%d4 | 4
	subq.w #1,%d4 | 2
| d3: mask used by AND and RPLC.
	moveq #-1,%d3 | 2
	clr.b %d3 | 2
	rol.l %d0,%d3 | 2
| Jump to loop entry.
	bra.s .L__s8_loopentry | 2

| AND.
.L__s8_Am:
	or.l %d3,%d1 | 2
	and.l %d1,(%a1) | 2

| Next line.
.L__s8_loop:
	lea 30(%a1),%a1 | 4

.L__s8_loopentry:
| Have we finished ?
	subq.w #1,%d2 | 2
	blt.s .L__s8_rts | 2

	moveq #0,%d1 | 2
	move.b (%a0)+,%d1 | 2
	lsl.l %d0,%d1 | 2
	cmp.w #1,%d4 | 4
	beq.s .L__s8_Am | 2
	tst.w %d4 | 2
	blt.s .L__s8_Xm | 2
	beq.s .L__s8_Om | 2
| RPLC.
	and.l %d3,(%a1) | 2

| OR.
.L__s8_Om:
	or.l %d1,(%a1) | 2
	bra.s .L__s8_loop | 2

| XOR
.L__s8_Xm:
	eor.l %d1,(%a1) | 2
	bra.s .L__s8_loop | 2

| Return
.L__s8_rts:
	move.l (%a7)+,%d3 | 2
	move.w (%a7)+,%d4 | 2
	rts | 2
