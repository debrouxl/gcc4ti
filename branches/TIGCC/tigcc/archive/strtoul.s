	.file	"strtoul.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	strtoul
strtoul:
	movm.l #0x1f3c,-(%sp)
	move.l %a0,%a4
	move.l %a1,%a5
	move.w %d0,%d4
	move.l %a0,%a3
.L2:
	clr.w %d3
	move.b (%a3)+,%d3
	cmp.w #32,%d3
	jbeq .L2
	cmp.w #43,%d3
	jbne .L6
	clr.w %d3
	move.b (%a3)+,%d3
.L6:
	tst.w %d4
	jbeq .L8
	cmp.w #16,%d4
	jbne .L7
.L8:
	cmp.w #48,%d3
	jbne .L7
	move.b (%a3),%d0
	cmp.b #120,%d0
	jbeq .L9
	cmp.b #88,%d0
	jbne .L7
.L9:
	clr.w %d3
	move.b 1(%a3),%d3
	addq.l #2,%a3
	moveq.l #16,%d4
.L7:
	tst.w %d4
	jbne .L10
	moveq.l #8,%d4
	cmp.w #48,%d3
	jbeq .L10
	moveq.l #10,%d4
.L10:
	move.w %d4,%a2
#APP
	
	move.l #-1,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AA*4),%a0
	jsr (%a0)
	move.l %d1,%a2
	move.l #-1,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AB*4),%a0
	jsr (%a0)
	move.w %d1,%d7
#NO_APP
	moveq.l #0,%d5
	clr.w %d6
.L13:
	move.w %d3,%d1
	add.w #-48,%d1
	cmp.w #9,%d1
	sls %d0
	ext.w %d0
	neg.w %d0
	jbne .L22
	clr.w %d2
	move.w %d3,%d1
	add.w #-65,%d1
	cmp.w #25,%d1
	jbls .L20
	move.w %d3,%d0
	add.w #-97,%d0
	cmp.w #25,%d0
	jbhi .L19
.L20:
	moveq.l #1,%d2
.L19:
	tst.w %d2
	jbeq .L14
	cmp.w #25,%d1
	sls %d0
	ext.w %d0
	neg.w %d0
	move.w %d3,%d1
	add.w #-55,%d1
	tst.w %d0
	jbne .L22
	add.w #-32,%d1
.L22:
	move.w %d1,%d3
	cmp.w %d1,%d4
	jbls .L14
	tst.w %d6
	jblt .L26
	cmp.l %d5,%a2
	jbcs .L26
	jbne .L25
	cmp.w %d1,%d7
	jbcc .L25
.L26:
	moveq.l #-1,%d6
	jbra .L15
	.even
.L25:
	moveq.l #1,%d6
#APP
	
	move.l %d5,%d0
	mulu %d4,%d0
	move.l %d5,%d1
	swap %d1
	mulu %d4,%d1
	swap %d1
	clr.w %d1
	add.l %d1,%d0
	move.l %d0,%d5
#NO_APP
	clr.l %d0
	move.w %d3,%d0
	add.l %d0,%d5
.L15:
	clr.w %d3
	move.b (%a3)+,%d3
	jbra .L13
	.even
.L14:
	tst.w %d6
	jbge .L28
	moveq.l #-1,%d5
.L28:
	cmp.w #0,%a5
	jbeq .L29
	lea (-1,%a3),%a0
	tst.w %d6
	jbne .L31
	move.l %a4,%a0
.L31:
	move.l %a0,(%a5)
.L29:
	move.l %d5,%d0
	movm.l (%sp)+,#0x3cf8
	rts
