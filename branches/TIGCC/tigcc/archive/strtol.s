	.file	"strtol.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	strtol
strtol:
	movm.l #0x1f3e,-(%sp)
	move.l %a0,%a5
	move.l %a1,%a6
	move.w %d0,%d5
	move.l %a0,%a3
	clr.w %d7
.L2:
	clr.w %d3
	move.b (%a3)+,%d3
	cmp.w #32,%d3
	jbeq .L2
	cmp.w #45,%d3
	jbeq .L7
	cmp.w #173,%d3
	jbne .L6
.L7:
	moveq.l #1,%d7
	jbra .L41
	.even
.L6:
	cmp.w #43,%d3
	jbne .L8
.L41:
	clr.w %d3
	move.b (%a3)+,%d3
.L8:
	tst.w %d5
	jbeq .L11
	cmp.w #16,%d5
	jbne .L10
.L11:
	cmp.w #48,%d3
	jbne .L10
	move.b (%a3),%d0
	cmp.b #120,%d0
	jbeq .L12
	cmp.b #88,%d0
	jbne .L10
.L12:
	clr.w %d3
	move.b 1(%a3),%d3
	addq.l #2,%a3
	moveq.l #16,%d5
.L10:
	tst.w %d5
	jbne .L13
	moveq.l #8,%d5
	cmp.w #48,%d3
	jbeq .L13
	moveq.l #10,%d5
.L13:
	move.l #-2147483648,%d4
	tst.w %d7
	jbne .L17
	subq.l #1,%d4
.L17:
	move.w %d5,%a2
#APP
	
	move.l %d4,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AA*4),%a0
	jsr (%a0)
	move.l %d1,%a2
	move.l %d4,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AB*4),%a0
	jsr (%a0)
	move.w %d1,%a4
#NO_APP
	moveq.l #0,%d4
	clr.w %d6
.L18:
	move.w %d3,%d1
	add.w #-48,%d1
	cmp.w #9,%d1
	sls %d0
	ext.w %d0
	neg.w %d0
	jbne .L27
	clr.w %d2
	move.w %d3,%d1
	add.w #-65,%d1
	cmp.w #25,%d1
	jbls .L25
	move.w %d3,%d0
	add.w #-97,%d0
	cmp.w #25,%d0
	jbhi .L24
.L25:
	moveq.l #1,%d2
.L24:
	tst.w %d2
	jbeq .L19
	cmp.w #25,%d1
	sls %d0
	ext.w %d0
	neg.w %d0
	move.w %d3,%d1
	add.w #-55,%d1
	tst.w %d0
	jbne .L27
	add.w #-32,%d1
.L27:
	move.w %d1,%d3
	cmp.w %d1,%d5
	jbls .L19
	tst.w %d6
	jblt .L31
	cmp.l %d4,%a2
	jbcs .L31
	jbne .L30
	cmp.w %a4,%d1
	jbls .L30
.L31:
	moveq.l #-1,%d6
	jbra .L20
	.even
.L30:
	moveq.l #1,%d6
#APP
	
	move.l %d4,%d0
	mulu %d5,%d0
	move.l %d4,%d1
	swap %d1
	mulu %d5,%d1
	swap %d1
	clr.w %d1
	add.l %d1,%d0
	move.l %d0,%d4
#NO_APP
	clr.l %d0
	move.w %d3,%d0
	add.l %d0,%d4
.L20:
	clr.w %d3
	move.b (%a3)+,%d3
	jbra .L18
	.even
.L19:
	tst.w %d6
	jbge .L33
	move.l #-2147483648,%d4
	tst.w %d7
	jbne .L36
	subq.l #1,%d4
	jbra .L36
	.even
.L33:
	tst.w %d7
	jbeq .L36
	neg.l %d4
.L36:
	cmp.w #0,%a6
	jbeq .L38
	lea (-1,%a3),%a0
	tst.w %d6
	jbne .L40
	move.l %a5,%a0
.L40:
	move.l %a0,(%a6)
.L38:
	move.l %d4,%d0
	movm.l (%sp)+,#0x7cf8
	rts
