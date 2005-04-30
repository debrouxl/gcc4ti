	.file	"strtoul.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	strtoul
strtoul:
	movm.l #0x1f38,-(%sp)
	move.l %a0,%d7
	move.l %a1,%a4
	move.w %d0,%d4
.L2:
	move.l %a0,%a3
	move.b (%a3)+,%d0
	clr.w %d3
	move.b %d0,%d3
	cmp.b #32,%d0
	jbne .L3
	move.l %a3,%a0
	jbra .L2
.L3:
	cmp.b #43,%d0
	jbne .L5
	clr.w %d3
	move.b (%a3),%d3
	lea (2,%a0),%a3
.L5:
	tst.w %d4
	jbeq .L7
	cmp.w #16,%d4
	jbne .L9
.L7:
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
	moveq #16,%d4
	jbra .L9
.L10:
	tst.w %d4
	jbne .L9
	cmp.w #48,%d3
	jbne .L16
	moveq #8,%d4
	jbra .L9
.L16:
	moveq #10,%d4
.L9:
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
	move.w %d1,%d6
#NO_APP
	moveq #0,%d5
	clr.w %d1
.L18:
	move.w %d3,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	jbhi .L19
	move.w %d0,%a1
	jbra .L21
.L19:
	move.w %d3,%d0
	add.w #-65,%d0
	cmp.w #25,%d0
	jbhi .L22
	moveq #55,%d0
	jbra .L24
.L22:
	move.w %d3,%d0
	add.w #-97,%d0
	cmp.w #25,%d0
	jbhi .L25
	moveq #87,%d0
.L24:
	move.w %d3,%a1
	sub.w %d0,%a1
.L21:
	cmp.w %a1,%d4
	jbls .L25
	tst.w %d1
	jblt .L28
	cmp.l %d5,%a2
	jbcs .L28
	jbne .L31
	cmp.w %a1,%d6
	jbcs .L28
.L31:
#APP
	
	move.l %d5,%d0
	mulu %d4,%d0
	move.l %d5,%d1
	swap %d1
	mulu %d4,%d1
	swap %d1
	clr.w %d1
	add.l %d1,%d0
	move.l %d0,%a0
#NO_APP
	moveq #0,%d0
	move.w %a1,%d0
	move.l %a0,%d5
	add.l %d0,%d5
	moveq #1,%d1
	jbra .L34
.L28:
	moveq #-1,%d1
.L34:
	clr.w %d3
	move.b (%a3)+,%d3
	jbra .L18
.L25:
	tst.w %d1
	jbge .L35
	moveq #-1,%d5
.L35:
	cmp.w #0,%a4
	jbeq .L37
	tst.w %d1
	jbeq .L39
	lea (-1,%a3),%a0
	jbra .L41
.L39:
	move.l %d7,%a0
.L41:
	move.l %a0,(%a4)
.L37:
	move.l %d5,%d0
	movm.l (%sp)+,#0x1cf8
	rts
