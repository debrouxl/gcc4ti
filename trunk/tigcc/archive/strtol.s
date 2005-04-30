	.file	"strtol.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	strtol
strtol:
	movm.l #0x1f3c,-(%sp)
	move.l %a0,%a4
	move.l %a1,%a5
	move.w %d0,%d5
.L2:
	move.l %a0,%a3
	move.b (%a3)+,%d0
	clr.w %d4
	move.b %d0,%d4
	cmp.b #32,%d0
	jbne .L3
	move.l %a3,%a0
	jbra .L2
.L3:
	cmp.b #45,%d0
	jbeq .L5
	cmp.b #-83,%d0
	jbne .L7
.L5:
	clr.w %d4
	move.b (%a3),%d4
	lea (2,%a0),%a3
	moveq #1,%d6
	jbra .L8
.L7:
	cmp.b #43,%d0
	jbne .L54
	clr.w %d4
	move.b (%a3),%d4
	lea (2,%a0),%a3
.L54:
	clr.w %d6
.L8:
	tst.w %d5
	jbeq .L11
	cmp.w #16,%d5
	jbne .L13
.L11:
	cmp.w #48,%d4
	jbne .L14
	move.b (%a3),%d0
	cmp.b #120,%d0
	jbeq .L16
	cmp.b #88,%d0
	jbne .L14
.L16:
	clr.w %d4
	move.b 1(%a3),%d4
	addq.l #2,%a3
	moveq #16,%d5
	jbra .L13
.L14:
	tst.w %d5
	jbne .L13
	cmp.w #48,%d4
	jbne .L20
	moveq #8,%d5
	jbra .L13
.L20:
	moveq #10,%d5
.L13:
	move.l #-2147483648,%d3
	tst.w %d6
	jbne .L24
	subq.l #1,%d3
.L24:
	move.w %d5,%a2
#APP
	
	move.l %d3,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AA*4),%a0
	jsr (%a0)
	move.l %d1,%a2
	move.l %d3,%d1
	move.l %a2,%d0
	move.l 0xC8,%a0
	move.l (%a0,0x2AB*4),%a0
	jsr (%a0)
	move.w %d1,%d7
#NO_APP
	moveq #0,%d3
	clr.w %d1
.L25:
	move.w %d4,%d0
	add.w #-48,%d0
	cmp.w #9,%d0
	jbhi .L26
	move.w %d0,%a1
	jbra .L28
.L26:
	move.w %d4,%d0
	add.w #-65,%d0
	cmp.w #25,%d0
	jbhi .L29
	moveq #55,%d0
	jbra .L31
.L29:
	move.w %d4,%d0
	add.w #-97,%d0
	cmp.w #25,%d0
	jbhi .L32
	moveq #87,%d0
.L31:
	move.w %d4,%a1
	sub.w %d0,%a1
.L28:
	cmp.w %a1,%d5
	jbls .L32
	tst.w %d1
	jblt .L35
	cmp.l %d3,%a2
	jbcs .L35
	jbne .L38
	cmp.w %a1,%d7
	jbcs .L35
.L38:
#APP
	
	move.l %d3,%d0
	mulu %d5,%d0
	move.l %d3,%d1
	swap %d1
	mulu %d5,%d1
	swap %d1
	clr.w %d1
	add.l %d1,%d0
	move.l %d0,%a0
#NO_APP
	moveq #0,%d0
	move.w %a1,%d0
	move.l %a0,%d3
	add.l %d0,%d3
	moveq #1,%d1
	jbra .L41
.L35:
	moveq #-1,%d1
.L41:
	clr.w %d4
	move.b (%a3)+,%d4
	jbra .L25
.L32:
	tst.w %d1
	jbge .L42
	move.l #-2147483648,%d3
	tst.w %d6
	jbne .L46
	subq.l #1,%d3
	jbra .L46
.L42:
	tst.w %d6
	jbeq .L46
	neg.l %d3
.L46:
	cmp.w #0,%a5
	jbeq .L48
	tst.w %d1
	jbeq .L50
	lea (-1,%a3),%a0
	jbra .L52
.L50:
	move.l %a4,%a0
.L52:
	move.l %a0,(%a5)
.L48:
	move.l %d3,%d0
	movm.l (%sp)+,#0x3cf8
	rts
