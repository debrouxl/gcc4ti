	.file	"fopen.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
	.text
	.set MT_TEXT,0x8000
	.text
	.set MT_XREF,0x9000
	.text
	.set MT_ICON,0xA000
	.text
	.set MT_CASCADE,0x4000
	.section	.rodata.__unalignedstr,"dmu"
.LC0:
	.ascii "wa+\0"
#NO_APP
	.text
	.even
	.globl	fopen
fopen:
	lea (-68,%sp),%sp
	movm.l #0x1f3e,-(%sp)
	move.l %a1,%a6
	move.l %a1,%a5
	move.b (%a5)+,%d6
	move.b (%a5),%d0
	cmp.b #98,%d0
	jbeq .L2
	tst.b %d0
	jbeq .L4
	cmp.b #98,2(%a1)
	jbne .L4
.L2:
	moveq #1,%d7
	jbra .L6
.L4:
	clr.w %d7
.L6:
	clr.b 40(%sp)
	lea (40,%sp),%a4
.L7:
	addq.l #1,%a4
	move.b (%a0)+,%d0
	move.b %d0,(%a4)
	jbne .L7
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l %a4,-(%sp)
	move.l 384(%a0),%a0
	jbsr (%a0)
	move.l %d0,%d4
	clr.w %d0
	swap %d0
	move.w %d0,%d1
	addq.l #4,%sp
	jbeq .L9
	move.l 200.w,%a0
	move.l 484(%a0),%a0
	move.w %d1,%d0
	swap %d0
	mov.w %d4,%d0
	move.l %d0,-(%sp)
	jbsr (%a0)
	move.l %a0,%a2
	move.w 10(%a0),%d0
	and.w #-32232,%d0
	addq.l #4,%sp
	jbeq .L11
	move.l 200.w,%a0
	pea .LC0
	move.l %a6,-(%sp)
	move.l 2520(%a0),%a0
	jbsr (%a0)
	addq.l #8,%sp
	cmp.w #0,%a0
	jbne .L13
.L11:
	move.w 12(%a2),%d3
	jbra .L14
.L9:
	cmp.b #114,%d6
	jbeq .L13
	clr.w %d3
.L14:
	move.l 200.w,%a0
	pea 20.w
	move.l 648(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a3
	addq.l #4,%sp
	cmp.w #0,%a0
	jbeq .L13
	cmp.b #114,%d6
	jbeq .L17
	cmp.b #97,%d6
	jbeq .L17
	sub.l %a2,%a2
	clr.w %d1
	jbra .L20
.L17:
	tst.w %d3
	jbne .L21
	moveq #119,%d6
	jbra .L23
.L21:
	move.l 200.w,%a0
	move.w %d3,-(%sp)
	move.l 612(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a2
	move.l 200.w,%a0
	move.w %d3,-(%sp)
	move.l 632(%a0),%a0
	jbsr (%a0)
	move.l %d0,14(%a3)
	moveq #1,%d1
	addq.l #4,%sp
.L20:
	cmp.b #119,%d6
	jbne .L24
.L23:
	move.l 200.w,%a0
	lea (90,%sp),%a2
	move.l %a2,-(%sp)
	move.l 1664(%a0),%a0
	jbsr (%a0)
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l %a4,-(%sp)
	move.l 368(%a0),%a0
	jbsr (%a0)
	move.l %d0,%d4
	move.l %d0,%d3
	clr.w %d3
	swap %d3
	move.w %d3,%d5
	move.l 200.w,%a0
	move.l %a2,-(%sp)
	move.l 1668(%a0),%a0
	jbsr (%a0)
	lea (12,%sp),%sp
	tst.w %d3
	jbeq .L25
	move.l 200.w,%a0
	pea 128.w
	move.l 576(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d3
	move.l 200.w,%a0
	move.l 484(%a0),%a0
	move.w %d5,%d0
	swap %d0
	mov.w %d4,%d0
	move.l %d0,-(%sp)
	jbsr (%a0)
	move.w %d3,12(%a0)
	addq.l #8,%sp
	jbne .L27
.L25:
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l %a4,-(%sp)
	move.l 376(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.l %a3,-(%sp)
	move.l 652(%a0),%a0
	jbsr (%a0)
	sub.l %a3,%a3
	addq.l #8,%sp
	jbra .L28
.L27:
	move.l #128,14(%a3)
	move.l 200.w,%a0
	move.w %d3,-(%sp)
	move.l 612(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a2
	addq.l #2,%sp
	tst.w %d7
	jbeq .L29
	clr.w (%a0)
	jbra .L49
.L29:
	move.l #327681,(%a0)
	move.l #536928256,4(%a0)
.L49:
	moveq #2,%d1
.L24:
	moveq #0,%d0
	move.w (%a2),%d0
	move.l %a2,%d2
	add.l %d0,%d2
	tst.w %d7
	jbeq .L31
	moveq #2,%d0
	jbra .L33
.L31:
	moveq #0,%d0
.L33:
	move.l %d2,%a0
	add.l %d0,%a0
	cmp.b #97,%d6
	jbne .L34
	move.l %a0,%d0
	moveq #2,%d1
	jbra .L36
.L34:
	tst.w %d7
	jbeq .L37
	moveq #2,%d0
	jbra .L39
.L37:
	moveq #5,%d0
.L39:
	add.l %a2,%d0
	cmp.l %a0,%d0
	jbne .L40
.L36:
	or.w #32,%d1
.L40:
	cmp.b #43,(%a5)
	jbeq .L41
	cmp.b #43,2(%a6)
	jbne .L43
.L41:
	or.w #3,%d1
.L43:
	tst.w %d7
	jbeq .L44
	or.w #64,%d1
.L44:
	move.w %d3,8(%a3)
	move.l %a2,4(%a3)
	move.w %d1,10(%a3)
	move.l %d0,(%a3)
	clr.w 12(%a3)
	move.w #128,18(%a3)
	jbra .L28
.L13:
	sub.l %a3,%a3
.L28:
	move.l %a3,%a0
	movm.l (%sp)+,#0x7cf8
	lea (68,%sp),%sp
	rts
