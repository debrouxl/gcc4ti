	.file	"fopen.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
	.set MT_TEXT,0x8000
	.set MT_XREF,0x9000
	.set MT_ICON,0xA000
	.set MT_CASCADE,0x4000
#NO_APP
	.section	.rodata.__unalignedstr,"dmu"
.LC0:
	.ascii "wa+\0"
	.text
	.even
	.globl	fopen
fopen:
	lea (-76,%sp),%sp
	movm.l #0x1f3e,-(%sp)
	move.l %a1,%a6
	sub.l %a4,%a4
	lea (62,%sp),%a5
	move.b (%a1),%d7
	clr.w 42(%sp)
	move.b 1(%a1),%d0
	cmp.b #98,%d0
	jbeq .L3
	tst.b %d0
	jbeq .L2
	cmp.b #98,2(%a1)
	jbne .L2
.L3:
	move.w #1,42(%sp)
.L2:
	clr.w %d6
	clr.w %d5
	clr.b (%a5)
.L4:
	addq.l #1,%a5
	move.b (%a0)+,%d0
	move.b %d0,(%a5)
	jbne .L4
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l 384(%a0),%a0
	move.l %a5,-(%sp)
	jbsr (%a0)
	move.l %d0,%d4
	clr.w %d0
	swap %d0
	addq.l #4,%sp
	tst.w %d0
	jbeq .L7
	move.l 200.w,%a0
	move.l %d4,-(%sp)
	move.l 484(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a2
	move.w 10(%a0),%d0
	and.w #-32232,%d0
	addq.l #4,%sp
	jbeq .L8
	move.l 200.w,%a0
	pea .LC0
	move.l %a6,-(%sp)
	move.l 2520(%a0),%a0
	jbsr (%a0)
	addq.l #8,%sp
	moveq.l #0,%d0
	cmp.w #0,%a0
	jbne .L1
.L8:
	move.w 12(%a2),%d5
	jbra .L9
	.even
.L7:
	moveq.l #0,%d0
	cmp.b #114,%d7
	jbeq .L1
.L9:
	move.l 200.w,%a0
	pea 20.w
	move.l 648(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a3
	addq.l #4,%sp
	moveq.l #0,%d0
	cmp.w #0,%a0
	jbeq .L1
	cmp.b #114,%d7
	jbeq .L13
	cmp.b #97,%d7
	jbne .L12
.L13:
	moveq.l #1,%d6
	tst.w %d5
	jbeq .L14
	move.l 200.w,%a0
	move.w %d5,-(%sp)
	move.l 612(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	move.l 200.w,%a0
	move.w %d5,-(%sp)
	move.l 632(%a0),%a0
	jbsr (%a0)
	move.l %d0,14(%a3)
	addq.l #4,%sp
	jbra .L12
	.even
.L14:
	moveq.l #119,%d7
.L12:
	cmp.b #119,%d7
	jbne .L16
	moveq.l #2,%d6
	move.l 200.w,%a0
	lea (44,%sp),%a2
	move.l %a2,-(%sp)
	move.l 1664(%a0),%a0
	jbsr (%a0)
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l 368(%a0),%a0
	move.l %a5,-(%sp)
	jbsr (%a0)
	move.l %d0,%d4
	clr.w %d0
	swap %d0
	addq.l #8,%sp
	tst.w %d0
	seq %d0
	move.b %d0,%d3
	ext.w %d3
	neg.w %d3
	move.l 200.w,%a0
	move.l %a2,-(%sp)
	move.l 1668(%a0),%a0
	jbsr (%a0)
	addq.l #4,%sp
	tst.w %d3
	jbne .L17
	move.l 200.w,%a0
	pea 128.w
	move.l 576(%a0),%a0
	jbsr (%a0)
	move.w %d0,%d5
	move.l 200.w,%a0
	move.l %d4,-(%sp)
	move.l 484(%a0),%a0
	jbsr (%a0)
	move.w %d5,12(%a0)
	addq.l #8,%sp
	seq %d0
	move.b %d0,%d3
	ext.w %d3
	neg.w %d3
.L17:
	move.l 200.w,%a0
	tst.w %d3
	jbeq .L18
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 376(%a0),%a0
	move.l %a5,-(%sp)
	jbsr (%a0)
	move.l 200.w,%a0
	move.l %a3,-(%sp)
	move.l 652(%a0),%a0
	jbsr (%a0)
	moveq.l #0,%d0
	addq.l #8,%sp
	jbra .L1
	.even
.L18:
	move.l #128,14(%a3)
	move.w %d5,-(%sp)
	move.l 612(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	addq.l #2,%sp
	tst.w 42(%sp)
	jbeq .L19
	clr.w (%a0)
	jbra .L16
	.even
.L19:
	move.l #327681,(%a0)
	move.l #536928256,4(%a0)
.L16:
	clr.l %d0
	move.w (%a4),%d0
	add.l %a4,%d0
	tst.w 42(%sp)
	jbeq .L21
	addq.l #2,%d0
.L21:
	cmp.b #97,%d7
	jbne .L22
	moveq.l #2,%d6
	move.l %d0,%a0
	jbra .L23
	.even
.L22:
	lea (2,%a4),%a0
	tst.w 42(%sp)
	jbne .L23
	lea (5,%a4),%a0
.L23:
	cmp.l %d0,%a0
	jbne .L26
	or.w #32,%d6
.L26:
	cmp.b #43,1(%a6)
	jbeq .L28
	cmp.b #43,2(%a6)
	jbne .L27
.L28:
	or.w #3,%d6
.L27:
	tst.w 42(%sp)
	jbeq .L29
	or.w #64,%d6
.L29:
	move.w %d5,8(%a3)
	move.l %a4,4(%a3)
	move.w %d6,10(%a3)
	move.l %a0,(%a3)
	clr.w 12(%a3)
	move.w #128,18(%a3)
	move.l %a3,%d0
.L1:
	move.l %d0,%a0
	movm.l (%sp)+,#0x7cf8
	lea (76,%sp),%sp
	rts
