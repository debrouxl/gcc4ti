	.file	"tmpnam.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
buff.0:
	.byte	0
	.skip 9
	.text
	.even
	.globl	tmpnam
tmpnam:
	movm.l #0x1c20,-(%sp)
	move.l %a0,%d4
	move.l #buff.0,%d5
.L2:
	move.l %d5,%a2
	addq.l #1,%a2
	moveq.l #7,%d3
	jbra .L5
	.even
.L9:
	jbsr rand
	moveq.l #25,%d2
	ext.l %d0
	divs.w %d2,%d0
	move.l %d0,%d1
	swap %d1
	add.b #97,%d1
	move.b %d1,(%a2)+
	subq.w #1,%d3
.L5:
	tst.w %d3
	jbge .L9
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l 384(%a0),%a0
	pea buff.0+9
	jbsr (%a0)
	addq.l #4,%sp
	tst.w %d0
	jbne .L2
	move.l #buff.0+1,%d0
	tst.l %d4
	jbeq .L1
	move.l 200.w,%a0
	move.l %d0,-(%sp)
	move.l %d4,-(%sp)
	move.l 2480(%a0),%a0
	jbsr (%a0)
	move.l %a0,%d0
	addq.l #8,%sp
.L1:
	move.l %d0,%a0
	movm.l (%sp)+,#0x438
	rts
