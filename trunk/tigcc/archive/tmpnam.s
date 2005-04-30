	.file	"tmpnam.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
buff.1358:
	.skip 10
	.text
	.even
	.globl	tmpnam
tmpnam:
	move.l %a2,-(%sp)
	move.l %d3,-(%sp)
	move.l %a0,%d3
.L2:
	lea buff.1358+1,%a2
.L3:
	jbsr rand
	moveq #25,%d2
	ext.l %d0
	divs.w %d2,%d0
	move.l %d0,%d1
	swap %d1
	add.b #97,%d1
	move.b %d1,(%a2)+
	cmp.l #buff.1358+9,%a2
	jbne .L3
#APP
	.xdef __ref_all___set_file_in_use_bit
#NO_APP
	move.l 200.w,%a0
	move.l %a2,-(%sp)
	move.l 384(%a0),%a0
	jbsr (%a0)
	addq.l #4,%sp
	tst.w %d0
	jbne .L2
	move.l #buff.1358+1,%d0
	tst.l %d3
	jbeq .L8
	move.l 200.w,%a0
	move.l %d0,-(%sp)
	move.l %d3,-(%sp)
	move.l 2480(%a0),%a0
	jbsr (%a0)
	move.l %a0,%d0
	addq.l #8,%sp
.L8:
	move.l %d0,%a0
	move.l (%sp)+,%d3
	move.l (%sp)+,%a2
	rts
