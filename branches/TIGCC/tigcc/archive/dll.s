	.file	"dll.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.section	.rodata.__unalignedstr,"dmu"
.LC0:
	.ascii "DLL\0\370\0"
	.text
	.even
	.globl	LoadDLL
LoadDLL:
	link.w %a6,#-12
	movm.l #0x1f3c,-(%sp)
	move.l %a0,%d7
	move.w %d1,%d6
	move.w %d2,%d5
	clr.w -10(%a6)
	clr.w %d4
	move.l #1145850912,-8(%a6)
	move.l %d0,-4(%a6)
#APP
	bsr 0f; 0:move.l (%sp)+,%d3
#NO_APP
	lea __get_hw_version,%a5
	jbsr (%a5)
	cmp.w #2,%d0
	jbne .L2
	moveq.l #1,%d1
	cmp.l #262143,%d3
	jbls .L1
.L2:
	tst.l __DLL_body_ptr
	jbeq .L3
	moveq.l #5,%d1
	jbra .L1
	.even
.L3:
	move.l 200.w,%a0
	move.w #2,-(%sp)
	clr.l -(%sp)
	move.l 432(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	addq.l #6,%sp
.L4:
	move.l 200.w,%a0
	move.l %d7,-(%sp)
	move.l %a4,-(%sp)
	move.l 2500(%a0),%a0
	jbsr (%a0)
	addq.l #8,%sp
	tst.w %d0
	jbne .L6
	move.w 12(%a4),%d1
	jbeq .L6
	move.b 10(%a4),%d0
	btst #2,%d0
	jbne .L6
	btst #1,%d0
	jbne .L8
	move.l 200.w,%a0
	move.w %d1,-(%sp)
	move.l 620(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	tst.w %d0
	jbne .L6
.L8:
	move.l 200.w,%a0
	move.w 12(%a4),-(%sp)
	move.l 600(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a3
	move.w (%a0),%d3
	addq.w #2,%d3
	move.l 200.w,%a0
	pea 5.w
	pea .LC0
	clr.l %d0
	move.w %d3,%d0
	pea -5(%a3,%d0.l)
	move.l 2496(%a0),%a0
	jbsr (%a0)
	lea (14,%sp),%sp
	tst.w %d0
	jbne .L6
	clr.w -10(%a6)
	lea (2,%a3),%a2
	jbra .L10
	.even
.L19:
	move.l 200.w,%a0
	pea 8.w
	pea -8(%a6)
	move.l %a2,-(%sp)
	move.l 2496(%a0),%a0
	jbsr (%a0)
	lea (12,%sp),%sp
	tst.w %d0
	jbne .L12
	cmp.w 8(%a2),%d6
	jbne .L17
	cmp.w 10(%a2),%d5
	jbls .L16
.L17:
	moveq.l #1,%d4
	jbra .L12
	.even
.L16:
	move.w %a2,%d0
	sub.w %a3,%d0
	move.w %d0,-10(%a6)
	clr.w %d4
.L12:
	addq.l #2,%a2
.L10:
	clr.l %d0
	move.w %d3,%d0
	lea -1(%a3,%d0.l),%a0
	cmp.l %a2,%a0
	jbls .L11
	tst.w -10(%a6)
	jbeq .L19
.L11:
	tst.w -10(%a6)
	jbne .L5
.L6:
	move.l 200.w,%a0
	move.l 436(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	cmp.w #0,%a0
	jbne .L4
.L5:
	moveq.l #6,%d1
	tst.w %d4
	jbne .L1
	moveq.l #2,%d1
	cmp.w #0,%a4
	jbeq .L1
	move.l 200.w,%a0
	move.w 12(%a4),%a4
	move.w %a4,-(%sp)
	move.l 616(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	moveq.l #3,%d1
	tst.w %d0
	jbeq .L1
	move.l 200.w,%a2
	move.w %a4,-(%sp)
	move.l 600(%a2),%a0
	jbsr (%a0)
	addq.l #2,%sp
	lea (2,%a0),%a3
	move.w (%a3),%d3
	addq.w #2,%d3
	and.l #0xFFFF,%d3
	move.l %d3,-(%sp)
	move.l 648(%a2),%a0
	jbsr (%a0)
	move.l %a0,%d0
	move.l %a0,__DLL_body_ptr
	addq.l #4,%sp
	move.l 200.w,%a0
	tst.l %d0
	jbne .L25
	move.w %a4,-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	moveq.l #4,%d1
	addq.l #2,%sp
	jbra .L1
	.even
.L25:
	move.l %d3,-(%sp)
	move.l %a3,-(%sp)
	move.l %d0,-(%sp)
	move.l 2472(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a2
	jbsr (%a5)
	move.l __DLL_body_ptr,%a0
	lea -1(%a0,%d3.l),%a1
	cmp.w #2,%d0
	jbne .L27
	add.l %d3,%a0
	move.l #262143,%a1
	add.l %a0,%a1
.L27:
	move.l %a1,-(%sp)
	jbsr (%a5)
	move.l __DLL_body_ptr,%a0
	move.l 1384(%a2),%a2
	move.l #262146,%a1
	add.l %a0,%a1
	cmp.w #2,%d0
	jbeq .L29
	lea (2,%a0),%a1
.L29:
	move.l %a1,-(%sp)
	jbsr (%a2)
	clr.l %d0
	move.w -10(%a6),%d0
	add.l __DLL_body_ptr,%d0
	subq.l #2,%d0
	move.l %d0,__DLL_interface_ptr
	move.l 200.w,%a0
	move.w %a4,-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	clr.w %d1
	lea (22,%sp),%sp
.L1:
	move.w %d1,%d0
	movm.l -48(%a6),#0x3cf8
	unlk %a6
	rts
	.even
	.globl	UnloadDLL
UnloadDLL:
	move.l __DLL_body_ptr,%d0
	jbeq .L30
	move.l 200.w,%a0
	move.l %d0,-(%sp)
	move.l 652(%a0),%a0
	jbsr (%a0)
	clr.l __DLL_body_ptr
	clr.l __DLL_interface_ptr
	addq.l #4,%sp
.L30:
	rts
	nop
	.globl	__DLL_interface_ptr
	.text
	.even
__DLL_interface_ptr:
	.long	0
	.globl	__DLL_body_ptr
	.even
__DLL_body_ptr:
	.long	0
