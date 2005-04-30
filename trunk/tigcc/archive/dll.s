	.file	"dll.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.globl	__DLL_body_ptr
	.text
	.even
__DLL_body_ptr:
	.skip 4
	.globl	__DLL_interface_ptr
	.even
__DLL_interface_ptr:
	.skip 4
	.section	.rodata.__unalignedstr,"dmu"
.LC0:
	.ascii "DLL\0\370\0"
	.text
	.even
	.globl	LoadDLL
LoadDLL:
	link.w %fp,#-16
	movm.l #0x1f3c,-(%sp)
	move.l %a0,-12(%fp)
	move.w %d1,-14(%fp)
	move.w %d2,%d7
	move.l #1145850912,-8(%fp)
	move.l %d0,-4(%fp)
#APP
	bsr 0f; 0:move.l (%sp)+,%a2
#NO_APP
	jbsr __get_hw_version
	move.w %d0,%d5
	cmp.w #2,%d0
	jbne .L2
	cmp.l #262143,%a2
	jbhi .L2
	moveq #1,%d0
	jbra .L5
.L2:
	tst.l __DLL_body_ptr
	jbeq .L6
	moveq #5,%d0
	jbra .L5
.L6:
	move.l 200.w,%a0
	move.w #2,-(%sp)
	clr.l -(%sp)
	move.l 432(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	clr.w %d6
	clr.w %d4
	addq.l #6,%sp
.L8:
	move.l 200.w,%a0
	move.l -12(%fp),-(%sp)
	move.l %a4,-(%sp)
	move.l 2500(%a0),%a0
	jbsr (%a0)
	addq.l #8,%sp
	tst.w %d0
	jbne .L9
	move.w 12(%a4),%d1
	jbeq .L9
	move.b 10(%a4),%d0
	btst #2,%d0
	jbne .L9
	btst #1,%d0
	jbne .L13
	move.l 200.w,%a0
	move.w %d1,-(%sp)
	move.l 620(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	tst.w %d0
	jbne .L9
.L13:
	move.l 200.w,%a0
	move.w 12(%a4),-(%sp)
	move.l 600(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a3
	move.l 200.w,%a0
	move.w (%a3),%d0
	addq.w #2,%d0
	and.l #65535,%d0
	lea (%a3,%d0.l),%a5
	pea 5.w
	pea .LC0
	pea -5(%a5)
	move.l 2496(%a0),%a0
	jbsr (%a0)
	lea (14,%sp),%sp
	tst.w %d0
	jbne .L9
	lea (2,%a3),%a2
	move.w %a2,%d3
	sub.w %a3,%d3
	clr.w %d6
	jbra .L16
.L17:
	move.l 200.w,%a0
	pea 8.w
	pea -8(%fp)
	move.l %a2,-(%sp)
	move.l 2496(%a0),%a0
	jbsr (%a0)
	lea (12,%sp),%sp
	tst.w %d0
	jbne .L20
	move.w -14(%fp),%d0
	cmp.w 8(%a2),%d0
	jbne .L21
	cmp.w 10(%a2),%d7
	jbhi .L21
	move.w %d3,%d6
	clr.w %d4
	jbra .L20
.L21:
	clr.w %d6
	moveq #1,%d4
.L20:
	addq.l #2,%a2
	addq.w #2,%d3
.L16:
	lea (-1,%a5),%a0
	cmp.l %a2,%a0
	jbls .L24
	tst.w %d6
	jbeq .L17
	jbra .L26
.L24:
	tst.w %d6
	jbne .L26
.L9:
	move.l 200.w,%a0
	move.l 436(%a0),%a0
	jbsr (%a0)
	move.l %a0,%a4
	cmp.w #0,%a0
	jbne .L8
.L26:
	tst.w %d4
	jbeq .L27
	moveq #6,%d0
	jbra .L5
.L27:
	cmp.w #0,%a4
	jbne .L29
	moveq #2,%d0
	jbra .L5
.L29:
	move.l 200.w,%a0
	move.w 12(%a4),%a4
	move.w %a4,-(%sp)
	move.l 616(%a0),%a0
	jbsr (%a0)
	addq.l #2,%sp
	tst.w %d0
	jbne .L31
	moveq #3,%d0
	jbra .L5
.L31:
	move.l 200.w,%a0
	move.l 648(%a0),%a2
	move.w %a4,-(%sp)
	move.l 600(%a0),%a0
	jbsr (%a0)
	lea (2,%a0),%a3
	move.w (%a3),%d4
	addq.w #2,%d4
	moveq #0,%d3
	move.w %d4,%d3
	move.l %d3,-(%sp)
	jbsr (%a2)
	move.l %a0,%d0
	move.l %a0,__DLL_body_ptr
	addq.l #6,%sp
	move.l 200.w,%a0
	tst.l %d0
	jbne .L33
	move.w %a4,-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	moveq #4,%d0
	addq.l #2,%sp
	jbra .L5
.L33:
	move.l %d3,-(%sp)
	move.l %a3,-(%sp)
	move.l %d0,-(%sp)
	move.l 2472(%a0),%a0
	jbsr (%a0)
	move.l 200.w,%a0
	move.l 1384(%a0),%a3
	move.l __DLL_body_ptr,%a2
	lea (12,%sp),%sp
	moveq #4,%d0
	swap %d0
	cmp.w #2,%d5
	jbeq .L37
	moveq #0,%d0
.L37:
	lea (%a2,%d0.l),%a0
	moveq #0,%d0
	move.w %d4,%d0
	lea -1(%a0,%d0.l),%a0
	move.l #262144,%a1
	cmp.w #2,%d5
	jbeq .L40
	sub.l %a1,%a1
.L40:
	move.l %a0,-(%sp)
	pea 2(%a1,%a2.l)
	jbsr (%a3)
	moveq #0,%d0
	move.w %d6,%d0
	add.l __DLL_body_ptr,%d0
	subq.l #2,%d0
	move.l %d0,__DLL_interface_ptr
	move.l 200.w,%a0
	move.w %a4,-(%sp)
	move.l 636(%a0),%a0
	jbsr (%a0)
	clr.w %d0
	lea (10,%sp),%sp
.L5:
	movm.l -52(%fp),#0x3cf8
	unlk %fp
	rts
	.even
	.globl	UnloadDLL
UnloadDLL:
	move.l __DLL_body_ptr,%d0
	jbeq .L46
	move.l 200.w,%a0
	move.l %d0,-(%sp)
	move.l 652(%a0),%a0
	jbsr (%a0)
	clr.l __DLL_body_ptr
	clr.l __DLL_interface_ptr
	addq.l #4,%sp
.L46:
	rts
