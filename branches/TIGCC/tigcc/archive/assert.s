	.xdef __assertion_failed

| This file requires the startup exit support.
	.xdef __ref_all___save_all_registers

.text
__assertion_failed:
	link.w %a6,#-1044
	move.l 0xC8,%a2
	move.w 16(%a6),(%sp)
	move.l 12(%a6),-(%sp)
	move.l 8(%a6),-(%sp)
	pea .L__assert_fmt
	lea -1000(%a6),%a4
	move.l %a4,-(%sp)
	move.l 332(%a2),%a0 /* sprintf */
	jsr (%a0)
	move.l 188(%a2),%a0 /* ScrRect */
	clr.l %d0
	move.b 2(%a0),%d0
	swap %d0
	move.b 3(%a0),%d0
	add.l #0xFFF5FFF1,%d0
	move.l %d0,(%sp)
	move.l #0xA000A,-(%sp)
	move.l 176(%a2),%a0 /* MakeWinRect */
	jsr (%a0)
	pea .L__assert_ttl
	move.w #0x1058,-(%sp)
	move.l %a0,-(%sp)
	pea -1042(%a6)
	move.l 120(%a2),%a0 /* WinOpen */
	jsr (%a0)
	move.l 4(%a2),%a0
	jsr (%a0)
	clr.w 4(%sp)
	move.l 76(%a2),%a0 /* WinFont */
	jsr (%a0)
	move.l %a4,4(%sp)
	move.l 148(%a2),%a0 /* WinStr */
	jsr (%a0)
	move.l #0x10008,4(%sp)
	move.l 212(%a2),%a0 /* DrawStaticButton */
	jsr (%a0)
.L__assert_wkey:
	move.l 324(%a2),%a0 /* ngetchx */
	jsr (%a0)
	cmp.w  #13,%d0
	jbne .L__assert_wkey
	move.l 44(%a2),%a0 /* WinClose */
	jsr (%a0)
	movea.l __save__sp__:l,%a7
	rts
.L__assert_ttl:
	.ascii "ASSERTION FAILED"
	.word 0
.L__assert_fmt:
	.byte 10
	.ascii " Condition: %s"
	.byte 10,10
	.ascii " File: %s  Line: %d"
	.word 0
