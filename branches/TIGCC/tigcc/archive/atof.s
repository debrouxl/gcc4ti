	.xdef atof

.text
atof:
	link.w %a6,#-80
	movem.l %d3-%d7/%a2-%a5,-(%sp)
	move.l 0xC8,%a4
	move.l 1060(%a4),%a3 /* top_estack */
	move.l (%a3),%d4
	pea -80(%a6)
	move.l 1360(%a4),%a0 /* ER_catch */
	jsr (%a0)
	tst.w %d0
	jbeq .L__atof_1
	move.l #0x7FFFAA00,%d0
	clr.l %d1
	clr.w %d2
	jbra .L__atof_3
.L__atof_1:
	move.l 8(%a6),(%sp)
	move.l 3880(%a4),%a0 /* push_parse_text */
	jsr (%a0)
	move.l (%a3),%a0
	move.b (%a0),%d5
	cmpi.b #0x7A,%d5
	bne.s .L__atof_2
	subq #1,%a0
.L__atof_2:
	move.l %a0,(%sp)
	move.l 3044(%a4),%a0 /* estack_number_to_Float */
	jsr (%a0)
	move.l %d4,(%a3)
	move.l 1364(%a4),%a0 /* ER_success */
	jsr (%a0)
	move.l (%a6,-10),%d0
	move.l (%a6,-6),%d1
	move.w (%a6,-2),%d2
	cmpi.b #0x7A,%d5
	bne.s .L__atof_3
	bset #31,%d0
.L__atof_3:
	movem.l -116(%a6),%d3-%d7/%a2-%a5
	unlk %a6
	rts
