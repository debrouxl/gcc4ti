	.xdef clrscr

.text
clrscr:
	move.l 0xC8,%a0
	move.l (%a0,0x674),%a0 /* MoveTo */
	clr.l -(%sp)
	jsr (%a0)
	addq.l #4,%sp
	move.l 0xC8,%a0
	move.l (%a0,0x678),%a0 /* ClrScr */
	jmp (%a0)
