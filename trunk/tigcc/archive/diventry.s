	.xdef __div_entry

.text
__div_entry:
	move.l (4,%sp),%d1
	move.l (8,%sp),%d0
	movea.l 0xC8,%a0
	movea.l (%a0,%d2.w),%a0
	jsr (%a0)
	move.l %d1,%d0
	rts
