	.xdef HeapReallocThrow

.text
HeapReallocThrow:
	pea.l (%a0)
	move.w %d0,-(%sp)
	move.l 0xC8,%a0
	move.l (%a0,0x9D*4),%a0 /* HeapRealloc */
	jsr (%a0)
	addq.l #6,%sp
	tst.w %d0
	jbne .L__finished
	.word 0xA000+670
.L__finished:
	rts
