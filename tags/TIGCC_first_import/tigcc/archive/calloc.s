	.xdef calloc

.text
calloc:
	mulu.w %d1,%d0
	move.l %d0,-(%sp)
	move.l 0xC8,%a0
	move.l 648(%a0),%a0 /* HeapAllocPtr */
	jsr (%a0)
	move.l %a0,%d0
	jbeq .L__calloc_1
	clr.w -(%sp)
	move.l %a0,-(%sp)
	move.l 0xC8,%a0
	move.l 2544(%a0),%a0 /* memset */
	jsr (%a0)
	addq.l #6,%sp
	jbra .L__calloc_2
.L__calloc_1:
	suba.l %a0,%a0
.L__calloc_2:
	addq.l #4,%sp
	rts
