	.xdef malloc_throw,HeapAllocPtrThrow

.text
malloc_throw:
HeapAllocPtrThrow:
	pea.l (%a0)
	move.l 0xC8,%a0
	move.l (%a0,0xA2*4),%a0 /* HeapAllocPtr */
	jsr (%a0)
	addq.l #4,%sp
	move.l %a0,%d0
	jbne .L__finished
	.word 0xA000+670
.L__finished:
	rts
