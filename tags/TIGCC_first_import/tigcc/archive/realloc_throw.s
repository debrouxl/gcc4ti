	.xdef realloc_throw

.text
realloc_throw:
	jbsr realloc
	move.l %a0,%d0
	jbne .L__finished
	.word 0xA000+670
.L__finished:
	rts
