	.xdef calloc_throw

.text
calloc_throw:
	jbsr calloc
	move.l %a0,%d0
	jbne .L__finished
	.word 0xA000+670
.L__finished:
	rts
