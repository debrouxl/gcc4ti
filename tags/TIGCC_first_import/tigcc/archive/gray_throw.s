	.xdef GrayOnThrow

.text
GrayOnThrow:
	jbsr GrayOn
	tst.w %d0
	jbne .L__finished
	.word 0xA000+850
.L__finished:
	rts
