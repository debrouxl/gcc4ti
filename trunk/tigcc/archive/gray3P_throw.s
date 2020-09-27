	.xdef Gray3POnThrow

.text
Gray3POnThrow:
	jbsr Gray3POn
	tst.w %d0
	jbne .L__finished
	.word 0xA000+850
.L__finished:
	rts
