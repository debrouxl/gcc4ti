	.xdef __nostub_retval

	.xdef __ref_all___complex_main

.section _st9990
__nostub_retval:
	move.l (%sp)+,%a0
	cmpi.w #0x21EE,(%a0)
	jbne .L__no_add_2
	addq.l #2,%a0
.L__no_add_2:
	jmp (%a0,4)
