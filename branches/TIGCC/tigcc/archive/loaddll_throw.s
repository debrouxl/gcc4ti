| Warning: This routine has the attribute __ATTR_LIB_C__!
| It is a simple wrapper for a C function.

	.xdef LoadDLLThrow

.text
LoadDLLThrow:
	lea.l .L__finished+2,%a0
	move.l (%sp)+,(%a0)
	jbsr LoadDLL
	tst.w %d0
	jbeq .L__finished
	.word 0xA000+850
.L__finished:
	jmp.l 0:l
