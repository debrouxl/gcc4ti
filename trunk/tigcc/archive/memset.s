| This just calls the actual ROM_CALL for GCC-internal use.
| Warning: This routine has the attribute __ATTR_TIOS__!

	.xdef memset

.text
memset:
	movea.l 0xC8,%a0
	movea.l (%a0,0x27C*4),%a0
	jmp (%a0) /* memset */
