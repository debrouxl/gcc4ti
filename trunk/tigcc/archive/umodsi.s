	.xdef __umodsi3

.text
__umodsi3:
	move.w #0x2AB*4,%d2 /* _mu32u32 */
	jra __div_entry
