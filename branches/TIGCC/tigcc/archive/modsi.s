	.xdef __modsi3

.text
__modsi3:
	move.w #0x2A9*4,%d2 /* _ms32s32 */
	jra __div_entry
