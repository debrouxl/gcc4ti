	.xdef __negbf2

.text
__negbf2:
	moveq.l #16,%d0
	jra __fp_entry
