	.xdef __floatsibf

.text
__floatsibf:
	moveq.l #28,%d0
	jra __fp_entry
