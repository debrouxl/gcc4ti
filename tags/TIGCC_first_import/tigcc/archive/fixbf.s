	.xdef __fixbfsi,__fixunsbfsi

.text
__fixbfsi:
__fixunsbfsi:
	moveq.l #24,%d0
	jra __fp_entry_1
