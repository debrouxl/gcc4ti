	.xdef __cmpbf2,__nebf2,__eqbf2,__gebf2,__ltbf2,__gtbf2,__lebf2

.text
__cmpbf2:
__nebf2:
__eqbf2:
__gebf2:
__ltbf2:
__gtbf2:
__lebf2:
	moveq.l #20,%d0
	jra __fp_entry_1
