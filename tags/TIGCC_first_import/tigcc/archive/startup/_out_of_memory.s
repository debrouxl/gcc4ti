	.xdef __out_of_memory,__out_of_memory_string

.section _st10020
__out_of_memory:
	pea.l __out_of_memory_string(%pc)
	bra.s __error_msg

.section _st10000

__out_of_memory_string:
	.asciz "Out of memory"
