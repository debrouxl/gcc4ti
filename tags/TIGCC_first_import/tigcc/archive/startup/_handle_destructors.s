	.xdef __handle_destructors

	.xdef __ref_all___complex_main

.section _st1010
__handle_destructors:
	movem.l %a2/%d3,-(%sp)
	lea __ld_destructors_start,%a2
	move.w #__ld_destructor_count-1,%d3
	bsr __call_function_vector__
	movem.l (%sp)+,%a2/%d3
