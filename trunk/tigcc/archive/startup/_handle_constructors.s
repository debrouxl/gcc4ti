	.xdef __handle_constructors

.section _st160
__handle_constructors:
	movem.l %a2/%d3,-(%sp)
	lea __ld_constructors_start,%a2
	move.w #__ld_constructor_count-1,%d3
	bsr __call_function_vector__
	movem.l (%sp)+,%a2/%d3
