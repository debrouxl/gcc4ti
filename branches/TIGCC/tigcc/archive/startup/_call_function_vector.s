	.xdef __call_function_vector__
.section _st10000

| %a2: Address of the function vector.
| %d3.w: Number of functions to call.
__call_function_vector__:
	move.l (%a2)+,%a0
	jsr (%a0)
	dbra %d3,__call_function_vector__
	rts
