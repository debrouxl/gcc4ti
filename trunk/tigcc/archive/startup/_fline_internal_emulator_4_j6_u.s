	.xdef __fline_handle_jump_6byte_body_AND_NOT___custom_int_handlers

.section _st10035
| Save the value of %a0, since we may not destroy any registers.
	pea.l (%a0)
| Check if it is a bsr.
	cmp.w #0xFFF0,(%a1)
	addq.l #6,%a1
| If not, do not push a return address.
	bne.s __fline_6byte_not_bsr__
| Push the return address on the user stack.
	move.l %usp,%a0
	move.l %a1,-(%a0)
	move.l %a0,%usp
__fline_6byte_not_bsr__:
| Load the target address into %a1.
	add.l -(%a1),%a1
| Replace the return address on the supervisor stack
| with the target address.
	move.l %a1,10(%sp)
| Restore the values of %a0 and %a1.
	movem.l (%sp)+,%a0-%a1
| Return to the calculated address.
	rte
