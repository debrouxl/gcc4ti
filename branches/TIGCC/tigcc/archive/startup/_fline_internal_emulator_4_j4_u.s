	.xdef __fline_handle_jump_4byte_body_AND_NOT___custom_int_handlers

.section _st10037
| Save the value of %a0, since we may not destroy any registers.
	pea.l (%a0)
| Check if it is a bsr.
	cmp.w #0xFFEE,(%a1)
	addq.l #4,%a1
| If not, do not push a return address.
	bne.s __fline_4byte_not_bsr__
| Push the return address on the user stack.
	move.l %usp,%a0
	move.l %a1,-(%a0)
	move.l %a0,%usp
__fline_4byte_not_bsr__:
| Get the location of the program entry point.
	lea.l __ld_entry_point_plus_0x8000(%pc),%a0
| Add the program-relative target address.
	adda.w -(%a1),%a0
| Replace the return address on the supervisor stack
| with the calculated target address.
	move.l %a0,10(%sp)
| Restore the values of %a0 and %a1.
	movem.l (%sp)+,%a0-%a1
| Return to the calculated address.
	rte
