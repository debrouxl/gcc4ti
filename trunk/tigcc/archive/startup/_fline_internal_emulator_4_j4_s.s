	.xdef __fline_handle_jump_4byte_body_AND___custom_int_handlers

	.xdef __ref_all___constructed_jsr

.section _st10037
| Save the value of %a0, since we may not destroy any registers.
	pea.l (%a0)
	lea.l __constructed_jmp(%pc),%a0
| Check if it is a bsr.
	cmp.w #0xFFEE,(%a1)
	addq.l #4,%a1
| If not, do not push a return address.
	bne.s __fline_4byte_not_bsr__
| Put the return address into the constructed jsr.
	move.l %a1,-(%a0)
	subq.l #2,%a0
__fline_4byte_not_bsr__:
| Replace the return address on the supervisor stack
| with the address of the constructed jmp/jsr.
	move.l %a0,10(%sp)
| Get the location of the program entry point.
	lea.l __ld_entry_point_plus_0x8000(%pc),%a0
| Add the program-relative target address.
	adda.w -(%a1),%a0
| Put it into the constructed jmp/jsr.
	lea.l __constructed_jmp_jsr_target(%pc),%a1
	move.l %a0,(%a1)
| Restore the values of %a0 and %a1.
	movem.l (%sp)+,%a0-%a1
| Return to the calculated address.
	rte
