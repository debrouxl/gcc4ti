| Most of this code was contributed by ExtendeD.

	.xdef __fline_handle_rom_call

.section _st10038
__fline_handle_rom_call:
.if 0
| Put the value of %d2 into %a0 and restore it later,
| since no known ROM call takes an address register parameter,
| but some (including _bcd_math) take arbitrary data register parameters.
	move.l %d2,%a0
.endif
| Ignore the saved value of %a1, for the same reason.
	addq.l #4,%sp
	move.w (%sp)+,%d2
	addq.l #4,%sp
| %sp (the supervisor stack pointer) is the same as it was before the
| interrupt was called.
| Now we restore the status register, which usually gets back to user mode
| (so %sp points to the the user stack from now on). Anyway, even if we were
| in supervisor mode before (i.e. in an interrupt handler), the situation
| will now be the same as before the call.
	move.w %d2,%sr
| Get the word at the address in %a1 (the return address). It contains
| _F_LINE+idx.
| Add 2 to %a1, so we will return to the next instruction instead of running
| into an endless loop.
	move.w (%a1)+,%d2
| Push the return address on the user stack (it will be used by the 'rts' at
| the end of the function).
	pea.l (%a1)
| Get the address of the jump table.
	move.l 0xC8,%a1
| Get the ROM call index. We could subtract _F_LINE from %d2, but this is
| faster.
	and.w #0x7FF,%d2
| Multiply by 4 (size of a pointer).
	lsl.w #2,%d2
| Calculate the address of the ROM Call in %a0.
	move.l (%a1,%d2.w),%a1
.if 0
| Restore the value of %d2.
	move.l %a0,%d2
.endif
| Jump to the ROM function. We can do it this way since we are in the mode in
| which the function has to be executed.
	jmp (%a1)
