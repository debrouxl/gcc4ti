	.xdef __handle_data_var_AND___kernel_format_data_var,__kernel_format_data_var_relocate,__kernel_format_data_var_cleanup_almost_end,__ld_insert_kernel_data_refs,__ld_insert_data_var_name

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file requires the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls
| SymFindPtr seems to require the in-use bit.
	.xdef __ref_all___set_file_in_use_bit

.section _st95
__kernel_format_data_var:
| Save registers that may be destroyed.
	movem.l %d3-%d5,-(%sp)
| Push flags for SymFindPtr.
	clr.l -(%sp)
| Push pointer to terminating 0 byte of variable name.
	pea.l __ld_data_var_name_end(%pc)
| Call SymFindPtr.
	move.l (%a5,0x283*4),%a0 /* SymFindPtr */
	jsr (%a0)
| Test the result and put it into %d5.
	move.l %a0,%d5
	beq __kernel_format_data_var_not_found
| Put the handle into %d4 and on the stack.
	move.w 12(%a0),%d4
	move.w %d4,(%sp)
| Call HeapDeref on the file handle.
	move.l (%a5,0x96*4),%a0 /* HeapDeref */
	jsr (%a0)
| Compare the size of the variable with the actual
| data size. Add 1 for the tag byte and 5 for the
| extension.
	cmp.w #__ld_data_size+5+1,(%a0)
	bne __kernel_format_data_var_corrupt

| Code is inserted here to:
| * put the address to use into %d3
| * if the file is copied, clear %d4

.section _st99
| Relocate the program.
__kernel_format_data_var_relocate:
	lea.l __ld_insert_kernel_data_refs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d1
__kernel_format_data_refs_loop_1:
	move.w (%a0)+,%d1
	beq.s __kernel_format_data_refs_done_1
	add.l %d3,(%a1,%d1.l)
	bra.s __kernel_format_data_refs_loop_1
__kernel_format_data_refs_done_1:

.section _st1061
__kernel_format_data_var_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_kernel_data_refs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d1
__kernel_format_data_refs_loop_2:
	move.w (%a0)+,%d1
	beq.s __kernel_format_data_refs_done_2
	sub.l %d3,(%a1,%d1.l)
	bra.s __kernel_format_data_refs_loop_2
__kernel_format_data_refs_done_2:

.section _st1065
__kernel_format_data_var_cleanup_almost_end:
| Clean up the stack and registers.
	movem.l (%sp)+,%d1-%d5
__kernel_format_data_var_cleanup_end:

.section _st10000, "d"

__ld_insert_kernel_data_refs:
| The format for the data reloc table is as follows:
| For each reloc...
|   2 bytes: offset into the program
| 2 bytes: 0
