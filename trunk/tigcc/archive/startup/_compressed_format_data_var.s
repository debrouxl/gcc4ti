| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_data_var_AND___compressed_format_data_var,__compressed_format_data_var_relocate,__compressed_format_data_var_cleanup_almost_end,__ld_insert_compressed_data_refs,__ld_insert_data_var_name

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file requires the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls
| SymFindPtr seems to require the in-use bit.
	.xdef __ref_all___set_file_in_use_bit

.section _st95
__compressed_format_data_var:
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
	beq __compressed_format_data_var_not_found
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
	bne __compressed_format_data_var_corrupt

| Code is inserted here to:
| * put the address to use into %d3
| * if the file is copied, clear %d4

.section _st99
| Relocate the program.
__compressed_format_data_var_relocate:
	lea.l __ld_insert_compressed_data_refs(%pc),%a0
	lea.l __ld_compressed_data_refs_ref-4(%pc),%a1
	moveq.l #0,%d1
	clr.w %d2
__compressed_format_data_refs_loop_1:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_data_refs_done_1
	add.l %d3,0(%a1,%d1.l)
	bra.s __compressed_format_data_refs_loop_1
__compressed_format_data_refs_done_1:

.section _st1061
__compressed_format_data_var_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_compressed_data_refs(%pc),%a0
	lea.l __ld_compressed_data_refs_ref-4(%pc),%a1
	moveq.l #0,%d1
	clr.w %d2
__compressed_format_data_refs_loop_2:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_data_refs_done_2
	sub.l %d3,0(%a1,%d1.l)
	bra.s __compressed_format_data_refs_loop_2
__compressed_format_data_refs_done_2:

.section _st1065
__compressed_format_data_var_cleanup_almost_end:
| Clean up the stack and registers.
	movem.l (%sp)+,%d1-%d5
__compressed_format_data_var_cleanup_end:

.section _st10000, "d"

__ld_insert_compressed_data_refs:
| See _compressed_format_relocs.s for the format of the data reloc table.
