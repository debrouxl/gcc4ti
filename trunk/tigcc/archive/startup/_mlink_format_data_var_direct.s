	.xdef __handle_data_var_AND___mlink_format_data_var_AND_NOT___data_var_create_copy,__handle_data_var_AND___mlink_format_data_var_AND___data_var_copy_if_archived,__mlink_format_data_var_use_directly

.section _st98
__mlink_format_data_var_use_directly:
| Call HLock with the file handle.
	move.l (%a5,0x99*4),%a0 /* HLock */
	jsr (%a0)
| Add 2 to skip the size bytes.
	addq.l #2,%a0
| Put the result into %d3.
	move.l %a0,%d3
	bra.s __mlink_format_data_var_relocate

| This strange order of sections is because of a bug
| in GNU as.
.section _st1064
__mlink_format_data_var_unlock_handle:
| Unlock the handle.
	move.l (%a5,0x9F*4),%a0 /* HeapUnlock */
	jsr (%a0)
	bra.s __mlink_format_data_var_cleanup_almost_end

.section _st1062
| Move the handle on the stack, check if it is 0.
	move.w %d4,(%sp)
	bne.s __mlink_format_data_var_unlock_handle
