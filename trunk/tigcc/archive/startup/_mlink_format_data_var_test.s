	.xdef __handle_data_var_AND___mlink_format_data_var_AND___data_var_copy_if_archived,__mlink_format_data_var_test

.section _st96
__mlink_format_data_var_test:
| Check if the variable is archived.
	move.l %d5,%a0
	btst #1,10(%a0)
	beq.s __mlink_format_data_var_use_directly
	bra.s __mlink_format_data_var_copy
