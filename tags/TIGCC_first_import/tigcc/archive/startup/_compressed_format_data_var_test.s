	.xdef __handle_data_var_AND___compressed_format_data_var_AND___data_var_copy_if_archived,__compressed_format_data_var_test

.section _st96
__compressed_format_data_var_test:
| Check if the variable is archived.
	move.l %d5,%a0
	btst #1,10(%a0)
	beq.s __compressed_format_data_var_use_directly
	bra.s __compressed_format_data_var_copy
