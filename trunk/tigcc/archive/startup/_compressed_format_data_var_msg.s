	.xdef __compressed_format_data_var_not_found,__compressed_format_data_var_corrupt

.section _st10020
__compressed_format_data_var_not_found:
	pea.l __compressed_format_data_var_cleanup_almost_end(%pc)
	pea.l __data_var_not_found_string(%pc)
	bra.s __error_msg

__compressed_format_data_var_corrupt:
	pea.l __compressed_format_data_var_cleanup_almost_end(%pc)
	pea.l __corrupt_data_var_string(%pc)
	bra.s __error_msg
