	.xdef __mlink_format_data_var_not_found,__mlink_format_data_var_corrupt

.section _st10020
__mlink_format_data_var_not_found:
	pea.l __mlink_format_data_var_cleanup_almost_end(%pc)
	pea.l __data_var_not_found_string(%pc)
	bra.s __error_msg

__mlink_format_data_var_corrupt:
	pea.l __mlink_format_data_var_cleanup_almost_end(%pc)
	pea.l __corrupt_data_var_string(%pc)
	bra.s __error_msg
