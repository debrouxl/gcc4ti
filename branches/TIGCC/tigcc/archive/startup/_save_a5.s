	.xdef __save_a5_AND_NOT___save_d3d5_AND_NOT___set_file_in_use_bit_AND_NOT___save_all_regs
| We have to specify _AND_NOT___set_file_in_use_bit because:
| * __set_file_in_use_bit imports __save_d3d5
| * __set_file_in_use_bit is protected against __kernel_library_header through
|   _AND_NOT___kernel_library_header
| * We do not care about the __set_file_in_use_bit_AND___kernel_library_header
|   case. This program startup section has no business being in a kernel library
|   anyway. So we don't need to check for AND ((NOT __set_file_in_use_bit) OR
|   __kernel_library_header).

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st82
__save_a5:
	pea.l (%a5)

.section _st1080
__restore_a5:
	movea.l (%sp)+,%a5
