| Main entry point, except in kernel mode with a "very simple main" (without extra stub code)

	.xdef __startup_code_AND_NOT___kernel_library_header,_main,__startup_entry_point

.section _st1
_main:
__startup_entry_point:
