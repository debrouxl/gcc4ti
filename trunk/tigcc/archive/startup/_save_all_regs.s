	.xdef __save_all_regs

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st80
__save_all_regs:
	movem.l %d3-%d7/%a2-%a6,-(%sp)

.section _st1082
__restore_all_regs:
	movem.l (%sp)+,%d3-%d7/%a2-%a6
