	.xdef __save_d3d5_AND_NOT___save_a5_AND_NOT___save_all_regs

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st81
__save_d3d5:
	movem.l %d3-%d5,-(%sp)

.section _st1081
__restore_d3d5:
	movem.l (%sp)+,%d3-%d5
