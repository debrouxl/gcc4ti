	.xdef __kernel_retval_var

	.xdef __ref_all___complex_main

.section _st9990
__kernel_retval_var:
	move.l _ROM_CALL_109,-(%sp) /* top_estack */
	move.l #0x40000000,-(%sp) /* STOF_ESI << 16 */
	pea __retval_var_name__(%pc)
	jsr _ROM_CALL_86 /* VarStore */
	lea (%sp,12),%sp
