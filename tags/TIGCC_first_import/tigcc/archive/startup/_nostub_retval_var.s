	.xdef __nostub_retval_var

	.xdef __ref_all___complex_main

.section _st9990
__nostub_retval_var:
	move.l 0xC8,%a0
	move.l (%a0,0x109*4),%a1 /* top_estack */
	move.l (%a1),-(%sp)
	move.l #0x40000000,-(%sp) /* STOF_ESI << 16 */
	pea __retval_var_name__(%pc)
	move.l (%a0,0x86*4),%a0 /* VarStore */
	jsr (%a0)
	lea (%sp,12),%sp
