	.xdef __fline_internal_emulator_AND___fline_jumps_AND_NOT___fline_jumps_4byte

.section _st10032
	cmp.w #0xFFF2,(%a1)
	bcc.s __call_default_fline_handler
