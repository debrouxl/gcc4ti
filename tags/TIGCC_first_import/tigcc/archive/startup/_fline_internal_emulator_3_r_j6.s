	.xdef __fline_internal_emulator_AND___fline_jumps_AND_NOT___fline_jumps_4byte_AND___fline_rom_calls

.section _st10033
| Check if it is an F-Line jump or ROM call.
	cmp.w #0xFFF0,(%a1)
	bcs.s __fline_handle_rom_call
	bra.s __fline_handle_jump_6byte
