	.xdef __save_screen_AND_NOT___optimize_rom_calls

| This object file needs the jump table in %a0.
	.xdef __ref_all___get_jump_table

.section _st111
| %a0 still contains the address of the jump table.
| See _get_jump_table.s
	move.l (%a0,0x26A*4),%a2 /* memcpy */
