	.xdef __save_screen_AND___optimize_rom_calls

.section _st111
| Since __set_file_in_use_bit is imported,
| %a5 contains the address of the jump table.
	move.l (%a5,0x26A*4),%a2 /* memcpy */
