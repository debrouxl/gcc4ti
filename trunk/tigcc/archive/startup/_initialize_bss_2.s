	.xdef __initialize_bss_AND_NOT___optimize_rom_calls_AND___startup_code

.section _st152
| Load the address of the jump table into %a0.
	move.l 0xC8,%a0
| Load the address of memset into %a0.
	move.l (%a0,0x27C*4),%a0 /* memset */
