	.xdef __initialize_bss_AND___optimize_rom_calls_AND___startup_code

.section _st152
| Load the address of memset into %a0.
	move.l (%a5,0x27C*4),%a0 /* memset */
