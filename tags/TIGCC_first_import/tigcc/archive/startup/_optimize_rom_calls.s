	.xdef __optimize_rom_calls

| This file requires %a5 to be saved and restored.
	.xdef __ref_all___save_a5

.section _st85
__optimize_rom_calls:
	move.l 0xC8,%a5
