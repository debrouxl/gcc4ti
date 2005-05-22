| Nostub comment header
| Copyright (C) 2003-2005 Kevin Kofler

	.xdef __nostub_comment_header

.section _stl30, "d"
	move.l (%a7),(%a7)
	bra.w end_extension_header | This MUST be a bra.w, not bra.s!
	.word 0x2e76, 0x5c7b, 0x4e74, 0x4e72, 0x4afc, 0
	.long 0x01010000 | Version number
	.word __ld_nostub_comment_count
__ld_insert_nostub_comments:

.section _stl31, "d"
end_extension_header:
