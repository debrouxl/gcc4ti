	.xdef __get_jump_table

.section _st20
__get_jump_table:
| Get the address of the jump table (needed a few times).
| Note that calling a function will destroy %a0; be sure to check whether
| this is still valid for screen-saving support whenever you make a change in
| between.
	move.l 0xC8,%a0
