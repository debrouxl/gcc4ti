	.xdef __enter_ghost_space_AND___complex_main

.section _st10501
| In case of a complex _main (i.e. called with jbsr
| instead of jra), modify the return address to stay
| in the ghost space.
	move.l __save__sp__,%a0
	or.l #0x40000,(%a0)
