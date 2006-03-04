| Function to compute the offset for nested function trampolines. Also handles
| the automatical global import of EXECUTE_IN_GHOST_SPACE (i.e. ghost space
| execution for HW2, HW3Patch detection for HW3).
| Copyright (C) 2003, 2006 Kevin Kofler.
| See License.txt for licensing conditions.

| This object file needs EXECUTE_IN_GHOST_SPACE.
	.xdef __ref_all___execute_in_ghost_space

	.xdef __trampoline_offset

__trampoline_offset:
	jbsr __get_hw_version
	moveq.l #0,%d1
	subq.w #2,%d0
	bne.s 0f
	bset.l #18,%d1
0:
	move.l %d1,%d0
	rts
