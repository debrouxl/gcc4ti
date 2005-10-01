	.xdef __set_file_in_use_bit_AND_NOT___kernel_library_header,__set_file_in_use_bit__,__clear_file_in_use_bit__,__set_clear_in_use_bit__

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This object file needs the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls
| This object file needs %d3-%d5 to be saved.
	.xdef __ref_all___save_d3d5

.section _st87
__set_file_in_use_bit__:
| First, look for the current address.
	lea.l (%pc),%a1
	move.l %a1,%d4
| This is to prevent problems with Kevin's HW2 TSR support.
	and.l #0x3FFFF,%d4
| Set the bit instead of clearing it.
	moveq.l #1,%d3
| Call function.
	bsr __set_clear_in_use_bit__
| On failure, try using the return address of the program, to do the same for
| a possible launcher.
	tst.w %d0
	jbne __in_use_done__
| Get the return address.
	move.l 16(%sp),%d4
| Check if it is in RAM range.
	cmp.l #0x1FFFFF,%d4
| If not, it's not the launcher. Now we don't know who started us.
	jbhi __in_use_done__
| This is to prevent problems with Kevin's HW2 TSR support.
	and.l #0x3FFFF,%d4
| Call function.
	bsr __set_clear_in_use_bit__
__in_use_done__:

.section _st1075
__clear_file_in_use_bit__:
| Call function.
	bsr __set_clear_in_use_bit__

.section _st10000

| This function relies on the address of the jump table being in %a5.
| %d4 is an arbitrary address somewhere in our program (or the launcher,
| respectively).
| If %d3.w is 0, clear the bit, otherwise set it. If action is to set the bit,
| the previous value is stored in %d3.
| Register %d5 is destroyed.
__set_clear_in_use_bit__:
| Call SymFindFirst with appropriate parameters to look for the handle.
	move.w #0x06,-(%sp) /* FO_RECURSE | FO_SKIP_TEMPS */
	clr.l -(%sp)
	move.l (%a5,0x6C*4),%a0 /* SymFindFirst */
	jbsr (%a0)
	addq.l #6,%sp
__symbol_search_loop__:
| Store failure value in %d0
	clr.w %d0
| If the SYM_ENTRY pointer is 0, quit.
	move.l %a0,%d5
	jbeq __symbol_search_done__
| Dereference the handle from the SYM_ENTRY structure.
	move.w (%a0,12),-(%sp)
	move.l (%a5,0x96*4),%a0 /* HeapDeref */
	jbsr (%a0)
	addq.l #2,%sp
| If the address returned is higher than the address we are looking for, the
| symbol can't be the one.
| Actually, if it is equal, then something is seriously broken, but we don't
| check for that here.
	cmp.l %d4,%a0
	jbhi __skip_symbol__
| Add the size of the variable to %a0.
| Note that we would actually need to add 2 more bytes to skip the size
| field, but we make up for that by using a <= comparison.
	moveq #0,%d0
	move.w (%a0),%d0
	add.l %d0,%a0
| If the end address is lower than the address we are looking for, the
| symbol can't be the one.
	cmp.l %d4,%a0
	jbls __skip_symbol__
| Store success result in %d0.
	moveq #1,%d0
| If %d3.w is 0, clear the in-use bit.
	move.l %d5,%a0
	lea 11(%a0),%a0
	tst.w %d3
	jbeq __clear_bit__
| Otherwise, store the previous value in %d3, set the bit, and quit.
	moveq #0x10,%d1
	move.b (%a0),%d3
	and.w %d1,%d3
	or.b %d1,(%a0)
	rts
__skip_symbol__:
| Call SymFindNext.
	move.l (%a5,0x6D*4),%a0 /* SymFindNext */
	jbsr (%a0)
| Go to beginning of loop.
	jra __symbol_search_loop__
__clear_bit__:
	and.b #0xEF,(%a0)
__symbol_search_done__:
	rts
