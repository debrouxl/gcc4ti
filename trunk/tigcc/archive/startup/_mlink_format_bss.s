| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003-2005 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_bss_AND___mlink_format_bss_AND___nostub

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file is optimized using the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st100
__mlink_format_bss:
| Allocate space for the BSS section.
	move.l %d3,-(%sp)
	pea.l __ld_bss_size
	move.l (%a5,0xA2*4),%a0 /* HeapAllocPtr */
	jsr (%a0)
	pea.l __mlink_format_bss_cleanup_almost_end(%pc)
	move.l %a0,%d3
	beq __out_of_memory
	addq.l #4,%sp
| Relocate the program.
	lea.l __ld_insert_mlink_bss_refs,%a0
	lea.l __ld_mlink_bss_refs_ref(%pc),%a1
__mlink_format_bss_refs_loop_1:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_bss_refs_done_1
	add.l %d3,(%a1)
	bra.s __mlink_format_bss_refs_loop_1
__mlink_format_bss_refs_done_1:

.section _st1060
__mlink_format_bss_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_mlink_bss_refs,%a0
	lea.l __ld_mlink_bss_refs_ref(%pc),%a1
__mlink_format_bss_refs_loop_2:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_bss_refs_done_2
	sub.l %d3,(%a1)
	bra.s __mlink_format_bss_refs_loop_2
__mlink_format_bss_refs_done_2:
| Free the memory block.
	move.l %d3,(%sp)
	move.l (%a5,0xA3*4),%a0 /* HeapFreePtr */
	jsr (%a0)
__mlink_format_bss_cleanup_almost_end:
| Clean up the stack and registers.
	movem.l (%sp)+,%d2-%d3
__mlink_format_bss_cleanup_end:

| See _mlink_format_relocs.s for the format of the BSS table.
