	.xdef __handle_bss_AND___kernel_format_bss_AND___nostub,__ld_insert_kernel_bss_refs

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file is optimized using the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st100
__kernel_format_bss:
| Allocate space for the BSS section.
	move.l %d3,-(%sp)
	pea.l __ld_bss_size
	move.l (%a5,0xA2*4),%a0 /* HeapAllocPtr */
	jsr (%a0)
	pea.l __kernel_format_bss_cleanup_almost_end(%pc)
	move.l %a0,%d3
	beq __out_of_memory
	addq.l #4,%sp
| Relocate the program.
	lea.l __ld_insert_kernel_bss_refs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d1
__kernel_format_bss_refs_loop_1:
	move.w (%a0)+,%d1
	beq.s __kernel_format_bss_refs_done_1
	add.l %d3,(%a1,%d1.l)
	bra.s __kernel_format_bss_refs_loop_1
__kernel_format_bss_refs_done_1:

.section _st1060
__kernel_format_bss_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_kernel_bss_refs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d1
__kernel_format_bss_refs_loop_2:
	move.w (%a0)+,%d1
	beq.s __kernel_format_bss_refs_done_2
	sub.l %d3,(%a1,%d1.l)
	bra.s __kernel_format_bss_refs_loop_2
__kernel_format_bss_refs_done_2:
| Free the memory block.
	move.l %d3,(%sp)
	move.l (%a5,0xA3*4),%a0 /* HeapFreePtr */
	jsr (%a0)
__kernel_format_bss_cleanup_almost_end:
| Clean up the stack and registers.
	movem.l (%sp)+,%d2-%d3
__kernel_format_bss_cleanup_end:

.section _st10000, "d"

__ld_insert_kernel_bss_refs:
| The format for the BSS table is as follows:
| For each reloc...
|   2 bytes: offset into the program
| 2 bytes: 0
