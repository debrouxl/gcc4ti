	.xdef __handle_relocs_AND___kernel_format_relocs_AND___nostub,__ld_insert_kernel_relocs

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st15
__kernel_format_relocs:
| Relocate the program.
	lea.l __ld_insert_kernel_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	moveq.l #0,%d1
__kernel_format_relocs_loop_1:
	move.w (%a0)+,%d1
	beq.s __kernel_format_relocs_done_1
	add.l %d0,(%a1,%d1.l)
	bra.s __kernel_format_relocs_loop_1
__kernel_format_relocs_done_1:

.section _st1090
__kernel_format_relocs_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_kernel_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	moveq.l #0,%d1
__kernel_format_relocs_loop_2:
	move.w (%a0)+,%d1
	beq.s __kernel_format_relocs_done_2
	sub.l %d0,(%a1,%d1.l)
	bra.s __kernel_format_relocs_loop_2
__kernel_format_relocs_done_2:

.section _st10000, "d"

__ld_insert_kernel_relocs:
| The format for the reloc table is as follows:
| For each reloc...
|   2 bytes: offset into the program
| 2 bytes: 0
