	.xdef __handle_rom_calls_AND___kernel_format_rom_calls_AND___nostub,__ld_insert_kernel_rom_calls

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file requires the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st90
__kernel_format_rom_calls:
| Relocate the program.
	lea.l __ld_insert_kernel_rom_calls(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d2
	move.w (%a0)+,%d0
__kernel_format_rom_calls_loop_1:
	move.w (%a0)+,%d1
	lsl.w #2,%d1
	move.l (%a5,%d1.w),%d1
__kernel_format_rom_calls_loop_2:
	move.w (%a0)+,%d2
	beq.s __kernel_format_rom_calls_done_2
	add.l %d1,(%a1,%d2.l)
	bra.s __kernel_format_rom_calls_loop_2
__kernel_format_rom_calls_done_2:
	dbra %d0,__kernel_format_rom_calls_loop_1

.section _st1070
__kernel_format_rom_calls_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_kernel_rom_calls(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	moveq.l #0,%d2
	move.w (%a0)+,%d0
__kernel_format_rom_calls_loop_3:
	move.w (%a0)+,%d1
	lsl.w #2,%d1
	move.l (%a5,%d1.w),%d1
__kernel_format_rom_calls_loop_4:
	move.w (%a0)+,%d2
	beq.s __kernel_format_rom_calls_done_4
	sub.l %d1,(%a1,%d2.l)
	bra.s __kernel_format_rom_calls_loop_4
__kernel_format_rom_calls_done_4:
	dbra %d0,__kernel_format_rom_calls_loop_3

.section _st10000, "d"

__ld_insert_kernel_rom_calls:
| The format for the ROM call table is as follows:
| 2 bytes: ROM call count - 1
| For each ROM call used...
|   2 bytes: ROM call number
|   For each reloc...
|     2 bytes: offset into the program
|   2 bytes: 0
