| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_rom_calls_AND___compressed_format_rom_calls_AND___nostub,__ld_insert_compressed_rom_calls

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file requires the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st90
__compressed_format_rom_calls:
| Relocate the program.
	lea.l __ld_insert_compressed_rom_calls(%pc),%a0
	lea.l __ld_compressed_rom_calls_ref-4(%pc),%a1
	clr.w %d2
	move.w #-2,-(%sp)
__compressed_format_rom_calls_loop_1:
	move.w (%sp)+,%d1 | get the previous [ROM_CALL number]*2
	subq.w #2,%d1     | next ROM_CALL, the decoding function adds 2 too much
	bsr __decode_compressed_offset
	bcs.s __compressed_format_rom_calls_done_1
	move.w %d1,-(%sp) | we need to reuse d1 for the offsets below
	add.w %d1,%d1 | multiply the offset by 2, because we want 4 * num,
	              | whereas decoding yields 2 * num
	move.l (%a5,%d1.w),%d0
	moveq.l #0,%d1
__compressed_format_rom_calls_loop_2:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_rom_calls_loop_1 | done with the inner loop
	add.l %d0,(%a1,%d1.l)
	bra.s __compressed_format_rom_calls_loop_2
__compressed_format_rom_calls_done_1:

.section _st1070
__compressed_format_rom_calls_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_compressed_rom_calls(%pc),%a0
	lea.l __ld_compressed_rom_calls_ref-4(%pc),%a1
	clr.w %d2
	move.w #-2,-(%sp)
__compressed_format_rom_calls_loop_3:
	move.w (%sp)+,%d1 | get the previous [ROM_CALL number]*2
	subq.w #2,%d1     | next ROM_CALL, the decoding function adds 2 too much
	bsr __decode_compressed_offset
	bcs.s __compressed_format_rom_calls_done_3
	move.w %d1,-(%sp) | we need to reuse d1 for the offsets below
	add.w %d1,%d1 | multiply the offset by 2, because we want 4 * num,
	              | whereas decoding yields 2 * num
	move.l (%a5,%d1.w),%d0
	moveq.l #0,%d1
__compressed_format_rom_calls_loop_4:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_rom_calls_loop_3 | done with the inner loop
	sub.l %d0,(%a1,%d1.l)
	bra.s __compressed_format_rom_calls_loop_4
__compressed_format_rom_calls_done_3:

.section _st10000, "d"

__ld_insert_compressed_rom_calls:
| The format for the ROM call table is as follows:
| For each ROM call used...
|  A compressed reloc table (see _compressed_format_relocs.s) encoding:
|    the offset of the ROM call number from [the last one + 1], or from 0 (base)
|    the actual reloc table
| 1 byte: 0 (empty compressed reloc table)
