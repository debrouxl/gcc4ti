| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003-2005 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_rom_calls_AND___mlink_format_rom_calls_AND___nostub,__ld_insert_mlink_rom_calls

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This file requires the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st90
__mlink_format_rom_calls:
| Relocate the program.
	lea.l __ld_insert_mlink_rom_calls(%pc),%a0
	clr.w -(%sp)
__mlink_format_rom_calls_loop_1:
	movea.w (%sp)+,%a1 | get the previous [ROM_CALL number]*2
	bsr __decode_mlink_offset
	bcc.s __mlink_format_rom_calls_done_1
	move.w %a1,-(%sp) | we need to reuse a1 for the offsets below
	adda.w %a1,%a1 | multiply the offset by 2, because we want 4 * num,
	               | whereas decoding yields 2 * num
	move.l (%a5,%a1.w),%d0
	lea.l __ld_mlink_rom_calls_ref(%pc),%a1
__mlink_format_rom_calls_loop_2:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_rom_calls_loop_1 | done with the inner loop
	add.l %d0,(%a1)
	bra.s __mlink_format_rom_calls_loop_2
__mlink_format_rom_calls_done_1:

.section _st1070
__mlink_format_rom_calls_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_mlink_rom_calls(%pc),%a0
	clr.w -(%sp)
__mlink_format_rom_calls_loop_3:
	movea.w (%sp)+,%a1 | get the previous [ROM_CALL number]*2
	bsr __decode_mlink_offset
	bcc.s __mlink_format_rom_calls_done_3
	move.w %a1,-(%sp) | we need to reuse a1 for the offsets below
	adda.w %a1,%a1 | multiply the offset by 2, because we want 4 * num,
	               | whereas decoding yields 2 * num
	move.l (%a5,%a1.w),%d0
	lea.l __ld_mlink_rom_calls_ref(%pc),%a1
__mlink_format_rom_calls_loop_4:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_rom_calls_loop_3 | done with the inner loop
	sub.l %d0,(%a1)
	bra.s __mlink_format_rom_calls_loop_4
__mlink_format_rom_calls_done_3:

.section _st10000, "d"

__ld_insert_mlink_rom_calls:
| The format for the ROM call table is as follows:
| For each ROM call used...
|  An mlink reloc table (see _mlink_format_relocs.s) encoding:
|    the offset of the ROM call number from the last one, or from 0 (base)
|    the actual reloc table
| 1 byte: 0 (empty mlink reloc table)
