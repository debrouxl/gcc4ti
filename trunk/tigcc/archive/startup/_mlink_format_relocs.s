| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003-2005 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_relocs_AND___mlink_format_relocs_AND___nostub,__ld_insert_mlink_relocs

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st15
__mlink_format_relocs:
| Relocate the program.
	lea.l __ld_insert_mlink_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	lea.l __ld_mlink_relocs_ref(%pc),%a1
__mlink_format_relocs_loop_1:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_relocs_done_1
	add.l %d0,(%a1)
	bra.s __mlink_format_relocs_loop_1
__mlink_format_relocs_done_1:

.section _st1090
__mlink_format_relocs_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_mlink_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	lea.l __ld_mlink_relocs_ref(%pc),%a1
__mlink_format_relocs_loop_2:
	bsr __decode_mlink_offset
	bcc.s __mlink_format_relocs_done_2
	sub.l %d0,(%a1)
	bra.s __mlink_format_relocs_loop_2
__mlink_format_relocs_done_2:

.section _st10000, "d"

__ld_insert_mlink_relocs:
| The format for the reloc table is as follows:
/*
Reloc offsets are encoded by bytes using the following structure:
1 bit: stop bit
7 bits: data

The offset is split in 7-bit chunks. The last bit is thrown away, so an offset
<256 fits into one byte despite the stop bit. In other words, the encoding is
as follows:
offset := actual_offset/2
* 0 <= offset < 128: 1 byte: 0x80+offset
* 128 <= offset < 16384: 2 bytes: offset/128, 0x80+(offset%128)
* 16384 <= offset < 2097152: 3 bytes: offset/16384, (offset%16384)/128,
  0x80+(offset%128)
(Anything higher isn't interesting for us because of the 64 KB file size
limit.)

A reloc table is a list of such encoded offsets terminated by a 0x00 byte.
*/
