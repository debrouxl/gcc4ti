| Copyright (C) 2002-2003 Sebastian Reichelt.
| Copyright (C) 2003 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __handle_relocs_AND___compressed_format_relocs_AND___nostub,__ld_insert_compressed_relocs

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st15
__compressed_format_relocs:
| Relocate the program.
	lea.l __ld_insert_compressed_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	move.l #__ld_compressed_relocs_ref-__ld_entry_point-4,%d1
	clr.w %d2
__compressed_format_relocs_loop_1:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_relocs_done_1
	add.l %d0,0(%a1,%d1.l)
	bra.s __compressed_format_relocs_loop_1
__compressed_format_relocs_done_1:

.section _st1090
__compressed_format_relocs_cleanup:
| Unrelocate the program.
	lea.l __ld_insert_compressed_relocs(%pc),%a0
	lea.l __ld_entry_point(%pc),%a1
	move.l %a1,%d0
	move.l #__ld_compressed_relocs_ref-__ld_entry_point-4,%d1
	clr.w %d2
__compressed_format_relocs_loop_2:
	bsr __decode_compressed_offset
	bcs.s __compressed_format_relocs_done_2
	sub.l %d0,0(%a1,%d1.l)
	bra.s __compressed_format_relocs_loop_2
__compressed_format_relocs_done_2:

.section _st10000, "d"

__ld_insert_compressed_relocs:
| The format for the reloc table is as follows:
/* The offset starts at __ld_entry_point - 4 bytes (the smallest encodable
   difference).
00: end of list
01-7F: add 004-100 (2(value+1)) to the previous offset to get the new one
80-BF: add 04-22 (2(nibble_value+2)). (Note that 01-7F can be used for
       that purpose as well if you are a lazy linker author. :-) But the
       nibble-encoding saves some space, so you'd better use it.)
       The bytes have the following values (the most significant nibble is
       the first one):
       Byte 1: n+6 (2+6=8 .. 5+6=B), first data nibble
       Byte 2: 2 data nibbles
       ...
       Byte n: 2 data nibbles
       Note that n = ([total number of data nibbles] + 1) / 2. It follows
       that the total number of data nibbles must be odd and >=3. Use
       byte-encoding (01-7F) otherwise (for all data if the number is <=2,
       for the last data nibble only if it is even and >=4). It also must
       not exceed 9 (5*2-1), in which case you need to split it up.
C0-FF: the byte starts a full word: 0xC000+difference
       add 2(difference+0x81) to the previous offset to get the new one
       FFFF is special: it adds 0x80FC to the offset, but the computation
                        does not stop here, instead another delta is added

(NOTE: numbers below are decimal unless prefixed by "0x"!)
In pseudo-code, this means:
offset=__ld_entry_point-4;
for each (byte) {
  switch (byte) {
    case 0: stop;
    case 0x01..0x7f:
      offset+=2*(byte+1);
      dump(offset);
      break;
    case 0x80..0xbf:
      n=(byte>>4)-6;
      offset+=2*((byte&7)+2);
      dump(offset);
      for(i=1;i<n;i++) {
        offset+=2*((byte>>4)+2);
        dump(offset);
        offset+=2*((byte&7)+2);
        dump(offset);
      }
    case 0xc0..0xff:
      complete the word;
      if (word==0xFFFF) {
        offset+=0x80FC;
      } else {
        offset+=2*(word-(0xC000-0x81));
        dump(offset);
      }
   }
} */
