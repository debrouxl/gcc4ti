| Copyright (C) 2005 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __decode_mlink_offset

.section _st10000
__decode_mlink_offset:
| Decode a mlink offset.
| IN: a0: pointer to the mlink reloc table
|     a1: base address
| OUT: a0: auto-advanced pointer
|      a1: computed address
|      C flag: clear = end of table, set = not end of table
| DESTROYS: d1

| Clear offset
	moveq.l #0,%d1
L.loop:
| Shift offset left by another 7 bits
	lsl.l #7,%d1
| Read byte from the relocation table
	move.b (%a0)+,%d1
| Test the ENTIRE offset for zero.  Mlink checks only the byte and it's
| wrong! Consider the offset 32768, encoded as 01 00 80.
	tst.l %d1
| If it is zero, the table stops here => return with C flag clear
|                                        (tst clears it)
	beq.s L.return
| Shift out stop bit and multiply by 2
	add.b %d1,%d1
| Stop bit clear => continue decoding
	bcc.s L.loop
| Stop bit set => we finished decoding our offset
| The C flag is set here
| Add our offset to our previous address (does not affect the flags)
	adda.l %d1,%a1
L.return:
	rts
