| Copyright (C) 2003 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __decode_compressed_offset

.section _st10000
__decode_compressed_offset:
| Decode a compressed offset.
| IN: a0: pointer to the compressed reloc table
|     d1: base offset
|     d2: nibble count
| OUT: a0: auto-advanced pointer
|      d1: computed offset
|      d2: nibble count
|      C flag: set = end of table, clear = not end of table

	addq.w #4,%d1 | overlapping relocs are not allowed, so increase the offset by 4

 	movem.l %d3/%d4,-(%sp) | save d3 and d4
	moveq.l #15,%d4 | mask to get a nibble
| Check if we have nibbles left.
	tst.w %d2
	bne.s L.nibbles

L.try_again:
| No nibbles left, so decode next byte.
	move.b (%a0)+,%d3

	subq.b #1,%d3
| Carry means the byte was 0. This means end of table.
	bcs.s L.return

	addq.b #1,%d3
| 01-7F means we have a byte.
	bmi.s L.notbyte
	subq.b #1,%d3 | offset is byte value - 1
	ext.w %d3 | make full word
	bra.s L.add_twice_d3
L.notbyte:

	cmp.b #0xC0,%d3
| 80-BF means we have nibbles.
	bcc.s L.notnibbles
| Get the byte count.
	move.w %d3,%d2
	lsr.b #4,%d2 | first nibble only
	ext.w %d2 | make full word
	subq.w #7,%d2 | convert to remaining byte count
| We want a nibble count, so multiply the byte count by 2.
	add.w %d2,%d2
	and.w %d4,%d3 | make the nibble into a full word
	bra.s L.add_twice_d3
L.notnibbles:

| We have a word. It is encoded as C000-FFFF.
	lsl.w #8,%d3
| Complete the word.
	move.b (%a0)+,%d3
| FFFF is special.
	addq.w #1,%d3
	bne.s L.notFFFF
| For FFFF, we have to add 16510*2=33020 to d1 and to decode another offset.
	add.w #33020,%d1
	bra.s L.try_again
L.notFFFF:
| The word is not FFFF. We have to subtract 49025+1=49026 (+1 because we added 1
| above).
	sub.w #49026,%d3
	| fall through

L.add_twice_d3:
| Add twice d3. This is because we represent only even offsets.
	add.w %d3,%d1
	add.w %d3,%d1
| Assuming the table is valid, this should never carry, so the C flag is 0 here.

L.return:
	movem.l (%sp)+,%d3/%d4 | restore d3 and d4, not affecting flags
	rts | return

L.nibbles:
| Decode nibbles.
| Check whether we want the first or the second nibble of the byte.
	btst.l #0,%d2
	bne.s L.second_nibble

| The remaining nibble count is even, so we want the first nibble.
	move.b (%a0),%d3 | get the byte without advancing to the next one
	lsr.b #4,%d3 | first nibble only
	ext.w %d3 | make it into a full word
	bra.s L.nibbles_done

L.second_nibble:
| The remaining nibble count is odd, so we want the second nibble.
	move.b (%a0)+,%d3 | get the byte and advance to the next one
	and.w %d4,%d3 | make the nibble into a full word

L.nibbles_done:
	subq.w #1,%d2 | decrease the remaining nibble count by 1
	bra.s L.add_twice_d3
