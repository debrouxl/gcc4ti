| TIGCC Program Starter - LZMA decompression support
| Copyright (C) 1999-2004 Igor Pavlov
| Copyright (C) 2004-2005 Kevin Kofler
|
| LICENSE
| -------
|
| LZMA SDK is licensed under two licenses:
|
| 1) GNU Lesser General Public License (GNU LGPL)
| 2) Common Public License (CPL)
|
| It means that you can select one of these two licenses and 
| follow rules of that license.
|
| SPECIAL EXCEPTION
| Igor Pavlov, as the author of this code, expressly permits you 
| to statically or dynamically link your code (or bind by name) 
| to the files from LZMA SDK without subjecting your linked 
| code to the terms of the CPL or GNU LGPL. 
| Any modifications or additions to files from LZMA SDK, however, 
| are subject to the GNU LGPL or CPL terms.

| Expected LZMA compression options: -lc0 -pb1 -mfbt2

| GET_UNCOMPRESSED_SIZE inline function
| INPUT: %a3.l: pointer to compressed data
| OUTPUT: %d6.l: uncompressed size
| SIDE EFFECTS: throws a Data Type error if not a valid compressed program
|               may throw a Memory error
|               may advance %a3 for later decompression
| DESTROYS: %d0-%d2/%a0-%a1
.macro GET_UNCOMPRESSED_SIZE
| Get the length of the variable.
moveq.l #0,%d6
move.w (%a3)+,%d6
| Get a pointer to the data type of the variable.
lea.l -1(%a3,%d6.l),%a0
| Check if it has type "LZMA".
cmp.b #0xf8,(%a0)
jbne invalid_archive
tst.b -(%a0)
jbne invalid_archive
cmp.b #'A',-(%a0)
jbne invalid_archive
cmp.b #'M',-(%a0)
jbne invalid_archive
cmp.b #'Z',-(%a0)
jbne invalid_archive
cmp.b #'L',-(%a0)
jbne invalid_archive
tst.b -(%a0)
jbne invalid_archive
cmp.l #0x2d000080,(%a3)+
jbne invalid_archive
tst.b (%a3)+
jbeq valid_archive
invalid_archive:
.word 0xA000+210 | ER_DATATYPE
valid_archive:
| Get the uncompressed size.
move.b (%a3)+,%d1
move.w (%a3)+,%d6
tst.b %d6
jbne invalid_archive
move.b %d1,%d6
tst.l (%a3)+
jbne invalid_archive
tst.b (%a3)+
jbne invalid_archive
.endm

| MEM_TO_MEM_DECOMPRESS inline function
| INPUT: %a3.l: pointer to compressed data
|        %a0.l: pointer to buffer for uncompressed data
|        %d6.l: uncompressed size
| OUTPUT: %a4.l: pointer to uncompressed data
| SIDE EFFECTS: may throw a Memory or Data Type error
| DESTROYS: %d0-%d2/%a0-%a2
.macro MEM_TO_MEM_DECOMPRESS
| LzmaDecode(src,dest,OutSize)
move.l %d6,-(%a7) | OutSize
pea.l (%a0) | dest
pea.l (%a3) | src
| Satisfy output constraint
movea.l %a0,%a4
jbsr LzmaDecode
lea.l 12(%a7),%a7
| Check return value
tst.w %d0
jbne invalid_archive
.endm

	.text
	.even
RangeDecoderBitDecode:
	movm.l #0x1c30,-(%sp)
	move.l %a0,%a3
	move.l %a1,%a2
	move.l 4(%a1),%d5
	move.l %d5,%d0
	moveq.l #11,%d1
	lsr.l %d1,%d0
	move.w (%a0),%d3
	move.w %d3,-(%sp)
	clr.w -(%sp)
	move.l %d0,-(%sp)
	jbsr __mulsi3
	addq.l #8,%sp
	move.l %d0,%d1
	move.l 8(%a2),%d4
	cmp.l %d4,%d0
	jbls .L2
	move.l %d0,4(%a2)
	move.w #2048,%d0
	sub.w %d3,%d0
	lsr.w #5,%d0
	add.w %d0,%d3
	move.w %d3,(%a3)
	cmp.l #16777215,%d1
	jbhi .L3
	lsl.l #8,%d4
	move.l (%a2),%a0
	or.b (%a0),%d4
	move.l %d4,8(%a2)
	addq.l #1,(%a2)
	move.l %d1,%d0
	lsl.l #8,%d0
	move.l %d0,4(%a2)
.L3:
	clr.w %d0
	jbra .L1
	.even
.L2:
	move.l %d5,%d2
	sub.l %d0,%d2
	move.l %d2,4(%a2)
	sub.l %d0,%d4
	move.l %d4,%d1
	move.l %d4,8(%a2)
	move.w %d3,%d0
	lsr.w #5,%d0
	sub.w %d0,%d3
	move.w %d3,(%a3)
	cmp.l #16777215,%d2
	jbhi .L5
	lsl.l #8,%d1
	move.l (%a2),%a0
	or.b (%a0),%d1
	move.l %d1,8(%a2)
	addq.l #1,(%a2)
	lsl.l #8,%d2
	move.l %d2,4(%a2)
.L5:
	moveq.l #1,%d0
.L1:
	movm.l (%sp)+,#0xc38
	rts
	.even
RangeDecoderBitTreeDecode:
	movm.l #0x1f00,-(%sp)
	move.l %a0,%d7
	move.w %d0,%d5
	move.l %a1,%d6
	moveq.l #1,%d1
	move.w %d0,%d4
	jbra .L7
	.even
.L11:
	move.w %d1,%d3
	add.w %d1,%d3
	move.w %d1,%a0
	add.l %a0,%a0
	move.l %d6,%a1
	lea (%a0,%d7.l),%a0
	jbsr RangeDecoderBitDecode
	move.w %d3,%d1
	add.w %d0,%d1
	subq.w #1,%d4
.L7:
	tst.w %d4
	jbgt .L11
	moveq.l #1,%d0
	lsl.w %d5,%d0
	sub.w %d0,%d1
	move.w %d1,%d0
	movm.l (%sp)+,#0xf8
	rts
	.even
LzmaLenDecode:
	movm.l #0x1820,-(%sp)
	move.l %a0,%a2
	move.l %a1,%d3
	move.w %d0,%d4
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbne .L13
	move.w %d4,%d1
	ext.l %d1
	lsl.l #4,%d1
	move.l %d3,%a1
	moveq.l #3,%d0
	lea 4(%a2,%d1.l),%a0
	jbsr RangeDecoderBitTreeDecode
	jbra .L12
	.even
.L13:
	move.l %d3,%a1
	lea (2,%a2),%a0
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbne .L14
	move.w %d4,%d1
	ext.l %d1
	lsl.l #4,%d1
	move.l %d3,%a1
	moveq.l #3,%d0
	lea 36(%a2,%d1.l),%a0
	jbsr RangeDecoderBitTreeDecode
	addq.w #8,%d0
	jbra .L12
	.even
.L14:
	move.l %d3,%a1
	moveq.l #8,%d0
	lea (68,%a2),%a0
	jbsr RangeDecoderBitTreeDecode
	add.w #16,%d0
.L12:
	movm.l (%sp)+,#0x418
	rts
	.even
	.globl	LzmaDecode
LzmaDecode:
	lea (-3692,%sp),%sp
	movm.l #0x1f3a,-(%sp)
	clr.w 50(%sp)
	clr.w 48(%sp)
	moveq.l #1,%d7
	move.l %d7,44(%sp)
	move.l %d7,40(%sp)
	move.l %d7,36(%sp)
	sub.l %a6,%a6
	moveq.l #0,%d0
	lea (64,%sp),%a0
	jbra .L16
	.even
.L20:
	move.w #1024,(%a0)+
	addq.l #1,%d0
.L16:
	cmp.l #1830,%d0
	jbcs .L20
	lea (52,%sp),%a1
	move.l 3732(%sp),(%a1)
	clr.l 8(%a1)
	moveq.l #-1,%d0
	move.l %d0,4(%a1)
	moveq.l #4,%d1
	jbra .L21
	.even
.L25:
	move.l 8(%a1),%d0
	lsl.l #8,%d0
	move.l (%a1),%a0
	or.b (%a0),%d0
	move.l %d0,8(%a1)
	addq.l #1,(%a1)
	subq.w #1,%d1
.L21:
	tst.w %d1
	jbge .L25
	jbra .L102
	.even
.L101:
	move.w %a6,%d5
	and.w #1,%d5
	lea (52,%sp),%a4
	move.w 50(%sp),%d3
	ext.l %d3
	move.l %d3,%d4
	add.l %d3,%d4
	move.w %d5,%a3
	lea (%a3,%d4.l),%a0
	add.l %a0,%a0
	moveq.l #64,%d6
	add.l %sp,%d6
	move.l %a4,%a1
	add.l %d6,%a0
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbne .L30
	move.l %sp,%d6
	add.l #2188,%d6
	cmp.w #3,50(%sp)
	jbgt .L31
	clr.w 50(%sp)
	jbra .L32
	.even
.L31:
	cmp.w #9,50(%sp)
	jbgt .L33
	subq.w #3,50(%sp)
	jbra .L32
	.even
.L33:
	subq.w #6,50(%sp)
.L32:
	moveq.l #1,%d0
	tst.w 48(%sp)
	jbeq .L47
	move.l %a6,%d0
	sub.l %d7,%d0
	move.l 3736(%sp),%a0
	move.b (%a0,%d0.l),%d5
	moveq.l #1,%d4
.L36:
	move.b %d5,%d3
	lsr.b #7,%d3
	and.w #1,%d3
	lsl.b #1,%d5
	addq.w #1,%d3
	move.w %d3,%d0
	ext.l %d0
	subq.w #1,%d3
	lsl.l #8,%d0
	move.l %d0,%a1
	lea (%a1,%d4.w),%a0
	add.l %a0,%a0
	move.l %a4,%a1
	add.l %d6,%a0
	jbsr RangeDecoderBitDecode
	lsl.w #1,%d4
	or.w %d0,%d4
	cmp.w %d3,%d0
	jbeq .L38
	jbra .L40
	.even
.L43:
	move.w %d4,%d3
	add.w %d4,%d3
	move.w %d4,%a0
	add.l %a0,%a0
	move.l %a4,%a1
	lea (%a0,%d6.l),%a0
	jbsr RangeDecoderBitDecode
	move.w %d3,%d4
	or.w %d0,%d4
.L40:
	cmp.w #255,%d4
	jble .L43
	jbra .L37
	.even
.L38:
	cmp.w #255,%d4
	jble .L36
.L37:
	move.b %d4,%d3
	clr.w 48(%sp)
	jbra .L46
	.even
.L47:
	move.w %d0,%d3
	add.w %d0,%d3
	move.w %d0,%a0
	add.l %a0,%a0
	move.l %a4,%a1
	lea (%a0,%d6.l),%a0
	jbsr RangeDecoderBitDecode
	or.w %d3,%d0
	cmp.w #255,%d0
	jble .L47
	move.b %d0,%d3
.L46:
	move.l 3736(%sp),%d0
	move.b %d3,(%a6,%d0.l)
	jbra .L104
	.even
.L30:
	move.w #1,48(%sp)
	move.l %d6,%a2
	add.l %d4,%a2
	move.l %a4,%a1
	lea (48,%a2),%a0
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbeq .L53
	move.l %a4,%a1
	lea (72,%a2),%a0
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbne .L54
	lsl.l #2,%d3
	move.l %d6,%d0
	add.l %d3,%d0
	lea (%a3,%a3.l),%a0
	lea (%a0,%d0.l),%a0
	move.l %a4,%a1
	lea (144,%a0),%a0
	jbsr RangeDecoderBitDecode
	tst.w %d0
	jbne .L59
	moveq.l #1,%d0
	cmp.w #0,%a6
	jbeq .L15
	moveq.l #9,%d0
	cmp.w #6,50(%sp)
	jble .L58
	moveq.l #11,%d0
.L58:
	move.w %d0,50(%sp)
	move.l %a6,%d0
	sub.l %d7,%d0
	move.l 3736(%sp),%a0
	move.b (%a0,%d0.l),(%a6,%a0.l)
.L104:
	addq.l #1,%a6
	jbra .L102
	.even
.L54:
	move.l %a4,%a1
	lea (96,%a2),%a0
	jbsr RangeDecoderBitDecode
	move.l 44(%sp),%d1
	tst.w %d0
	jbeq .L61
	move.l %a4,%a1
	lea (120,%a2),%a0
	jbsr RangeDecoderBitDecode
	move.l 40(%sp),%d1
	tst.w %d0
	jbeq .L63
	move.l 36(%sp),%d1
	move.l 40(%sp),36(%sp)
.L63:
	move.l 44(%sp),40(%sp)
.L61:
	move.l %d7,44(%sp)
	move.l %d1,%d7
.L59:
	move.w %d5,%d0
	move.l %a4,%a1
	lea (1608,%sp),%a0
	jbsr LzmaLenDecode
	move.w %d0,%d5
	moveq.l #8,%d0
	cmp.w #6,50(%sp)
	jble .L65
	moveq.l #11,%d0
.L65:
	move.w %d0,50(%sp)
	jbra .L66
	.even
.L53:
	move.l 40(%sp),36(%sp)
	move.l 44(%sp),40(%sp)
	move.l %d7,44(%sp)
	moveq.l #7,%d0
	cmp.w #6,50(%sp)
	jble .L68
	moveq.l #10,%d0
.L68:
	move.w %d0,50(%sp)
	move.w %d5,%d0
	move.l %a4,%a1
	lea (1028,%sp),%a0
	jbsr LzmaLenDecode
	move.w %d0,%d5
	cmp.w #3,%d0
	jble .L69
	moveq.l #3,%d0
.L69:
	ext.l %d0
	lsl.l #7,%d0
	move.l %d6,%a0
	add.l %d0,%a0
	move.l %a4,%a1
	moveq.l #6,%d0
	lea (192,%a0),%a0
	jbsr RangeDecoderBitTreeDecode
	move.w %d0,%d2
	move.w %d0,%d1
	ext.l %d1
	move.l %d1,%d7
	cmp.w #3,%d0
	jble .L93
	move.w %d0,%d4
	asr.w #1,%d4
	move.w %d4,%a3
	subq.w #1,%a3
	moveq.l #1,%d7
	and.l %d1,%d7
	moveq.l #2,%d0
	or.l %d0,%d7
	clr.l %d0
	move.w %a3,%d0
	lsl.l %d0,%d7
	cmp.w #13,%d2
	jbgt .L71
	move.l %d7,%d0
	add.l %d7,%d0
	add.l %d6,%d0
	add.l %d1,%d1
	sub.l %d1,%d0
	move.l %d0,%a2
	lea (702,%a2),%a2
	moveq.l #1,%d3
	clr.w %d6
	clr.w %d4
	jbra .L72
	.even
.L76:
	move.w %d3,%a0
	add.l %a0,%a0
	move.l %a4,%a1
	lea (%a2,%a0.l),%a0
	jbsr RangeDecoderBitDecode
	add.w %d3,%d3
	add.w %d0,%d3
	lsl.w %d4,%d0
	or.w %d0,%d6
	addq.w #1,%d4
.L72:
	cmp.w %a3,%d4
	jblt .L76
	jbra .L103
	.even
.L71:
	move.l 4(%a4),%d0
	move.l 8(%a4),%d1
	moveq.l #0,%d3
	move.w %d4,%d2
	subq.w #5,%d2
	jbra .L79
	.even
.L85:
	lsr.l #1,%d0
	add.l %d3,%d3
	cmp.l %d1,%d0
	jbhi .L83
	sub.l %d0,%d1
	moveq.l #1,%d4
	or.l %d4,%d3
.L83:
	cmp.l #16777215,%d0
	jbhi .L81
	lsl.l #8,%d0
	lsl.l #8,%d1
	move.l (%a4),%a0
	or.b (%a0),%d1
	addq.l #1,(%a4)
.L81:
	subq.w #1,%d2
.L79:
	tst.w %d2
	jbgt .L85
	move.l %d0,4(%a4)
	move.l %d1,8(%a4)
	lsl.l #4,%d3
	add.l %d3,%d7
	lea (996,%sp),%a2
	moveq.l #1,%d3
	clr.w %d6
	clr.w %d4
	jbra .L87
	.even
.L91:
	move.w %d3,%a0
	add.l %a0,%a0
	move.l %a4,%a1
	lea (%a2,%a0.l),%a0
	jbsr RangeDecoderBitDecode
	add.w %d3,%d3
	add.w %d0,%d3
	lsl.w %d4,%d0
	or.w %d0,%d6
	addq.w #1,%d4
.L87:
	cmp.w #4,%d4
	jblt .L91
.L103:
	move.l %d7,%a0
	lea (%a0,%d6.w),%a0
	move.l %a0,%d7
.L93:
	addq.l #1,%d7
.L66:
	tst.l %d7
	jbeq .L28
	moveq.l #1,%d0
	cmp.l %d7,%a6
	jbcs .L15
	addq.w #2,%d5
	move.l 3736(%sp),%a0
	sub.l %d7,%a0
	lea (%a6,%a0.l),%a0
.L96:
	move.l 3736(%sp),%a1
	move.b (%a0)+,(%a6,%a1.l)
	addq.l #1,%a6
	subq.w #1,%d5
	tst.w %d5
	jble .L102
	cmp.l 3740(%sp),%a6
	jbcs .L96
.L102:
	cmp.l 3740(%sp),%a6
	jbcs .L101
.L28:
	clr.w %d0
.L15:
	movm.l (%sp)+,#0x5cf8
	lea (3692,%sp),%sp
	rts
