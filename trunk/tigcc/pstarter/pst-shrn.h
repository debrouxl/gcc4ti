| TIGCC Program Starter - Shrink92 decompression support
| SHRINK92 - Copyright 1998, 1999 David Kuehling
| Adaptation for TI-89/92+ - Copyright 2002, 2003, 2004, 2005 Patrick Pelissier
| Adaptation for pstarter - Copyright 2005 Kevin Kofler
| Additional Optimizations - By Samuel Stearley
|
| This file is based on the SHRINK92 Library.
|
| The SHRINK92 Library is free software; you can redistribute it and/or modify
| it under the terms of the GNU Lesser General Public License as published by
| the Free Software Foundation; either version 2.1 of the License, or (at your
| option) any later version.
|
| The SHRINK92 Library is distributed in the hope that it will be useful, but
| WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
| or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
| License for more details.
|
| You should have received a copy of the GNU Lesser General Public License
| along with the SHRINK92 Library; see the file COPYING.LIB.  If not, write to
| the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
| MA 02111-1307, USA.

| Version : 1.00.P4-tigcc-2 by Kevin Kofler Apr 23 2005
| * adjust calling convention for pstarter changes

| Version : 1.00.P4-tigcc-1 by Kevin Kofler Mar 18 2005
| * change tios.h to doorsos.h
| * delete TI-92 support
| * delete archive pack support
| * allocate Table of Unresolved Elements on the stack
| * use HeapAllocPtr to allocate archive descriptor
| * throw an error if allocating the archive descriptor fails
| * don't allocate memory in Extract
| * free archive descriptor at end of Extract, delete CloseArchive
| * always extract first section
| * read uncompressed size in OpenArchive
| * adjust calling conventions for pstarter
| * use nostub ROM_CALL convention
| * convert to GNU as

| Version : 1.00.P4 by Patrick Pelissier Feb 10 2005
| Version : 1.00.P3 by Patrick Pelissier Nov 10 2004
| Version : 1.00.P2 by Patrick Pelissier Nov 24 2003

.equ RLE_ESC,256
.equ RLE_BITS,5
.equ REP_ESC,257
.equ START_REP_BITS,4
.equ START_REP_CODES,16
.equ REP_CODES,64
.equ MIN_REP,3
.equ CODES,256+REP_CODES+1

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
| Check if it has type "SHRN".
cmp.b #0xf8,(%a0)
jbne invalid_archive
tst.b -(%a0)
jbne invalid_archive
cmp.b #'N',-(%a0)
jbne invalid_archive
cmp.b #'R',-(%a0)
jbne invalid_archive
cmp.b #'H',-(%a0)
jbne invalid_archive
cmp.b #'S',-(%a0)
jbne invalid_archive
tst.b -(%a0)
jbeq valid_archive
invalid_archive:
.word 0xA000+210 | ER_DATATYPE
valid_archive:
bsr OpenArchive
.endm

| MEM_TO_MEM_DECOMPRESS inline function
| INPUT: %a3.l: pointer to compressed data
|        %a0.l: pointer to buffer for uncompressed data
|        %d6.l: uncompressed size
| OUTPUT: %a4.l: pointer to uncompressed data
| SIDE EFFECTS: may throw a Memory or Data Type error
| DESTROYS: %d0-%d2/%a0-%a2
.macro MEM_TO_MEM_DECOMPRESS
bsr Extract
.endm

|| **************************************************************************
|  OpenArchive
| --------------------------------------------------------------------------
|  Input:  %a3.l = pointer to archive
|  Output: %a3.l = pointer to archive descriptor
|          %d6.l = length of section
|  Destroys: %d1-%d2/%a0-%a1
|  Throws ER_MEMORY if not enough memory.
| /
OpenArchive:	movem.l	%d0/%d3-%d5/%d7/%a2/%a4/%a6,-(%a7)	
		movea.l	%a7,%a6			|  %a6 = saved SP for removing stack arguments later
		movea.l	%a3,%a2			|  %a2 = pointer to archive (%a2 isn't modified by ROM functions)

	|| ***** Initialize The Table Of The Unresolved Leaves/Junctions| /
		|  Table's format: <weight (word)> <address (word)> ...
                |   <address>: pointer,relative to the huffman tree's begin
                |              if the MSB is set: it represents a compression CODE (character)
		| /
	     ||  allocate memory for the table| /
		lea	-(CODES*4)(%a7),%a7
		movea.l	%a7,%a3
		
	     ||  uncompress frequency table from archive's begin into that table| /
		|  %a2 = pointer to archive begin (RLE compressed freqency table| each byte is a frequency) -- input
		|  %a3 = pointer to Unresolved Table -- output
		move.w	#0x8000,%d7		|  %d7 = <address>-counter (code | 0x8000)
		movea.l	%a3,%a1			|  %a1 = output-pointer (is increased whereas %a3 points to the table's begin)
		clr.w	%d1			|  %d1.w = one byte (high byte has to be cleared)
		moveq	#-1,%d3			|  %d3.w = number of entries in table - 1
OAExtrFreqLoop:	clr.w	%d0			   |  %d0 = RLE repetition length - 1 (0 = ONE character -- no RLE)
		move.b	(%a2)+,%d1		   |  %d1 = current byte / RLE ESC byte
		cmpi.w	#0xE0,%d1 		   |  is it an RLE ESC character ?
		blt.s	OA__RLEloop		      |  (if it isn't: cylcle one time through RLE loop)
		andi.w	#0x001F,%d1		      |  extract RLE length from RLE ESC byte
		move.w	%d1,%d0			 
		move.b	(%a2)+,%d1		      |  read RLE byte
OA__RLEloop:	tst.w	%d1			         |  don't add entries with a frequency of zero
		beq.s	OA____Skip
		move.w	%d1,(%a1)+                        |  copy frequency and <address> (%d7) to output table (%a1)
		move.w	%d7,(%a1)+
		addq.w	#1,%d3			         |  (%d3 = number of table entries)
OA____Skip:	addq.w	#1,%d7
		dbra	%d0,OA__RLEloop
		cmpi.w	#CODES+0x8000,%d7
		blt.s	OAExtrFreqLoop

	     ||  align input address on even address| /
		move.l	%a2,%d1
		addq.l	#1,%d1
		bclr.b	#0,%d1
		movea.l	%d1,%a2

	|| ***** Create Archive Descriptor,Containing the Huffman Tree| /
		|  %d3 = number of entries in Table of Unresolved Elements - 1
		|  %a2 = aligned pointer directly after the RLE compressed frequency table in the input archive
		|  %a3 = pointer to first entry in Table of Unresolved Elements
	     ||  allocate memory for archive descriptor| /
		move.w	(%a2)+,-(%a7)		|  at the current position in the archive the size of the descriptor is stored
		clr.w	-(%a7)			|  it is a long value
		move.l	0xa2*4(%a5),%a0		|  HeapAllocPtr
		jsr	(%a0)
		move.l	%a0,%d5			|  %a0 = %d5 = pointer to archive descriptor
		beq.s	OAError

		move.l	%a2,(%a0)+		|  store archive address into descriptor
		movea.l	%a0,%a1			|  %a1 = working pointer| %a0 = pointer to huffman tree
		clr.w	(%a1)+			|  leave space for root pointer
		moveq	#0,%d6			|  extend 2(%a2) to longword in %d6
		move.w	2(%a2),%d6		|  %d6 = length of section

	     ||  create the huffman tree| /
OAHuffTreeLoop:	||  find the two elements with the lowest weight| /
		bsr.s	OAGetMinWeight		   |  %a2 = pointer to min element| %d0 = its weight
		move.w	%d0,%d2			   |  %d2 = lowest weight 1
		move.w	2(%a2),%d4		   |  %d4 = relative pointer to lowest elm 1
		move.l	(%a3)+,(%a2) 	           |  "delete" element from unresolved list (replace it by first elm)
		subq.w	#1,%d3			   |  decrease number of entries
		bmi.s	OAHuffTreeEnd		   |  if number of entries was #1: end of Huffman Tree creation
		bsr.s	OAGetMinWeight		   |  %a2 = pointer to min element| %d0 = it's weight
	
		||  replace that element by a newly created junction of the two min elements| /
		|  %d2 = lowest weight1
		|  %d4 = relative pointer to lowest elm 1
		|  %a2 = pointer to entry of loweset elm1 in unresolved table: (%a2) = weight| 2(%a2) = pointer
		add.w	%d2,(%a2)+		   |  weight of min2 += weight of min1
		move.l	%a1,%d1		   	   |  %d1 = relative address of new junction
		sub.l	%a0,%d1
		move.w	%d4,(%a1)+		   |  new Huffman Tree element: set left branch of junction
		move.w	(%a2),(%a1)+		   |                            set right branch of junction
		move.w	%d1,(%a2)		   |  entry in Unresolved Table: set the pointer to the Huffman Tree element
		bra.s	OAHuffTreeLoop		   |  continue...
	
OAHuffTreeEnd:	move.w	%d4,(%a0)  		|  store root pointer to archive descriptor

		movea.l	%d5,%a3			|  %a3 = pointer to archive descriptor
		movea.l	%a6,%a7			|  restore stored %a7 -- remove all stack elements
		movem.l	(%a7)+,%d0/%d3-%d5/%d7/%a2/%a4/%a6	|  restore registers
		rts
OAError:
	.word 0xa000+670 |ER_MEMORY

	| 
 	|  Get the element witht the minimum weight in the table of unresolved tree elements.
 	|  Input:  %a3 = pointer to first table entry
	|          %d3 = number of entries - 1
  	|  Output: %a2 = pointer to element with the minimum weight < 32767 or NULL if no such elements are left
	|          %d0 = minimum weight 
	|  %a4/%d1 destroyed!
	| /
OAGetMinWeight:	moveq	#-1,%d0			|  %d0 = minimum weight = 32768 (unsigned)
		move.w	%d3,%d1			|  %d1 = counter
		movea.l	%a3,%a4			|  %a4 = scanning pointer
OACmpLoop:	cmp.w	(%a4),%d0		   |  current element's weight less than minimum weight ?
		bls.s	OA__NotLess
		movea.l	%a4,%a2			   |  minimum element found: update %a2 and %d0
		move.w	(%a4),%d0
OA__NotLess:	addq.l	#4,%a4			   |  go to next element
		dbra	%d1,OACmpLoop
		rts


|| *************************************************************************************************
|  Bitwise Input
| /
|| *************************************************************************************************
|  ReadBit
| --------------------------------------------------------------------------------------------------
|  Input:  %a2.l = pointer to next byte
| 	   %d0.b = contents of current byte
| 	   %d1.b = last bit number
|  Output: zeroflag set or cleared,depending on value of bit,
|          %a2,%d0,%d1 adjusted to current bit
| /
ReadBit:
		subq.b	#1,%d1			|  increase bit number  --  go to current bit
		bne.s	RBSameByte
			move.b	(%a2)+,%d0	|  read new byte,begin again with bit #0
			moveq	#8,%d1
RBSameByte:	lsr.b	#1,%d0			|  test bit -- set carry/extend flag accordingly
		rts

|| *************************************************************************************************
|  ReadNBits
| --------------------------------------------------------------------------------------------------
|  Input:  %a2.l = pointer to next byte
| 	   %d0.b = contents of current byte
| 	   %d1.b = last bit number
|          %d7.w = number of bits to read
|  Output: %a2,%d0,%d1 adjusted to last bit of bits read
|          %d7 = -1 
|          %d3 = read bits (first read bit is high bit)
| /
ReadNBits:	subq.w	#1,%d7		|  %d7 = counter
		moveq	#0,%d3		|  %d3 = bits

RNBReadLoop:	bsr.s	ReadBit		|  read a bit
		addx.l  %d3,%d3		|  move in the bit
RNB__ZeroBit:	dbra	%d7,RNBReadLoop
		rts
		

|| *************************************************************************************************
|  Extract
| --------------------------------------------------------------------------------------------------
|  Input:  %a3.l = pointer to archive descriptor
|          %a0.l = address of extraction destination
|          %d6.l = length of section
|  Output: %a4.l = address of extraction destination
|  Throws ER_DATATYPE for an invalid archive.
|  Destroys: %d0-%d2/%a0-%a2
| /
Extract: 	movem.l	%d3-%d7/%a3/%a6,-(%a7)

		||  Prepare Extraction...| /
		move.l	(%a3),%a2			|  %a2 = pointer to archive data (after frequency table)
		adda.w	(%a2),%a2			|  %a2 = address of first section (expect offset <32768)
|SNS changed it to a 1
		moveq	#1,%d1			|  %d1 = bit number (1 forces 'ReadBit' to read new byte into %d0)
		moveq	#0,%d4			|  %d4 = input offset
		movea.l	%a0,%a1			|  %a1 = working destination pointer

	    |  %a0 = address of destination memory block
	    |  %a1 = address of destination memory block (working pointer that is increased during extraction)
	    |  %a2 = address of archive section begin (is increased during extraction)
	    |  %a3 = pointer to huffman tree begin (within descriptor)
	    |  %d0 = current byte
	    |  %d1 = bit number within current byte '%d0'
	    |  %d2 = handle of allocated destination block or original value 
	    |  %d6 = length of section
	    |  %d4 = current input offset
	|| ***** Extraction Loop| /
EXExtractLoop:
	     ||  check whether end of file| /
		cmp.l	%d6,%d4
		beq.s	EXExtractEnd
		bgt.s	EXError
		
	     ||  read a Huffman code| /
		moveq   #0,%d7

EX__HuffLoop: 		move.w	4(%a3, %d7.w),%d7|  %d7 = relative pointer to root element
						 |  if bit #15 in "address" is set -> it is a leaf| %d7 contains the code
			bmi.s	EX__HuffDone	 |  %d7 points to current element| (%d7) = left branch 2(%d7) = right branch
			bsr.s	ReadBit
			bcc.s	EX__HuffLoop
			addq.w	#2,%d7	|  continue with right branch
			bra.s	EX__HuffLoop
|SNS: unlike the previous version it has not yet cleared the high bit of d7
EX__HuffDone:	|  %d7.w = current code
		cmp.w	#256+32768,%d7		|  check code
		beq.s	EX__RLE			   |  code = 256: RLE
		bgt.s	EX__Rep			   |  code > 256: Rep

		||  'code' (%d7) is a character| /
		move.b	%d7,(%a1)+		   |  copy code to destination
		addq.l	#1,%d4
		bra.s	EXExtractLoop

		||  RLE| /
EX__RLE:	move.b	-1(%a1),%d5		   |  %d5 = RLE repeating character
		moveq	#RLE_BITS,%d7		   |  read length of RLE sequence
		bsr	ReadNBits		   
		addq.w	#1,%d3			   |  %d3 = RLE repetition length - 1
		add.l	%d3,%d4
		addq.l  #1,%d4

EX____RLEloop:		move.b	%d5,(%a1)+		      |  output characters...
			dbra	%d3,EX____RLEloop
		bra.s	EXExtractLoop

		||  Repetition encoding| /
EX__Rep:		sub.w	#32768+REP_ESC-MIN_REP+1,%d7   |  %d7 = length of repetition - 1
		move.w	%d7,-(%a7)		   	|  store it on the stack
		moveq	#START_REP_BITS-1,%d7	  	|  get the number of bits that is required for coding an offset
		moveq	#START_REP_CODES/2,%d3
EX____IncrBitN:		addq.l	#1,%d7
			add.l	%d3,%d3
			move.l	%d3,%d5
			addq.l	#MIN_REP,%d5
			cmp.l	%d4,%d5		|  codable range is too small ? --> increase bit number and try again
			bls.s	EX____IncrBitN
						|  %d7 = number of bits
		bsr	ReadNBits		|  %d3 = repetition offset
		move.w	(%a7)+,%d7		|  %d7 = length of repetition - 1
		lea	0(%a0,%d3.l),%a4		|  %a4 = pointer to repetition
		add.l	%d7,%d4
		addq.l  #1,%d4
EX____RepCopyLp:	move.b	(%a4)+,(%a1)+	|  copy repetition to current output position...
			dbra	%d7,EX____RepCopyLp
		bra.s	EXExtractLoop

EXError:		suba.l	%a0,%a0			|  %a0 = zero (indicates error)
EXExtractEnd:	movea.l	%a0,%a4
		pea.l	(%a3)			|  Free Archive Descriptor
		move.l	0xa3*4(%a5),%a0		|  HeapFreePtr
		jsr	(%a0)
		addq.l	#4,%a7
		move.l	%a4,%d0
		beq	invalid_archive
		movem.l	(%a7)+,%d3-%d7/%a3/%a6
		rts
