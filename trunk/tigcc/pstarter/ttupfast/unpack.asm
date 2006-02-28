;-----------------------------------------------------------------------
;INTRODUCTION:
;
;   This is a 100% assembly version of the ttunpack routine, which is
;   based on code by Pasi 'Albert' Ojala, albert@cs.tut.fi, then
;   reduced by Thomas Nussbaumer to fit his needs.  For a full details
;   on the algorithm see:
;
;	  http://www.cs.tut.fi/~albert/Dev/pucrunch/index.html
;
;   Version 2.28 Fast
;
;
;THE LICENSE:
;
; Copyright (C) 2004-2005 Samuel Stearley
;
;               wxWindows Library Licence, Version 3.1
;               ======================================
;
; Copyright (C) 1998-2005 Julian Smart, Robert Roebling et al
;
; Everyone is permitted to copy and distribute verbatim copies
; of this licence document, but changing it is not allowed.
;
;                      WXWINDOWS LIBRARY LICENCE
;    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;
; This library is free software; you can redistribute it and/or modify it
; under the terms of the GNU Library General Public Licence as published by
; the Free Software Foundation; either version 2 of the Licence, or (at
; your option) any later version.
;
; This library is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
; General Public Licence for more details.
;
; You should have received a copy of the GNU Library General Public Licence
; along with this software, usually in a file named COPYING.LIB.  If not,
; write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
; Boston, MA 02111-1307 USA.
;
; EXCEPTION NOTICE
;
; 1. As a special exception, the copyright holders of this library give
; permission for additional uses of the text contained in this release of
; the library as licenced under the wxWindows Library Licence, applying
; either version 3.1 of the Licence, or (at your option) any later version of
; the Licence as published by the copyright holders of version
; 3.1 of the Licence document.
;
; 2. The exception is that you may use, copy, link, modify and distribute
; under your own terms, binary object code versions of works based
; on the Library.
;
; 3. If you copy code from files distributed under the terms of the GNU
; General Public Licence or the GNU Library General Public Licence into a
; copy of this library, as this licence permits, the exception does not
; apply to the code that you add in this way.  To avoid misleading anyone as
; to the status of such modified files, you must delete this exception
; notice from such code and/or adjust the licensing conditions notice
; accordingly.
;
; 4. If you write modifications of your own for this library, it is your
; choice whether to permit this exception to apply to your modifications. 
; If you do not wish that, you must delete the exception notice from such
; code and/or adjust the licensing conditions notice accordingly.
;
;
;NOTES:
;
;1) There were several other names associated with this, but not
;   anymore ;)
;
;
;2) There should be an unpack.c distributed with this that has
;   additional comments that will help clarify what this is doing.
;   But maybe not.  It depends on where you got this.  you should
;   always be able to find this on my site:  http://www.nyall.net
;
;
;3) If caller wants, they can alter the default int priority to
;   speed up execution before calling this routine.
;
;
;4) The code was written with wordpad.  It might look better if
;   opened with that program.
;
;
;5) A version of this is included as a hex array for use with programs
;   written with tigcc.  OR you can use this file directly with a
;   tigcc project: add it to the project and remove the ';' from
;   before the 'CALL_DIRECTLY' below.  Be sure to have the latest
;   version of tigcc when doing this.
;----------------------------------------------------------------------

;CALL_DIRECTLY		;if this label is defined then this code can be used
				;  directly as part of a tigcc project.

DELTA_LZ			;If defined it will include code to do DELTA lz decompression
				;  ebooks and ppg compressed programs currently do not take advantage of
				;  the -fdelta option of the compressor.

;UNROLL_RLE

;UNROLL_LZ



 ifd CALL_DIRECTLY
	public ttunpack_decompress
 endc


 ifnd CALL_DIRECTLY
	include "os.h"
	xdef _ti89
	xdef _nostub
 endc



osize_lo		equ	0	;original size lowbyte
osize_hi		equ	1	;original size highbyte
magic1		equ	2	;must be equal to UNPACK_MAGIC1
magic2		equ	3	;must be equal to UNPACK_MAGIC2
csize_lo		equ	4	;compressed size lowbyte
csize_hi		equ	5	;compressed size lowbyte
esc1			equ	6	;escape >> (8-escBits)
notused3		equ	7	;
notused4		equ	8	;
esc2			equ	9	;escBits
gamma1		equ	10	;maxGamma + 1
gamma2		equ	11	;(1<<maxGamma)
extralz		equ	12	;extraLZPosBits
notused1		equ	13	;
notused2		equ	14	;
rleentries		equ	15	;rleUsed

HEADER_SIZE		equ	16

__MAGIC_CHAR1	equ	$54
__MAGIC_CHAR2	equ	$50

__ERRPCK_OKAY		equ	0
__ERRPCK_NOESCFOUND	equ	8		;248
__ERRPCK_ESCBITS		equ	7		;249
__ERRPCK_MAXGAMMA		equ	6		;250
__ERRPCK_EXTRALZP		equ	5		;251
__ERRPCK_NOMAGIC		equ	4		;252
__ERRPCK_OUTBUFOVERRUN	equ	3		;253
__ERRPCK_LZPOSUNDERRUN	equ	2		;254

;-----------------------------------------------------------------------
;Notes on register useage, might be good Idea to print this.
;
;	a0 => trashing, but may not be used by the subroutines
;	a1 => extra z position bits
;	a2 => a table that maps a byte to the highest bit set in it.
;	a3 => the bytecodevec table
;	a4 => The point to jump to when doing a continue
;	a5 => the destination buffer
;	a6 => points to the next byte of the compressed data.
;		updated when d7 overflows.
;
;	d0 => trashing, output of __GetBits and __GetValue
;	d1 => trashing, input to __GetBits
;	d2 => prepared mask for isolating # of escape bits
;	d3 => trashing but only by the main routine, not the subroutines
;		It is usually the prepared count.
;	d4 => # of escape bits
;	d5 => start escape
;	d6 => the current compressed byte, updated when d7 overflows.
;	d7 => which bit we are at in the compressed data.
;-----------------------------------------------------------------------

CORRECT_IN_MASK	MACRO
	tst.b		d7
	bne.s		\@fun
	moveq		#8,d7
	move.b	(a6)+,d6
\@fun
	ENDM



ttunpack_decompress:

 ifnd CALL_DIRECTLY
	dc.b		"UNPACK_0"
 endc

	movem.l	d3-d7/a2-a6,-(a7)
	move.l	4+10*4(a7),a6
	move.l	8+10*4(a7),a5
;--------------------------------------------------------
;  startesc = cth->esc1;     //d5
;  bytecodevec = &src[15];   //a3
;  __imask__   = 0x80;	     //d7
;
;These are initialized here to insure that certain
;branches can use the short form.
;--------------------------------------------------------
	lea		15(a6),a3			;'bytecodevec'
	moveq		#0,d5
	move.b	esc1(a6),d5			;'StartEsc'
	moveq		#8,d7				;which bit i am at
;-------------------------------------------------------------------------------------------------
;  if (cth->magic1 != __MAGIC_CHAR1 || cth->magic2 != __MAGIC_CHAR2) return __ERRPCK_NOMAGIC;
;  if (cth->gamma1 != 8 || cth->gamma2 != 128)                       return __ERRPCK_MAXGAMMA;
;  if ((escbits = cth->esc2) > 8)                                    return __ERRPCK_ESCBITS;
;  if ((extralzposbits = cth->extralz) > 4)                          return __ERRPCK_EXTRALZP;
;-------------------------------------------------------------------------------------------------
	moveq		#__ERRPCK_NOMAGIC,d0
	cmp.b		#__MAGIC_CHAR1,magic1(a6)	;these could be optimized into 1 word compare
	bne.s		__ReturnError			;  if evenly aligned.
	cmp.b		#__MAGIC_CHAR2,magic2(a6)
	bne.s		__ReturnError

	moveq		#__ERRPCK_MAXGAMMA,d0
	cmp.b		#8,gamma1(a6)
	bne.s		__ReturnError
	cmp.b		#128,gamma2(a6)
	bne.s		__ReturnError

	moveq		#__ERRPCK_ESCBITS,d0
	moveq		#0,d1
	move.b	esc2(a6),d1
	move.l	d1,d4
	subq.w	#8,d1
	bhi.s		__ReturnError

	moveq		#__ERRPCK_EXTRALZP,d0
	moveq		#0,d1
	move.b	extralz(a6),d1
	move.l	d1,a1			;extralz pos bits
	subq.w	#5,d1
	bgt.s		__ReturnError
;--------------------------------------------------------
;  escbits8 = 8 - escbits;   //not used
;  __ibuffer__ = src + sizeof(__PACKHEADER) + cth->rleentries;  //a6
;--------------------------------------------------------
	moveq		#0,d2
	bset		d4,d2
	subq.w	#1,d2			;the mask for the number of escape bits
	ror.b		d4,d2			;set the upper bits
	ror.b		d4,d5			;shift up start escape
	moveq		#0,d0
	move.b	(a3),d0		
	lea		HEADER_SIZE(a6,d0),a6
;--------------------------------------------------------
;To remove checking if escbits == 0 in the literal byte
;  loop, there are two points used for continuing.
;--------------------------------------------------------
	lea		__ReturnPointForNonZeroEscapeBits(pc),a4
	tst.b		d4
	bne.s		__Skipper
	lea		__SelIsStartEscape(pc),a4
__Skipper:
;--------------------------------------------------------
;a2 will point to a table that maps a byte to the
;  highest bit set in it.  0 will be mapped to zero,
;  this is important.
;--------------------------------------------------------
	lea		-260(a7),a7
	move.l	a7,a2
	clr.b		(a2)+
	moveq		#0,d0
	moveq		#7,d1
AllBytes:
	moveq		#0,d3
	bset		d0,d3
ThisByte:
	move.b	d0,(a2)+
	subq.w	#1,d3
	bne.s		ThisByte
	addq.w	#1,d0
	dbra    	d1,AllBytes
	move.l	a7,a2
	move.b	(a6)+,d6		;very first byte of compressed data
	jmp		(a4)			;jump into the loop
;---------------------------------------------------------
;And when all is done branch here
;---------------------------------------------------------
__WeAreDone:
	moveq		#0,d0			;return value
	lea		260(a7),a7
__ReturnError:
	neg.b		d0			;turn loaded error code into actual value
	movem.l	(a7)+,d3-d7/a2-a6
	rts

;---------------------------------------------------------
;  *outbuffer++ = (sel<<escbits8) | __GetBits(escbits8);
;  continue;
;---------------------------------------------------------
__SelIsNOTStartEscape:
	move.b	d0,(a5)+			;put litteral byte to the output
;---------------------------------------------------------
;The while(1) loop:
;
;  sel = (escbits) ? __GetBits(escbits) : startesc;
;  if (sel == startesc) {
;---------------------------------------------------------
__ReturnPointForNonZeroEscapeBits
	move.b	d6,-(a7)
	move.w	(a7)+,d0
	move.b	(a6)+,d6
	move.b	d6,d0
	lsr.w		d7,d0				;__get8bits() done
	move.b	d2,d1
	and.b		d0,d1				;isolate the upper escape bits
	cmp.b		d5,d1				;are they 'startesc'
	bne.s		__SelIsNOTStartEscape
	subq.l	#2,a6
	move.b	(a6)+,d6
	sub.w		d4,d7
	bhi.s		__StillInSameByte2
	move.b	(a6)+,d6
	addq.w	#8,d7
__StillInSameByte2:
;---------------------------------------------------------
;The following code is entered if sel == startesc
;but it does not actually use the sel variable
;
;  lzlen = __GetValue();
;  if (lzlen != 1) {
;---------------------------------------------------------
__SelIsStartEscape:
	bsr		__GetValue		;get a value for 'lzlen'
	move.w	d0,d3			;save 'lzlen'
	subq.w	#1,d0			;does 'lzlen' == 1 ?
	beq.s		__RleDecoding
;---------------------------------------------------------
;Zip decoding
;
;  lzposhi = __GetValue() - 1;
;  if (lzposhi == 254) {
;---------------------------------------------------------

 ifnd DELTA_LZ
	bsr		__GetValue		;if it equals 254, then lzlen must be > 3 because
	addq.b	#1,d0			;  there is no other possiblity because Delta decompression
	beq.s		__WeAreDone		;  is not being used.  It the branch is not taken it
 endc						;  drops through to the __LzPosHi_IsNot254


 ifd DELTA_LZ
		bsr		__GetValue		;get 'lzposhi', dont subtract 1 yet
		addq.b	#1,d0			;does 'lzposhi' == 254, remember it has not yet subtracted 1
		bne.s		__LzPosHi_IsNot254
		subq.w	#4,d3			;is 'lzlen' greater than 3?
		bcs.s		__WeAreDone		;destination is above
	;---------------------------------------------------------
	;  add   = __Get8Bit();			//put into d1
	;  lzpos = __Get8Bit() ^ 0xff;	//put into d0
	;
	;take advantage of the fact that two __Get8Bit's are used
	;sequentially by inlining them.
	;
	;the bit inverstion and the pointer subtraction will be
	;  combined
	;---------------------------------------------------------
		addq.w	#4,d3			;undo above subq.w
		move.b	d6,d1			;put current byte into d1
		swap		d1			;put it in the upper word
		move.b	(a6)+,-(a7)		;get the second byte
		move.w	(a7)+,d1		;put it in d1
		move.b	(a6)+,d1		;the third byte
		move.b	d1,d6
		lsr.l		d7,d1
		moveq		#-1,d0
		move.b	d1,d0			;d0 is -'lzpos' +1
		move.w	d1,-(a7)
		move.b	(a7)+,d1		;shift it 8 bits to the right
	;---------------------------------------------------------
	;  for (i=0; i<=lzlen; i++) {
	;    *outbuffer = *(outbuffer - lzpos - 1) + add;
	;    outbuffer++;
	;  }
	;---------------------------------------------------------
		lea		0(a5,d0.w),a0
	__WriteDataLoop2:
		move.b	(a0)+,d0
		add.b		d1,d0
		move.b	d0,(a5)+
		dbra		d3,__WriteDataLoop2
		jmp		(a4)				;continue
 endc
;----------------------------------------------------------------------------------------
;  if (extralzposbits) lzposhi = (lzposhi<<extralzposbits) | __GetBits(extralzposbits);
;  lzposlo = __Get8Bit() ^ 0xff;
;  lzpos   = COMBINE_LOWHIGH(lzposlo,lzposhi);
;
; d0= lzPosHi, d3.w = lzlen, d1.l =lzpos
;----------------------------------------------------------------------------------------
__LzPosHi_IsNot254:
	subq.b	#2,d0				;undo the addq.b #1
	move.w	a1,d1				;is 'extralzposbits' == 0?
	beq.s		__extralZposBitsIsZero
	lsl.w		d1,d0				;shift it by 'extralzposbits'
	moveq		#0,d1
	bset.w	d7,d1
	subq.w	#1,d1
	and.b		d6,d1
	sub.w		a1,d7
	bhi.s		__StillInSameByte61
	move.b	(a6)+,d6
	move.b	d1,-(a7)
	move.w	(a7)+,d1
	move.b	d6,d1
	addq.w	#8,d7
__StillInSameByte61:
	lsr.w		d7,d1
	or.w		d1,d0				;or them back together
__extralZposBitsIsZero:				;at this point d0.b == 'lzposhi'
	moveq		#0,d1
	move.b	d0,-(a7)			;a faster way to do lsl.w #8
	move.w	(a7)+,d1			;shift it into the high byte of 'lzpos', first step of COMBINE_LOWHIGH
	move.b	d6,-(a7)			;start the __Get8Bit
	move.w	(a7)+,d0
	move.b	(a6)+,d6
	move.b	d6,d0
	lsr.w		d7,d0				;get 8 bit done
	move.b	d0,d1				;COMBINE_LOWHIGH
	not.b		d1
	not.l		d1
;-----------------------------------------------------------------------------------
;  for (i=0; i<=lzlen; i++) {
;    *outbuffer = *(outbuffer - lzpos - 1); //no 'add' variable
;    outbuffer++;
;  }
;
; d2.l must be -lzpos+1
; d3.w must be lzlen
;------------------------------------------------------------------------------------
	lea		0(a5,d1.l),a0

 ifd UNROLL_LZ
	lsr.w		#1,d3
	bcc.s		__FirstByte
__WriteDataLoop:
	move.b	(a0)+,(a5)+
__FirstByte:
	move.b	(a0)+,(a5)+
	dbra		d3,__WriteDataLoop
 endc

 ifnd UNROLL_LZ
__WriteDataLoop:
	move.b	(a0)+,(a5)+
	dbra		d3,__WriteDataLoop
 endc

	jmp		(a4)				;continue
;----------------------------------------------------------------------
;'lzlen' = 1
;    RLE decoding, OR 2 byte zip, OR escape from the escape
;----------------------------------------------------------------------
__RleDecoding:
	subq.b	#1,d7
	btst.b	d7,d6
	beq.s		__NextBitClear_DoZipAfterAll

 CORRECT_IN_MASK

;The second NEXT_BIT_SET()

	subq.b	#1,d7
	btst.b	d7,d6
	beq		__NextBitIsClear_EscapeFromEscape
;-----------------------------------------------------------------
;  rlelen = __GetValue();
;  if (rlelen >= 128) {
;     rlelen = ((rlelen-128)<<1) | __GetBits(1);
;     rlelen |= (((__GetValue())-1)<<8);
;  }
;------------------------------------------------------------------
__NextBitIsSet_RunLengthEncoding:
 CORRECT_IN_MASK

	bsr.s		__GetValue		;get 'rlelen' into d0, limited to 8 bit values.
	move.b	d0,d3			;copy it and check sign bit to see if >= 128
	bpl.s		__LessThan128
;	sub.w		#128,d3		;not needed
	add.b		d3,d3			;double it, this clears the high bit so there is no need to subtract 128

;-----An Inlined version of __GetBits optimized for an input of 1 --------

	subq.b	#1,d7
	btst.b	d7,d6
	sne		d0
	sub.b		d0,d3			;subtract -1 if the bit was set = add 1
 CORRECT_IN_MASK

	bsr.s		__GetValue
	subq.b	#1,d0
	move.w	d3,-(a7)
	move.b	d0,(a7)
	move.w	(a7)+,d3
;-------------------------------------------------------------------
;Need the byte to copy
;
;  bytecode = __GetValue();
;  if (bytecode < 32) byte = bytecodevec[bytecode];
;  else               byte = ((bytecode-32)<<3) | __GetBits(3); //subracting 32 not needed
;-------------------------------------------------------------------
__LessThan128:
	bsr.s		__GetValue			;get bytecode
	cmp.b		#32,d0
	bcc.s		__GreaterThanOrEqual32
	move.b	0(a3,d0.w),d0		;byte = bytecodevec[bytecode];
	bra.s		__RleCopy
__GreaterThanOrEqual32:

;-----An Inlined version of GetBits that is optimized for an input of 3-------

	lsl.b		#3,d0
	move.b	d6,d1
	subq.w	#3,d7
	bhi.s		__StillInSameByte2345
	move.b	(a6)+,d6
	move.b	d1,-(a7)
	move.w	(a7)+,d1
	move.b	d6,d1
	addq.w	#8,d7
__StillInSameByte2345:
	lsr.w		d7,d1
	and.b		#$7,d1
	or.b		d1,d0
;--------------------------------------------------------
;  for (i=0; i<=rlelen; i++) *outbuffer++ = byte;
;  continue;   // continue the main loop ...
;
;Rle loop expects
; d0 = byte
; d3 = number of em to copy, no need to subtract 1 for
;	 dbra
;--------------------------------------------------------
__RleCopy:

 ifd UNROLL_RLE
	lsr.w		#1,d3
	bcc.s		__FirstRleByte
__RleLoop:
	move.b	d0,(a5)+
__FirstRleByte:
	move.b	d0,(a5)+
	dbra		d3,__RleLoop
 endc

 ifnd UNROLL_RLE
__RleLoop:
	move.b	d0,(a5)+
	dbra		d3,__RleLoop
 endc

	jmp		(a4)
;---------------------------------------------------------
;  lzpos = __Get8Bit() ^ 0xff;
;---------------------------------------------------------
__NextBitClear_DoZipAfterAll:
 CORRECT_IN_MASK
	move.b	d6,-(a7)		;current byte
	move.w	(a7)+,d1
	move.b	(a6)+,d6		;next byte
	move.b	d6,d1
	lsr.w		d7,d1
	moveq		#-1,d3
	move.b	d1,d3
	lea		0(a5,d3.l),a0
	move.b	(a0)+,(a5)+
	move.b	(a0)+,(a5)+		;'lzlen' is 1, means 2 byte copy.
	jmp		(a4)			;continue
;-------------------------------------------------------------
;  __GetValue returns a value in d0.l
;  __GetBits takes as its input d1.b, returns a value in d0.l
;
; The outputs can be manipulated as:
;	unsigned character,
;	unsigned shorts,
;	unsigned longs
;
; They may update
;	d6, d7, a6
;
; They may not destroy 
;	d3-d5/a0-a5
;
; They expect a2 to be a table that tells the number of the
;	highest bit that is set
;
;--------------------------------------------------------------
__GetValue:					;This function has a goal of counting till it finds
	moveq		#0,d0
	move.w	d7,d1
	bset.w	d7,d0
	subq.w	#1,d0			;made the mask
	and.b		d0,d6			;mask out bits of interest, clear bits that we have already
						;  passed by
	eor.b		d6,d0			;invert the bits of interest
	bne.s		__BitsSetInThisByte
	move.b	(a6),d0
	not.b		d0
	addq.w	#8,d1
__BitsSetInThisByte:
	sub.b		0(a2,d0.w),d1
	cmp.b		#8,d1
	bcs.s		__LessThan7
	moveq		#8,d1
	addq.w	#1,d7
__LessThan7:
	sub.w		d1,d7
	bhi.s		__SameByte
	addq.w	#8,d7
	move.b	(a6)+,d6
__SameByte:
	moveq		#0,d0
	subq.b	#1,d1
	beq.s       __inl_exit4
	bset.w	d7,d0
	subq.w	#1,d0
	and.b		d6,d0
	sub.w		d1,d7
	bhi.s		__StillInSameByte4
	move.b	(a6)+,d6
	move.b	d0,-(a7)
	move.w	(a7)+,d0
	move.b	d6,d0
	addq.w	#8,d7
__StillInSameByte4:
	lsr.w		d7,d0
__inl_exit4:
	bset.b	d1,d0
	rts
;-----------------------------------------------------------------
;  newesc = __GetBits(escbits);
;  *outbuffer++ = (startesc<<escbits8) | __GetBits(escbits8);
;  startesc = newesc;
;  continue;
;-----------------------------------------------------------------
__NextBitIsClear_EscapeFromEscape:
 CORRECT_IN_MASK
	move.b	d6,-(a7)
	move.w	(a7)+,d0
	move.b	(a6)+,d0
	move.b	d0,d6
	lsr.w		d7,d0			;get8bits done
	move.b	d5,d1			;save old 'startesc'
	move.b	d2,d5
	and.b		d0,d5			;set the new start esc
	not.b		d2
	and.b		d2,d0			;isolate the lower bits
	not.b		d2
	or.b		d1,d0			;recombine
	move.b	d0,(a5)+
	jmp		(a4)


 ifnd CALL_DIRECTLY
	dc.b		"UNPACK_1"
 endc