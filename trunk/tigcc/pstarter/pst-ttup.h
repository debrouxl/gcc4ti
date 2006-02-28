| TIGCC Program Starter - ttunpack decompression support
| Copyright (C) 2004-2005 Samuel Stearley, Kevin Kofler
|
| THE LICENSE:
|
|               wxWindows Library Licence, Version 3.1
|               ======================================
|
| Copyright (C) 1998-2005 Julian Smart, Robert Roebling et al
|
| Everyone is permitted to copy and distribute verbatim copies
| of this licence document, but changing it is not allowed.
|
|                      WXWINDOWS LIBRARY LICENCE
|    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
|
| This library is free software; you can redistribute it and/or modify it
| under the terms of the GNU Library General Public Licence as published by
| the Free Software Foundation; either version 2 of the Licence, or (at
| your option) any later version.
|
| This library is distributed in the hope that it will be useful, but
| WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
| General Public Licence for more details.
|
| You should have received a copy of the GNU Library General Public Licence
| along with this software, usually in a file named COPYING.LIB.  If not,
| write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
| Boston, MA 02111-1307 USA.
|
| EXCEPTION NOTICE
|
| 1. As a special exception, the copyright holders of this library give
| permission for additional uses of the text contained in this release of
| the library as licenced under the wxWindows Library Licence, applying
| either version 3.1 of the Licence, or (at your option) any later version of
| the Licence as published by the copyright holders of version
| 3.1 of the Licence document.
|
| 2. The exception is that you may use, copy, link, modify and distribute
| under your own terms, binary object code versions of works based
| on the Library.
|
| 3. If you copy code from files distributed under the terms of the GNU
| General Public Licence or the GNU Library General Public Licence into a
| copy of this library, as this licence permits, the exception does not
| apply to the code that you add in this way.  To avoid misleading anyone as
| to the status of such modified files, you must delete this exception
| notice from such code and/or adjust the licensing conditions notice
| accordingly.
|
| 4. If you write modifications of your own for this library, it is your
| choice whether to permit this exception to apply to your modifications. 
| If you do not wish that, you must delete the exception notice from such
| code and/or adjust the licensing conditions notice accordingly.

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
| Check if it has type "ppg".
cmp.b #0xf8,(%a0)
jbne invalid_archive
tst.b -(%a0)
jbne invalid_archive
cmp.b #'g',-(%a0)
jbne invalid_archive
moveq.l #'p',%d0
cmp.b -(%a0),%d0
jbne invalid_archive
cmp.b -(%a0),%d0
jbne invalid_archive
tst.b -(%a0)
jbeq valid_archive
| We don't need to check the magic number in the ttunpack header,
| the decompression routine does that already.
invalid_archive:
.word 0xA000+210 | ER_DATATYPE
valid_archive:
| Get the uncompressed size.
move.w (%a3),%d6
rol.w #8,%d6
.endm

| MEM_TO_MEM_DECOMPRESS inline function
| INPUT: %a3.l: pointer to compressed data
|        %a0.l: pointer to buffer for uncompressed data
|        %d6.l: uncompressed size
| OUTPUT: %a4.l: pointer to uncompressed data
| SIDE EFFECTS: may throw a Memory or Data Type error
| DESTROYS: %d0-%d2/%a0-%a2
.macro MEM_TO_MEM_DECOMPRESS
move.l %a0,%a4 | satisfy output constraint
jbsr ttunpack_decompress
tst.w %d0
jbne invalid_archive
.endm

.text
ttunpack_decompress:
	.word	18663
	.word	7998
	.word	6699
	.word	6
	.word	32384
	.word	28680
	.word	21643
	.word	3163
	.word	21584
	.word	26140
	.word	23179
	.word	6171
	.word	-18432
	.word	25108
	.word	3163
	.word	2176
	.word	26126
	.word	-12245
	.word	3
	.word	17395
	.word	2300
	.word	3091
	.word	5
	.word	25874
	.word	19679
	.word	31992
	.word	20085
	.word	24886
	.word	-16059
	.word	29192
	.word	-28156
	.word	24894
	.word	4288
	.word	24874
	.word	-20475
	.word	26354
	.word	24892
	.word	13824
	.word	21312
	.word	26438
	.word	24884
	.word	20992
	.word	26586
	.word	21760
	.word	4627
	.word	24866
	.word	29191
	.word	24852
	.word	17920
	.word	17536
	.word	4336
	.word	2303
	.word	20939
	.word	-6
	.word	24788
	.word	4612
	.word	28672
	.word	24586
	.word	-7393
	.word	25602
	.word	7193
	.word	-9210
	.word	-11968
	.word	21249
	.word	27378
	.word	17600
	.word	20085
	.word	29702
	.word	25066
	.word	21706
	.word	-4
	.word	29190
	.word	-28094
	.word	28673
	.word	24808
	.word	25052
	.word	25794
	.word	25048
	.word	25752
	.word	25062
	.word	5632
	.word	27148
	.word	25038
	.word	16128
	.word	25052
	.word	21248
	.word	7808
	.word	13855
	.word	25044
	.word	21762
	.word	27398
	.word	4147
	.word	3
	.word	24580
	.word	29186
	.word	25012
	.word	4288
	.word	28672
	.word	24738
