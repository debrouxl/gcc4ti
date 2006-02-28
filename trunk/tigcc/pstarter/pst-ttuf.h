| TIGCC Program Starter - ttunpack decompression support (fast & large version)
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
pea.l (%a0) | destination
pea.l (%a3) | source
move.l %a0,%a4 | satisfy output constraint
jbsr ttunpack_decompress
addq.l #8,%a7
tst.w %d0
jbne invalid_archive
.endm

.text
ttunpack_decompress:
	.word	18663
	.word	7998
	.word	11375
	.word	44
	.word	10863
	.word	48
	.word	18414
	.word	15
	.word	31232
	.word	6702
	.word	6
	.word	32264
	.word	28676
	.word	3118
	.word	84
	.word	2
	.word	26236
	.word	3118
	.word	80
	.word	3
	.word	26228
	.word	28678
	.word	3118
	.word	8
	.word	10
	.word	26218
	.word	3118
	.word	128
	.word	11
	.word	26210
	.word	28679
	.word	29184
	.word	4654
	.word	9
	.word	10241
	.word	20801
	.word	25172
	.word	28677
	.word	29184
	.word	4654
	.word	12
	.word	8769
	.word	23361
	.word	28230
	.word	29696
	.word	2498
	.word	21314
	.word	-6086
	.word	-6083
	.word	28672
	.word	4115
	.word	19958
	.word	16
	.word	18938
	.word	60
	.word	18948
	.word	26116
	.word	18938
	.word	82
	.word	20463
	.word	-260
	.word	9295
	.word	16922
	.word	28672
	.word	29191
	.word	30208
	.word	451
	.word	5312
	.word	21315
	.word	26362
	.word	21056
	.word	20937
	.word	-14
	.word	9295
	.word	7198
	.word	20180
	.word	28672
	.word	20463
	.word	260
	.word	17408
	.word	19679
	.word	31992
	.word	20085
	.word	6848
	.word	7942
	.word	12319
	.word	7198
	.word	4102
	.word	-4504
	.word	4610
	.word	-15872
	.word	-19963
	.word	26348
	.word	21902
	.word	7198
	.word	-25020
	.word	25092
	.word	7198
	.word	20551
	.word	24832
	.word	272
	.word	13824
	.word	21312
	.word	26488
	.word	24832
	.word	262
	.word	20992
	.word	26156
	.word	22851
	.word	26044
	.word	22595
	.word	4614
	.word	18497
	.word	7966
	.word	12831
	.word	4638
	.word	7169
	.word	-4439
	.word	28927
	.word	4097
	.word	16129
	.word	4639
	.word	16885
	.word	0
	.word	4120
	.word	-12287
	.word	6848
	.word	20939
	.word	-8
	.word	20180
	.word	21760
	.word	12809
	.word	26396
	.word	-7320
	.word	29184
	.word	4033
	.word	21313
	.word	-15866
	.word	-25015
	.word	25098
	.word	7198
	.word	7937
	.word	12831
	.word	4614
	.word	20551
	.word	-4503
	.word	-32703
	.word	29184
	.word	7936
	.word	12831
	.word	7942
	.word	12319
	.word	7198
	.word	4102
	.word	-4504
	.word	4608
	.word	17921
	.word	18049
	.word	16885
	.word	6144
	.word	6872
	.word	20939
	.word	-4
	.word	20180
	.word	21255
	.word	3846
	.word	26474
	.word	18951
	.word	26116
	.word	32264
	.word	7198
	.word	21255
	.word	3846
	.word	26368
	.word	198
	.word	18951
	.word	26116
	.word	32264
	.word	7198
	.word	24944
	.word	5632
	.word	27164
	.word	-10749
	.word	21255
	.word	3846
	.word	22208
	.word	-27136
	.word	18951
	.word	26116
	.word	32264
	.word	7198
	.word	24920
	.word	21248
	.word	16131
	.word	7808
	.word	13855
	.word	24910
	.word	3072
	.word	32
	.word	25606
	.word	4147
	.word	0
	.word	24602
	.word	-6392
	.word	4614
	.word	22343
	.word	25098
	.word	7198
	.word	7937
	.word	12831
	.word	4614
	.word	20551
	.word	-4503
	.word	513
	.word	7
	.word	-32767
	.word	6848
	.word	20939
	.word	-4
	.word	20180
	.word	18951
	.word	26116
	.word	32264
	.word	7198
	.word	7942
	.word	12831
	.word	7198
	.word	4614
	.word	-4503
	.word	30463
	.word	5633
	.word	16885
	.word	14336
	.word	6872
	.word	6872
	.word	20180
	.word	28672
	.word	12807
	.word	4032
	.word	21312
	.word	-13312
	.word	-17152
	.word	26118
	.word	4118
	.word	17920
	.word	20545
	.word	-28110
	.word	0
	.word	3073
	.word	8
	.word	25860
	.word	29192
	.word	21063
	.word	-25023
	.word	25092
	.word	20551
	.word	7198
	.word	28672
	.word	21249
	.word	26390
	.word	4032
	.word	21312
	.word	-16378
	.word	-25023
	.word	25098
	.word	7198
	.word	7936
	.word	12319
	.word	4102
	.word	20551
	.word	-4504
	.word	960
	.word	20085
	.word	18951
	.word	26116
	.word	32264
	.word	7198
	.word	7942
	.word	12319
	.word	4126
	.word	7168
	.word	-4504
	.word	4613
	.word	6658
	.word	-13824
	.word	17922
	.word	-16382
	.word	17922
	.word	-32767
	.word	6848
	.word	20180
