|divdi3 routine copyright (C) 2002, Kevin Kofler
|WARNING: Division by 0 will be handled the same way as in the unsigned variant.
|         For my __udivdi3, this means:
|         A division by 0 will not cause an exception, but just crash in an
|         infinite loop!
|ANOTHER WARNING: This will ONLY work with an __udivdi3 which does NOT destroy a1!
|                 Mine works of course.

	.xdef __divdi3

.text
__divdi3:
	tst.b 4(%a7)
	blt.s .L__divdi3_numer_negative
	tst.b 12(%a7)	
	blt.s .L__divdi3_denom_negative
.L__divdi3_udivdi3:
	jbra __udivdi3

.L__divdi3_numer_negative:
	neg.l 8(%a7)
	negx.l 4(%a7)
	tst.b 12(%a7)	
	bge.s .L__divdi3_denom_positive
	neg.l 16(%a7)
	negx.l 12(%a7)
	bra.s .L__divdi3_udivdi3

.L__divdi3_denom_negative:
	neg.l 16(%a7)
	negx.l 12(%a7)
.L__divdi3_denom_positive:
	move.l (%a7)+,%a1
	bsr.s .L__divdi3_udivdi3
	neg.l %d1
	negx.l %d0
	jmp (%a1)
