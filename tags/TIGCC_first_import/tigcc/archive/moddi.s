|moddi3 routine copyright (C) 2002, Kevin Kofler
|WARNING: Division by 0 will be handled the same way as in the unsigned variant.
|         For my __umoddi3, this means:
|         A division by 0 will not cause an exception, but just crash in an
|         infinite loop!

	.xdef __moddi3

.text
__moddi3:
	tst.b 12(%a7)	
	bge.s .L__moddi3_denom_positive
	neg.l 16(%a7)
	negx.l 12(%a7)
.L__moddi3_denom_positive:
	tst.b 4(%a7)
	blt.s .L__moddi3_numer_negative
.L__moddi3_umoddi3:
	jbra __umoddi3

.L__moddi3_numer_negative:
	neg.l 8(%a7)
	negx.l 4(%a7)
	lea.l (.L__moddi3_return_jump+2,%PC),%a0
	move.l (%a7)+,(%a0)
	bsr.s .L__moddi3_umoddi3
	neg.l %d1
	negx.l %d0
.L__moddi3_return_jump:
	jmp.l 0:l
