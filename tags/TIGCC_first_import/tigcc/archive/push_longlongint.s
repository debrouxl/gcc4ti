	.xdef push_longlongint

.text
push_longlongint:
| Saving %d2 as well is a really dirty trick to reserve four extra bytes on the stack.
| Only two are needed, but this is smaller than subtracting and adding.
	movem.l %d2-%d7/%a5,-(%sp)
| Put address of push_quantum into %a5.
	move.l 0xC8,%a5
	move.l (%a5,3000),%a5  /* push_quantum */
| %d7: Number of bytes.
	clr.w %d7
| Put POSINT_TAG in %d6.
	moveq.l #31,%d6
| Move parameter to %d3/%d4.
	movem.l 32(%sp),%d3-%d4
| If negative, put NEGINT_TAG in %d6 and negate it.
| We only need to look at %d3 because of how numbers are stored.
	tst.l %d3
	jbge .L__mainloop
	moveq.l #32,%d6
	neg.l %d4
	negx.l %d3
.L__mainloop:
| Finished if remaining value is zero.
	move.l %d3,%d0
	or.l %d4,%d0
	jbeq .L__finished
| Push rightmost quantum of the value.
	move.w %d4,(%sp)
	jsr (%a5)
| Add 1 to the number of bytes.
	addq.w #1,%d7
| Shift the value to the right by one quantum.
	move.b %d3,%d4
	lsr.l #8,%d3
	ror.l #8,%d4
| Always repeat the loop at this point.
	jbra .L__mainloop
.L__finished:
| Push number of bytes.
	move.w %d7,(%sp)
	jsr (%a5)
| Push tag.
	move.w %d6,(%sp)
	jsr (%a5)
| See above.
| There is no problem in "restoring" %d2, since it may be trashed.
	movem.l (%sp)+,%d2-%d7/%a5
	rts
