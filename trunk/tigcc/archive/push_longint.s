	.xdef push_longint

.text
push_longint:
| Saving %d2 as well is a really dirty trick to reserve four extra bytes on the stack.
| Only two are needed, but this is smaller than subtracting and adding.
	movem.l %d2-%d6/%a5,-(%sp)
| Put address of push_quantum into %a5.
	move.l 0xC8,%a5
	move.l (%a5,3000),%a5  /* push_quantum */
| %d4: Number of bytes.
	clr.w %d4
| Put POSINT_TAG in %d6.
	moveq.l #31,%d6
| Move parameter to %d3.
	move.l 28(%sp),%d3
| If it is zero, the value should be zero bytes long.
	jbeq .L__finished
| If negative, put NEGINT_TAG in %d6 and negate it.
	jbge .L__mainloop
	moveq.l #32,%d6
	neg.l %d3
.L__mainloop:
| Push rightmost quantum of the value.
	move.w %d3,(%sp)
	jsr (%a5)
| Add 1 to the number of bytes.
	addq.w #1,%d4
| Shift the value to the right by one quantum.
	lsr.l #8,%d3
| Finished if remaining value is zero.
	jbne .L__mainloop
.L__finished:
| Push number of bytes.
	move.w %d4,(%sp)
	jsr (%a5)
| Push tag.
	move.w %d6,(%sp)
	jsr (%a5)
| See above.
| There is no problem in "restoring" %d2, since it may be trashed.
	movem.l (%sp)+,%d2-%d6/%a5
	rts
