| Warning: This routine has the attribute __ATTR_TIOS_CALLBACK__!

	.xdef fputchar

.text
fputchar:
	link.w %a6,#-20
	movem.l %d3-%d7/%a2-%a5,-(%sp)
	clr.w %d6
	move.b (%a6,9),%d6
	move.l 0xC8,%a5
	move.l (%a5,1592),%a0 /* FontGetSys */
	jsr (%a0)
	ext.w %d0 /* Yes, I know we technically shouldn't sign-extend an
	             unsigned char, but it is always positive anyway. */
	move.w %d0,%d4
	lsl.w #1,%d4
	addq.w #6,%d4
	move.l (%a5,188),%a3 /* ScrRect */
	pea (%a6,-18)
	move.l (%a5,1664),%a0 /* SaveScrState */
	jsr (%a0)
	move.w (%a6,-8),%d5
	move.w (%a6,-6),%d3
	move.w %d6,-(%sp)
	move.l (%a5,1600),%a0 /* FontCharWidth */
	jsr (%a0)
	move.w %d0,%d7
	cmp.b #10,%d6
	jbeq .L__fputchar_1
	move.w %d5,%d0
	add.w %d7,%d0
	clr.w %d1
	move.b (%a3,2),%d1
	cmp.w %d0,%d1
	jbge .L__fputchar_2
.L__fputchar_1:
	clr.w %d5
	add.w %d4,%d3
.L__fputchar_2:
	move.w %d3,%d0
	add.w %d4,%d0
	clr.w %d1
	move.b (%a3,3),%d1
	cmp.w %d0,%d1
	jbge .L__fputchar_3
	clr.w (%sp)
	move.w %d4,-(%sp)
	move.l %a3,-(%sp)
	move.l %a3,-(%sp)
	move.l (%a5,1580),%a0 /* ScrRectScroll */
	jsr (%a0)
	sub.w %d4,%d3
.L__fputchar_3:
	move.w #4,(%sp)
	move.w %d6,-(%sp)
	move.w %d3,-(%sp)
	move.w %d5,-(%sp)
	cmp.b #10,%d6
	jbeq .L__fputchar_4
	move.l (%a5,1680),%a0 /* DrawChar */
	jsr (%a0)
	add.w %d7,(%sp)
.L__fputchar_4:
	move.l (%a5,1652),%a0 /* MoveTo */
	jsr (%a0)
	move.w %d6,%d0
	movm.l -56(%a6),%d3-%d7/%a2-%a5
	unlk %a6
	rts
