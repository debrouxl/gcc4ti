	.xdef OSVRegisterTimer

.text
OSVRegisterTimer:
	subq.w #1,%d0
	cmpi.w #2,%d0
	bcc.s .L__timer_rfai
	muls.w #12,%d0
	move.l 0x74,%a0
	cmpi.l #132133782,(%a0,-4)
	beq.s .L__timer_rins
	lea .L__timer_told(%pc),%a1
	move.l %a0,(%a1)
	lea .L__timer_rti5(%pc),%a0
.L__timer_rins:
	lea -32(%a0,%d0.w),%a1
	cmpi.l #-1,(%a1)
	bne.s .L__timer_rfai
	move.l %d3,(%a1)+
	move.l %d3,(%a1)+
	move.l %d4,(%a1)
	bclr.b #2,0x600001
	move.l %a0,0x74:w
	bset.b #2,0x600001
	moveq #1,%d0
	rts
.L__timer_rfai:
	clr.w %d0
	rts
	.even
.L__timer_ttab:
	.long -1,0,0,-1,0,0
.L__timer_told:
	.long 0
	.long 132133782 /* magic */
	.even
.L__timer_rti5:
	move.w #0x2700,%sr
	movem.l %d0-%d7/%a0-%a6,-(%sp)
	lea .L__timer_ttab(%pc),%a4
	moveq #1,%d4
.L__timer_i5lp:
	cmpi.l #-1,(%a4)
	beq.s .L__timer_i5sk
	subq.l #1,(%a4,4)
	bne.s .L__timer_i5sk
	move.l (%a4),(%a4,4)
	move.l (%a4,8),%a0
	jsr (%a0)
.L__timer_i5sk:
	lea (%a4,12),%a4
	dbra %d4,.L__timer_i5lp
	movem.l (%sp)+,%d0-%d7/%a0-%a6
	move.l .L__timer_told(%pc),-(%sp)
	rts
