	.xdef EV_getAppID

.text
EV_getAppID:
	movem.l %d3/%a2-%a3,-(%sp)
	lea .L__appid_tb(%pc),%a2
	moveq.l #9,%d3
	move.l 0xC8,%a3
	cmp.l #1000,-4(%a3)
	jbls .L__appid_lp
	move.l %a0,-(%sp)
	move.l 4432(%a3),%a0 /* TIOS_EV_getAppID */
	jsr (%a0)
	addq.l #4,%sp
	tst.w %d0
	jbne .L__appid_end
	moveq.l #-1,%d0
	jbra .L__appid_end
.L__appid_lp:
	move.l %a2,-(%sp)
	move.l %a0,-(%sp)
	move.l 2500(%a3),%a0 /* strcmp */
	jsr (%a0)
	addq.l #8,%sp
	tst.w %d0
	jbeq .L__appid_fnd
.L__appid_skp:
	tst.b (%a2)+
	dbne %d3,.L__appid_lp
	jbne .L__appid_skp
.L__appid_fnd:
	move.w %d3,%d0
	move.l %a3,%d1
	and.l #0xE00000,%d1
	cmp.l #0x400000,%d1
	jbne .L__appid_end
	cmp.w #6,%d0
	jble .L__appid_end
	addq.w #1,%d0
.L__appid_end:
	movem.l (%sp)+,%d3/%a2-%a3
	rts
	.even
.L__appid_tb:
	.asciz "TISLFTST"
	.asciz "TIINSLVR"
	.asciz "TITEXTED"
	.asciz "TIPRGMED"
	.asciz "TIDMED"
	.asciz "TITABLED"
	.asciz "TIGRAPH"
	.asciz "TIWINDED"
	.asciz "TIEQUED"
	.asciz "TIHOME"
