| Workaround for HomePushEStack which does not redraw the Home screen
| Copyright (C) Samuel Stearley, 2002

	.xdef HomeStore

.text
HomeStore:
	move.l %a3,-(%sp)
	move.l 0xC8,%a1
	move.l 0x2A3*4(%a1),%a3 /* EV_hook */
	move.l (%a3),-(%sp)
	lea .L__NewHook,%a0
	move.l %a0,(%a3)
	move.l 0x10F*4(%a1),%a0 /* HomePushEStack */
	jsr (%a0)
	move.l (%sp)+,(%a3)
	move.l (%sp)+,%a3
	rts
.L__NewHook:
	move.l 4(%sp),%a0
	move.w #0x700,(%a0)
	rts
