	.xdef kbd_queue

.text
kbd_queue:
	moveq.l #6,%d0
	trap #9
	rts
