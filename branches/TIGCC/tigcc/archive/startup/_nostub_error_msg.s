	.xdef __error_msg___AND___nostub_AND_NOT___display_message_and_exit

.section _st10022
	move.l (%a5,0xE6*4),%a0 /* ST_helpMsg */
	jsr (%a0)
	addq.l #4,%sp
	rts
