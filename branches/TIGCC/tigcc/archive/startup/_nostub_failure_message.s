	.xdef __display_message_and_exit_AND___nostub,__display_message_and_exit__

| This object file needs the jump table in %a0.
	.xdef __ref_all___get_jump_table

.section _st50
__display_message_and_exit__:
	move.l (%a0,0xE6*4),%a0 /* ST_helpMsg */
	jsr (%a0)
	addq.l #4,%sp
	rts
