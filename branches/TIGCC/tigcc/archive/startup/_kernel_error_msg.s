	.xdef __error_msg___AND___kernel_AND_NOT___display_message_and_exit

| ngetchx seems to require the in-use bit.
	.xdef __ref_all___set_file_in_use_bit

.section _st10022
	move.l (%a5,0xE6*4),%a0 /* ST_helpMsg */
	jsr (%a0)
	addq.l #4,%sp
| We have to wait for a keypress because the screen is restored automatically.
	move.l (%a5,0x51*4),%a0 /* ngetchx */
	jmp (%a0)
