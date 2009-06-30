	.xdef __save_screen,__restore_screen

| This file requires cleanup code.
	.xdef __ref_all___complex_main

| Screen save & restore used to be done by calling memcpy, even if doing so
| yields an executable 16 bytes larger (though a very tiny fraction of a
| second faster). SAVE_SCREEN is present in most AMS native programs, so its
| implementation should be focused on a low footprint.

.section _st110
__save_screen:
| Execute the loop LCD_SIZE/4 times (4 bytes are transferred at a time).
	move.w #(3840/4)-1,%d0
| LCD_MEM.
	lea.l 0x4c00.w,%a0
| Push data on the stack.
0:
	move.l (%a0)+,-(%sp)
	dbf.w %d0,0b

.section _st1050
__restore_screen:
| Execute the loop LCD_SIZE/4 times.
	move.w #(3840/4)-1,%d0
| LCD_MEM + LCD_SIZE.
	lea.l 0x5b00.w,%a0
| Pop data from the stack.
0:
	move.l (%sp)+,-(%a0)
	dbf.w %d0,0b
