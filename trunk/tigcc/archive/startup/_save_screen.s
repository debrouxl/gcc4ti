	.xdef __save_screen,__restore_screen

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st110
__save_screen:
| Save %a2 since we may not destroy it when we return to the AMS.
	pea.l (%a2)
| Get LCD_SIZE bytes on the stack.
	lea.l (%sp,-3840),%sp
| Push LCD_SIZE (size parameter).
	pea.l 3840
| Push LCD_MEM (source parameter).
	pea.l 0x4C00
| Push pointer to the space on the stack (dest parameter).
	pea.l (%sp,8)

| Code to load memcpy into %a2 is inserted here.

.section _st112
	jsr (%a2)

.section _st1050
__restore_screen:
| Push LCD_SIZE (size parameter).
	pea.l 3840
| Push pointer to saved data on stack (source parameter).
	pea.l (%sp,16)
| Push LCD_MEM (dest parameter).
	pea.l 0x4C00
| %a2 is still a pointer to memcpy since the function wasn't allowed to destroy it.
	jsr (%a2)
| Clean up the stack (data and parameters).
	lea.l (%sp,3864),%sp
| Restore %a2 for the AMS.
	movea.l (%sp)+,%a2
