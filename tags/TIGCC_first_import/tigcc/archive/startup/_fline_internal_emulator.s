	.xdef __fline_internal_emulator,__install_fline_emulator,__uninstall_fline_emulator,__fline_handler,__call_default_fline_handler,__current_fline_instruction__

| This file requires cleanup code.
	.xdef __ref_all___complex_main

.section _st130
__install_fline_emulator:
	lea.l __ams_fline_vec__(%pc),%a0
	move.l 0x2C,(%a0)
	lea.l __fline_handler(%pc),%a0
	bclr.b #2,0x600001
	move.l %a0,0x2C:w
	bset.b #2,0x600001

.section _st1030
__uninstall_fline_emulator:
	bclr.b #2,0x600001
	move.l __ams_fline_vec__(%pc),0x2C:w
	bset.b #2,0x600001

.section _st10030
__fline_handler:
| Save %a1, in case we may not destroy it.
	pea.l (%a1)
| Load the position of the F-Line instruction into %a1.
	move.l 6(%sp),%a1

| Code to check against lower and upper bounds is inserted here.
| If it fails, it jumps to __call_default_fline_handler.

| Code to handle the instruction is inserted here.

.section _st10039
__call_default_fline_handler:
| Restore %a1.
	move.l (%sp)+,%a1
| Opcode of a jmp instruction.
	.word 0x4EF9
| This is where the original F-Line handler is stored.
__ams_fline_vec__:
	.long 0
