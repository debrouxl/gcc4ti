| Kernel Program Stub
| This belongs to the kernel headers.

	.xdef __kernel_program_stub,__no_kernel_msg
.section _stl11

| This is the kernel program stub starting point.
__kernel_program_stub:
| Test the data at 0x30, which is nonzero if a kernel has been
| installed.
	tst.w 0x30
| No kernel: Display failure message.
	jbeq .L__disp_no_kernel_msg
| We have a kernel. Move the location of the kernel launcher
| (stored in memory location 0x34) to %a0.
	move.l 0x34,%a0
| Call the kernel launcher.
	jmp (%a0)

| This routine is not a normal function: It returns from the entire program.
.L__disp_msg:
| Get the address of the jump table in %a0.
	move.l 0xC8,%a0
| Get the address of ST_helpMsg in %a0.
	move.l (%a0,0xE6*4),%a0  /* ST_helpMsg */
| Call ST_helpMsg.
	jsr (%a0)
| Clean up the stack.
| Remove the last return address as well
| (from the bsr.w at the beginning of the program).
	addq.w #8,%sp
| Return from the entire program.
	rts

| This routine displays the "Kernel required" message in the status line.
.L__disp_no_kernel_msg:
| This does two things:
| 1. Put the address of __no_kernel_msg on the stack.
| 2. Jump to .L__disp_msg.
| Note that it will not return normally, but remove even four more bytes from
| the stack.
	jbsr .L__disp_msg
__no_kernel_msg:
	.asciz "Kernel required"
	.even
