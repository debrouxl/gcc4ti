| Kernel Library Stub
| This belongs to the kernel headers.

	.xdef __kernel_library_stub,__not_executable_msg
.section _stl12

| This is the kernel library stub starting point.
__kernel_library_stub:
| This does two things:
| 1. Put the address of __not_executable_msg on the stack.
| 2. Jump to .L__disp_msg.
| Note that it will not return normally, but remove even four more bytes from
| the stack.
	jbsr .L__disp_msg
__not_executable_msg:
	.asciz "Libraries are not executable"
	.even

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
