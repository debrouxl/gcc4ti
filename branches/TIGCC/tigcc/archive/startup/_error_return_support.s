	.xdef __special_error_return_support_AND___complex_main,__error_return_support,__error_return_support_cleanup

| This file requires cleanup code.
	.xdef __ref_all___complex_main
| This object file needs the jump table in %a5.
	.xdef __ref_all___optimize_rom_calls

.section _st140
__error_return_support:
| Get 60 bytes on the stack (size of an error frame).
	lea (%sp,-60),%sp
| Push a pointer to these 60 bytes.
	pea.l (%sp)
| Call ER_catch
	move.l (%a5,0x154*4),%a0 /* ER_catch */
	jsr (%a0)
| Now, this is a little complicated:
| If an error is thrown later, a second return from ER_catch with an error
| in %d0 is simulated.
| So, if %d0 contains 0, then we resume normally, otherwise we go to the
| cleanup block, but remember the error.
| Store the error in a variable.
	lea.l __error_num__(%pc),%a0
	move.w %d0,(%a0)
	bne __error_returned__

.section _st1020
__error_return_support_cleanup:
| We have terminated successfully, so remove the error frame.
	move.l (%a5,0x155*4),%a0 /* ER_success */
	jsr (%a0)
| This is the place where we should go if an error happened.
| %d0 is not 0 in this case.
__error_returned__:
| Remove the error frame and the parameter from the stack.
	lea (%sp,64),%sp

.section _st1500
| If we had any cleaning up to do, it absolutely MUST be finished at this
| point. That is, we have to be able to return from here under any given
| circumstances.
| Go to __no_error__ if no error happened.
	lea.l __error_num__(%pc),%a0
	tst.w (%a0)
	beq.s __no_error__
| Construct an A-Line value.
	or.w #0xA000,(%a0)

| AMS 1.xx error return support is inserted here.

.section _st1502, "x"
| This is where the A-Line error throwing is constructed.
__error_num__:
	.word 0
__no_error__:
	clr.w (%a0)
