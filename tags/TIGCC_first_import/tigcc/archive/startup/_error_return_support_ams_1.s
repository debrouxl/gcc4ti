	.xdef __special_error_return_support_ams_1,__error_return_support_ams_1

	.xdef __ref_all___error_return_support

.section _st1501
__error_return_support_ams_1:
| In AMS 1.xx, we need to unlock our own handle.
| Otherwise, the program cannot be started again.
| Since we are going to throw an error, we can destroy %a2.
	move.l 0xC8,%a2
	cmp.l #1000,(%a2,-4) /* TIOS_entries */
	jbcc __ams_1_err_handling_not_needed__
| Get the starting address of the program and push it on the stack.
	pea.l __ld_entry_point-2(%pc)
| Convert it to a handle.
	move.l (%a2,0x23A*4),%a0 /* HeapPtrToHandle */
	jsr (%a0)
| The handle is now in %d0. It is 0 on failure.
	move.w %d0,(%sp)
	jbeq __ams_1_err_handling_not_needed__
| Unlock it.
	move.l (%a2,0x9F*4),%a0 /* HeapUnlock */
	jsr (%a0)
| No need to clean up the stack, since we are going to throw an error anyway.
__ams_1_err_handling_not_needed__:
