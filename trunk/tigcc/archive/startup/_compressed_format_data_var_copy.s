	.xdef __handle_data_var_AND___compressed_format_data_var_AND___data_var_create_copy,__compressed_format_data_var_copy

.section _st97
__compressed_format_data_var_copy:
| Put the data size on the stack for HeapAllocPtr.
	move.l #__ld_data_size,(%sp)
| Call HeapAllocPtr.
	move.l (%a5,0xA2*4),%a0 /* HeapAllocPtr */
	jsr (%a0)
| Put the return address for the "out of memory"
| message on the stack.
	pea.l __compressed_format_data_var_cleanup_almost_end(%pc)
| Put the result of HeapDeref into %d3.
	move.l %a0,%d3
| If the result is 0, give an "out of memory" message.
	beq __out_of_memory
	addq.l #4,%sp
| Put the file handle on the stack for HeapDeref.
	move.w %d4,(%sp)
| Call HeapDeref.
	move.l (%a5,0x96*4),%a0 /* HeapDeref */
	jsr (%a0)
| Put the size on the stack for memcpy (len parameter).
	move.l #__ld_data_size,(%sp)
| Push the result on the stack for memcpy (src
| parameter). Add 2 to skip the size bytes.
	pea.l 2(%a0)
| Push the destination address on the stack for memcpy
| (dest parameter).
	move.l %d3,-(%sp)
| Call memcpy.
	move.l (%a5,0x26A*4),%a0 /* memcpy */
	jsr (%a0)
| Clean up the stack.
	addq.l #8,%sp
| Clear %d4 to meet the convention.
	moveq.l #0,%d4
| Go to the relocation routine.
	bra.s __compressed_format_data_var_relocate

.section _st1063
| Instead of unlocking the file handle, free the copy.
	move.l %d3,(%sp)
	move.l (%a5,0xA3*4),%a0 /* HeapFreePtr */
	jsr (%a0)
	bra.s __compressed_format_data_var_cleanup_almost_end
