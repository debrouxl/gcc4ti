| Kernel Program/Library BSS Table
| This belongs to the kernel headers.

	.xdef __kernel_bss_table,__ld_insert_kernel_bss_refs
.section _stl13, "d"

__kernel_bss_table:
__kernel_bss_size_pos:
	.long __ld_bss_size
__ld_insert_kernel_bss_refs:
| The format for the BSS table is as follows:
| For each reloc...
|   2 bytes: offset into the program
| 2 bytes: 0
