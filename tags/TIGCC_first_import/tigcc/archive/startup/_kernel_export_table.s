| Kernel Program/Library Export Table
| This belongs to the kernel headers.

	.xdef __kernel_export_table,__ld_insert_kernel_exports
.section _stl14, "d"

__kernel_export_table:
__kernel_export_count_pos:
	.word __ld_export_count
__ld_insert_kernel_exports:
| The format for the export table is as follows:
| For each export...
|   2 bytes: offset into the program
| 2 bytes: 0
