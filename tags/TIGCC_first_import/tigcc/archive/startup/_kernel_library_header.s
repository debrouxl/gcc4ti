| Kernel Library Header
| The format of this header is documented in ProgFormat.txt of the DoorsOS
| developer version.

	.xdef __kernel_library_header_AND_NOT___preos_headers,__kernel_entry_point,__kernel_fixed_header_end,__ld_insert_kernel_libs,__ld_insert_kernel_rom_calls,__ld_insert_kernel_ram_calls,__ld_insert_kernel_relocs,__bss_handle

| Fixed kernel program header.
.section _stl1, "d"
__kernel_library_header:
| This is the kernel program entry point.
__kernel_entry_point:
| This is not actually a real bsr: The library stub will clear the return
| address from the stack. This may also be a bra.w, but maybe some kernels
| rely on the bsr.w.
	bsr.w __kernel_library_stub
__kernel_library_signature:
	.ascii "68kL"
__kernel_reloc_count_pos:
	.word 0
__kernel_comment_pos:
	.word _comment-__kernel_entry_point
__kernel_main_pos:
	.word 0
__kernel_exit_pos:
	.word _exit-__kernel_entry_point
__kernel_flags_pos:
	.byte __ld_file_version
	.byte __ld_kernel_flags
__kernel_bss_handle_pos:
__bss_handle:
	.word 0
__kernel_bss_table_pos:
	.word __ld_kernel_bss_table-__kernel_entry_point
__kernel_export_table_pos:
	.word __ld_kernel_export_table-__kernel_entry_point
__kernel_extra_pos:
	.word _extraram-__kernel_entry_point
__kernel_fixed_header_end:

| Kernel library import table.
.section _stl2, "d"
__kernel_lib_table:
__kernel_lib_count_pos:
	.word __ld_referenced_lib_count
__ld_insert_kernel_libs:
| The format for the library table is as follows:
| For each library...
|   8 bytes: name
|   1 byte: 0
|   1 byte: minimum version
| For each library...
|   2 bytes: function import count - 1
|   For each imported function...
|     2 bytes: function index in the library
|     For each reloc...
|       2 bytes: offset into the program
|     2 bytes: 0

| ROM call import table.
.section _stl3, "d"
__kernel_rom_call_table:
__kernel_has_rom_calls_pos:
	.word __ld_has_rom_calls
__ld_insert_kernel_rom_calls:
| The format for the ROM call table is as follows:
| ROM call count - 1
| For each ROM call used...
|   2 bytes: ROM call number
|   For each reloc...
|     2 bytes: offset into the program
|   2 bytes: 0

| RAM call import table.
.section _stl4, "d"
__kernel_ram_call_table:
__kernel_has_ram_calls_pos:
	.word __ld_has_ram_calls
__ld_insert_kernel_ram_calls:
| The format for the RAM call table is as follows:
| RAM call count - 1
| For each RAM call used...
|   2 bytes: RAM call number
|     Bit 14: extra RAM address
|     Bit 15: 2-byte references (instead of 4)
|   For each reloc...
|     2 bytes: offset into the program
|   2 bytes: 0

| Relocation table.
.section _stl5, "d"
__kernel_reloc_table:
__ld_insert_kernel_relocs:
| The format for the reloc table is as follows:
| For each reloc...
|   2 bytes: offset into the program
| 2 bytes: 0
