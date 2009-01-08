| Flash OS Update Header
| The format of this header is documented in TIBFormat.txt of PedroM.

	.xdef __flash_os_header

| Fixed TIB header.
.section _stl1, "d"
__tib_header:
	.word 0x800f                 | Flash OS header
	.long __ld_program_size+(__tib_header-.) | The encapsulated bytes which follow
	.word 0x8011                 | First Part of the Product ID
	.byte __ld_hardware_id       | CALC: $03 for TI-89 / $01 for TI-92+ / $08 for Voyage 200 / $09 for TI-89 Titanium
	.word 0x8021                 | Third part of Product ID
	.byte 0x03                   | Default 03. 06 for 2.02 / 1 for 1.01, etc
	.word 0x8032                 | Fourth part of Product ID
	.word 0x384F                 | 0038 / 004F (???)
	.word 0x80A1                 | Second part of Product ID
	.byte 0x02                   | Version identifier (HW1: >=0, HW2:>=1, V200 or HW3:>=2)
	.word 0x804D                 | Product Name
	.byte 0x1D                   | 29: Size of the next field, a non zero terminated string.
	.ascii "Advanced Mathematics Software" | This should be user-settable at some point...
	.word 0x0326                 | Product Code ?
	.word 0x0904                 | Date Stamp.
        .long __ld_link_time_timestamp
	.word 0x020D                 | Signature follows
	.byte 0x40                   | Encrypted TI Date Stamp Signature
	.space 64                    | Dummy signature for First Encryption - 64 NULL Chars
	.word 0x807F                 | Actual Flash OS code
	.long __ld_program_size+(__tib_header-.) | Size
	.long 0xCCCCCCCC             | Start of OS code (End of Header)
	.even
 __tib_header_end:
