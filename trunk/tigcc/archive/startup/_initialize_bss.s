	.xdef __initialize_bss_AND___startup_code

.section _st151
__initialize_bss:
| Push the length of the BSS section (num parameter).
	pea.l __ld_bss_size
| Push 0 (c parameter).
	clr.w -(%sp)
| Push the address of the BSS section (buffer parameter).
	pea.l __ld_bss_start

| Code to load memset into %a0 is inserted here.

.section _st153
| Call memset.
	jsr (%a0)
| Clean up the stack.
	lea.l (%sp,10),%sp
