	.xdef __detect_calc
	
| This object file needs the jump table in %a0.
	.xdef __ref_all___get_jump_table

.section _st30
__detect_calc:
| At first, assume TI-92 Plus.
| It takes less space to write the values into %d0 first and then check whether they are correct.
	moveq #1,%d0
| Move the address of the jump table to %d1.
	move.l %a0,%d1
	and.l #0x400000,%d1
| Now %d1 is 0 on a TI-89, TI-89 Titanium or V200, or 0x400000 on a TI-92+.
	jbne __calc_in_d0__
| Otherwise, assume TI-89 / TI-89 Titanium.
	clr.w %d0
| Get the address of the ScrRect variable into a1.
	move.l (%a0,0x2F*4),%a1 /* ScrRect */
| If ScrRect+2 (x2) is less than 200, it is a TI-89 or TI-89 Titanium.
	cmp.b #200,2(%a1)
	jbcs __calc_in_d0__
| Otherwise, it is a V200.
	moveq #3,%d0
__calc_in_d0__:
