	.xdef __fill_calculator_var

	.xdef __ref_all___detect_calc

.section _st31
__fill_calculator_var:
| Fill __calculator now, since __calc_is_OK__ may be located at different places.
	lea __calculator(%pc),%a1
	move.w %d0,(%a1)
