	.xdef __test_for_specific_calc,__test_against_specific_calc,__wrong_calc__

| This object file needs the failure message function.
	.xdef __ref_all___display_message_and_exit

.section _st34
| A wrong calculator model is used. Display a failure message and exit.
	pea.l __wrong_calc__(%pc)
	bra.s __display_message_and_exit__

.section _st10000, "d"

__wrong_calc__:
	.asciz "Wrong calculator model"
