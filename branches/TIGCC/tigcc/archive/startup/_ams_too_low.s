	.xdef __ams_version_not_OK__,__ams_too_low__

| This object file needs the failure message function.
	.xdef __ref_all___display_message_and_exit

.section _st42
| The AMS version was too low.
| So print an error message and exit.
__ams_version_not_OK__:
	pea.l __ams_too_low__(%pc)
	bra.s __display_message_and_exit__

.section _st10010, "d"
__ams_too_low__:
	.ascii "AMS "

| AMS number and rest of string is inserted here.
