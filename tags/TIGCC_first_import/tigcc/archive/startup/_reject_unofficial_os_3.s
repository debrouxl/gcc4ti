	.xdef __reject_unofficial_os_AND_NOT___MIN_AMS_required

.section _st39
	bne.s __ams_version_is_OK__
	bra.s __ams_version_not_OK__

.section _st10012, "d"
	.asciz "required"
