	.xdef __MIN_AMS_required_AND_NOT___kernel_library_header

.section _st41
	bcc.s __ams_version_is_OK__
	bra.s __ams_version_not_OK__

.section _st10012, "d"
	.asciz " or higher needed"
