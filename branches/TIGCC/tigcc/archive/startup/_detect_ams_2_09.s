	.xdef __MIN_AMS_2_09_AND_NOT___kernel_library_header

.section _st61
	bne.s __ams_version_2_09_OK__ /* AMS >2.09 */
	movea.l (%a0,0x440*4),%a1 /* ReleaseVersion (AMS 2 only ROM_CALL) */
	cmpi.b #'9',3(%a1) /* AMS 2.09 */
	bne.s __ams_version_not_OK__
__ams_version_2_09_OK__:
