	.xdef __MIN_AMS_2_03_AND_NOT___kernel_library_header

.section _st61
	bne.s __ams_version_2_03_OK__ /* AMS >2.03 */
	movea.l (%a0,0x440*4),%a1 /* ReleaseVersion (AMS 2 only ROM_CALL) */
	cmpi.b #'2',3(%a1) /* AMS 2.02 */
	beq.s __ams_version_not_OK__
__ams_version_2_03_OK__:
