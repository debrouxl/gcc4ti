	.xdef __MIN_AMS_1_05_AND_NOT___kernel_library_header

.section _st61
	bne.s __ams_version_1_05_OK__ /* AMS >1.05 */
	cmpa.l #0x278eac,%a0 /* TI-89 AMS 1.05 */
	beq.s __ams_version_1_05_OK__
	cmpa.l #0x478280,%a0 /* TI-92+ AMS 1.05 */
	bne.s __ams_version_not_OK__
__ams_version_1_05_OK__:
