	.xdef __special_error_return_support_AND___complex_main_AND_NOT___special_error_return_support_ams_1

.section _st1501, "d" | Make sure the linker NEVER EVER tries to optimize those
                      | NOPs away.
| In AMS 2.00 or higher, we don't need to unlock our own handle, so we need to
| add NOP padding, or there will be a pipeline-related bug!
| DO NOT remove this if you don't understand this, we already had a regression
| once (TIGCC 0.94 SP4 -> 0.95 Beta 1) because those NOPs went amiss.
	nop
	nop
