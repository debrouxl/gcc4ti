	.xdef __reject_unofficial_os

.section _st38
| Check PedroM.
| The kernel identifier (0x32) must match "RO".
	cmp.w #0x524F,0x32
