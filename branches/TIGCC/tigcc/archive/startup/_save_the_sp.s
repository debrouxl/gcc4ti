	.xdef __save_the_sp

.section _st995
__save_the_sp:
	lea.l __save__sp__(%pc),%a0
	move.l %sp,(%a0)
