	.file	"fsetbufsize.c"
#NO_APP
	.text
tigcc_compiled.:
	.text
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fsetbufsize
fsetbufsize:
	tst.w %d0
	jbeq .L5
	cmp.w #0,%a0
	jbeq .L5
	move.w %d0,18(%a0)
.L5:
	rts
