	.file	"fsetbufsize.c"
#NO_APP
	.text
tigcc_compiled.:
#APP
	.set _A_LINE,0xA000
#NO_APP
	.text
	.even
	.globl	fsetbufsize
fsetbufsize:
	tst.w %d0
	jbeq .L1
	cmp.w #0,%a0
	jbeq .L1
	move.w %d0,18(%a0)
.L1:
	rts
	nop
