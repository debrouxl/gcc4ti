	.xdef __enter_ghost_space

.section _st10502
	bsr.w .L__ghost_space_0
.L__ghost_space_0:
	move.l (%sp)+,%d0
	cmpi.l #0x40000,%d0
	bcc .L__ghost_space_2
	movea.l 0xC8,%a0
	cmpi.l #1000,(%a0,-4)
	bcs .L__ghost_space_2
	movem.l %a2-%a6/%d3-%d7,-(%sp)
	lea (%sp,-20),%sp
	move.l #0x3E000,%a3
	move.l %a0,%d0
	andi.l #0xE00000,%d0
	addi.l #0x20000,%d0
	move.l %d0,(%sp,12)
	move.l %d0,(%sp,16)
	trap #0xC
	move.w #0x2700,%sr
	move.l #0xF,%d3
	pea .L__ghost_space_1(%pc)
	bset.b #2,(%sp,1)
	clr.w  -(%sp)
	move.l 0xAC,%a0
	jmp (%a0)
.L__ghost_space_1:
	lea (%sp,20),%sp
	movem.l (%sp)+,%a2-%a6/%d3-%d7

| The following code was compiled from:
|
|    /* If we are on AMS 2, we have to set the "last executed program" to somewhere
|       in the last 4 KB of RAM, or else APD may crash under certain circumstances.
|       The code below looks for the "last executed program" variable. That variable
|       is cleared during initialization, immediately after the stack fence is set up.
|       So we look for the value of the stack fence (0xDEADDEAD) in the initialization
|       code and add 8 to get the wanted short pointer, which must then be
|       sign-extended to an actual pointer. (The sign extension is implicit in the
|       generated code, as it should be.) */
|    if (!(AMS_1xx||*(short*)0x32==('R'<<8)+'O')) {
|      /* rb and q are factored out in order to get more efficient code. */
|      char *rb=ROM_base;
|      char *p=rb+0x12000;
|      char *q=rb+0x18000;
|
|      while (p<q && *(unsigned long*)p!=0xDEADDEAD) p+=2;
|      p+=2[(short *)p]?8:12;
|      *(void **)(long)*(short *)p=(void*)0x3f000;
|    }

	move.l 0xC8,%d0
	move.l %d0,%a0
	cmp.l #999,-4(%a0)
	jbls .L1
	cmp.w #0x524F,0x32
	jbeq .L1
	and.l #0xE00000,%d0
	move.l %d0,%a0
	add.l #0x12000,%a0
	add.l #0x18000,%d0
	jbra .L3
	.even
.L7:
	addq.l #2,%a0
.L3:
	cmp.l %a0,%d0
	jbls .L4
	cmp.l #0xDEADDEAD,(%a0)
	jbne .L7
.L4:
	tst.w 4(%a0)
	bne.s 0f
	addq.l #4,%a0
0:
	move.w 8(%a0),%a0
	move.l #0x3F000,(%a0)
.L1:

| End compiled code.

.L__ghost_space_2:
	move.l (%sp)+,%d0
	bset.l #18,%d0
	movea.l %d0,%a0
	jmp (%a0)
