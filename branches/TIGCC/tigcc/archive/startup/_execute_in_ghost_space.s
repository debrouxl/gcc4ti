	.xdef __execute_in_ghost_space

.section _st10
__execute_in_ghost_space:
| Detect HW3 first:

    |--------------------------------------------------------------------------
    | get the HW parm block using the algorithm suggested by Julien Muchembled
    |--------------------------------------------------------------------------
	move.l   0xc8:w,%d0
	andi.l   #0xE00000,%d0 | get the ROM base
	movea.l  %d0,%a0
	movea.l  260(%a0),%a1  | get pointer to the hardware param block
	adda.l   #0x10000,%a0
	cmpa.l   %a0,%a1       | check if the HW parameter block is near enough
	bcc.s    L.is_hw1or2   | if it is too far, it is HW1
	cmpi.w   #22,(%a1)     | check if the parameter block contains HW ver
	bls.s    L.is_hw1or2   | if it is too small, it is HW1

    |--------------------------------------------------------------------------
    | check for VTI (trick suggested by Julien Muchembled)
    |--------------------------------------------------------------------------
	trap     #12         | enter supervisor mode. returns old (%sr) in %d0.w
	move.w   #0x3000,%sr | set a non-existing flag in %sr (but keep s-flag)
	move.w   %sr,%d1     | get %sr content and check for non-existing flag
	move.w   %d0,%sr     | restore old %sr content
	btst.l   #12,%d1     | this non-existing flag can only be set on the VTI
	bne.s    L.is_hw1or2 | flag set -> VTI -> treat as HW1

    |--------------------------------------------------------------------------
    | check for HW3
    |--------------------------------------------------------------------------
	cmp.l    #3,22(%a1)  | check the hardware version
	bcs.s    L.is_hw1or2 | <3 -> proceed

    |--------------------------------------------------------------------------
    | now check for an appropriate ROM patch (HW3Patch?)
    |--------------------------------------------------------------------------
	move.l   0xc8:w,%a0
	move.l   0xc0*4(%a0),%a1 | abuse EX_stoBCD
	tst.w    92(%a1)         | check for HW3Patch (ROM resident)
	beq.w    __ghost_done    | if it is installed, we're done
	cmp.l    #0x100,0xac:w   | check for HW3Patch (RAM resident)
	beq.w    __ghost_done    | if it is installed, we're done

    |--------------------------------------------------------------------------
    | now we have a problem: we are on unpatched HW3 -> can't proceed
    |--------------------------------------------------------------------------
	pea.l __patch_required__(%pc)
	move.l (%a0,0xE6*4),%a0 /* ST_helpMsg */
	jsr (%a0)
	addq.l #4,%sp
	rts

L.is_hw1or2:

        lea __ld_entry_point(%pc),%a0
        move.l %a0,%d0
        cmpi.l #0x40000,%d0
        bcc __ghost_done
        bset.l #18,%d0
        clr.l %d1
        move.w (%a0,-2),%d1
        add.l %d0,%d1
        subq.l #1,%d1
        move.l %d1,-(%sp)
        move.l %d0,-(%sp)
        movea.l 0xC8,%a0
        movea.l (%a0,1384),%a0
        jsr (%a0)
        addq.l #8,%sp
        movea.l 0xC8,%a0
        cmpi.l #1000,(%a0,-4)
        bcc.s __ghost_install
        pea __ghost_done(%pc)
        bset.b #2,(%sp,1)
        rts
__ghost_install:
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
        pea __ghost_cont(%pc)
        bset.b #2,(%sp,1)
        clr.w  -(%sp)
        move.l 0xAC,%a0
        jmp (%a0)
__ghost_cont:
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

__ghost_done:


| Error message for unpatched HW3
.section _st10000, "d"

__patch_required__:
	.asciz "HW3Patch required"
