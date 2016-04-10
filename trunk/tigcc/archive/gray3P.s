|******************************************************************************
|
| project name:    GrayScale-Support for TIGCC
| author:          thomas.nussbaumer@gmx.net
|                  Julien Muchembled (original implementation for UniversalOS)
|                  Kevin@tigcc.ticalc.org (3 plane version - 7/8 grayscales)
|
|
| compatible with HW1/HW2 on all AMS versions up to 2.05
|
|
| $Id: gray.s,v 3.10 2002/04/05 11:34:23 tnussb Exp $
|******************************************************************************

|------------------------------------------------------------------------------
| uncomment the following global to simulate HW2 on the VTI
| (this will not use port 70001D, therefore it flickers extremely; additionally
|  the complete HW detection is bypassed and always reports HW2)
|------------------------------------------------------------------------------
|.globl ALWAYS_HW2_TESTING


.data
	.xdef Gray3POn,Gray3POff,__gray3P_D_plane,__gray3P_M_plane,__gray3P_L_plane,__gray3P_handle,__gray3P_hw_type
	.xdef __gray3P_switch_cnt,__gray3P_old_int1_hw1,__gray3P_old_int1_hw2
	.xdef __gray3P_sync_n_count,__gray3P_plane_index
	.xdef __gray3P_dbl_offset,__gray3P_L_plane2,__gray3P_M_plane2,__gray3P_D_plane2

.even
|==============================================================================
| EXPORTED: Gray3POn function (turn grayscales on) - trashes d1/a0/a1
| Parameter: %d0:w = 0 -> 7 grays, non-0 -> 8 grays
|==============================================================================
Gray3POn:
	movem.l  %d2-%d7/%a2-%a6,-(%a7)
	tst.w    %d0
	beq.s    __gray3P_7grays
|Patches for 8 grays:
|__gray3P_phase_reset_hw1
|0x700c = moveq.l #12,%d0
|__gray3P_phase_indir_hw1
|0x3030 000c = move.l 12(%a0,%d0:w),%d0
|__gray3P_phase_reset_hw2
|0x10bc 000c = move.b #12,(%a0)
|__gray3P_phase_indir_hw2
|0x3230 100c = move.l 12(%a0,%d1:w),%d1
        move.w   #0x700c,__gray3P_phase_reset_hw1
        move.w   #0xc,__gray3P_phase_indir_hw1+2
        move.w   #0xc,__gray3P_phase_reset_hw2+2
        move.w   #0x100c,__gray3P_phase_indir_hw2+2
	bra __gray3P_8grays
__gray3P_7grays:
|Patches for 7 grays:
|__gray3P_phase_reset_hw1
|0x700a = moveq.l #10,%d0
|__gray3P_phase_indir_hw1
|0x3030 0000 = move.l 0(%a0,%d0:w),%d0
|__gray3P_phase_reset_hw2
|0x10bc 000a = move.b #10,(%a0)
|__gray3P_phase_indir_hw2
|0x3230 1000 = move.l 0(%a0,%d1:w),%d1
        move.w   #0x700a,__gray3P_phase_reset_hw1
        clr.w    __gray3P_phase_indir_hw1+2
        move.w   #0xa,__gray3P_phase_reset_hw2+2
        move.w   #0x1000,__gray3P_phase_indir_hw2+2
__gray3P_8grays:
	lea      (__gray3P_switch_cnt,%pc),%a0| reset plane switch counter to 0
	clr.l    (%a0)
	bsr.s    __gray3P_check_hw_version    | evaluate HW version and store it
	lea      (__gray3P_hw_type,%pc),%a0
	move.w   %d0,(%a0)
	bsr      __gray3P_init_mem            | allocate and initialize memory
	movem.l  (%a7)+,%d2-%d7/%a2-%a6
	move.w   (__gray3P_handle,%pc),%d0
	bne      __gray3P_init_handler        | jump to interrupt handler setup if
	                                      | memory was allocated correctly
	moveq    #0x0,%d0                     | otherwise return 0 (GrayOn failed)
	rts

|==============================================================================
| checks for HW version (VTI is treated as HW1, because port 0x70001D is not
|                        emulated by the VTI and this would cause NEVER switch
|                        planes behaviour if we would use the HW2 support)
|
| returns 0 in d0.w for HW1 and 1 for HW2
|
| IMPORTANT NOTE: This function patches 2 locations in the code of the
|                 grayscale support depending on the HW version. Patching the
|                 locations for both types IS necessary, because if a program
|                 is transfered from one calc to another one the default values
|                 got already overwritten (if program was not archived)
|==============================================================================
__gray3P_check_hw_version:
.ifdef ALWAYS_HW2_TESTING
	bra.s __gray3P_always_hw2_proceed
.endif
	move.l   0xc8,%d0
	and.l    #0xE00000,%d0            | get the ROM base
	move.l   %d0,%a0
	moveq    #0,%d0
	move.l   260(%a0),%a1             | get pointer to the hardware param block
	add.l    #0x10000,%a0
	cmp.l    %a0,%a1                  | check if the HW parameter block is near
	bcc.s    __gray3P_patches_for_hw1 | if it is too far, it is HW1
	cmp.w    #22,(%a1)                | check if the parameter block contains HW
	bls.s    __gray3P_patches_for_hw1 | if it is too small, it is HW1
	cmp.l    #1,22(%a1)               | check the hardware version
	beq.s    __gray3P_patches_for_hw1 | if not 1, it is HW2 (or an unknown HW)
    |--------------------------------------------------------------------------
    | check for VTI (trick suggested by Julien Muchembled)
    | optimized by Lionel Debroux
    |--------------------------------------------------------------------------
	trap   #12         | enter supervisor mode. returns old (%sr) in %d0.w
	move.w #0x3000,%sr | set a non-existing flag in %sr (but keep s-flag !!)
	move.w %d0,%d1     | save %d0.w content in %d1
	move.w %sr,%d0     | get %sr content and check for non-existing flag
	move.w %d1,%sr     | restore old %sr.
	lsl.w  #3,%d0
	bpl.s  __gray3P_hw2type_detected  | flag not set -> no VTI
    |--------------------------------------------------------------------------
    | HW1 detected
    |--------------------------------------------------------------------------
	moveq    #0,%d0
    |--------------------------------------------------------------------------
    | patches code for HW1 version
    |
    | necessary memory == 2 planes + 8 Bytes == 7688
    |
    | the additionally 8 bytes are necessary for rounding later to a multiple
    | of 8
    |--------------------------------------------------------------------------
__gray3P_patches_for_hw1:
	lea (__gray3P_size_to_allocate,%pc),%a0
	move.w #0x1e08,(%a0)
	clr.w (__gray3P_size_to_add-__gray3P_size_to_allocate,%a0)
	rts
    |--------------------------------------------------------------------------
    | HW2 detected
    |--------------------------------------------------------------------------
__gray3P_hw2type_detected:
.ifdef ALWAYS_HW2_TESTING
__gray3P_always_hw2_proceed:
.endif
	moveq    #1,%d0
    |--------------------------------------------------------------------------
    | patches code for HW2 version
    |
    | necessary memory == 3 planes + 8 Bytes == 11528
    |
    | the additionally 8 bytes are necessary for rounding later to a multiple
    | of 8
    |--------------------------------------------------------------------------
	lea      (__gray3P_size_to_allocate,%pc),%a0
	move.w   #0x2d08,(%a0)
	move.w   #0xf00,(__gray3P_size_to_add - __gray3P_size_to_allocate,%a0)
	rts
__gray3P_hw_type:    | stores HW type (0==HW1 or VTI  1==HW2)
	.word 0
|==============================================================================
| Interrupt 1 handler for HW1
|==============================================================================
__gray3P_int1_handler_hw1:
	movem.l  %d0/%a0,-(%a7)
    |--------------------------------------------------------------------------
    | Load skip counter and increment it (count = (count+1)&0x3). Skip any
    | further operation if count is 1, 2 or 3. This means that every 4th call
    | of the INT1 handler is a candidate for a plane switch
    |--------------------------------------------------------------------------
	lea      (__gray3P_skipcount,%pc),%a0
	addq.b   #1,(%a0)
	andi.b   #0x3,(%a0)+            | IMPORTANT: a0 points now to __gray3P_phase!
	bne.s    __gray3P_proceed_old
    |--------------------------------------------------------------------------
    | to evaluate which plane we use counter __gray3P_phase. This counter
    | performs the following counting 8->6->4->2->0(->12)->10->8.
    |--------------------------------------------------------------------------
	move.w   (%a0),%d0
	subq.w   #2,%d0                  | subtract 4 from phase counter
	bcc.s    __gray3P_store          | not negative -> don't reset
__gray3P_phase_reset_hw1:
	moveq    #0x10,%d0               | reset phase counter to 10/12
__gray3P_store:
	move.w   %d0,(%a0)               | store new phase counter value
	lea (__gray3P_7gray_phases),%a0
__gray3P_phase_indir_hw1:
	move.w 0(%a0,%d0:w),%d0
	lea (__gray3P_D_plane,%pc),%a0
    |--------------------------------------------------------------------------
    | doublebuffer extension ... add content of __gray3P_dbl_offset to %d0
    |--------------------------------------------------------------------------
	add.w  (__gray3P_dbl_offset-__gray3P_D_plane,%a0),%d0
	suba.w %d0,%a0
	move.l   (%a0),%d0               | load the address of this plane
	lsr.l    #3,%d0                  | reduce to address / 8
	move.w   %d0,0x600010            | set new plane startaddress
	lea      (__gray3P_switch_cnt,%pc),%a0  | increment switch count
	addq.l   #1,(%a0)
__gray3P_proceed_old:
	movem.l  (%a7)+,%d0/%a0
    |--------------------------------------------------------------------------
    |  JUMP to previous installed interrupt handler
    |--------------------------------------------------------------------------
	.word    0x4ef9                  | "JMP address" opcode
__gray3P_old_int1_hw1:
	.long    0x00000000              | address of old int1 gots stored here
__gray3P_dummy1:                         | NOT used yet (just for alignment)
	.byte    0x00
|------------------------------------------------------------------------------
| __gray3P_skipcount is a one byte counter which performs the following counting:
| 3 -> 0 -> 1 -> 2 -> 3
|------------------------------------------------------------------------------
__gray3P_skipcount:
	.byte    0x03
__gray3P_phase:
	.word    0x08                    | performs: 8->6->4->2->0(->12)->10->8
__gray3P_switch_cnt:
	.long    0x00000000
|==============================================================================
| INTERNAL: allocates memory
|
| modifies: __gray3P_handle
|           __gray3P_used_mem
|           __gray3P_M_plane
|           __gray3P_L_plane
|
| Note: __gray3P_D_plane will not be changed by this function! (will be set by
|                                                        __gray3P_init_handler)
|==============================================================================
__gray3P_init_mem:
	lea      (__gray3P_handle,%pc),%a5          | if __gray3P_handle is not 0
	tst.w    (%a5)                              | we have already allocated
	bne.s    __gray3P_init_return               | memory -> out here
    |--------------------------------------------------------------------------
    | HeapAllocHigh(HW1=7688 bytes or HW2=11528 bytes)
    |--------------------------------------------------------------------------
	movea.l  0xc8,%a0
	movea.l  (0x248,%a0),%a2
	.word    0x4878                       | opcode of "PEA value"
__gray3P_size_to_allocate:                    | the size gets patched !!
	.word    0x2d08
	jsr      (%a2)
	addq.w   #4,%a7
	move.w   %d0,(%a5)+                   | store handle in handle variable
	beq.s    __gray3P_init_return         | alloc failed (handle=0) -> out here
	clr.w    (%a5)                        | clears __gray3P_dbl_offset
    |--------------------------------------------------------------------------
    | HeapDeref(__gray3P_handle)
    |--------------------------------------------------------------------------
	move.w   %d0,-(%a7)
	movea.l  0xc8,%a0
	movea.l  (0x258,%a0),%a2
	jsr      (%a2)
	addq.l   #2,%a7
    |--------------------------------------------------------------------------
    | align memory address to next 8-byte boundary and store address in
    | __gray3P_used_mem
    |
    | for HW1: __M_plane gets set to the same address as __gray3P_used_mem
    | for HW2: __M_plane gets set to __gray3P_used_mem + 0xf00
    |--------------------------------------------------------------------------
	move.l   %a0,%d0
	addq.l   #7,%d0
	andi.b   #0xF8,%d0
	lea      (__gray3P_used_mem,%pc),%a0
	move.l   %d0,(%a0)
	.word    0x0680              | opcode of "ADDI.L #value,%a0"
	.word    0x0000
__gray3P_size_to_add:
	.word    0x0F00              | gets patched (HW1:0 HW2:0x0f00)
	move.l   %d0,(__gray3P_M_plane - __gray3P_used_mem,%a0)
	addi.l   #0x0f00,%d0
	move.l   %d0,(__gray3P_L_plane - __gray3P_used_mem,%a0)
__gray3P_init_return:
	rts

|------------------------------------------------------------------------------
| handle to allocated memory used by grayscale
|------------------------------------------------------------------------------
__gray3P_handle:
	.word    0

|------------------------------------------------------------------------------
| DOUBLEBUFFER extension
|------------------------------------------------------------------------------
__gray3P_dbl_offset: | has to be directly AFTER __gray3P_handle!!
	.word    0
__gray3P_L_plane2:
	.long    0x0
__gray3P_M_plane2:
	.long    0x0
__gray3P_D_plane2:
	.long    0x0

|------------------------------------------------------------------------------
| pointer to light plane
| HW1: same as __gray3P_used_mem
| HW2: __gray3P_used_mem + 0xf00
|------------------------------------------------------------------------------
__gray3P_L_plane:
	.long    0x00004c00
|------------------------------------------------------------------------------
| pointer to medium plane
| HW1: __gray3P_used_mem + 0xf00
| HW2: __gray3P_used_mem + 0x1e00
|------------------------------------------------------------------------------
__gray3P_M_plane:
	.long    0x00004c00
|------------------------------------------------------------------------------
| pointer to dark plane (set by __gray3P_init_handler)
| HW1: 0x4c00
| HW2: same as __gray3P_used_mem
|------------------------------------------------------------------------------
__gray3P_D_plane:
	.long    0x00004c00
|------------------------------------------------------------------------------
| pointer to allocated memory ALIGNED to 8-byte boundary
|------------------------------------------------------------------------------
__gray3P_used_mem:
	.long    0x00004c00
|------------------------------------------------------------------------------
| This variable is very hard to describe. Indeed this isn't one variable,
| but two variables combined in one.
|
| Description will be added later ....
|------------------------------------------------------------------------------
__gray3P_sync_n_count:
	.word    0x0000
|------------------------------------------------------------------------------
| holds the index of the plane which should be drawn next (NOTE: this label
| is never addressed directly, but indirectly from label __gray3P_sync_n_count.
| So don't move it do somewhere else!)
|------------------------------------------------------------------------------
__gray3P_plane_index:
	.word    0x0008
|==============================================================================
| Interrupt 1 handler for HW2
|
| port 70001D (bit 7) is used to synchronized to the LCD hardware. Here are the
| docs of this port (taken from Johan Eilert's j89hw.txt)
|
| $70001D RW ($06)
|	:7	 Toggles every FS (every time the LCD restarts at line 0)
|	:6-4	-
|	:3	 Battery checker bit B (? ???)
|	:2	 ? (set)
|	:1	 Screen enable (clear this bit to shut down LCD)
|	:0	 Battery checker bit A (? enable $600000:2)
|	     (AMS:) The battery checker bits must both be set (AB=11) prior to
|	     checking the voltage level with $600000:2.  Then, after use, bit B
|	     must be cleared (AB=10) while the battery trig hardware settles to
|	     the "rest" voltage value (%111).  Finally, both bits should be
|	     cleared.
|==============================================================================
__gray3P_int1_handler_hw2:
	move.w   %sr,-(%a7)                | save content of status register on stack
	move.w   #0x2700,%sr               | disable ALL interrupts (no one should
	                                   | interrupt us ...)
	movem.l  %d0-%d7/%a0-%a6,-(%a7)
__gray3P_startagain:
	moveq    #0x0,%d1
	lea      (__gray3P_sync_n_count,%pc),%a0
	move.w   (%a0),%d0
	bne.s    __gray3P_copy_first_or_sec | there is a third of the plane left to
	                                    | copy -> do it now!
	move.l   (%a0),%d0
.ifdef ALWAYS_HW2_TESTING
	move.b   %d0,%d1
	eor.b    #0x80,%d1
.else
	move.b   0x70001D,%d1              | get flipping bit
	eor.b    %d0,%d1
	bpl      __gray3P_to_oldint        | not flipped yet -> proceed to previous
	                                   | installed int handler
.endif
	eor.l    %d1,(%a0)                 | store new flip "bit" and reset the
	                                   | work left status

    |--------------------------------------------------------------------------
    | NOTE: if we detect a pageflip we start our copying work with the lowest
    |       third. this way it will not interfere with the LCD hardware refresh
    |
    | The 3 thirds are copied in the following order:
    |
    | last third -> first third -> second third
    |--------------------------------------------------------------------------
	move.w   #0xA00,%d0
__gray3P_copy_next_third:
	addq.w   #1,(%a0)+
	bra.s    __gray3P_perform_copying
__gray3P_copy_first_or_sec:
    |--------------------------------------------------------------------------
    | if __gray3P_sync_n_count == 1 -> copy first third of screen
    | otherwise -> set __gray3P_sync_n_count to 0 and copy second third of screen
    |--------------------------------------------------------------------------
	subq.w   #1,%d0
	beq.s    __gray3P_copy_next_third
	clr.w    (%a0)+
	move.w   #0x500,%d0             | setup to copy second third of screen
__gray3P_perform_copying:
	move.b   (%a0),%d1              | fetch index of plane to draw next

    |--------------------------------------------------------------------------
    | If we'll come here we will copy 1 third of the screen from a specific
    | plane to the video buffer at 0x4c00. Register D0 holds the offset of
    | which third should be copied and register D1 contains the "index" of the
    | "source" plane
    |
    | The copying is performed in a kind of "hardcore" style by using 13
    | registers. This way 52 Bytes are copied with a single instruction.
    |--------------------------------------------------------------------------

	lea (__gray3P_7gray_phases),%a0
__gray3P_phase_indir_hw2:
	move.w 0(%a0,%d1:w),%d1
    |--------------------------------------------------------------------------
    | doublebuffer extension ... add content of __gray3P_dbl_offset to %d0
    |--------------------------------------------------------------------------
	add.w    (__gray3P_dbl_offset,%pc),%d1
	neg.w    %d1
	movea.l  (__gray3P_D_plane,%pc,%d1.w),%a0

	lea      0x4C00,%a1
	adda.w   %d0,%a0
	adda.w   %d0,%a1
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x34,%a1)  | size of this instruction: 6 bytes
	movem.l  (%a0)+,%d0-%d7/%a2-%a6      | size of this instruction: 4 bytes
	movem.l  %d0-%d7/%a2-%a6,(0x68,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x9C,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0xD0,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x104,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x138,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x16C,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x1A0,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x1D4,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x208,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x23C,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x270,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x2A4,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x2D8,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x30C,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x340,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x374,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x3A8,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x3DC,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x410,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x444,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x478,%a1)
	movem.l  (%a0)+,%d0-%d7/%a2-%a6
	movem.l  %d0-%d7/%a2-%a6,(0x4AC,%a1)
	movem.l  (%a0)+,%d0-%d7
	movem.l  %d0-%d7,(0x4E0,%a1)

    |--------------------------------------------------------------------------
    | evaluate if there is still a third of the screen to copy or if we
    | should proceed to the next plane
    |--------------------------------------------------------------------------
__gray3P_update_index:
	lea      (__gray3P_sync_n_count,%pc),%a0
	move.w   (%a0)+,%d0
	beq      __gray3P_startagain        | no third left to copy -> check again
	                                    | the pageflip bit if yet a pageflip
	                                    | had occured
	subq.w   #1,%d0
	bne.s    __gray3P_to_oldint         | if there is "copy work" left ->
	                                    | don't modify the plane to display

	lea      (__gray3P_switch_cnt,%pc),%a1| increment switch count here, because
	addq.l   #1,(%a1)                     | a complete page was drawn if we come here

	subq.b   #2,(%a0)                   | cycle __gray3P_plane_index by decrementing
	bcc.s    __gray3P_to_oldint         | it and wrap around to 10/12 if negative.
__gray3P_phase_reset_hw2:
	move.b   #0x10,(%a0)
__gray3P_to_oldint:
	movem.l  (%a7)+,%d0-%d7/%a0-%a6
	move.w   (%a7)+,%sr                 | restore content of status register
    |--------------------------------------------------------------------------
    |  JUMP to previous installed interrupt handler
    |--------------------------------------------------------------------------
	.word    0x4ef9                     | opcode of "JMP address" instruction
__gray3P_old_int1_hw2:
	.long    0x00000000
|==============================================================================
| INTERNAL: initialize grayscale handler
|==============================================================================
__gray3P_init_handler:
	lea      (__gray3P_M_plane,%pc),%a0
	move.w   #0x77F,%d1
	move.w   (__gray3P_hw_type,%pc),%d0
	tst.w    %d0
	beq.s    __gray3P_init_hw1_handler

    |--------------------------------------------------------------------------
    | HW2 specific initializations:
    |
    | (1) set __D_plane to __gray3P_used_mem
    | (2) copy content of 0x4c00 to darkplane
    | (3) "backup" old INT1 handler in __gray3P_old_int1_hw2 (the address part
    |     of a JUMP address instruction at the end of the HW2 int handler)
    | (4) install our own INT1 HW2 handler
    |--------------------------------------------------------------------------
	movea.l  (__gray3P_used_mem,%pc),%a1
	move.l   %a1,(0x4,%a0)               | set __D_plane
	movea.w  #0x4C00,%a0
	move.w   #0x3BF,%d0
__gray3P_cpy_d_plane:
	move.l   (%a0)+,(%a1)+
	dbf      %d0, __gray3P_cpy_d_plane
	lea      (__gray3P_int1_handler_hw2,%pc),%a0

    | !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    | the following command should be unnecessary; I commented it out (TOM)
    | (__L_plane should be already set by __gray3P_init_mem)
    | !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	|move.l   %a1,(__L_plane - __gray3P_int1_handler_hw2,%a0)

	move.l   0x64,(__gray3P_old_int1_hw2 - __gray3P_int1_handler_hw2,%a0)
	bra.s    __gray3P_init_proceed
    |--------------------------------------------------------------------------
    | HW1 specific initializations:
    |
    | (1) "backup" old INT1 handler in __gray3P_old_int1_hw1 (the address part
    |     of a JUMP address instruction at the end of the HW1 int handler)
    | (2) install our own INT1 HW1 handler
    |--------------------------------------------------------------------------
__gray3P_init_hw1_handler:
	move.l   (%a0),%a1
	lea      (__gray3P_int1_handler_hw1,%pc),%a0
	move.l   0x64,(__gray3P_old_int1_hw1 - __gray3P_int1_handler_hw1,%a0)
__gray3P_init_proceed:
	move.l   %a0,%d2
	lea      0x600001,%a0
	moveq    #2,%d0
	bclr.b   %d0,(%a0)
	move.l   %d2,0x64.w
	bset.b   %d0,(%a0)
__gray3P_clr_l_plane:
    |--------------------------------------------------------------------------
    | clear medium and light planes (done for both HW types)
    |--------------------------------------------------------------------------
	clr.l    (%a1)+
	dbf      %d1, __gray3P_clr_l_plane
    |--------------------------------------------------------------------------
    | PortSet(__D_plane,239,127)
    |--------------------------------------------------------------------------
	move.l   #0xEF007F,-(%a7)
	move.l   (__gray3P_D_plane,%pc),-(%a7)
	movea.l  0xC8,%a0
	movea.l  (0x688,%a0),%a1
	jsr      (%a1)
	addq.w   #8,%a7
__gray3P_ok:
	lea (__gray3P_L_plane,%pc),%a0
	lea (__gray3P_L_plane2,%pc),%a1
	move.l (%a0)+,(%a1)+        | copy __L_plane to __L_plane2
	move.l (%a0)+,(%a1)+        | copy __M_plane to __M_plane2
	move.l (%a0)+,(%a1)+        | copy __D_plane to __D_plane2
	moveq    #0x1,%d0
	rts
|==============================================================================
|  EXPORTED: Gray3POff function (turn grayscales off)
|            NOTE: ALWAYS returns 1 !!
|==============================================================================
Gray3POff:
	movem.l  %a2-%a3,-(%a7)
	lea      (__gray3P_handle,%pc),%a3
	tst.w    (%a3)
	beq      __gray3P_off_out                   | no handle? -> nothing to do
	lea      0x600001,%a0
	move.w   (__gray3P_hw_type,%pc),%d0
	beq.s    __gray3P_hw1_cleanup
    |--------------------------------------------------------------------------
    | cleanup for HW2 calcs
    |--------------------------------------------------------------------------
	moveq    #2,%d0
	bclr.b   %d0,(%a0)
	move.l   (__gray3P_old_int1_hw2,%pc),0x64:w   | restore old INT1 handler
	bset.b   %d0,(%a0)
	movea.l  (__gray3P_D_plane,%pc),%a1
	lea      0x4C00,%a0
	move.w   #0x3BF,%d0                   | copy content of darkplane to 0x4c00
__gray3P_dark2lcd:
	move.l   (%a1)+,(%a0)+
	dbf      %d0, __gray3P_dark2lcd
	bra.s    __gray3P_continue_cleanup
    |--------------------------------------------------------------------------
    | cleanup for HW1 calcs (on HW1 0x4c00 is used as darkplane. We haven't to
    | set it)
    |--------------------------------------------------------------------------
__gray3P_hw1_cleanup:
	move.w   #0x980,0x600010-0x600001(%a0)       | restore used plane to 0x4c00
	move.l   (__gray3P_old_int1_hw1,%pc),0x40064 | restore old INT1 handler
	| (We can use 0x40064 here because it is HW1 only.)

__gray3P_continue_cleanup:
	lea      (__gray3P_L_plane,%pc),%a0  | restore plane pointers to 0x4c00 for sure
	lea      0x4C00.w,%a1
	move.l   %a1,(%a0)+
	move.l   %a1,(%a0)+
	move.l   %a1,(%a0)+
	move.l   %a1,(%a0)
    |--------------------------------------------------------------------------
    | HeapFree(__gray3P_handle)
    |--------------------------------------------------------------------------
	movea.l  0xc8,%a0
	movea.l  (0x25c,%a0),%a2
	move.w   (%a3),-(%a7)
	jsr      (%a2)
	addq.l   #2,%a7
    |--------------------------------------------------------------------------
    | PortRestore()
    |--------------------------------------------------------------------------
	movea.l  0xc8,%a0
	movea.l  (0x68c,%a0),%a2
	jsr      (%a2)
	clr.l    (%a3)                     | 0->handle AND(!!) 0->__gray3P_dbl_offset
	lea      (__gray3P_sync_n_count,%pc),%a0
	clr.l    (%a0)
__gray3P_off_out:
	movem.l  (%a7)+,%a2-%a3
	jbra     __gray3P_ok

__gray3P_7gray_phases:
	.word	8
	.word	0
	.word	4
	.word	0
	.word	4
	.word	0

__gray3P_8gray_phases:
	.word	4
	.word	0
	.word	8
	.word	0
	.word	0
	.word	4
	.word	0

| #############################################################################
|  Revision History
| #############################################################################
|
| $Log: gray.s,v $
| Revision 3.12 2005/08/08 07:53:00  Lionel Debroux
| Optimized the routine for size the same way I optimized the original 2-plane
| routine.
| Making a version which always uses three consecutive planes outside of
| LCD_MEM, even on HW1, like I did for the original 2-plane routine, is trivial.
|
| Revision 3.11-3P-1 2004-06-05  Kevin Kofler
| Merged:
|  Revision 3.11 2004/02/25 03:49:03  Kevin Kofler
|  Don't use 0x40000 to set interrupts on code path that affect HW3.
|  Use 0xE00000 as ROM_base mask instead of 0x600000.
|
| Revision 3.10-3P-3 2003-01-14 Kevin Kofler
| Fixed misleading comment in the code automodifying the HW2 phase reset
|
| Revision 3.10-3P-2 2002-09-06 Kevin Kofler
| Fixed typo in switch counting code
|
| Revision 3.10-3P 2002-07-29 Kevin Kofler
| Grayscale routine adapted to handle 7/8 grayscales
|
| Revision 3.10 2002/04/05 11:34:23  tnussb
| (1) Resets now __D_plane2,__L_plane2 and __gray_dbl_offset at end of GrayOn()
|     to make sure nothing happens if doublebuffer macros get called without
|     setting a buffer first. Nevertheless NO program should call one of the
|     doublebuffer macros without setting a buffer previously.
| (2) Some further size optimizations. Now the size is exactly as long as
|     without before the integration of doublebuffering. Quite smart, isn't it?
|     More functionality, some minor drawbacks fixed and no increase in size ...
| (3) Changed return value of GrayOff() function to void.
|
| Revision 3.9  2002/04/04 18:50:36  tnussb
| first working version of internal doublebuffer support (not checked on
| real calcs yet, but on the VTI)
|
| Revision 3.8  2002/04/04 16:39:05  tnussb
| Debug version checked in previously. This one is the correct one.
|
| Revision 3.7  2002/04/04 16:35:20  tnussb
| (1) documentation of HW2 interrupt handler heavily extended
| (2) HW2: plane switch counting fixed (was incremented too often previously)
| (3) global ALWAYS_HW2_TESTING added which can be used to force the use of
|     HW2 grayscales. Now the HW2 grayscales can be tested on the VTI if
|     this global is defined. Of course this will flicker extremely, because
|     the VTI doesn't simulate port 70001D, but its still better than no
|     testing possibility.
| (4) don't trashes %sr on HW2 anymore (restores previous setting)
| (5) This version was tested on a real HW1 and a real HW2 calc. It works.
|     (Thanx to Sebastian Reichelt and stoopid guy)
|
| Revision 3.6  2002/04/04 11:58:19  tnussb
| (1) size optimizations
| (2) unnecessary cleanup removed from GrayOff()
| (3) interrupt handler for HW1 rewritten (uses now plane pointer directly)
| (4) "heavily" documented
|
| Revision 3.5  2002/04/04 11:54:39  tnussb
| (1) exports __gray_old_int1_hw1 and __gray_old_int1_hw2. this way it is
|     possible to modify them after grayscales are turned on
| (2) comments changed to GNU Assembler style
| [NOTE: CVS time and date doesn't fit to real implementation data]
|
| Revision 3.0  2002/04/04 11:50:56  tnussb
| grayscale support used for TIGCC up to version v0.93
| [NOTE: CVS time and date doesn't fit to real implementation data]
|
