| Hardware version detection, adapted from gray.s
| Copyright (C) 2002 Thomas Nussbaumer.
| Copyright (C) 2003 Kevin Kofler.
| See License.txt for licensing conditions.

	.xdef __get_hw_version

__get_hw_version:
    |--------------------------------------------------------------------------
    | get the HW parm block using the algorithm suggested by Julien Muchembled
    |--------------------------------------------------------------------------
	move.l   0xc8:w,%d0
	andi.l   #0xE00000,%d0 | get the ROM base
	movea.l  %d0,%a0
	movea.l  260(%a0),%a1  | get pointer to the hardware param block
	adda.l   #0x10000,%a0
	cmpa.l   %a0,%a1       | check if the HW parameter block is near enough
	bcc.s    L.is_hw1      | if it is too far, it is HW1
	cmpi.w   #22,(%a1)     | check if the parameter block contains HW ver
	bls.s    L.is_hw1      | if it is too small, it is HW1

    |--------------------------------------------------------------------------
    | check for VTI (trick suggested by Julien Muchembled)
    |--------------------------------------------------------------------------
	trap     #12         | enter supervisor mode. returns old (%sr) in %d0.w
	move.w   #0x3000,%sr | set a non-existing flag in %sr (but keep s-flag)
	move.w   %sr,%d1     | get %sr content and check for non-existing flag
	move.w   %d0,%sr     | restore old %sr content
	btst.l   #12,%d1     | this non-existing flag can only be set on the VTI
	beq.s    L.not_vti   | flag not set -> no VTI

    |--------------------------------------------------------------------------
    | VTI detected -> treat as HW1
    |--------------------------------------------------------------------------
	| Fall through...

L.is_hw1:
    |--------------------------------------------------------------------------
    | HW1 detected
    |--------------------------------------------------------------------------
	moveq.l  #1,%d0      | set %d0 to 1 (HW1)
	rts                  | return 1

L.not_vti:
    |--------------------------------------------------------------------------
    | Real calculator detected, so read the HW version from the HW parm block
    |--------------------------------------------------------------------------
	move.l   22(%a1),%d0 | get the hardware version
	rts                  | return it
