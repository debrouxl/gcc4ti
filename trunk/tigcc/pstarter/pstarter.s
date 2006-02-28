| TIGCC Program Starter
| Copyright (C) 2004-2005 Kevin Kofler, Lionel Debroux.
|
| This launcher is free software; you can redistribute it and/or
| modify it under the terms of the GNU Lesser General Public
| License as published by the Free Software Foundation; either
| version 2.1 of the License, or (at your option) any later version.
|
| As a special exception, UNMODIFIED copies of this launcher may also be
| redistributed or sold without source code, for any purpose, as long as
| such redistribution or selling is allowed by the license of the chosen
| decompression routine. (The Lesser General Public License restrictions
| do apply in other respects; for example, they cover modification of
| the launcher. Licensing restrictions of the chosen decompression routine
| also apply.) Merely replacing "tempprog" with the name of your program
| shall not be considered a modification for the purposes of this
| paragraph. This exception notice must be removed on modified copies of
| this launcher.
|
| Please make sure you also read the licensing terms of the decompression
| routine of your choice. They are written in the corresponding header
| files.
|
| This launcher is distributed in the hope that it will be useful,
| but WITHOUT ANY WARRANTY; without even the implied warranty of
| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
| Lesser General Public License for more details.
|
| You should have received a copy of the GNU Lesser General Public
| License along with this library; if not, write to the Free Software
| Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

.ifdef lzma
.include "pst-lzma.h"
.endif
.ifdef ttunpack
.include "pst-ttup.h"
.endif
.ifdef ttunpack_fast
.include "pst-ttuf.h"
.endif
.ifdef shrink92
.include "pst-shrn.h"
.endif

.xdef _ti89
.xdef _ti92plus
.xdef _v200
.xdef _ti89ti
.xdef _tigcc_native

.section _st1

| INITIALIZATION

movem.l %d3-%d7/%a2-%a5,-(%sp)
move.l 0xC8.w,%a5
| Check if this is a TI-89 Titanium.
move.l %a5,%d0
| Size optimization: the highest bit (#31) determines the sign (N bit in ccr).
| 23+8 = 31 => we can use rol #8 (2 bytes) instead of btst # (4 bytes).
rol.l #8,%d0 | btst.l #23,%d0
smi.b %d7 | sne.b %d7
| First, look for the current address.
lea.l (%pc),%a1
move.l %a1,%d4
| This is to prevent problems with Kevin's HW2 TSR support.
and.l #0x3FFFF,%d4
| Set the bit instead of clearing it.
moveq.l #1,%d3
| Call function.
jbsr __set_clear_in_use_bit__
| On failure, try using the return address of the program, to do the same for
| a possible launcher.
tst.w %d0
jbne __in_use_done__
| Get the return address.
move.l 32(%sp),%d4
| Check if it is in RAM range.
cmp.l #0x1FFFFF,%d4
| If not, it's not the launcher. Now we don't know who started us.
jbhi __in_use_done__
| This is to prevent problems with Kevin's HW2 TSR support.
and.l #0x3FFFF,%d4
| Call function.
jbsr __set_clear_in_use_bit__
__in_use_done__:
| Save variables for reentrancy
move.l ercatcherpc(%pc),-(%a7)
lea.l DecompressedHandle(%pc),%a0
move.l (%a0),-(%a7)
| Initialize the handles to 0
clr.l (%a0)
__error_return_support:
| Get 60 bytes on the stack (size of an error frame).
lea (%sp,-60),%sp
| Push a pointer to these 60 bytes.
pea.l (%sp)
| Call ER_catch
move.l (%a5,0x154*4),%a0 /* ER_catch */
jsr (%a0)
| Now, this is a little complicated:
| If an error is thrown later, a second return from ER_catch with an error
| in %d0 is simulated.
| So, if %d0 contains 0, then we resume normally, otherwise we go to the
| cleanup block, but remember the error.
| Store the error in a variable.
lea.l __error_num__(%pc),%a0
move.w %d0,(%a0)
jbne __error_returned__


| FIND FILE TO DECOMPRESS

| Convert program name to SYM_STR
lea.l ProgramName(%pc),%a0
symstr_loop:
tst.b (%a0)+
jbne symstr_loop
| SymFindPtr(SYMSTR(ProgramName),0)
clr.w -(%a7)
pea.l -1(%a0)
move.l (%a5,0x283*4),%a0 /* SymFindPtr */
jsr (%a0)
| File found?
move.l %a0,%d0
jbne file_found
file_not_found:
.word 0xA000+850 /* ER_PRGM_NOT_FOUND */
file_found:
| Get handle
lea.l CompressedHandle(%pc),%a2
move.w 12(%a0),%d6
jbeq file_not_found
| Check if locked
move.w %d6,(%a7)
move.l (%a5,0x9B*4),%a0 /* HeapGetLock */
jsr (%a0)
tst.w %d0
jbne already_locked
| Save handle for unlocking if not already locked
move.w %d6,(%a2)
already_locked:
| Lock handle and get pointer to compressed data
move.w %d6,(%a7)
move.l (%a5,0x99*4),%a0 /* HLock */
jsr (%a0)
move.l %a0,%a3
addq.l #6,%a7


| DECOMPRESS FILE

| Check if the archive is valid and compute the uncompressed size
GET_UNCOMPRESSED_SIZE
| Allocate the memory needed to decompress the program
| NOTE: We can't use HeapAllocPtr here because of kernel-based programs.
move.l %d6,-(%a7)
| Add the stub bytes for the Titanium
tst.b %d7
jbeq no_add_bytes
add.l #102,(%a7)
no_add_bytes:
move.l (%a5,0x93*4),%a0 /* HeapAllocThrow */
jsr (%a0)
move.w %d0,(%a7)
move.w %d0,-(%a2) | save handle for freeing
move.l (%a5,0x99*4),%a0 /* HLock */
jsr (%a0)
addq.l #4,%a7
| Decompress the archive
MEM_TO_MEM_DECOMPRESS


| LAUNCH DECOMPRESSED PROGRAM
| NOTE: Some of this code is the same as in ttstart. This is
|       because this is code _I_ contributed to ttstart. So
|       it is being relicensed by the original author.

| If we're not on a Titanium, switch the execution pointer to
| the ghost space.
tst.b %d7
jbne dont_use_ghost_space
moveq.l #4,%d1
swap %d1
adda.l %d1,%a4
| Now enter the ghost space.
jbsr own_enter_ghost_space
| Set the ERROR_FRAME PC to the ghost space.
ori.w #4,56(%a7)
dont_use_ghost_space:
| Skip the size bytes and read the actual size (upper word of d6 is 0).
move.w (%a4)+,%d6
| Relocate the program.
pea.l -1(%a4,%d6.l) | tag_ptr
pea.l (%a4) | base_addr
move.l (%a5,0x15a*4),%a0 /* EX_patch */
jsr (%a0)
addq.l #8,%a7
| The following code was compiled and hand-edited from:
| /* If we are on AMS 2, we have to set the "last executed program" to somewhere
|    in the last 4 KB of RAM, or else APD may crash under certain circumstances.
|    The code below looks for the "last executed program" variable. That variable
|    is cleared during initialization, immediately after the stack fence is set up.
|    So we look for the value of the stack fence (0xDEADDEAD) in the initialization
|    code and add 8 to get the wanted short pointer, which must then be
|    sign-extended to an actual pointer. (The sign extension is implicit in the
|    generated code, as it should be.) */
| if (!(AMS_1xx||*(short*)0x32==('R'<<8)+'O')) {
|   /* rb and q are factored out in order to get more efficient code. */
|   char *rb=ROM_base;
|   char *p=rb+0x12000;
|   char *q=rb+0x18000;
|
|   while (p<q && *(unsigned long*)p!=0xDEADDEAD) p+=2;
|   p+=2[(short *)p]?8:12;
|   *(void **)(long)*(short *)p=isTitanium?(dest+2):(void*)0x3f000;
| }
| Then, it was optimized by Lionel Debroux. A consequence is that the assembly
| code does not look like the C code above at all.
cmp.l #1000,-4(%a5)
jbcs .Llastexec1
cmp.w #0x524F,0x32.w
jbeq .Llastexec1
move.l %a5,%d0
andi.l #0xE00000,%d0
add.l #0x12000,%d0
move.l %d0,%a0
move.w #((0x18000-0x12000)/2)-1,%d0 | Set up iteration counter
0:
cmpi.l #0xDEADDEAD,(%a0) | Look for the stack fence
addq.l #2,%a0 | Increment pointer, condition codes unchanged
dbeq %d0,0b
tst.w 2(%a0)
bne.s 3f
addq.l #4,%a0
3:
move.w 6(%a0),%a0
tst.b %d7
jbne use_dest_pointer
move.l #0x3F000,(%a0)
jbra .Llastexec1
use_dest_pointer:
move.l %a4,(%a0)
.Llastexec1:
| Now actually run the program.
movem.l %d1-%d7/%a0-%a6,-(%sp)
tst.b %d7
jbeq not_titanium
| Round the length up to an even size
| NOTE: The handle size is rounded up automatically.
addq.l #1,%d6
moveq.l #0xfffffffe,%d0
and.l %d0,%d6
| Save error catcher PC
lea.l ercatcherpc(%pc),%a0
move.l 112(%sp),(%a0)
| Set new error catcher PC
lea.l 0(%a4,%d6.l),%a0
lea.l (ercatcher-middle)(%a0),%a1
move.l %a1,112(%sp)
| Append the stub to the program
lea.l middle(%pc),%a1
moveq.l #50,%d0
stub_copy_loop:
move.w (%a1)+,(%a0)+
dbra %d0,stub_copy_loop
| Unprotect and run the program
pea target(%PC)
movea.l %a4,%a3
pea (%a3,%d6.l)
jbra unprotect_a3_jumpto_a4
target:
| Clean up
lea (%sp,36),%sp
move.l (%sp)+,%d0
bra.s 0f
not_titanium:
| Run the program in the ghost space
jsr (%a4)
| Handle RETURN_VALUE
st.b %d0
bra.s 0f
clr.b %d0
0: movem.l (%sp)+,%d1-%d7/%a0-%a6
| If %d0==0, we have to fix the return address.
tst.b %d0
jbne no_return_value
lea.l 108(%sp),%a1
move.l (%a1),%a0
cmpi.w #0x21EE,(%a0)
jbne .L__no_add_2
addq.l #2,(%a1)
.L__no_add_2:
addq.l #4,(%a1)
no_return_value:


| ERROR HANDLING

__error_return_support_cleanup:
| We have terminated successfully, so remove the error frame.
move.l (%a5,0x155*4),%a0 /* ER_success */
jsr (%a0)
| This is the place where we should go if an error happened.
| %d0 is not 0 in this case.
__error_returned__:
| Remove the error frame and the parameter from the stack.
lea (%sp,64),%sp
jbsr __set_clear_in_use_bit__


| PSTARTER CLEANUPS

| Free decompressed program handle, if any.
pea.l DecompressedHandle(%pc)
move.l (%a5,0x98*4),%a0 /* HeapFreeIndir */
jsr (%a0)
| Unlock compressed program handle, if it wasn't already locked
move.w CompressedHandle(%pc),(%a7)
jbeq no_unlock
move.l (%a5,0x9f*4),%a0 /* HeapUnlock */
jsr (%a0)
no_unlock:
addq.l #4,%a7
| Restore variables for reentrancy
lea.l ercatcherpc(%pc),%a0
move.l (%a7)+,DecompressedHandle-ercatcherpc(%a0)
move.l (%a7)+,(%a0)


| GENERIC CLEANUPS

| If we had any cleaning up to do, it absolutely MUST be finished at this
| point. That is, we have to be able to return from here under any given
| circumstances.
| Go to __no_error__ if no error happened.
lea.l __error_num__(%pc),%a0
tst.w (%a0)
beq.s __no_error__
| Construct an A-Line value.
or.w #0xA000,(%a0)
__error_return_support_ams_1:
| In AMS 1.xx, we need to unlock our own handle.
| Otherwise, the program cannot be started again.
cmp.l #1000,(%a5,-4) /* TIOS_entries */
jbcc __ams_1_err_handling_not_needed__
| Get the starting address of the program and push it on the stack.
pea.l __ld_entry_point-2(%pc)
| Convert it to a handle.
move.l (%a5,0x23A*4),%a0 /* HeapPtrToHandle */
jsr (%a0)
| The handle is now in %d0. It is 0 on failure.
move.w %d0,(%sp)
jbeq __ams_1_err_handling_not_needed__
| Unlock it.
move.l (%a5,0x9F*4),%a0 /* HeapUnlock */
jsr (%a0)
| No need to clean up the stack, since we are going to throw an error anyway.
__ams_1_err_handling_not_needed__:
| This is where the A-Line error throwing is constructed.
__error_num__:
.word 0
__no_error__:
clr.w (%a0)
movem.l (%sp)+,%d3-%d7/%a2-%a5
rts


| IN-USE BIT SUPPORT

| This function relies on the address of the jump table being in %a5.
| %d4 is an arbitrary address somewhere in our program (or the launcher,
| respectively).
| If %d3.w is 0, clear the bit, otherwise set it. If action is to set the bit,
| the previous value is stored in %d3.
| Register %d5 is destroyed.
__set_clear_in_use_bit__:
| Call SymFindFirst with appropriate parameters to look for the handle.
move.w #0x06,-(%sp) /* FO_RECURSE | FO_SKIP_TEMPS */
clr.l -(%sp)
move.l (%a5,0x6C*4),%a0 /* SymFindFirst */
jbsr (%a0)
addq.l #6,%sp
__symbol_search_loop__:
| Store failure value in %d0
clr.w %d0
| If the SYM_ENTRY pointer is 0, quit.
move.l %a0,%d5
jbeq __symbol_search_done__
| Dereference the handle from the SYM_ENTRY structure.
move.w (%a0,12),-(%sp)
move.l (%a5,0x96*4),%a0 /* HeapDeref */
jbsr (%a0)
addq.l #2,%sp
| If the address returned is higher than the address we are looking for, the
| symbol can't be the one.
| Actually, if it is equal, then something is seriously broken, but we don't
| check for that here.
cmp.l %d4,%a0
jbhi __skip_symbol__
| Add the size of the variable to %a0.
| Note that we would actually need to add 2 more bytes to skip the size
| field, but we make up for that by using a <= comparison.
moveq #0,%d0
move.w (%a0),%d0
add.l %d0,%a0
| If the end address is lower than the address we are looking for, the
| symbol can't be the one.
cmp.l %d4,%a0
jbls __skip_symbol__
| Store success result in %d0.
moveq #1,%d0
| If %d3.w is 0, clear the in-use bit.
move.l %d5,%a0
lea 11(%a0),%a0
tst.w %d3
jbeq __clear_bit__
| Otherwise, store the previous value in %d3, set the bit, and quit.
moveq #0x10,%d1
move.b (%a0),%d3
and.w %d1,%d3
or.b %d1,(%a0)
rts
__skip_symbol__:
| Call SymFindNext.
move.l (%a5,0x6D*4),%a0 /* SymFindNext */
jbsr (%a0)
| Go to beginning of loop.
jra __symbol_search_loop__
__clear_bit__:
and.b #0xEF,(%a0)
__symbol_search_done__:
rts


| GHOST SPACE ENTERING ROUTINE
| WARNING: This routine wants #0x40000 in %d1.l.

own_enter_ghost_space:
move.l (%sp),%d0
cmp.l %d1,%d0
jbcc 2f
cmpi.l #1000,(%a5,-4)
jbcs 2f
movem.l %a2-%a6/%d3-%d7,-(%sp)
lea.l 0x3E000,%a3
lea.l 1f(%pc,%d1.l),%a4
jbra unprotect_a3_jumpto_a4
1:
lea (%sp,20),%sp
movem.l (%sp)+,%a2-%a6/%d3-%d7
2:
move.l (%sp)+,%d0
bset.l #18,%d0
movea.l %d0,%a0
jmp (%a0)


| CODE EXECUTED AT END OF PROGRAM
| NOTE: This is the same code as in ttstart, because the code in ttstart
|       is actually MY code.

middle:
| Handle RETURN_VALUE
st.b %d0
bra.s 0f
clr.b %d0
0:
| Unprotect and jump to the launcher
movea.l (%sp)+,%a4
lea __ld_entry_point-target(%a4),%a3
move.l %d0,16(%sp)
| Unprotection routine
unprotect_a3_jumpto_a4:
lea (%sp,-20),%sp
move.l %a5,%d0
andi.l #0xE00000,%d0
addi.l #0x20000,%d0
move.l %d0,(%sp,12)
move.l %d0,(%sp,16)
move.l 20(%sp),(%sp)
move.l 24(%sp),4(%sp)
trap #0xC
move.w #0x2700,%sr
moveq #0xF,%d3
pea (%a4)
clr.w  -(%sp)
move.l 0xAC.w,%a0
jmp (%a0)
ercatcherentrypoint: .long __ld_entry_point
ercatcherpc: .long 0
.even
ercatcher:
movem.l %d0-%d7/%a0-%a6,-(%sp)
move.l ercatcherentrypoint(%PC),%a3
lea.l ercatchercleanup-__ld_entry_point(%a3),%a4
jbra unprotect_a3_jumpto_a4


| LAUNCHING CLEANUP CODE EXECUTED IN THE LAUNCHER'S EXECUTION SPACE

ercatchercleanup:
lea (%sp,20),%sp
movem.l (%sp)+,%d0-%d7/%a0-%a6
move.l ercatcherpc(%PC),-(%a7)
rts


| DATA

.data

.byte 0
ProgramName:      | program name
.asciz "tempprog" | to be patched in by TIGCC

DecompressedHandle: .word 0 | handle to free, 0 if no freeing needed
CompressedHandle: .word 0 | handle to unlock, 0 if no unlocking needed

