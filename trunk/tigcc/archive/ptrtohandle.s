| This routine returns the HANDLE of the block containing the address
| where the given pointer points to, or H_NULL if there's no such block.
|
| This routine is NOT a duplicate of HeapPtrToHandle: unlike HeapPtrToHandle,
| PtrToHandle can meaningfully handle pointers that do not point to the
| beginning of the block.
| Kernels have provided PtrToHandle for years as "Ptr2Hd".
|
| Copyright (C) Lionel Debroux 2003 (contribution to TIGCC)
| Copyright (C) Lionel Debroux 2009 (rename, modifications & integration to GCC4TI).
|
| Equivalent C code:
/*
HANDLE PtrToHandle(void *ptr asm("d0"))
{
  HANDLE h = H_NULL;
  void *ptr2;
  while (h < 2000)
  {
     h++;
     ptr2 = HeapDeref(h);
     if (ptr >= ptr2 && (unsigned char *)ptr < (unsigned char *)ptr2 + HeapSize(h)) return h;
  }
  return H_NULL;
}
*/

.text
.even
.globl PtrToHandle
PtrToHandle:
    movem.l  %d3-%d4/%a2-%a4,-(%sp)

| Save the pointer into non-call-clobbered a2.
    movea.l  %a0,%a2
| Put the addresses of HeapDeref and HeapSize in the registers.
    movea.l  0xC8.w,%a4
    movea.l  0x96*4(%a4),%a3 | HeapDeref.
    movea.l  0x9E*4(%a4),%a4 | HeapSize.
        
| We'll check handles 1-1999 in ascending order, because:
| * HeapSize(H_NULL) crashes the calculator;
| * since handles are allocated in ascending order, pointers pointing to an
|   allocated memory area will be found more quickly.
| Checking the handles in ascending order yields a larger routine, though.
    clr.w    -(%sp)
    move.w   #2000,%d3

.L__PtrToHandle_loop:
| In theory, we should re-create the argument each time HeapDeref is called and each time HeapSize is called.
| This is because callees, among which AMS functions, _can_ destroy their stack arguments...
| Neither HeapDeref nor HeapSize has ever done so, be it on AMS (discontinued) or PedroM (which behaves the
| same way as AMS does, for compatibility and implementation sanity reasons).
    addq.w   #1,(%sp)
    cmp.w    (%sp),%d3
    beq.s    .L__PtrToHandle_not_found

| We've not run out of the heap table yet.
    jsr      (%a3) | HeapDeref.

| If this handle is not allocated, skip it, so as not to emulate the bug of HeapPtrToHandle(NULL) returning
| the index of the first free entry in the heap table (instead of H_NULL, which would make more sense, as NULL
| doesn't belong to any block). This matches the behaviour of kernel::Ptr2Hd.
    move.l   %a0,%d4
    beq.s    .L__PtrToHandle_loop

| If a2 < the lowest address of the current handle, the current handle is not the one we're looking for.
    cmpa.l   %d4,%a2
    blt.s    .L__PtrToHandle_loop

| a2 >= the lowest address of the current handle.
    jsr      (%a4) | HeapSize.
    add.l    %d0,%d4

| If a2 >= the lowest address and < the highest address, the current handle is the one we're looking for.
    cmp.l    %a2,%d4
    blt.s    .L__PtrToHandle_loop

| Found the handle.
    move.w   (%sp)+,%d0

.L__PtrToHandle_end:
    movem.l  (%sp)+,%d3-%d4/%a2-%a4
    rts

| The whole heap table was traversed, but no handle was found. Return H_NULL.
.L__PtrToHandle_not_found:
    moveq    #0,%d0
    addq.l   #2,%sp
    bra.s    .L__PtrToHandle_end
