|atexit function copyright (C) 2002, Kevin Kofler
|requires new exit support (__save__sp__)
|Many thanks to Patrick Pélissier and Stephan Effelsberg for ideas on how to
|implement this. Stephan Effelsberg's C implementation inspired this mostly,
|but I have changed it so that exit and atexit will not be included if not
|used. There is only a small 20-byte exit support needed.

	.xdef atexit

| This file requires the startup exit support.
	.xdef __ref_all___save_all_registers_main

.text

	.equ __malloc,0xa2 /* HeapAllocPtr */
	.equ __free,0xa3   /* HeapFreePtr */
	.equ __num_regs,10 |number of registers saved by the exit support

atexit:
|find the return address:
	movea.l __save__sp__,%a0 |stack pointer before restoring the registers
	                         |(NOT PC-relative because of programs >32 KB)
|The return address is now at (a0).

|check if the return address is .L__atexit__new__return
	cmpi.l #.L__atexit__new__return:l,(%a0)
	beq.s .L__atexit__return__address__ok |if it is, skip ahead
|Else:
|- move the current return address to .L__atexit__return+2
	move.l (%a0),.L__atexit__return+2
|- change the return address to .L__atexit__new__return
	move.l #.L__atexit__new__return:l,(%a0)
	clr.l .L__atexit__num__funcs |initialize the number of atexit functions to 0

|allocate a handle for the functions
	pea.l 4:w
	movea.l 0xc8,%a0
	move.l (__malloc*4,%a0),%a0
	jsr (%a0) |a0=malloc(4);
	addq.l #4,%a7
	bra.s .L__atexit__handle__allocated

.L__atexit__return__address__ok:
|reallocate the handle for the functions
	move.l (.L__atexit__num__funcs,%PC),%d0
	lsl.l #2,%d0
	addq.l #4,%d0
	move.l (.L__atexit__ptr__funcs,%PC),%a0
	jbsr realloc |a0=realloc(.L__atexit__ptr__funcs,.L__atexit__num__funcs*4+4);

.L__atexit__handle__allocated:
|If a0 is NULL, there was not enough memory, so we return an error code.
	moveq #0,%d0
	cmpa.l %d0,%a0 |if (!a0)
	seq.b %d0
	beq.s .L__atexit__rts |return 255;

	move.l %a0,.L__atexit__ptr__funcs |save a0 to .L__atexit__ptr__funcs
|Now we store the function given as an argument into the allocated memory.
	move.l (.L__atexit__num__funcs,%PC),%d1
	lsl.l #2,%d1
	move.l (4,%a7),(0,%a0,%d1:l) |.L__atexit__ptr__funcs[.L__atexit__num__funcs]=4(a7);

|And there is 1 more atexit function now:
	addq.l #1,.L__atexit__num__funcs

.L__atexit__rts:
	rts

|This will be executed when the _main function or the exit function tries to
|return:
.L__atexit__new__return:
	move.l (.L__atexit__num__funcs,%PC),%d0 |if there are no functions to call, return
	beq.s .L__atexit__no__funcs             |immediately

|	movem.l %a2/%a5/%d3,-(%a7)
|no need to save and restore the registers, the exit support will do it for us
	movea.l 0xc8,%a5
	movea.l (.L__atexit__ptr__funcs,%PC),%a2
	move.l %d0,%d3 |save d0 to d3
	lsl.l #2,%d0 |point a2 to the address of the last function + 4
	add.l %d0,%a2 |that is, .L__atexit__ptr__funcs[.L__atexit__num__funcs]

	subq.l #1,%d3 |subtract 1 from the number of functions for the dbra loop

|call all atexit functions now
.L__atexit__loop:
	movea.l -(%a2),%a0
	jsr (%a0)
	dbra.w %d3,.L__atexit__loop

|a2 now points to the beginning of the allocated memory for the pointers
|free this memory
	pea.l (%a2)
	move.l (__free*4,%a5),%a0
	jsr (%a0) |free(.L__atexit__ptr__funcs);
	addq.l #4,%a7

|	movem.l (%a7)+,%a2/%a5/%d3
|no need to save and restore the registers, the exit support will do it for us
.L__atexit__no__funcs:

|This will return to the actual return address (which will be patched in
|instead of the 0 by the first call to atexit).
.L__atexit__return: jmp.l 0:l

|data:
.L__atexit__ptr__funcs: .long 0 |pointer to the atexit functions
.L__atexit__num__funcs: .long 0 |number of atexit functions
