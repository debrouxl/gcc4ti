|getsn() (gets() with maximum size) implementation for TIGCCLIB
|Copyright (C) Kevin Kofler, 2002-2003

/* Prototype:
   char *getsn(char *string asm("a2"), unsigned long maxlen asm("d3")); */

.equ __SaveScrState,0x1a0 /* SaveScrState */
.equ __MoveTo,0x19d       /* MoveTo */

.text
.xdef getsn
getsn:
movem.l %a3-%a5,-(%a7)
|SaveScrState(&ss);
link.w %a6,#-18
pea.l (%a7)
move.l 0xc8:w,%a3
move.l __SaveScrState*4(%a3),%a0
jsr (%a0)
|fputchar('_');
move.w #'_',(%a7)
jbsr fputchar
move.l %a2,%a4 |current string pointer
clr.b (%a4) | null-terminate the string
move.l __MoveTo*4(%a3),%a5 |prepare MoveTo for repeated calling
lea.l printf,%a3 |prepare printf for repeated calling
1:
jbsr __fgetchar
cmp.w #13,%d0 |KEY_ENTER
beq.s 0f
cmp.w #257,%d0 |KEY_BACKSPACE
bne.s 2f
cmp.l %a4,%a2
beq.s 1b |don't go back if we are already at the first char
clr.b -(%a4) |delete the last character and null-terminate the string
bra.s 3f
2:
|if (current_pointer>=string+maxlen-1), refuse additional input
lea.l -1(%a2,%d3:l),%a0
cmpa.l %a0,%a4
bcc.s 1b
move.b %d0,(%a4)+ |store the character
clr.b (%a4) |null-terminate the string
3: |output the changed string
|MoveTo(ss.CurX,ss.CurY);
move.l -8(%a6),(%a7)
jsr (%a5)
|printf("%s_  ",string);
move.l %a2,(%a7)
pea.l L.format1(%pc)
jsr (%a3)
addq.l #4,%a7 |cleanup the stack (we don't want our loop to overflow it)
bra.s 1b |next character
0: |output the final string
|MoveTo(ss.CurX,ss.CurY);
move.l -8(%a6),(%a7)
jsr (%a5)
|printf("%s  ",string);
move.l %a2,(%a7)
pea.l L.format2(%pc)
jsr (%a3)
|MoveTo(ss.CurX,ss.CurY);
move.l -8(%a6),(%a7)
jsr (%a5)
|printf("%s",string);
move.l %a2,(%a7)
pea.l L.format3(%pc)
jsr (%a3)
unlk %a6
movem.l (%a7)+,%a3-%a5
movea.l %a2,%a0 |return the string
rts

L.format1: .asciz "%s_  "
L.format2: .asciz "%s  "
L.format3: .asciz "%s"
