|improved fgetchar() implementation for TIGCCLIB
|Copyright (C) Kevin Kofler, 2002-2003
|Based on documentation examples and the original fgetchar implementation, both
|by Zeljko Juric

/* Prototype:
   short fgetchar(void); */

.text
.xdef fgetchar

__fgetchar_clipzone: .byte 0,0,239,127

.equ __SaveScrState,0x1a0 /* SaveScrState */
.equ __MoveTo,0x19d       /* MoveTo */
.equ __DrawClipChar,0x191 /* DrawClipChar */

fgetchar:
movem.l %a2-%a4/%d3,-(%a7)
5:
link.w %a6,#-18
pea.l (%a7)
move.l 0xc8:w,%a2
move.l __SaveScrState*4(%a2),%a0
jsr (%a0)
move.w #4,(%a7)
pea.l __fgetchar_clipzone(%pc)
move.w #'_',-(%a7)
move.l -8(%a6),-(%a7)
move.l __DrawClipChar*4(%a2),%a3
jsr (%a3)
jbsr __fgetchar
move.w %d0,%d3
move.w #2,(%a7)
pea.l __fgetchar_clipzone(%pc)
move.w #'_',-(%a7)
move.l -8(%a6),-(%a7)
jsr (%a3)
move.l -8(%a6),(%a7)
move.l __MoveTo*4(%a2),%a0
jsr (%a0)
cmp.w #255,%d3
bhi.s 2f
move.w %d3,(%a7)
cmp.w #13,(%a7)
bne.s 3f
subq.w #3,(%a7)
3:
jbsr fputchar
2:
move.w %d3,%d0
unlk %a6
movem.l (%a7)+,%a2-%a4/%d3
rts
