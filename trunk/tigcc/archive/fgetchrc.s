|improved fgetchar()/gets()/getsn() implementation for TIGCCLIB - core routine
|Copyright (C) Kevin Kofler, 2002-2003
|Based on documentation examples and the original fgetchar implementation, both
|by Zeljko Juric

/* Prototype:
   short __fgetchar(void); */

.xdef __ref_all___set_file_in_use_bit

.text
.xdef __fgetchar

__fgetchar_CaptureHandler:
move.l 4(%a7),%a0
cmp.w #0x723,(%a0)
bne.s 0f
move.l 8(%a0),%a0
lea.l __fgetchar_keycode+2(%pc),%a1
clr.w %d0
move.b (%a0),%d0
move.w %d0,(%a1)
0:
rts

.equ __ngetchx,0x51            /* ngetchx */
.equ __EV_captureEvents,0xc6   /* EV_captureEvents */
.equ __EV_defaultHandler,0x157 /* EV_defaultHandler */

__fgetchar:
pea.l (%a2)
2:
move.l 0xc8:w,%a2
move.l __ngetchx*4(%a2),%a0
jsr (%a0)
cmp.w #4139,%d0
bne.s 1f
lea.l __fgetchar_keycode+2(%pc),%a0
clr.w (%a0)
pea.l __fgetchar_CaptureHandler(%pc)
move.l __EV_captureEvents*4(%a2),%a2
jsr (%a2)
clr.w (%a7)
move.w %d3,-(%a7)
move.w #4096,-(%a7)
subq.l #6,%a7
move.w #0x710,-(%a7)
pea.l (%a7)
move.l __EV_defaultHandler*4(%a2),%a0
jsr (%a0)
clr.l (%a7)
jsr (%a2)
lea.l 16(%a7),%a7
__fgetchar_keycode:
move.w #0,%d0
beq.s 2b
1:
movea.l (%a7)+,%a2
rts
