|sscanf() implementation for TIGCCLIB - callback functions
|Copyright (C) Kevin Kofler, 2002-2003

/*Prototypes:
  typedef struct {char *buffer; unsigned short pos;} __sscanf_string;
  CALLBACK short __sscanf_get(__sscanf_string *param);
  CALLBACK void __sscanf_unget(short c, __sscanf_string *param);
  */

.text
.xdef __sscanf_get
__sscanf_get:
movea.l 4(%a7),%a0
moveq.l #0,%d1
move.w 4(%a0),%d1
movea.l (%a0),%a1
clr.w %d0
move.b 0(%a1,%d1:l),%d0
beq.s 0f
addq.w #1,4(%a0)
rts
0:
moveq.l #-1,%d0 |EOF
rts

.xdef __sscanf_unget
__sscanf_unget:
movea.l 6(%a7),%a0
addq.l #4,%a0
tst.w (%a0)
beq.s 1f
subq.w #1,(%a0)
1:
rts
