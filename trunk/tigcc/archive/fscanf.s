|fscanf() implementation for TIGCCLIB - callback functions
|Copyright (C) Kevin Kofler, 2002-2003

/*Prototypes:
  CALLBACK short __fscanf_get(FILE *param);
  CALLBACK void __fscanf_unget(short c, FILE *param);
  */

.text
.xdef __fscanf_get
__fscanf_get:
|extern short fgetc(FILE*)__ATTR_LIB_C__;
move.l 4(%a7),%a0
jra fgetc

.xdef __fscanf_unget
__fscanf_unget:
|#define ungetc(c,f) ((f)->unget=((c)|0x8000))
bset.b #7,4(%a7)
movea.l 6(%a7),%a0
move.l 4(%a7),12(%a0)
rts
