|cbprintf() (non-virtual vcbprintf()) implementation for TIGCCLIB
|Copyright (C) Kevin Kofler, 2003

| Warning: This routine has the attribute __ATTR_TIOS__!

.xdef cbprintf
cbprintf:
|Copy the arguments for vcbprintf(). We have to copy them because of the stack
|parameter convention of AMS. Register parameters wouldn't need a copy.
	pea.l 16(%a7) | arglist: do NOT copy this one, pass a pointer to it
	move.l 16(%a7),-(%a7) | copy format
	move.l 16(%a7),-(%a7) | copy param
	move.l 16(%a7),-(%a7) | copy callback

|Now call vcbprintf using the usual hack.
|Copied from Zeljko Juric's printf.
	movea.l 0xC8,%a0
	movea.l (%a0,0x14C),%a0 /* vcbprintf */
	lea (%a0,32),%a0
	movea.w (%a0),%a1
	jsr (%a0.l,%a1)

|Pop the arguments from the stack and return.
	lea.l 16(%a7),%a7
	rts
