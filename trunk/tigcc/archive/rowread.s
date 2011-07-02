	.xdef _rowread

.text
_rowread:
| Write the mask to the port
	move.w %d0,0x600018
| Wait for a few milliseconds until the I/O can return a valid value
	moveq #24,%d0
0:
	dbra %d0,0b
| Read the port and write it to %d0 (return value register)
	moveq #0,%d0
	move.b 0x60001B,%d0
| Invert the byte
	not.b %d0
| Return
	rts
