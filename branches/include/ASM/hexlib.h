;----------------------------------------------------------------------------
; put_char()
;
; Function: Prints a hex character
;
; input:  D0.L = character
;         D1.L = row
;         D2.L = column
; output: nothing
;----------------------------------------------------------------------------
hexlib::put_char	equ	hexlib@0000

;----------------------------------------------------------------------------
; put_bin()
;
; Function: prints a binary long word
;
; input:  D0.L = long word
;         D1.L = row
;         D2.L = column
;         D4.L = no. of digits - 1
; output: nothing
;----------------------------------------------------------------------------
hexlib::put_bin		equ	hexlib@0001

;----------------------------------------------------------------------------
; put_hex()
;
; Function: prints a hexadecimal long word
;
; input:  D0.L = long word
;         D1.L = row
;         D2.L = column
;         D4.L = no. of digits - 1
; output: nothing
;----------------------------------------------------------------------------
hexlib::put_hex		equ	hexlib@0002
