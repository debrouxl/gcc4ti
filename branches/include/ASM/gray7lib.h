;----------------------------------------------------------------------------
; on(void)
;
; Function: Activate 7 shade gray scale display
;
; Return: D0.L = nonzero:success, zero=failure
;----------------------------------------------------------------------------
gray7lib::on		equ	gray7lib@0000

;----------------------------------------------------------------------------
; off(void)
;
; Function: Deactivate gray scale display
;
; Return: nothing
;
;  D0-D7/A0-D6 destroyed
;----------------------------------------------------------------------------
gray7lib::off		equ	gray7lib@0001

;----------------------------------------------------------------------------
; plane0: long address of bitplane 0
; plane1: long address of bitplane 1
; plane2: long address of bitplane 2 (always = LCD_MEM)
;----------------------------------------------------------------------------
gray7lib::plane0	equ	gray7lib@0002
gray7lib::plane1	equ	gray7lib@0003
gray7lib::plane2	equ	gray7lib@0004
