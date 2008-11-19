;----------------------------------------------------------------------------
; fline()
;
; Function: Draws a line, _FAST_
;
; Input:    D0.W = X1
;           D1.W = Y1
;           D2.W = X2
;           D3.W = Y2
;           A0   = Address to top left corner (usually $4440)
;              
;----------------------------------------------------------------------------
linelib::fline		equ	linelib@0000
