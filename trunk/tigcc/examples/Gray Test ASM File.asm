; Gray test program from DoorsOS

	include "doorsos.h"
	include "graphlib.h"
	include "userlib.h"
	xdef	_ti89
	xdef	_ti92plus
	xdef	_comment
	xdef	_main
_main:

	jsr	graphlib::gray4	;switches to 4 grayshades mode
	move.w	#1,graphlib::choosescreen	;set all graphlib functions to grayscale mode

	move.l	graphlib::plane0,a1	;adress of the 2nd bitplane
	jsr	graphlib::clr_scr	;clears the main screen (1st bitplane)
	move.l	graphlib::plane1,a1	;adress of the 2nd bitplane
	jsr	graphlib::clr_scr	;clears the second bitplane


	move.l	graphlib::plane0,a0	;adress of the 1st bitplane
	lea	1500(a0),a0		;50th line
	move.w	#374,d0
\loop1	move.l	#$FFFFFFFF,(a0)+	;put black over the next 50 lines
	dbra	d0,\loop1


	move.l	graphlib::plane1,a0	;adress of the 2nd bitplane
	lea	750(a0),a0		;25th line
	move.w	#374,d0
\loop2	move.w	#$FFFF,(a0)+		;put black over the next 25 lines
	dbra	d0,\loop2

	move.l	graphlib::plane1,a0	;adress of the 2nd bitplane
	lea	2250(a0),a0		;75th line
	move.w	#374,d0
\loop3	move.w	#$FFFF,(a0)+		;put black over the next 25 lines
	dbra	d0,\loop3
;so we get:
;25 lines : white
;25 lines : low gray
;25 lines : dark gray
;25 lines : black

	clr.w	graphlib::choosescreen	;set all graphlib functions to Black and White mode
	jsr	userlib::idle_loop
	jsr	graphlib::gray2		;restores Black and White mode
	rts


_comment   dc.b    "Gray4 test",0
        end
