;-------------------------------------------------------------
;    Graphlib: graphical functions
;		by the Doors Team
;	xvassor@mail.dotcom.fr   or	deucalion@mail.dotcom.fr
;		http://start.at/doors
;-------------------------------------------------------------

graphlib::fill	equ	graphlib@0000
;--------------------------------------------------------------
;fill(x,y,width,height,color)
;
;   Fills the rectangle delimited by x, y, width and lenght with 
;  the specified color
;
;Input:	d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = height
;	d4.w = color:	0 -> Video Invert
;			1 -> White
;			2 -> Black
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------


graphlib::put_sprite	equ	graphlib@0001
;--------------------------------------------------------------
;put_sprite(x,y,sprite)
;
;   Puts the sprite pointed to by a0 on the screen 
;  at (d0,d1)
;
;Input:	d0.w = x
;	d1.w = y
;	a0.l = adress of the sprite & the mask
;
;	Sprite format is:
; sprite:	dc.w	5 	;-> height of the sprite
;		dc.w	1	;width in bytes
;		dc.b	%11111000
;		dc.b	%01110000
;		dc.b	%00100000
;		dc.b	%01110000
;		dc.b	%11111000
;
; mask:		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------
graphlib::put_sprite2		equ	graphlib@000C
;--------------------------------------------------------------
;put_sprite(x,y,sprite,maskadress)
;
;   Puts the sprite pointed to by a0 on the screen 
;  at (d0,d1). The adress of the mask is a2
;
;Input:	d0.w = x
;	d1.w = y
;	a0.l = adress of the sprite
;	a2.l = adress of the mask
;	Sprite format is:
; sprite:	dc.w	5 	;-> height of the sprite
;		dc.w	1	;width in bytes
;		dc.b	%11111000
;		dc.b	%01110000
;		dc.b	%00100000
;		dc.b	%01110000
;		dc.b	%11111000
;
;	(...)
;
; mask:		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;		dc.b	%11111000
;
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::put_sprite_mask	equ	graphlib@000B
;--------------------------------------------------------------
;put_sprite_mask(x,y,mask,sprite)
;
;	Does the same as put_sprite, but you don't have to create
;	a mask sprite after the sprite itself
;	Instead, you have to define a 'constant mask'
;	For example %00000000 as a constant mask will make all
;	zeroes of you sprite being transparent
;
;Input:	d0.w = x
;	d1.w = y
;	d3.b = constant mask
;	a0.l = adress of the sprite & the mask
;
;	Sprite format is:
; sprite:	dc.w	5 	;-> height of the sprite
;		dc.w	1	;width in bytes
;		dc.b	%11111000
;		dc.b	%01110000
;		dc.b	%00100000
;		dc.b	%01110000
;		dc.b	%11111000
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::smallbox	equ	graphlib@0002
;--------------------------------------------------------------
;smallbox(title)
;
;  Draws a box in the middle of the screen
;
;Input:	a0.l = title of the box
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::box		equ	graphlib@0003
;--------------------------------------------------------------
;box(x,y,width,height)
;
;    Draws a box at (x,y)
;Input: d0.w = x
;	d1.w = y
;	d2.w = width
;	d3.w = height
;	a0.l = title of the box
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::frame 	equ	graphlib@0004
;--------------------------------------------------------------
;frame(x,y,width,height)
;
;  Draws a frame at (x,y)
;
;Input: d0.w = x
;	d1.w = y
;	d4.w = width
;	d5.w = height
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::clr_scr	equ	graphlib@0005
;--------------------------------------------------------------
;clr_scr(void)
;
;   clears the entire screen
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::clr_scr2	equ	graphlib@0014	
;--------------------------------------------------------------
;clr_scr(void)
;
;   clears the screen except the status line
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::vert  	equ	graphlib@0006
;--------------------------------------------------------------
;vert(x,y1,y2)
;
;   Draws a vertical line
;
;Input:	d0.w = x
;	d1.w = y1
;	d2.w = y2
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::horiz 	equ	graphlib@0007
;--------------------------------------------------------------
;horiz(x1,y,x2,color)
;
;   Draws an horizontal line in the specified color
;
;Input:	d0.w = x1
;	d1.w = y
;	d2.w = x2
;	d3.w = color  	0 -> Video Invert
;			1 -> White
;			2 -> Black
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::bigbox	equ	graphlib@0008
;--------------------------------------------------------------
;bigbox(void)
;
;  Draws a big box in the middle of the screen
;
;Input:	a0.l = title of the box
;
;Output: nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

graphlib::scrtomem	equ	graphlib@0009
;--------------------------------------------------------------
;scrtomem(x,y,lenght,width)
;
;This function copies a part of the screen to memory.
;
;Input:	d0.w = X of top left-hand corner in bytes(0<X<30)
;	d1.w = Y of top left-hand corner (0<Y<128)
;	d2.w = width in bytes (0<d2<30)  
;	d3.w = height (0<d3<128)         
;
;Output: d4.w = handle to the memory containg the part of the screen.
;	d4.w = 0 -> Not enough memory to save the screen
;	NO REGISTERS DESTROYED
;
;NOTE:This function doesn't use real coordinates because it would take much more place
;--------------------------------------------------------------

graphlib::memtoscr	equ	graphlib@000A
;--------------------------------------------------------------
;memtoscr(x,y,lenght,width,handle)
;
;This function copies a handle in memory to screen.
;
;Input:	d0.w = X of top left-hand corner in bytes(0<X<30)
;	d1.w = Y of top left-hand corner (0<Y<128)
;	d2.w = width in bytes (0<d2<30) 
;	d3.w = height (0<d3<128)	
;	d4.w = handle previously created by memtoscr(containing the
;					part of the screen to restore)
;
;Output:nothing
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

;--------------------------------------------------------------
; line()
;
; Function: Draws a line, _FAST_
;
; Input:    D0.W = X1
;           D1.W = Y1
;           D2.W = X2
;           D3.W = Y2
;           A0   = Address to top left corner (usually $4C00)
;              
;----------------------------------------------------------------------
graphlib::line		equ	graphlib@0017

graphlib::choosescreen	equ	graphlib@000D
;--------------------------------------------------------------
;	graphlib::choosescreen is a word sized variable.
;
;	If it is null, the adress of the screen for every graphlib
;	 function will be LCD_MEM
;	Else, the adress of the bitplane you want graphlib to use on
;	 will be stored in a1
;
;	This allows you, for example, to use all graphlib functions
;	 with grayscale !
;--------------------------------------------------------------


graphlib::show_dialog equ	graphlib@0015
;----------------------------------------------------------------------------
; show_dialog()
;
; Function: Displays a dialog box.
;
; input: A6=pointer to dialog struct
; output: nothing
;
;Dialog struct:
;0(a6).w : x1 (top-left)
;2(a6).w : y1 (top-left)
;4(a6).w : x2 (down-right)
;6(a6).w : y2 (down-right)
;8(a6).w : x of the string relative to x1
;10(a6).w :y of the string relative to y1
;12(a6).l : adress of the string
;
;example of dialog structure
;
;dial	dc.w	10,15,50,40,10,5
;	dc.l	str
;
;str	dc.b	"HELLO !",0
;----------------------------------------------------------------------------

graphlib::clear_dialog equ	graphlib@0016
;----------------------------------------------------------------------------
; clear_dialog()
;
; Function: Erases the last dialog box drawn by show_dialog(). The area
;           previously under the dialog will need to be redrawn.
;
; input:  nothing
; output: nothing
;----------------------------------------------------------------------------

graphlib::erase_rect		equ	graphlib@0018
;----------------------------------------------------------------------------
; erase_rect(rect r)
;
; Function: Fills the rectangle {r} with solid white.
;
; Return: nothing
;----------------------------------------------------------------------------

graphlib::frame_rect		equ	graphlib@0019
;----------------------------------------------------------------------------
; frame_rect(rect r)
;
; Function: Draws the rectangle frame {r}.
;
; Return: nothing
;----------------------------------------------------------------------------

graphlib::gray2	equ	graphlib@000E
;----------------------------------------------------------------------------
; gray2(void)
;
; Function: Deactivate gray scale display
;
; Return: d0 = 0
;----------------------------------------------------------------------------

graphlib::gray4	equ	graphlib@000F
;----------------------------------------------------------------------------
; gray4(void)
;
; Function: Activate 4 shade gray scale display
;
; Return: D0.L = nonzero:success, zero=failure
;----------------------------------------------------------------------------

graphlib::gray7	equ	graphlib@0010
;----------------------------------------------------------------------------
; gray7(void)
;
; Function: Activate 7 shade gray scale display
;
; Return: D0.L = nonzero:success, zero=failure
;----------------------------------------------------------------------------

graphlib::plane0	equ	graphlib@0011
;--------------------------------------------------------------
;plane0 is the adress of the 1st bitplane (always = LCD_MEM)
;--------------------------------------------------------------

graphlib::plane1	equ	graphlib@0012
;--------------------------------------------------------------
;plane1 is the adress of the 2nd bitplane
;--------------------------------------------------------------

graphlib::plane2	equ	graphlib@0013
;--------------------------------------------------------------
;plane2 is the adress of the 3rd bitplane
;--------------------------------------------------------------

graphlib::getlength	equ	graphlib@001A
; input:
; a0 : pointer to a string
; output:
; d3.w = length of the string, in pixels, in the current font
; d4.w = height of the string, in pixels, in the current font
