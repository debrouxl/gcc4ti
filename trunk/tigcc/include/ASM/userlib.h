userlib::idle_loop      equ     userlib@0000
;------------------------------------------------------------
; Waits for a key input and returns the keycode value in d0
; Supports APD and shuts down if user presses <DIAMOND>+<ON>
;------------------------------------------------------------

userlib::random		equ	userlib@0001
;----------------------------------------------------------------------------
; random(void)
;
; Function: Return a pseudorandom number
;
; input:  D0.W = upper limit
; output: D0.W = random number in [0..limit-1]
;----------------------------------------------------------------------------

userlib::rand_seed	equ	userlib@0002
;----------------------------------------------------------------------------
; WORD rand_seed
;
; Random seed used by random(). You may store values to it to initialize
; the random number seed.
;----------------------------------------------------------------------------


userlib::exec		equ	userlib@0003
;----------------------------------------------------------------------------
;executes a program
;the parameter for this function is pushed on the stack
;input:	the handle of the program to execute
;output: d0.w = result
;
;Here are d0 values as result:
;0 -> everything is alright
;1 -> Not enough memory
;2 -> Lib not found
;3 -> Wrong lib version
;4 -> File format is not valid
;5 -> File is in use
;other -> file was not a Doors OS file but was run.
;----------------------------------------------------------------------------

userlib::FindSymEntry	equ	userlib@0004
userlib::DrawCharXY		equ	userlib@0005

userlib::InputStr		equ	userlib@0006
;---------------------------------------------------------------------
; InputStr(x,y,maxchar)
;
;   Inputs a string at x,y
;
;Input:d1.w = x
;	d2.w = y
;	d3.w = maxchar
;
;Output: d0.w = string lenght
;	 a0.l = adress of the string
;---------------------------------------------------------------------

userlib::getpassword		equ	userlib@0007
;---------------------------------------------------------------------
;getpassword(void)
;
;   Asks for the Doors password and return in d1 the result
;
;Input: nothing
;
;Output: d1.w = 0  -> Good password
;	Otherwise the password was bad.
;
;	NO OTHER REGISTERS DETROYED
;---------------------------------------------------------------------

userlib::changepass		equ	userlib@0008
;---------------------------------------------------------------------
;changepass(void)
;
;   Asks for the old Doors password and if it is good, asks for a new Doors password
;
;Input and Output: nothing
;
;   NO REGISTERS DESTROYED
;---------------------------------------------------------------------

userlib::lockcalc		equ	userlib@0009
;---------------------------------------------------------------------
;lockcalc(void)
;
;   Turn off the calculator. When turned on, the Doors password is asked.
;	If the password is bad, the calc is turned off again.
;
;Input: nothing
;
;Output: d4=0 -> screen wasn't restored
;
;   NO OTHER REGISTERS DESTROYED
;
;NOTE:this function requires 3840 bytes free (2000 on a 89) to save the screen
;and later restore it. If there isn't place enough, the screen wont'be restored.
;---------------------------------------------------------------------

userlib::idle_hot		equ	userlib@000A
;---------------------------------------------------------------------
;idle_hot(void)
;
;   Same as idle_loop but:
;	-pressing the F7 (Home on the 89) key will protect your calc by turning off 
;	and asking for a password
;	-pressing the F8 (Mode on the 89) key will turn off your calc
;
;Input: nothing
;
;Output: d0.w = getkey code of key pressed
;	NO OTHER REGISTERS DETROYED
;---------------------------------------------------------------------

userlib::getfreeRAM		equ	userlib@000B
;---------------------------------------------------------------------
;  getfreeRAM(void)
;
;	returns the free memory, in bytes
;
;Input: nothing
;
;Output: d0.l = number of bytes free in memory
;
;	NO OTHER REGISTERS DESTROYED
;---------------------------------------------------------------------

userlib::smallmenu		equ	userlib@000C
;-------------------------------------------------------
; smallmenu(x,y,nbitem,txtlist)
;
;	Draws a small menu. Automatically adjusts width and height of the menu, taking into account the current font, the width and the height of the text
;	This function restores the part of the screen it takes,
;  so you don't have to redraw it.
;
;Input:
;	d0.w = x
;	d1.w = y
;	d2.b = nbitem
;	a0.l = string list; adding an extra null byte between 2 strings will force
;		smallmenu to draw an horizontal line.
;
;Output:
;	d0.w = Selected Item
;	d1.w = Last key pressed
;	d2.w = 0  -> ENTER pressed
;		Otherwise, another key has been pressed
;If there is not enough memory to save the screen, the menu won't be displayed
;
;-------------------------------------------------------

userlib::getfreearchive		equ	userlib@000D
;---------------------------------------------------------------------
;  getfreearchive(void)
;
;	returns the free memory in archive, in bytes
;
;Input: nothing
;
;Output: d0.l = number of bytes free in archive memory
;
;	NO OTHER REGISTERS DESTROYED
;---------------------------------------------------------------------

userlib::set_APD			equ	userlib@000E
;---------------------------------------------------------------------
;  set_APD(short new_APD)
;
;	Sets the Auto Power Down timer to shut down the calc after <new_APD> seconds
;
;Input: d0.w = new_APD
;
;Output: nothing
;
;	NO REGISTERS DESTROYED
;---------------------------------------------------------------------

userlib::get_APD			equ	userlib@000F
;---------------------------------------------------------------------
;  short get_APD(void)
;
;	Gets the APD value (in seconds)
;
;Input: nothing
;
;Outut: d0.w = APD value
;
;	NO REGISTERS DESTROYED
;---------------------------------------------------------------------

userlib::runprog	equ	userlib@0010
;---------------------------------------------------------------------
;  short runprog(char* commandline)
;
;Input: the parameter is pushed on the stack, it is a pointer to 
;	an ASCII string containing the commandline to run
;  example: "main\doors()"
;Output: d0.w : error code. Zero means no error.
;
;---------------------------------------------------------------------
