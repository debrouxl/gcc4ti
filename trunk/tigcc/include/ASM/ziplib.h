;----------------------------------------------------------------------------
; check_cmem()
;
; Function: Checks if there are enough memory free for
;           compression ( 2560 B needed ) **AND** for evaluating 
;	    compressed size ( 1140 needed )
;
; Input:    A0   = Pointer to data which will be compressed
;	    D0.W = Length of data which will be compressed
;
; Output:   D0.W = 0 if NOT enough memory
;             NO OTHER REGISTERS DESTROYED
;
;----------------------------------------------------------------------------
ziplib::check_cmem	equ	ziplib@0000

;----------------------------------------------------------------------------
; check_emem()
;
; Function: Checks if there are enough memory free for
;           extraction (same command as Hufflib's : check_mem)
;
; Input:    A0   = Pointer to compressed data
;
; Output:   D0.W = 0 if NOT enough memory
;	    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::check_emem	equ	ziplib@0001

;----------------------------------------------------------------------------
; eval_cmem()
;
; Function: Evaluates the mem size of the compressed file.
;
; Input:    A0   = Pointer to datas which will be compressed 
;	    D0.L = Length of datas which will be compressed
;
; Output:   D0.W = mem size of datas when they will be compressed
;		    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::eval_cmem	equ	ziplib@0002

;----------------------------------------------------------------------------
; eval_emem()
;
; Function: Evaluates the mem size of the extracted file.
;		(works only with single file archive)
;
; Input:    A0   = Pointer to datas which will be uncompressed 
;
; Output:   D0.W = mem size of datas when they will be uncompressed
;		    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::eval_emem	equ	ziplib@0003

;----------------------------------------------------------------------------
; compress()
;
; Function: compress data
;
; Input:    A0   = Pointer to uncompressed data
;           A1   = Pointer to where the compressed data should be stored
;	    D0.W = Length of datas which will be compressed
;----------------------------------------------------------------------------
ziplib::compress	equ	ziplib@0004

;----------------------------------------------------------------------------
; extract()
;
; Function: Extracts data ( in fact same routine as hufflib's one )
;
; Input:    A0   = Pointer to compressed data
;           A1   = Pointer to where the uncompressed data should be stored
;           D3.W = File number to extract (not necessary if not multifile)
;----------------------------------------------------------------------------
ziplib::extract	equ	ziplib@0005

;----------------------------------------------------------------------------
; zipfile()
;
; Function: Compress or Extract a file
;
; Input:    A0   = Pointer to file in the VAT
;	    D0.B = 0 disable commentary 
;		   1 semi-commentary (error displayed)
;		   else enable commentary (Windows, Confirmations,...)
;
; Output:        D0.B = 0 no error 				
;		   	1 can't manipulate because file is archived
;         	   	2 compressed file bigger than original
;		   	3 not enough memory
;			4 Program running
;		    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::zipfile	equ	ziplib@0006


;----------------------------------------------------------------------------
; iscomp()
;
; Function: Test if a file is compressed
;
; Input:    A0   = Pointer to file in the VAT
;
; Output:        D0.W = 0 compressed 				
;		   	1 uncompressed
;
;		    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::iscomp		equ	ziplib@000B


;----------------------------------------------------------------------------
; tempfile()
;
; Function: Extract a compressed file to a temporary handle
;
; Input:    A0   = Pointer to file in the VAT
;	    D0.B = 0 disable commentary 
;		   1 semi-commentary (error displayed)
;		   else enable commentary (Windows, Confirmations,...)
;
; Output:    	 D0.B = 0 no error 				
;		   	1 can't manipulate because file is archived, locked or hidden
;		   	3 not enough memory
;			5 File is not a compressed file
;		 D1.W = handle of the temporary file
;		    NO OTHER REGISTERS DESTROYED
;----------------------------------------------------------------------------
ziplib::tempfile	equ	ziplib@0007

;----------------------------------------------------------------------------
; extract_string()
;
; Function: Extracts a string from huffman compressed data
;
; Input:    A0   = Pointer to huffman compressed data
;           A1   = Pointer to where the string should be stored
;           D3.W = File number to extract (not necessary if not multifile)
;           D4.W = String number. First string = number 0
;----------------------------------------------------------------------------
ziplib::extract_string	equ	ziplib@0008

;----------------------------------------------------------------------------
; write_string()
;
; Function: Writes a compressed string with the current font. The string
;           should not be longer than 80 characters.
;
; Input:    A0   = Pointer to huffman compressed data
;           D0.W = x location
;           D1.W = y location
;           D3.W = File number to extract (not necessary if not multifile)
;           D4.W = String number. First string = number 0
;----------------------------------------------------------------------------
ziplib::write_string	equ	ziplib@0009

;----------------------------------------------------------------------------
; write_string_inv()
;
; Function: Writes a compressed string with the current font (inverted color)
;           The string should not be longer than 80 characters.
;
; Input:    A0   = Pointer to huffman compressed data
;           D0.W = x location
;           D1.W = y location
;           D3.W = File number to extract (not necessary if not multifile)
;           D4.W = String number. First string = number 0
;----------------------------------------------------------------------------
ziplib::write_string_inv	equ	ziplib@000A