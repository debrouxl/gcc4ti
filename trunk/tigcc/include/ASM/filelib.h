;-------------------------------------------------------------
;  Filelib: file/folder operations functions
;		by the Doors Team
;	xvassor@mail.dotcom.fr   or	deucalion@mail.dotcom.fr
;		http://start.at/doors
;-------------------------------------------------------------

;Before using this library, it would be better if you read some doc
;about the TI89/TI92+ VAT (Variable Allocation Table).
;You can probably find docs at TiCalc.org
;	http://www.ticalc.org

;NOTE: If you want to use these functions on a folder
; you have to do: "moveq.w #doorsos::FolderListHandle,d0"
;NOTE2: the index of a file(or a folder) is its place in the 
;file(or folder) list.
;for example if you have the following file list:

;	filelib
;	graphlib
;	shell
;	userlib

;Then the index of filelib is 0, the index of graphlib is 1...

;When d2=result is output it has the following values:
;		0 -> error
;		1 -> OK

filelib::sortlist 	equ	filelib@0000
;--------------------------------------------------------------
;sortlist(list handle)
;
;   Sorts file/folder list d0 in alphabetical order
;
;Input:	d0.w = file/folder list handle
;
;Output: nothing
;
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::delete		equ	filelib@0001
;--------------------------------------------------------------
;delete(folder,file/folder)
;
;    Deletes the file/folder d1 in folder d0
;
;Input: d0.w = folder handle
;	d1.w = file/folder index
;
;Output: d2.w	= result
:	
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::copy		equ	filelib@0002
;--------------------------------------------------------------
;copy(folder,file,newfolder)
;
;   Copies the file d1 in folder d0 to folder d2
;
;Input:d0.w = source folder handle
;	d1.w = file index
;	d2.w = dest folder handle
;
;Output: d2.w	= result
;	NO OTHER REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::move		equ	filelib@0003
;--------------------------------------------------------------
;move(folder,file,newfolder)
;
;   Moves the file d1 in folder d0 to folder d2
;
;Input:d0.w = source folder handle
;	d1.w = file index
;	d2.w = dest folder handle
;
;Output:d2.w = result
;	NO REGISTERS DESTROYED
;
;--------------------------------------------------------------

filelib::rename		equ	filelib@0004
;--------------------------------------------------------------
;rename(folder,file/folder,newname)
;
;    Renames the file/folder d1 in folder d0 with new name a0
;
;Input: d0.w = folder handle
;	d1.w = file/folder index
;	a0.l = adress of the new name
;
;Output: d2.w = result
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::createfolder	equ	filelib@0005
;--------------------------------------------------------------
;createfolder(name)
;
;	Creates a new folder
;
;Input: a0.l = name of the folder
;
;Output: d2.w	= result
;
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::protect	equ	filelib@0006
;-------------------------------------------------------------
;protect(folder,file/folder)
;
;   Protects the file/folder d1 in folder d0 so that this file
; is unreacheable in TI OS and quite invisible in ASM programs
;   You can access it again only with the Doors shell
;
;Input: d0.w = folder handle
;	d1.w = file/folder index
;
;Output: d2.w = result
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::hide		equ	filelib@0007
;--------------------------------------------------------------
;hide(folder,file/folder)
;
;	used on a file, this function makes it disappear from TIOS
;	used on a folder, this function hides it in the Var-Link
;   But the file is always appears in ASM programs
;Input:	d0.w = folder handle
;		d1.w = file/folder index
;
;Output: nothing
;--------------------------------------------------------------

filelib::unhide		equ	filelib@0008
;--------------------------------------------------------------
;unhide(folder,file/folder)
;
;	Cancelles the effects of hide
;
;Input:	d0.w = folder handle
;		d1.w = file/folder index
;
;Output: nothing
;--------------------------------------------------------------

filelib::hdltoindex	equ	filelib@0009
;--------------------------------------------------------------
;hdltoindex(file/folder handle)
;
;   Returns the index of the file given its handle
;   It searches in all folders.
;   This function is very useful if you want to use filelib and you only 
;   possess the handle of a file
;
;Input:d2.w =file/folder handle to search
;
;Output: d1.w = file index
;	d0.w = folder handle (=0 -> the handle wasn't found)
;	NO OTHER REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::gettype	equ	filelib@000A
;--------------------------------------------------------------
;gettype(folder,file)
;
;	returns the type of the file d1 in folder d0
;
;Input: d0.w = folder handle
;	d1.w = file index
;
;Output: d2.w = type of the file
;
;The values for d2 are:
;
;ASM	->0
;LIB	->1
;PROG	->2
;FUNC	->3
;MAT	->4
;LIST	->5
;MACR	->6
;TEXT	->7
;STR	->8
;DATA	->9
;FIG	->10
;PIC	->11
;GDB	->12
;EXPR	->13
;OTHER	->14
;
;	NO OTHER REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::search		equ	filelib@000B
;--------------------------------------------------------------
;search(file)
;
;looks in all folders if the file exists, and then returns its VAT entry adress in a0
;
;Input: a0.l: pointer to the name of the file
;	d1.w : filelib::search will run d1 searches before returning, so that if different 
;	folders have the same file name, not only the first file will be found
;
;Output: a0.l: adress of the VAT entry of the file
;	d0.w:	handle of the folder of the file
;--------------------------------------------------------------

filelib::createfile		equ	filelib@000C
;--------------------------------------------------------------
;createfile(name,folder)
;
;	Creates a new file (0 bytes long)
;
;Input: a0.l = name of the file
;	d0.w  = folder handle
;
;Output: a0.l = VAT entry adress of the file
;
;	if d2.w = 0, the file was succesfully created, else:
;	1	-> Not enough memory
;	2	-> File already exists
;	3	-> Invalid name of the file
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::resizefile		equ	filelib@000D
;--------------------------------------------------------------
;resizefile(VAT Entry adress, size)
;
;	Resizes a file
;
;Input: a0.l = VAT entry adress of the file (you get it with userlib::FindSymEntry or filelib::search)
;	 d0.w = new size of the file
;
;Output: d0.w = size of the file
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::readfile		equ	filelib@000E
;--------------------------------------------------------------
;readfile(VAT entry adress, size, position, buffer)
;
;	Reads a file
;
;Input: a0.l = VAT entry adress of the file (you get it with userlib::FindSymEntry or filelib::search)
;	 d0.w = nb of bytes to read (>0)
;	 d1.w = position of the first byte of the file to read
;	 a1.l = pointer to the buffer where to place all read bytes
;
;Output: d0.w = nb of bytes correctly read
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::writefile		equ	filelib@000F
;--------------------------------------------------------------
;writefile(VAT entry adress, size, position, buffer)
;
;	Writes to a file, automatically tries to enlarge the file needed
;
;Input: a0.l = VAT entry adress of the file (you get it with userlib::FindSymEntry or filelib::search)
;	 d0.w = nb of bytes to write (>0)
;	 d1.w = position of the first byte of the file to write
;	 a1.l = pointer to the buffer containing the bytes to write
;
;Output: d0.w = nb of bytes correctly written
;	NO REGISTERS DESTROYED
;--------------------------------------------------------------

filelib::archive		equ	filelib@0010
;--------------------------------------------------------------
;archive(folder,file)
;
;	Archives the file
;
;Input: d0.w = handle of the folder containing the file
;	 d1.w = index of the file
;
;Output:
;	d2.w = result
;--------------------------------------------------------------

filelib::unarchive		equ	filelib@0011
;--------------------------------------------------------------
;archive(folder,file)
;
;	Unarchives the file
;
;Input: d0.w = hanle of the folder containing the file
;	 d1.w = index of the file
;
;Output:
;	d2.w = result
;--------------------------------------------------------------

filelib::FindSymEntry	equ	filelib@0012
filelib::topath		equ	filelib@0013
filelib::sendvar		equ	filelib@0014