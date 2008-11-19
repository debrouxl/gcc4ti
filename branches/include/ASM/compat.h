tios::ST_eraseHelp equ _ROM_CALL_0E3
tios::ST_showHelp equ _ROM_CALL_0E6
tios::HeapFree equ _ROM_CALL_097
tios::HeapAlloc equ _ROM_CALL_090
tios::ER_catch equ _ROM_CALL_154
tios::ER_success equ _ROM_CALL_155
tios::reset_link equ _ROM_CALL_24C
tios::flush_link equ _ROM_CALL_24D
tios::tx_free equ _ROM_CALL_252
tios::transmit equ _ROM_CALL_250
tios::receive equ _ROM_CALL_24F
tios::HeapFreeIndir equ _ROM_CALL_098
tios::ST_busy equ _ROM_CALL_0E2
tios::ER_throwVar equ _ROM_CALL_153
tios::HeapRealloc equ _ROM_CALL_09D
tios::sprintf equ _ROM_CALL_053
tios::DrawStrXY equ _ROM_CALL_1A9
tios::FontSetSys equ _ROM_CALL_18F
tios::DrawTo equ _ROM_CALL_19C
tios::MoveTo equ _ROM_CALL_19D
tios::PortSet equ _ROM_CALL_1A2
tios::PortRestore equ _ROM_CALL_1A3
tios::WinActivate equ _ROM_CALL_001
tios::WinClose equ _ROM_CALL_00B
tios::WinOpen equ _ROM_CALL_01E
tios::WinStrXY equ _ROM_CALL_026
tios::HeapAllocThrow equ _ROM_CALL_093
tios::strcmp equ _ROM_CALL_271
tios::FontGetSys equ _ROM_CALL_18E
tios::strlen equ _ROM_CALL_27E
tios::strncmp equ _ROM_CALL_272
tios::strncpy equ _ROM_CALL_26D
tios::strcat equ _ROM_CALL_26E
tios::strchr equ _ROM_CALL_274
tios::memset equ _ROM_CALL_27C
tios::memcmp equ _ROM_CALL_270
tios::memcpy equ _ROM_CALL_26A
tios::memmove equ _ROM_CALL_26B
tios::_du32u32 equ _ROM_CALL_2AA
tios::_ds32s32 equ _ROM_CALL_2A8
tios::_du16u16 equ _ROM_CALL_2A6
tios::_ds16u16 equ _ROM_CALL_2A4
tios::_ru32u32 equ _ROM_CALL_2AB
tios::_rs32s32 equ _ROM_CALL_2A9
tios::_ru16u16 equ _ROM_CALL_2A7
tios::_rs16u16 equ _ROM_CALL_2A5
tios::DerefSym equ _ROM_CALL_079
tios::MenuPopup equ _ROM_CALL_03B
tios::MenuBegin equ _ROM_CALL_036
tios::MenuOn equ _ROM_CALL_03A
tios::ERD_dialog equ _ROM_CALL_151

;****************************************************************************
; defines

tios::NULL		equ	0
tios::H_NULL		equ	0
tios::RAND_MAX		equ	$7FFF

; codes for ST_busy()
ACTIVITY_IDLE		equ	0
ACTIVITY_BUSY		equ	1
ACTIVITY_PAUSED		equ	2

; codes for ER_throw()
tios::ER_STOP		equ	2
tios::ER_DIMENSION	equ	230
tios::ER_MEMORY		equ	670
tios::ER_MEMORY_DML	equ	810

; tags
tios::UNDEFINED_TAG	equ	$2A
tios::LIST_TAG		equ	$D9
tios::MATRIX_TAG	equ	$DB
tios::END_TAG		equ	$E5

tios::STOF_ESI		equ	$4000
tios::STOF_HESI		equ	$4003

;****************************************************************************
; structures

tios::SYM_ENTRY.name	equ	0	; name of symbol
tios::SYM_ENTRY.flags	equ	8	; flags
tios::SYM_ENTRY.hVal	equ	10	; handle of symbol


tios::globals	equ	$4C00
tios::kb_globals equ	_ROM_CALL_2A3+$15a
tios::Heap	equ	_ROM_CALL_02F+$104+$16
tios::ROM_base	equ	_RAM_CALL_003
tios::FindSymEntry	equ	userlib::FindSymEntry
tios::DrawCharXY	equ	userlib::DrawCharXY
tios::kb_vars		equ	_ROM_CALL_2A3+$15a
tios::ST_flags		equ	doorsos::ST_flags
tios::DEREF		equ	doorsos::DEREF
tios::ER_throw		equ	doorsos::ER_throw
tios::DEREF_SYM		equ	doorsos::DEREF_SYM

FOLDER_LIST_HANDLE	equ	doorsos::FolderListHandle
tios::DefTempHandle	equ	doorsos::DefTempHandle


;flib::find_pixel	equ	graphlib::find_pixel
;flib::pixel_on		equ	graphlib::pixel_on
;flib::pixel_off	equ	graphlib::pixel_off
;flib::pixel_chg	equ	graphlib::pixel_chg
flib::clr_scr		equ	graphlib::clr_scr2
flib::zap_screen	equ	graphlib::clr_scr
flib::idle_loop		equ	userlib::idle_loop
flib::random		equ	userlib::random
flib::rand_seed		equ	userlib::rand_seed
flib::show_dialog	equ	graphlib::show_dialog
flib::clear_dialog	equ	graphlib::clear_dialog
flib::frame_rect	equ	graphlib::frame_rect
flib::erase_rect	equ	graphlib::erase_rect


gray4lib::on		equ	graphlib::gray4
gray4lib::off		equ	graphlib::gray2
gray4lib::plane0	equ	graphlib::plane1
gray4lib::plane1	equ	graphlib::plane0

gray7lib::on		equ	graphlib::gray7
gray7lib::off		equ	graphlib::gray2
gray7lib::plane0	equ	graphlib::plane2
gray7lib::plane1	equ	graphlib::plane0
gray7lib::plane2	equ	graphlib::plane1

kernel::exec		equ	userlib::exec

EXEC_NO_MEM		equ	1
EXEC_LIB_NOT_FOUND	equ	2
EXEC_LIB_RANGE_ERR	equ	3
EXEC_NOT_EXEC		equ	4
EXEC_UNKNOWN_FORMAT	equ	4

hufflib::extract	equ	ziplib::extract
hufflib::extract_string	equ	ziplib::extract_string
hufflib::write_string	equ	ziplib::write_string
hufflib::write_string_inv equ	ziplib::write_string_inv
hufflib::check_mem	equ	ziplib::check_emem
