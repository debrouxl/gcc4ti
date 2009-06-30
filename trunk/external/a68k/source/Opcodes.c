/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*                Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*          Opcode table and scan routine - April 16, 1991	    */
/*								    */
/*   This program may be copied for personal, non-commercial use    */
/*   only, provided that the above copyright notice is included	    */
/*   on all copies of the source code.  Copying for any other use   */
/*   without the consent of the author is prohibited.		    */
/*								    */
/*------------------------------------------------------------------*/
/*								    */
/*		Originally published (in Modula-2) in		    */
/*	    Dr. Dobb's Journal, April, May, and June 1986.	    */
/*								    */
/*	 AmigaDOS conversion copyright 1991 by Charlie Gibbs.	    */
/*								    */
/*------------------------------------------------------------------*/

#include "A68kdef.h"
#include "A68kglb.h"

static int optabsize = 0;	/* Size of opcode table */
static int oplimits['Z'-'A'+2];	/* Table limits by first letter */

/* Opcode table */

struct OpTab {
    char Mnem[8];	/* Instruction mnemonic */
    int  OpBits;	/* Op code bits */
    int  AMA;		/* Address mode bits */
    int  AMB;		/* More address mode bits */
};

static struct OpTab MnemTab[] = {
	"=",     0,      0xFFFF, Equ,
	"ABCD",  0xC100, Rx911 | RegMem3 | Ry02, 0,
	"ADD",   0xD000, OpM68D, EA05y,
	"ADDA",  0xD000, OpM68A, EA05a,
	"ADDI",  0x0600, 0, Size67 | EA05e | Exten,
	"ADDQ",  0x5000, Data911, Size67 | EA05d,
	"ADDX",  0xD100, Rx911 | RegMem3 | Ry02, Size67,
	"AND",   0xC000, OpM68D, EA05x,
	"ANDI",  0x0200, 0, Size67 | EA05e | Exten,
	"ASL",   0xE100, CntR911, 0,
	"ASR",   0xE000, CntR911, 0,
	"BCC",   0x6400, Brnch, 0,
	"BCHG",  0x0040, 0, EA05e | Exten | Bit811,
	"BCLR",  0x0080, 0, EA05e | Exten | Bit811,
	"BCS",   0x6500, Brnch, 0,
	"BEQ",   0x6700, Brnch, 0,
	"BGE",   0x6C00, Brnch, 0,
	"BGT",   0x6E00, Brnch, 0,
	"BHI",   0x6200, Brnch, 0,
	"BHS",   0x6400, Brnch, 0, /*BHS=BCC, added by Kevin Kofler in v.2.71.F3a */
	"BLE",   0x6F00, Brnch, 0,
	"BLO",   0x6500, Brnch, 0, /*BLO=BCS, added by Kevin Kofler in v.2.71.F3a */
	"BLS",   0x6300, Brnch, 0,
	"BLT",   0x6D00, Brnch, 0,
	"BMI",   0x6B00, Brnch, 0,
	"BNE",   0x6600, Brnch, 0,
	"BPL",   0x6A00, Brnch, 0,
	"BRA",   0x6000, Brnch, 0,
	"BSET",  0x00C0, 0, EA05e | Exten | Bit811,
	"BSR",   0x6100, Brnch, 0,
	"BSS",   0,      0xFFFF, BSS,
	"BTST",  0x0000, 0, EA05c | Exten | Bit811,
	"BVC",   0x6800, Brnch, 0,
	"BVS",   0x6900, Brnch, 0,
	"CHK",   0x4180, Rx911, EA05b,
	"CLR",   0x4200, 0, Size67 | EA05e,
	"CMP",   0xB000, OpM68C, EA05a,
	"CMPA",  0xB000, OpM68A, EA05a,
	"CMPI",  0x0C00, 0, Size67 | EA05e | Exten,
	"CMPM",  0xB108, Rx911 | Ry02, Size67,
	"CNOP",  0,      0xFFFF, Cnop,
	"CODE",  0,      0xFFFF, CSeg,
	"CSEG",  0,      0xFFFF, CSeg,
	"DATA",  0,      0xFFFF, DSeg,
	"DBCC",  0x54C8, DecBr, 0,
	"DBCS",  0x55C8, DecBr, 0,
	"DBEQ",  0x57C8, DecBr, 0,
	"DBF",   0x51C8, DecBr, 0,
	"DBGE",  0x5CC8, DecBr, 0,
	"DBGT",  0x5EC8, DecBr, 0,
	"DBHI",  0x52C8, DecBr, 0,
	"DBHS",  0x54C8, DecBr, 0, /*DBHS=DBCC, added by Kevin Kofler in v.2.71.F3a */
	"DBLE",  0x5FC8, DecBr, 0,
	"DBLO",  0x55C8, DecBr, 0, /*DBLO=DBCS, added by Kevin Kofler in v.2.71.F3a */
	"DBLS",  0x53C8, DecBr, 0,
	"DBLT",  0x5DC8, DecBr, 0,
	"DBMI",  0x5BC8, DecBr, 0,
	"DBNE",  0x56C8, DecBr, 0,
	"DBPL",  0x5AC8, DecBr, 0,
	"DBRA",  0x51C8, DecBr, 0,
	"DBT",   0x50C8, DecBr, 0,
	"DBVC",  0x58C8, DecBr, 0,
	"DBVS",  0x59C8, DecBr, 0,
	"DC",    0,      0xFFFF, DC,
	"DCB",   0,      0xFFFF, DCB,
	"DIVS",  0x81C0, Rx911, EA05b,
	"DIVU",  0x80C0, Rx911, EA05b,
	"DS",    0,      0xFFFF, DS,
	"DSEG",  0,      0xFFFF, DSeg,
	"END",   0,      0xFFFF, End,
	"ENDC",  0,      0xFFFF, EndC,
	"ENDIF", 0,      0xFFFF, EndC,
	"EOR",   0xB000, OpM68X, EA05e,
	"EORI",  0x0A00, 0, Size67 | EA05e | Exten,
	"EQU",   0,      0xFFFF, Equ,
	"EQUR",  0,      0xFFFF, Equr,
	"EVEN",  0,      0xFFFF, Even,
	"EXG",   0xC100, OpM37, 0,
	"EXT",   0x4800, OpM68S, 0,
	"FAR",   0,      0xFFFF, Far,
	"IDNT",  0,      0xFFFF, Idnt,
	"IFC",   0,      0xFFFF, IfC,
	"IFD",   0,      0xFFFF, IfD,
	"IFEQ",  0,      0xFFFF, IfEQ,
	"IFGE",  0,      0xFFFF, IfGE,
	"IFGT",  0,      0xFFFF, IfGT,
	"IFLE",  0,      0xFFFF, IfLE,
	"IFLT",  0,      0xFFFF, IfLT,
	"IFNC",  0,      0xFFFF, IfNC,
	"IFND",  0,      0xFFFF, IfND,
	"IFNE",  0,      0xFFFF, IfNE,
	"ILLEGAL", 0x4AFC, 0, 0,
	"INCBIN", 0,     0xFFFF, Incbin,
	"INCLUDE", 0,    0xFFFF, Include,
	"JMP",   0x4EC0, 0, EA05f,
	"JSR",   0x4E80, 0, EA05f,
	"LEA",   0x41C0, Rx911, EA05f,
	"LINK",  0x4E50, Ry02, Exten,
	"LIST",  0,      0xFFFF, DoList,
	"LSL",   0xE308, CntR911, 0,
	"LSR",   0xE208, CntR911, 0,
	"MACRO", 0,      0xFFFF, Macro,
	"MOVE",  0x0000, 0, Sz1213A | EA611,
	"MOVEA", 0x0040, Rx911, Sz1213 | EA05a,
	"MOVEM", 0x4880, 0, Size6 | EA05z | Exten,
	"MOVEP", 0x0008, OpM68R, Exten,
	"MOVEQ", 0x7000, Data07, 0,
	"MULS",  0xC1C0, Rx911, EA05b,
	"MULU",  0xC0C0, Rx911, EA05b,
	"NBCD",  0x4800, 0, EA05e,
	"NEAR",  0,      0xFFFF, Near,
	"NEG",   0x4400, 0, Size67 | EA05e,
	"NEGX",  0x4000, 0, Size67 | EA05e,
	"NOL",   0,      0xFFFF, NoList,
	"NOLIST",0,      0xFFFF, NoList,
	"NOP",   0x4E71, 0, 0,
	"NOT",   0x4600, 0, Size67 | EA05e,
	"OR",    0x8000, OpM68D, EA05x,
	"ORG",   0,      0xFFFF, Org,
	"ORI",   0x0000, 0, Size67 | EA05e | Exten,
	"PAGE",  0,      0xFFFF, Page,
	"PEA",   0x4840, 0, EA05f,
	"PUBLIC",0,      0xFFFF, Public,
	"REG",   0,      0xFFFF, Reg,
	"RESET", 0x4E70, 0, 0,
	"ROL",   0xE718, CntR911, 0,
	"ROLX",  0xE510, CntR911, 0, /*ROLX=ROXL, added by Kevin Kofler in v.2.71.F3a */
	"ROR",   0xE618, CntR911, 0,
	"RORG",  0,      0xFFFF, Org,
	"RORX",  0xE410, CntR911, 0, /*RORX=ROXR, added by Kevin Kofler in v.2.71.F3a */
	"ROXL",  0xE510, CntR911, 0,
	"ROXR",  0xE410, CntR911, 0,
	"RTE",   0x4E73, 0, 0,
	"RTR",   0x4E77, 0, 0,
	"RTS",   0x4E75, 0, 0,
	"SBCD",  0x8100, Rx911 | RegMem3 | Ry02, 0,
	"SCC",   0x54C0, 0, EA05e,
	"SCS",   0x55C0, 0, EA05e,
	"SECTION", 0,    0xFFFF, Section,
	"SEQ",   0x57C0, 0, EA05e,
	"SET",   0,      0xFFFF, Set,
	"SF",    0x51C0, 0, EA05e,
	"SGE",   0x5CC0, 0, EA05e,
	"SGT",   0x5EC0, 0, EA05e,
	"SHI",   0x52C0, 0, EA05e,
	"SHS",   0x54C0, 0, EA05e, /*SHS=SCC, added by Paul Froissart in v.2.71.F3c*/
	"SLE",   0x5FC0, 0, EA05e,
	"SLO",   0x55C0, 0, EA05e, /*SLO=SCS, added by Paul Froissart in v.2.71.F3c*/
 	"SLS",   0x53C0, 0, EA05e,
	"SLT",   0x5DC0, 0, EA05e,
	"SMI",   0x5BC0, 0, EA05e,
	"SNE",   0x56C0, 0, EA05e,
	"SPC",   0,      0xFFFF, Space,
	"SPL",   0x5AC0, 0, EA05e,
	"ST",    0x50C0, 0, EA05e,
	"STOP",  0x4E72, 0, Exten,
	"SUB",   0x9000, OpM68D, EA05y,
	"SUBA",  0x9000, OpM68A, EA05a,
	"SUBI",  0x0400, 0, Size67 | EA05e | Exten,
	"SUBQ",  0x5100, Data911, Size67 | EA05d,
	"SUBX",  0x9100, Rx911 | RegMem3 | Ry02, Size67,
	"SVC",   0x58C0, 0, EA05e,
	"SVS",   0x59C0, 0, EA05e,
	"SWAP",  0x4840, Ry02, 0,
	"TAS",   0x4AC0, 0, EA05e,
	"TITLE", 0,      0xFFFF, Title,
	"TRAP",  0x4E40, Data03, 0,
	"TRAPV", 0x4E76, 0, 0,
	"TST",   0x4A00, 0, Size67 | EA05e,
	"TTL",   0,      0xFFFF, Title,
	"UNLK",  0x4E58, Ry02, 0,
	"XDEF",  0,      0xFFFF, Xdef,
	"XREF",  0,      0xFFFF, Xref,
	"",0,0,0};		/* End-of-table flag */



int Instructions (loc) int loc;
/* Looks up opcode and addressing mode bit patterns
   If the opcode corresponds to an executable instruction,
     returns TRUE with the following fields set up:
	Op       - operation code bits
	AdrModeA - addressing mode bits
	AdrModeB - more addressing mode bits
	Dir      - None
   If the opcode corresponds to a directive (AdrModeA in the table
     is 0xFFFF), returns TRUE with the following fields set up:
	Op       - 0
	AdrModeA - 0
	AdrModeB - 0
	Dir      - the appropriate directive value
   If not found, returns FALSE with all the above fields set to zero.

   NOTE: The binary search doesn't use strcmp because this function
    returns incorrect values under MS-DOS Lattice 2.12.		      */
{
    register char *i, *j, ch;
    register int  lower, upper, mid;	/* Binary search controls */


    if (optabsize == 0) {	/* Determine size of opcode table. */
	while (MnemTab[optabsize].Mnem[0])
	    optabsize++;
	oplimits[0] = 0;
	oplimits['Z'-'A'+1] = optabsize;
	mid = 0;
	for (lower = 0; lower < optabsize; lower++) {
	    upper = (unsigned int) MnemTab[lower].Mnem[0] - 'A' + 1;
	    if (upper != mid) {
		if (upper > 0) {	/* Start of the next letter */
		    mid++;
		    while (mid < upper)
			oplimits[mid++] = lower;
		    oplimits[mid] = lower;
		}
	    }
	}
	mid++;
	while (mid < 'Z'-'A'+1) {
	    oplimits[mid++]=optabsize;	/* In case we didn't get to Z */
	}
    }
    mid = (unsigned int) toupper(OpCode[0]) - 'A' + 1;
    if (mid < 0) {			/* This catches stuff like "=". */
	lower = 0;
	upper = oplimits[1];
    } else if (mid > 'Z'-'A'+1) {
	lower = upper = 0;		/* Reject this one. */
    } else {
	lower = oplimits[mid++];
	upper = oplimits[mid];
    }
    while (lower < upper) {
	mid = (lower + upper) / 2;	/* Search the opcode table. */
	for (i = OpCode, j = MnemTab[mid].Mnem; (ch = toupper(*i)) == *j; i++, j++)
	    if (ch == '\0')
		break;		/* Find the first non-match. */
	if (ch < *j)
	    upper = mid;	/* Search lower half of table. */
	else if (ch > *j)
	    lower = mid + 1;	/* Search upper half of table. */
	else if (MnemTab[mid].AMA != 0xFFFF) {	/* Found it. */
	    Op = MnemTab[mid].OpBits;	/* Executable instruction */
	    AdrModeA = MnemTab[mid].AMA;
	    AdrModeB = MnemTab[mid].AMB;
	    Dir = None;
	    return (TRUE);
	} else {
	    Op = AdrModeA = AdrModeB = 0;	/* Directive */
	    Dir = MnemTab[mid].AMB;
	    return (TRUE);
	}
    }
    Op = AdrModeA = AdrModeB = Dir = 0;
    return (FALSE);			/* We didn't find it. */
}
