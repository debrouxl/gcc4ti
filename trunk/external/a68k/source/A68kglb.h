/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*		  Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*                 Global variables - April 16, 1991		    */
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

#ifdef PRIMARY
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL char SourceFN[MAXFN];	/* Source file name */
GLOBAL char HeaderFN[MAXFN];	/* Header file name (-h) */
GLOBAL char SrecFN[MAXFN];	/* Object file name (-o) */
GLOBAL char InclList[MAXLINE];	/* List of directories to search (-i) */
GLOBAL char IdntName[MAXLINE];	/* Program unit name */

struct fs {
    int  fd;		/* File handle */
    char *Buf;		/* Pointer to buffer */
    char *Ptr;		/* Current position in buffer */
    char *Lim;		/* Logical end of buffer */
};
GLOBAL struct fs In;	/* Input file */
GLOBAL struct fs Eq;	/* Equate file */
GLOBAL struct fs List;	/* Listing file */
GLOBAL struct fs Srec;	/* Object file */

/*	Command-line options	*/

GLOBAL int AllRelocs;	/* -a Output all relocs. - added by Kevin Kofler in
                     	      v.2.71.F3l */
GLOBAL int DumpSym;	/* -d Dump the symbol table. */
GLOBAL char DumpSymList[MAXLINE];	/* -d selection list */
GLOBAL int FwdProc;	/* -f Special processing for forward references */
GLOBAL int GlobalXREF;	/* -g Make all unknown globals XREFed */
GLOBAL int SuppList;	/* (neither -l nor -x) Suppress listing file. */
GLOBAL long DataOffset;	/* -m Offset to small data base (normally 32767) */
GLOBAL int NoOpt;	/* -n Suppress all optimization. */
GLOBAL int LnMax;	/* -p Maximum number of lines per page */
GLOBAL int Quiet;	/* -q Line no. display interval (0 to suppress) */
GLOBAL long optim;	/* -r Remove optimization mask - added by Paul Froissart
                                                     in v.2.71.F3c */
GLOBAL int KeepTabs;	/* -t Keep tabs in the listing file. */
GLOBAL int Unaligned;	/* -u Allow unaligned DC.W, DC.L, DCB.W, DCB.L, DS.W,
                     	      DS.L and code. - added by Kevin Kofler in
                     	      v.2.71.F3k */
GLOBAL long HashSize;	/* -w Number of entries in the hash table */
GLOBAL int XrefList;	/* -x Produce a cross-reference listing. */
GLOBAL int HashStats;	/* -y Display hashing statistics. */
GLOBAL int DebugStart;	/* -z Debug display starts here. */
GLOBAL int DebugEnd;	/* -z Debug display ends here. */

GLOBAL char TTLstring[MAXLINE];	/* Title string */

GLOBAL int  LabLine;		/* Last labeled line number */
GLOBAL int  LineCount;		/* Source line counter */
GLOBAL char Line[MAXLINE];	/* Current source line */
GLOBAL char Label[MAXLINE];	/* Instruction label */
GLOBAL char OpCode[MAXLINE];	/* Instruction mnemonic */
GLOBAL char SrcOp[MAXLINE];	/* First (source) operand */
GLOBAL char DestOp[MAXLINE];	/* Second (destination) operand */
GLOBAL int  LabLoc, OpLoc;	/* Label and mnemonic start here */
GLOBAL int  SrcLoc, DestLoc;	/* Operands start here. */
GLOBAL int  Dir, PrevDir;	/* Assembler directive */
GLOBAL int  NumSyms;		/* Number of symbols */
GLOBAL long ObjOp;		/* OpCode object code */
GLOBAL long ObjSrc;		/* Source operand object code */
GLOBAL long ObjDest;		/* Destination operand object code */
GLOBAL char ObjString[MAXLINE];	/* String data */
GLOBAL int  nO, nS, nD, nX;	/* Length of above components */
GLOBAL int  PrntAddr;		/* Print AddrCnt on listing. */
GLOBAL int  MakeHunk;		/* We must make a hunk. */
GLOBAL int  ListOff;		/* NOLIST is supressing listing lines. */
GLOBAL int  LnCnt;		/* Number of lines on current page */
GLOBAL int  PgCnt;		/* Page number */
GLOBAL long Hunk2;		/* Hunk number (from GetValue) */
GLOBAL int  DefLine2;		/* Definition line number */
GLOBAL int  SingleFlag;		/* Expression consists of a single term. */
GLOBAL int  GotEqur;		/* We have register equates. */
GLOBAL int  SmallData;		/* Register for small data model (or -1) */
GLOBAL int  AnyNear;		/* We got at least one NEAR directive. */
GLOBAL int  FwdShort;		/* Forward reference could be made short. */

GLOBAL int  Pass2;	/* Pass 2 flag */
GLOBAL long AddrCnt;	/* Location counter */
GLOBAL long AddrAdv;	/* Bump AddrCnt by this much. */
GLOBAL long DupFact;	/* Duplication factor for DCB, 1 otherwise */
GLOBAL long OrgHigh;	/* Highest address reached if we ORG backwards */
GLOBAL long OrgSeek;	/* Return here in Srec if we ORG backwards. */
GLOBAL long EndAddr;	/* END statement transfer address */
GLOBAL long SectStart;	/* Current section (or portion) starts here. */
GLOBAL int  SectLine;	/* Line number where section started */
GLOBAL int  HunkSeq;	/* Hunk sequence number */
GLOBAL long HunkType;	/* Current hunk type */
GLOBAL long HunkFlags;	/* Hunk flags (MEMF_FAST or MEMF_CHIP) */
GLOBAL long CurrHunk;	/* Current hunk number */
GLOBAL long NextHunk;	/* Next available hunk number */
GLOBAL long LenPos;	/* Seek position of current hunk length */
GLOBAL char *LenPtr;	/* Pointer to length if in buffer, else NULL */
GLOBAL int  InclErrs;	/* Error processing INCLUDE statement(s) */
GLOBAL int  InnrFMac;	/* An inner file has been read by a user macro. */
GLOBAL int  OrgFlag;	/* ORG may require object file fixup. */

GLOBAL int  SFormat;	/* Generate S-record format. */
GLOBAL long StartAddr;	/* Address that record starts on */
GLOBAL long TempAddr;	/* Address of where we are now */

GLOBAL int  IncStart;	/* Start line number of skippable INCLUDE */
GLOBAL struct InFCtl *IncPtr;	/* Copy of InF for skippable INCLUDE */
struct SkipEnt {		/* Skippable INCLUDE description */
    struct SetFixup *Set1;	/*  Pointer to first SET fixup */
    int Start;			/*  Starting line number of INCLUDE */
    int Finish;			/*  Ending line number of INCLUDE */
    int MCount;			/*  Value of MacCount at end of INCLUDE */
    int LLine;			/*  Value of LabLine at end of INCLUDE
						    -- added by Kevin Kofler in v.2.71.F3i */
};
struct SetFixup {		/* SET symbol fixup entry */
    struct SymTab *Sym;		/* Pointer to symbol table entry */
    long Val;			/* Fixup value */
    long Hunk;			/* Fixup hunk number */
};
GLOBAL struct SkipEnt *SkipLim;	/* Logical end of skippable INCLUDEs */
GLOBAL struct SkipEnt *SkipIdx;	/* Current skippable INCLUDE entry */
GLOBAL struct SetFixup *SetFixLim;	/* Next available SetFixup */

struct SymTab {				/* Symbol table */
    struct SymTab *Link;	/* Link to next entry in hash chain */
    char *Nam;	/* Pointer to symbol */
    long Val;	/* Value */
    long Hunk;	/* Hunk number (ORed with MEMF_CHIP or MEM_FAST
			if applicable SECTION
		   ~(pointer to symbol) if XREF
		   Pointer to macro text if MACRO		*/
    int  Defn;	/* Line number where defined */
    int  Flags;	/* Flags bits:	0 - XREF
				1 - XDEF
				2 - SET
				3 - MACRO (symbol is preceded by blank)
				4 - SECTION (name preceded by 2 blanks
					and 4-digit hex sequence number)
				5 - register name (EQUR)
				6 - register list (REG)
				7 - PUBLIC (XREF or XDEF will be set)
				8 - An EQU that doesn't yet know whether
				    it is an XREF, an XDEF, or just a
				    plain forward reference */
    struct Ref *Ref1;	/* Pointer to first reference entry */
};
GLOBAL struct SymTab *SymStart;	/* The symbol table starts here. */
GLOBAL struct SymTab *SymLim;	/* The symbol table ends here. */
GLOBAL struct SymTab *SymCurr;	/* Start of current chunk of data */
GLOBAL struct SymTab *Sym;	/* ReadSymTab sets this up. */
GLOBAL struct SymTab *Sect;	/* Current section's entry */
GLOBAL struct SymTab *SymChunk;	/* Current symbol chunk for NextSym */
GLOBAL struct SymTab *SymChLim;	/* End of *SymChunk chunk */
GLOBAL struct SymTab **Hash;	/* Pointer to hash table */
GLOBAL struct SymTab **SymSort;	/* Symbol sort area */
GLOBAL int  *HashCount;		/* Hashing summary table */

struct NameChunk {		/* Chunk of labels or macro text */
    struct NameChunk *Link;	/* Link to the next chunk */
    char *Data[CHUNKSIZE-sizeof(struct NameChunk *)];	/* Data area */
};
GLOBAL struct NameChunk *NameStart;	/* Start of first name chunk */
GLOBAL char             *NameLim;	/* Next available name entry */
GLOBAL struct NameChunk *NameCurr;	/* Start of current name chunk */

struct Ref {			/* Reference entry */
    struct Ref *NextRef;	/* Pointer to next reference entry */
    int RefNum[MAXREF];		/* Reference line numbers */
};
GLOBAL struct Ref *RefLim;	/* Next available reference entry */

struct RelTab {			/* Relocation table entry */
    struct RelTab *Link;	/* Link to the next entry */
    long Offset;		/* Offset to relocatable value */
    long Hunk;			/* Hunk type to relocate relative to */
    int  Size;			/* Size of relocatable value */
    int  IsPC;
/* added by Kevin Kofler in v.2.71.F3l: */
    long Target;  /* Reloc target. -1 if stored at Offset. */
    int Negative; /* Reloc is negative. */
};
GLOBAL struct RelTab *RelStart;	/* Relocation data starts here */
GLOBAL struct RelTab *RelLim;	/* Relocation data ends here */
GLOBAL struct RelTab *RelCurr;	/* Start of current chunk of data */
GLOBAL struct RelTab *RelLast;	/* Last relocation entry added */

struct TermStack {		/* Parser's term stack */
    long value;			/* Value */
    long hunk;			/* Hunk number */
    int  oploc;			/* Location in source statement */
    int  defline;		/* Line number where defined */
    int  isaddrdiff;	/* term is an address difference
                    	   added by Kevin Kofler in v.2.71.F3l */
};
GLOBAL struct TermStack *Term;	/* Term stack pointer */

struct OpStack {		/* Parser's operator stack */
    char chr;			/* Operator character */
    int  prec;			/* Precedence */
};
GLOBAL struct OpStack *Ops;	/* Operator stack pointer */

GLOBAL char OpPrec[256];	/* Operator precedence look-up table */

GLOBAL int  InFNum;		/* Current input nesting level */
struct InFCtl {
    long Pos;			/* Current position in input */
    char *UPtr;			/* Current position in user macro or 0 */
    char *NPtr;			/* File name stack pointer */
    int  Line;			/* Current line number in this file */
    int  NArg;			/* Number of macro arguments or -1 */
    int  MCnt;			/* Macro expansion number (for \@) */
};
GLOBAL struct InFCtl *InF;	/* Macro/include file stack pointer */
GLOBAL struct InFCtl *LowInF;	/* "Low-water mark" for InF */
GLOBAL char *Heap2;		/* Secondary heap */
GLOBAL char *NextFNS;		/* Next input file path/name */
GLOBAL char *High2;		/* Secondary high-water mark */
GLOBAL char *Low2;		/* Low limit from top of heap */

GLOBAL int  OuterMac;		/* Level number of outermost macro */
GLOBAL int  MacCount;		/* Number of macros expanded */
GLOBAL int  SkipNest;		/* Skipped IF/ENDC nesting count */
GLOBAL char MacSize[2];		/* Macro call size ("B", "W", or "L") */

struct OpConfig {		/* Operand configuration */
    long Value;	/* Value */
    long Hunk;	/* Hunk number */
    int  Defn;	/* Line number where defined */
    int  Mode;	/* Addressing mode */
    int  Loc;	/* Location of operand on Line */
    int  Rn;	/* Register number */
    int  Xn;	/* Index register number */
    int  Xsize;	/* Size of index */
    int  X;	/* Is index Data or Address reg? */
    int  Single; /* The operand is a single term. */
/* added by Kevin Kofler in v.2.71.F3o: */
    int  PCConv;  /* Adjustment for PC-relative operand */
/* added by Kevin Kofler in v.2.71.F3t: */
    int  Unopt;   /* Unoptimizable operand (hardcoded size) */
};
GLOBAL struct OpConfig Src, Dest;	/* Source and destination operands */

GLOBAL int Size;	/* Size for OpCode */
GLOBAL int InstSize;	/* Size of instruction, including operands */
GLOBAL int AdrModeA;	/* Addressing modes for this instruction */
GLOBAL int AdrModeB;	/*                 ditto                 */
GLOBAL int Op;		/* Raw bit pattern for OpCode */

GLOBAL struct FwdBr {	/* Forward branch optimization candidates */
    long Loc;		/* Location of Bcc instruction */
    struct SymTab *FwdSym;	/* Pointer to target label entry */
    int  Line;		/* Line number (copied to FwdTable if OK) */
} FwdBranch[64];
GLOBAL struct FwdBr *FwdLim1;	/* Logical end of FwdBranch */
GLOBAL struct SymTab *FwdBranchFix[2048]; /* Symbol table entries to fix */
/* This was: GLOBAL struct SymTab *FwdBranchFix[128];
   However, that won't work if one puts lots of labels at the same place.
   This change allows to assemble PedroM correctly.
   -- Kevin Kofler, v.2.71.F3k-pre1 */
GLOBAL struct SymTab **FwdFixLimit;	/* Logical end of fixup table */

struct FwdTable {
    struct FwdTable *Link;
    int FwdLine[(FWDSIZE-sizeof(struct FwdTable *)) / sizeof (int)];
};
GLOBAL struct FwdTable *FwdStart;	/* Start of first chunk */
GLOBAL int             *FwdLim2;	/* Next available entry */
GLOBAL struct FwdTable *FwdCurr;	/* Start of current chunk */
GLOBAL int             *FwdPtr;		/* Current position in pass 2 */

GLOBAL int ErrorCount;

/* Error message tables */
GLOBAL int ErrLim, ErrCode[ERRMAX], ErrPos[ERRMAX];

GLOBAL int isError; /* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3a */
GLOBAL int noExplicitSize; /* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b */
struct AddrDiff {
    struct AddrDiff *Link;
    long paddr, phunk, naddr, nhunk;
};
GLOBAL struct AddrDiff * AddrDiffs; /* added by Kevin Kofler in v.2.71.F3l */
