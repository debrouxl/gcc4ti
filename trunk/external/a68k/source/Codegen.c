/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*                Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*              Object code generator - April 16, 1991		    */
/*								    */
/*   This program may be copied for personal, non-commercial use    */
/*   only, provided that the above copyright notice is included	    */
/*   on all copies of the source code.  Copying for any other use   */
/*   without the consent of the author is prohibited.		    */
/*								    */
/*------------------------------------------------------------------*/
/*								    */
/*		Originally published (in Modula-2) in		    */
/*	    Dr. Dobb's Journal, April, May, and June 1986.          */
/*								    */
/*	 AmigaDOS conversion copyright 1991 by Charlie Gibbs.	    */
/*								    */
/*------------------------------------------------------------------*/

#include "A68kdef.h"
#include "A68kglb.h"



void GetObjectCode (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Determines the object code for the operation as well as the operands */
/* Returns each (up to 3 fields), along with the length of each.     */
{
    int  quick;
    int  ExtL;		/* Bit pattern for instruction extension word */
    long templong;
    char tempop[MAXLINE];
    register int i, j;
    struct AddrDiff *SrcAddrDiffs=0; /* added by Kevin Kofler in v.2.71.F3l */
    struct AddrDiff *DestAddrDiffs=0; /* added by Kevin Kofler in v.2.71.F3l */
    long SrcVal; /* added by Kevin Kofler in v.2.71.F3l */
/*    register char *x; */ /* removed by Paul Froissart in v.2.71.F3c */

    i = Instructions (OpLoc);		/* Analyze the opcode. */

    if (Dir != None) {
	ObjDir (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);			/* Process directives. */
	return;
    }
    if ((Label[0] != '\0') || (OpCode[0] != '\0'))
	PrntAddr = TRUE;		/* Print address, at least. */

    if (OpCode[0] == '\0') {
	MakeHunk |= PrntAddr;
	return;				/* No opcode, exit now. */
    }
    if (!i) {				/* Unrecognized opcode */
	if ((Quiet < 0) && (InF->UPtr == 0))
	    ShowLine (InF->Line);	/* Show where we are. */
	if (Size == Byte)		/* Set up \0 parameter. */
	    MacSize[0] = 'B';
	else if (Size == Long)
	    MacSize[0] = 'L';
	else
	    MacSize[0] = 'W';

	AddrAdv = InstSize = 0;
	PrntAddr = FALSE;
	Dir = MacCall;			/* Assume it's a macro call. */
	tempop[0] = ' ';
	tempop[1] = '\0';
	strcat (tempop, OpCode);	/* Prepend a blank to OpCode. */
	if (ReadSymTab (tempop)) {	/* Search for user macro. */
	    AddRef (LineCount);
	    if (InF->UPtr == 0) {	/* If we're reading from a file */
		InF->Pos = lseek (In.fd, 0L,1); /*  remember where we are. */
		InF->Pos -= In.Lim - In.Ptr;
	    }
	    if(Sym->Defn < LineCount) {	/* Only if previously defined! */
		InFNum++;
		if (--InF < LowInF)
		    LowInF = InF;
		MacCount++;
		Heap2Space (0);			/* Check for space. */
		InF->UPtr = (char *) Hunk2;	/* MACRO statement */
		InF->UPtr += strlen(InF->UPtr) + 1; /* Skip over it. */
		InF->NPtr = NextFNS;	/* New stack pointer */
		InF->Line = 0;		/* Line number in macro */
		InF->NArg=GetArgs (""); /* Get arguments. */
		InF->MCnt = MacCount;	/* Macro number */
		if (OuterMac == 0)
		    OuterMac = InFNum;	/* Outer macro */
		return;
	    }
	}
	if (!OpenIncl (OpCode, InclList)) {
	    Error (OpLoc, NoCode);	/* Couldn't open file. */
	    Dir = BadMac;		/* Invalid macro */
	    return;			/* Return to outer file. */
	}
	InFNum++;			/* Bump nesting level. */
	if (--InF < LowInF)
	    LowInF = InF;
	InF->UPtr = 0;			/* Not a user macro. */
	InF->NPtr = NextFNS;		/* New stack pointer */
	InF->Line = 0;			/* Line number in macro */
	InF->MCnt = MacCount;		/* Macro number */
	MacCount++;
	Heap2Space (0);			/* Check for space. */
	InF->NArg = GetArgs (OpCode);	/* Get arguments. */
	if (OuterMac == 0)
	    OuterMac = InFNum;		/* Outer macro */
	return;
    }

    MakeHunk = TRUE;			/* We have something for a hunk. */
    /* AddrCnt = AddrBndW (AddrCnt); */	/* It'll be word-aligned. */
	if (!Unaligned) AddrCnt = AddrBndW (AddrCnt);
	/* It will be word-aligned unless -u is passed.
	   -- Kevin Kofler, v.2.71.F3l */

    if ((AdrModeA != 0) || (AdrModeB != 0)) {
	Src.Loc = SrcLoc;
	Dest.Loc = DestLoc;
	if (SrcPC IN AdrModeB)	/* Source operand can be PC-relative. */
	    if (EA05z IN AdrModeB)
		i = 4;		/* Displacement offset is 4 for MOVEM. */
	    else
		i = 2;		/* Offset is 2 for all others. */
	else
	    i = 0;		/* PC-relative modes are not allowed. */
	GetOperand (SrcOp, &Src, i);
	SrcAddrDiffs = AddrDiffs; /* added by Kevin Kofler in v.2.71.F3l */
	AddrDiffs = 0; /* added by Kevin Kofler in v.2.71.F3l */
	GetOperand (DestOp, &Dest, EA05c IN AdrModeB ? 4 : 0);
	DestAddrDiffs = AddrDiffs; /* added by Kevin Kofler in v.2.71.F3l */
	AddrDiffs = 0; /* added by Kevin Kofler in v.2.71.F3l */
    }

    /* added by Kevin Kofler in v.2.71.F3l: */
    SrcVal=Src.Value;
    {struct AddrDiff *p=SrcAddrDiffs;
     while (p) {
       SrcVal+=p->paddr-p->naddr;
       p=p->Link;
     }
    }

    /* if ((EA05z IN AdrModeB) && !NoOpt) { */	/* MOVEM */
    if ((EA05z IN AdrModeB) && !NoOpt && (optim&OPTIM_MOVEM)) {	/* MOVEM */
/* only if not disabled by -r switch - Paul Froissart, v.2.71.F3c */
	if ((Src.Mode != MultiM) && (Dest.Mode != MultiM)) {
	    if (Src.Mode == Imm) {	/* Assume source is a register mask. */
		j = 0;	/* Flip it since A68k will flip it back again later. */
		for (i = 0; i < 16; i++)
		    if (Src.Value & (1<<i))
			j |= (1 << (15-i));
		Src.Value = j;
		Src.Mode = MultiM;
	    } else if (Dest.Mode == Imm) {
		Dest.Mode = MultiM;	/* Assume dest. is a register mask. */
	    } else {
		OpCode[4] = '\0';	/* MOVEM of a single register */
		Instructions (OpLoc);	/*  becomes a straight MOVE.  */
	    }
	}
    }
    if ((Src.Mode == Imm)		/* Immediate instructions */
    && (Src.Hunk == ABSHUNK) && (Src.Defn < LineCount)
    && !NoOpt
    &&     (((EA611 IN AdrModeB) && (Dest.Mode == DReg)	/* MOVE */
/*	    && (Size == Long) && (Src.Value >= -128) && (Src.Value <= 127)) */
	    && (Size == Long) && (SrcVal >= -128) && (SrcVal <= 127)) /* 2.71.F3l */
	|| ((EA05y IN AdrModeB)				/* ADD/SUB */
        && !SrcAddrDiffs /* added by Kevin Kofler in v.2.71.F3e */
	    && (Src.Value > 0) && (Src.Value <= 8)))) {
		strcat (OpCode, "Q");   /* Make it ADDQ/SUBQ/MOVEQ. */
		Instructions (OpLoc);
    }
    else if (!NoOpt && (optim&OPTIM_LEA) && (Dest.Mode == ARDir) && (Src.Mode == ARDisp)
    && (Src.Hunk == ABSHUNK) && (Src.Defn < LineCount)
    /* bugfix: line added by Kevin Kofler in v.2.71.F3d:
       optimize only if it can be optimized in pass 1, else it will not work! */
    && !SrcAddrDiffs /* added by Kevin Kofler in v.2.71.F3l */
    && (abs(Src.Value) <= 8) && (Src.Value != 0) && (Src.Rn == Dest.Rn)
    && !strcasecmp(OpCode, "LEA")) {		/* LEA x(An),An */
    /* optimization added by Paul Froissart in v.2.71.F3c */
	    if (Src.Value>0) strcpy (OpCode, "ADDQ");
	    else { Src.Value = -Src.Value;
	       strcpy (OpCode, "SUBQ"); }	/* make it ADDQ/SUBQ */
	    Instructions (OpLoc);
	    Src.Mode = Imm;
    }
    else if (!NoOpt && (optim&OPTIM_ADDA) && (Dest.Mode == ARDir) && (Src.Mode == Imm)
    && ((Size == Word) || ((Src.Hunk == ABSHUNK) && (Src.Defn < LineCount)))
    /* bugfix: line added by Kevin Kofler in v.2.71.F3d:
       optimize only if it can be optimized in pass 1, else it will not work! */
/*    && (abs(Src.Value) > 8) && (abs(Src.Value) <= 32767) */
    && (abs(SrcVal) > 8 || SrcAddrDiffs) && (abs(SrcVal) <= 32767)
/* use SrcVal, not Src.Value, use LEA optimization for address differences where
   ADDQ/SUBQ optimization is impossible -- Kevin Kofler, v.2.71.F3l */
    && (EA05y IN AdrModeB)) {		/* ADD or SUB */
    /* optimization added by Paul Froissart in v.2.71.F3c */
/*	    if (OpCode[1]=='S') Src.Value=-Src.Value; */
	    if (OpCode[0]=='S' || OpCode[0]=='s') {
/* C arrays START WITH ZERO!!! Also, it can be both S and s.
   - Kevin Kofler, v.2.71.F3e */
	      Src.Value=-Src.Value;
	      /* added by Kevin Kofler in v.2.71.F3l: invert all address differences */
	      {struct AddrDiff *p=SrcAddrDiffs;
	       while (p) {
	         p->phunk^=p->nhunk;p->nhunk^=p->phunk;p->phunk^=p->nhunk;
	         p->paddr^=p->naddr;p->naddr^=p->paddr;p->paddr^=p->naddr;
	         p=p->Link;
	       }
	      }
	    }
	    strcpy (OpCode, "LEA");     /* ADD #x,An x>8 becomes LEA x(An),An */
	    Instructions (OpLoc);
	    Src.Mode = ARDisp;
	    Src.Rn = Dest.Rn;    }
    else if ((Dest.Mode == ARDir) && (Src.Mode <= 12)
    && (((EA05y | EA611) IN AdrModeB)	/* ADD, SUB, or MOVE */
	|| (OpM68C IN AdrModeA))) {	/* CMP */
	    strcat (OpCode, "A");       /* ADD op,An becomes ADDA etc. */
	    Instructions (OpLoc);
    }
    else if ((Src.Mode == Imm)		/* Immediate instructions */
    && ((OpM68D | OpM68C | OpM68X) IN AdrModeA)) {
	strcat (OpCode, "I");           /* ADD/AND/OR/SUB, CMP, EOR */
	Instructions (OpLoc);		/* ADD #op,d becomes ADDI etc. */
    }
    else if ((Src.Mode == ARPost) && (Dest.Mode == ARPost)
    && (OpM68C IN AdrModeA)) {		/* CMP */
	strcat (OpCode, "M");           /* Generate CMPM if necessary. */
	Instructions (OpLoc);
    }


/*=================== Operand validation routines ====================*/

    if (Pass2) {

    /* If an immediate operand isn't absolute, it must be a long word. */

	/*if ((Src.Mode == Imm) && (Size != Long) && (Src.Hunk != ABSHUNK))
	    Error (SrcLoc, RelErr);*/

    /* ------- Check for instructions with too many operands. ------- */
    /* Some specialized instruction routines contain their own tests. */

	if (AdrModeA == 0) {		/* Should have only one operand. */
	    if ((AdrModeB == EA05e) || (AdrModeB == (Size67 | EA05e))) {
		if (Dest.Mode != Null)
		    Error (DestLoc, OperErr);
/*	    } else if (AdrModeB == 0) { */  /* Should have no operands. */
/*		if (Src.Mode != Null)
		    Error (SrcLoc, OperErr);
		if (Dest.Mode != Null)
		    Error (DestLoc, OperErr); De-activated for now */
	    }
	}
	if ((AdrModeA != 0) || (AdrModeB != 0)) {
	    if(Dest.Mode != NOMODE) { /* We should never find 3 operands. */
		j = DestLoc + strlen (DestOp);
		if ((Line[j] != '\0')
		&& (Line[j] != ';')
		&& (!isspace (Line[j]))) {
		    Error (j, OperErr);
		}
	    }

    /* ---------------- Check for missing operands. ----------------- */

	    if (Src.Mode == Null) {
		Error (OpLoc+strlen(OpCode), OperErr);	/* No source */
	    } else if (Dest.Mode == Null) {
		if (((ImmMode & AdrModeB) == ImmMode)
		|| (TwoOpsA IN AdrModeA)
		|| (TwoOpsB IN AdrModeB)) {
		    Error (SrcLoc+strlen(SrcOp), OperErr);  /* No dest. */
		}
	    }
	}
    }

/*====================================================================*/

    /* ---------------- Decrement and Branch (DBcc) ----------------- */

    if (DecBr IN AdrModeA) {
	if (Pass2) {
	    if (Src.Mode != DReg)
		Error (SrcLoc, ModeErr);
	    if (Dest.Value & 1)
		Error (DestLoc, AlignErr);  /* Boundary alignment error */

	    /* if (Dest.Hunk == CurrHunk) { */
	    if (Dest.Hunk == CurrHunk && !AllRelocs) {
	    /* Always let the linker worry about it in all-reloc mode
	       -- Kevin Kofler, v.2.71.F3l */
		ObjSrc = Dest.Value-AddrCnt-2;	/* Relative branch distance */
		Dest.Hunk = ABSHUNK;		/* Displacement is absolute */
	    } else
		ObjSrc = Dest.Value;	/* Let the linker worry about it. */

	    /* In all-reloc mode, we need to compute the distance here.
	       -- Kevin Kofler, v.2.71.F3o */
	    if ((AllRelocs && Dest.Hunk == CurrHunk) ?
	           ((ObjSrc-AddrCnt-2 > 32767) || (ObjSrc-AddrCnt-2 < -32768)) :
	           ((ObjSrc > 32767) || (ObjSrc < -32768)))
/*	    if ((ObjSrc > 32767) || (ObjSrc < -32768))*/
		Error (DestLoc,BraErr);	/* Too far to branch */

	    /*if (Dest.Hunk!=CurrHunk) { */	/* DBcc to another section */
	    if (Dest.Hunk!=CurrHunk || AllRelocs) {	/* DBcc to another section
	      or -a given  -- Kevin Kofler, v.2.71.F3l */
		templong = AddrCnt + 2;	/* 16-bit relocatable */
		PutRel (templong, Dest.Hunk, 2, 1);
	    }
	    ObjOp = Op | Src.Rn;

	    /* Emit destination address diffs -- Kevin Kofler, v.2.71.F3l */
	    AddrDiffs=DestAddrDiffs;
		templong = AddrCnt + 2;	/* 16-bit relocatable */
	    EmitAddrDiffs(templong, 2);
	    DestAddrDiffs=0;
	}
	/* Get rid of leftover AddrDiffs lists -- Kevin Kofler, v.2.71.F3l */
	while (SrcAddrDiffs) {
	  struct AddrDiff *next=SrcAddrDiffs->Link;
	  free(SrcAddrDiffs);
	  SrcAddrDiffs=next;
	}
    while (DestAddrDiffs) {
      struct AddrDiff *next=DestAddrDiffs->Link;
      free(DestAddrDiffs);
      DestAddrDiffs=next;
    }
	AddrAdv = 4;
	nO = nS = 2;
	return;
    }

    /* ------------ Branch (Bcc, including BRA and BSR) ------------- */

    if (Brnch IN AdrModeA) {
	if (Src.Value & 1)
	    Error (SrcLoc, AlignErr);	/* Boundary alignment error */

/*	if (!Pass2 && !NoOpt && (Size != Byte)) { */
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
	if (!Pass2 && !NoOpt && noExplicitSize && (Size != Byte)) {
/* optimize only if no explicit size was given! */
	    if ((Src.Defn < LineCount)	/* Backward branch */
	    && (Src.Hunk == CurrHunk)) {
		ObjSrc = Src.Value-AddrCnt-2;	/* Relative branch distance */
		/*Src.Hunk = ABSHUNK;*/		/* Displacement is absolute.
		  -- no, actually it sometimes isn't (all-relocs mode); if it is,
		     that's handled by the code below. -- Kevin Kofler, v.2.71.F3q */
		if ((ObjSrc != 0) && (ObjSrc >= -128) && (ObjSrc <= 127)) {
		    Size = Byte;	/* Convert to a short branch. */
		    if ((char *) FwdLim2 >= ((char *) FwdCurr + FWDSIZE)) {
			FwdCurr->Link =	/* Get a new chunk. */
			    (struct FwdTable *) malloc ((unsigned) FWDSIZE);
			if (FwdCurr->Link == NULL)
			{/* Get rid of leftover AddrDiffs lists -- Kevin Kofler, v.2.71.F3l */
				while (SrcAddrDiffs) {
				  struct AddrDiff *next=SrcAddrDiffs->Link;
				  free(SrcAddrDiffs);
				  SrcAddrDiffs=next;
				}
			    while (DestAddrDiffs) {
			      struct AddrDiff *next=DestAddrDiffs->Link;
			      free(DestAddrDiffs);
			      DestAddrDiffs=next;
			    }
			    quit_cleanup ("Out of memory!\n");
			}
			FwdCurr = FwdCurr->Link;
			FwdCurr->Link = NULL;
			FwdLim2 = (int *)
			    ((char *) FwdCurr + sizeof (struct FwdTable *));
		    }
		    *FwdLim2++ = LineCount;	/* Flag branch in pass 2. */
		}
	    } else if (Src.Single && (Src.Defn == NODEF)) {	/* Forward */
		if (!ReadSymTab (SrcOp)) {
		    /* Store candidate target. */
		    AddSymTab (SrcOp, 0L, (long)ABSHUNK, NODEF, 0);
		}
		PackFwdBranch (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Drop expired entries. */
		FwdLim1->Loc = AddrCnt;	/* Location of candidate */
		FwdLim1->FwdSym = Sym;	/* Symbol table entry for target */
		FwdLim1->Line = LineCount;	/* Line number */
		FwdLim1++;
	    }
	}

/*	if (Pass2 && !NoOpt && (Size != Byte)) { */	/* Pass 2 */
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
	if (Pass2 && !NoOpt && noExplicitSize && (Size != Byte)) {	/* Pass 2 */
/* optimize only if no explicit size was given! */
	    FwdCurr = FwdStart;	/* Scan the entire foward branch table. */
	    FwdPtr = (int *) ((char *) FwdCurr + sizeof (struct FwdTable *));
	    while (FwdPtr != FwdLim2) {
		if (*FwdPtr == LineCount) {	/* We flagged it in pass 1. */
		    Size = Byte;	/* Make it a short branch */
		    break;
		} else {
		    FwdPtr++;		/* Scan forward in table. */
		    if (FwdPtr == FwdLim2) {
			break;
		    }
		    if ((char *) FwdPtr >= ((char *) FwdCurr + FWDSIZE)) {
			FwdCurr = FwdCurr->Link;	/* Next chunk */
			FwdPtr = (int *)
			    ((char *) FwdCurr + sizeof (struct FwdTable *));
		    }
		}
	    }
	}
    /* if (Src.Hunk == CurrHunk) { */
    if (Src.Hunk == CurrHunk && !AllRelocs) {
    /* Always let the linker worry about it in all-reloc mode
       -- Kevin Kofler, v.2.71.F3l */
	    ObjSrc=Src.Value-AddrCnt-2;	/* Relative branch distance */
	    Src.Hunk = ABSHUNK;		/* Displacement is absolute. */
	    if ((Size == Byte) && (ObjSrc == 0))
		Error (SrcLoc, BccSDsp0);	/* Can't do this! */
/*	    if (Pass2 && FwdProc && (Size != Byte) 
	    && (ObjSrc >= -128) && (ObjSrc <= 129) && (ObjSrc != 2)) */

/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
	    if (Pass2 && FwdProc && (Size != Byte) 
	    && (ObjSrc >= -128) && (ObjSrc <= 129) && (ObjSrc != 2)
        && noExplicitSize)
/* flag only if no explicit size was given! */

		FwdShort=TRUE;	/* Forward reference could be made short. */
	} else {
	    ObjSrc = Src.Value;		/* Let the linker worry about it. */
/* disabled by Kevin Kofler in v.2.71.F3l: */
/*	    if (Size == Byte)
		Error (SrcLoc, BraErr);*/	/* No external short branches! */
/* added by Kevin Kofler in v.2.71.F3l: */
	    if (Size == Byte)
		ObjSrc--; /* The branch offset is relative to the byte FOLLOWING
		             this one, so adjust the offset. */
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
	    if (Src.Hunk != CurrHunk /* bugfix by Kevin Kofler in v.2.71.F3n */
	        && SrcOp[0] == '\\')
		Error (SrcLoc, Undef);	/* No external references to local labels! */
	}

	if (Size != Byte) {
	    InstSize = 4;
	    nS = 2;
	    templong = 32767;
	} else {
	    InstSize = 2;
	    Op |= (ObjSrc & 0x00FF);
	    templong = 127;
/* added by Kevin Kofler in v.2.71.F3q: adjust the offset back for the range
                                        checking below */
	    if (Src.Hunk != CurrHunk || AllRelocs)
		ObjSrc++;
	}
	/* In all-reloc mode, we need to compute the distance here.
	   -- Kevin Kofler, v.2.71.F3o */
	if ((AllRelocs && Src.Hunk == CurrHunk) ?
	       ((ObjSrc-AddrCnt-2 > templong) || (ObjSrc-AddrCnt-2 < -templong-1)) :
	       ((ObjSrc > templong) || (ObjSrc < -templong-1)))
/* bugfix by Kevin Kofler in v.2.71.F3l (the new fix above is cleaner):
	if ((Src.Hunk == CurrHunk) && ((Src.Value-AddrCnt-2 > templong)
	                               || (Src.Value-AddrCnt-2 < -templong-1))) */
/*	if ((ObjSrc > templong) || (ObjSrc < -templong-1))*/
	    Error (SrcLoc, BraErr);	/* Too far to branch */

	if (Dest.Mode != Null)
	    Error (DestLoc, OperErr);	/* No Destination operand! */

	AddrAdv = InstSize;
	ObjOp = Op;
	nO = 2;
	/* if (Src.Hunk != CurrHunk) { */	/* Bcc to another section, or external */
	if (Src.Hunk != CurrHunk || AllRelocs) {	/* Bcc to another section, or
	  external, or -a given -- Kevin Kofler, v.2.71.F3l */
	    if (GlobalXREF && Pass2 && Src.Defn == NODEF && ReadSymTab (SrcOp))
	      Src.Hunk = Sym->Hunk;

	  if (Size != Byte) { /* support for 8-bit relocs added by Kevin Kofler,
	                         v.2.71.F3l */
	    templong = AddrCnt + 2;	/* 16-bit relocatable */
	    /* Mark explicit word branches unoptimizable -- Kevin Kofler, v.2.71.F3t */
	    if (!noExplicitSize) templong |= UNOPTIMIZABLE;
	    PutRel (templong, Src.Hunk, 2, 1);
	    /* Emit source address diffs -- Kevin Kofler, v.2.71.F3l */
	    AddrDiffs=SrcAddrDiffs;
	    EmitAddrDiffs(templong, 2);
	    SrcAddrDiffs=0;
	  } else { /* short branch -> 8-bit reloc */
	    templong = AddrCnt + 1;	/* 8-bit relocatable */
	    PutRel (templong, Src.Hunk, 1, 1);
	    /* Emit source address diffs -- Kevin Kofler, v.2.71.F3l */
	    AddrDiffs=SrcAddrDiffs;
	    EmitAddrDiffs(templong, 1);
	    SrcAddrDiffs=0;
	  }
	}
	/* Get rid of leftover AddrDiffs lists -- Kevin Kofler, v.2.71.F3l */
	while (SrcAddrDiffs) {
	  struct AddrDiff *next=SrcAddrDiffs->Link;
	  free(SrcAddrDiffs);
	  SrcAddrDiffs=next;
	}
    while (DestAddrDiffs) {
      struct AddrDiff *next=DestAddrDiffs->Link;
      free(DestAddrDiffs);
      DestAddrDiffs=next;
    }
	return;
    }

    /* --------------- JSR.S and JMP.S are not valid! --------------- */

    if (((Op == JMP) || (Op == JSR)) && (Size == Byte))
	Error (OpLoc + 4, SizeErr);

/*  Uses information from Instructions & GetOperand (among others)  */
/*  to complete calculation of Object Code.                         */
/*  Op, AdrModeA, AdrModeB, Size, and Src & Dest records are all    */
/*  Global variables imported from the SyntaxAnalyzer MODULE.       */

    ExtL = 0;
    quick = FALSE;

    /* Ignore size specification on BCHG, BCLR, BSET, and BTST. */

    if (Bit811 IN AdrModeB)
	Size = Word;

    /* ------------ Check for boundary alignment errors. ------------ */
    /*  BCHG, BCLR, BSET, BTST, LEA, NBCD, PEA, Scc, TAS are exempt.  */

    if (Pass2) {
	if ((Size != Byte)
	&& (Op != LEA)
	&& (Op != PEA)
	&& !((AdrModeA == 0) && (AdrModeB == 0))
	&& !(AdrModeB & EA05c)	    /* BTST */
	&& !((AdrModeB&EA05e) && (AdrModeA==0) && !(AdrModeB&Size67))) {
	    if (Src.Value & 1)
/*		if ((Src.Mode >= ARDisp) && (Src.Mode <= PCDisX))
		    Error (SrcLoc, AlignErr);
	    if (Dest.Value & 1)
		if ((Dest.Mode >= ARDisp) && (Dest.Mode <= PCDisX))
		    Error (DestLoc, AlignErr); */
		if ((Src.Mode >= AbsW) && (Src.Mode <= PCDisp))
		    Error (SrcLoc, AlignErr);
	    if (Dest.Value & 1)
		if ((Dest.Mode >= AbsW) && (Dest.Mode <= PCDisp))
		    Error (DestLoc, AlignErr);
/* An odd displacement does NOT mean that the resulting address is odd!!!
   Odd displacements are still flagged as errors for PC+displacement (but
   NOT PC+displacement+index), since PC is always even.
   Bugfix by Julien Muchembled -- committed in v.2.71.F3a */
	}

/* Check for special cases first */

	if (Op == STOP) {
	    if (Src.Mode != Imm)
		Error (SrcLoc, ModeErr);
	    if (Dest.Mode != Null)
		Error (DestLoc, OperErr);
	}

	if (Op == LINK) {
	    Op |= Src.Rn;
	    if (Src.Mode != ARDir)
		Error (SrcLoc, ModeErr);
	    if (Dest.Mode != Imm)
		Error (DestLoc, ModeErr);
	    else if (Dest.Value & 1)
		Error (DestLoc, AlignErr);  /* Boundary alignment error */
	}

	if (Op == SWAP) {
	    if (!(EA05f IN AdrModeB)) {	/* Ignore if PEA instruction. */
		Op |= Src.Rn;
		if (Src.Mode != DReg)
		    Error (SrcLoc, ModeErr);
		if (Dest.Mode != Null)
		    Error (DestLoc, OperErr);
	    }
	}

	if (Op == UNLK) {
	    Op |= Src.Rn;
	    if (Src.Mode != ARDir)
		Error (SrcLoc, ModeErr);
	    if (Dest.Mode != Null)
		Error (DestLoc, OperErr);
	}

/* Now do generalized address modes */

	if ((Ry02 IN AdrModeA)&&(Rx911 IN AdrModeA)) {	/* Two registers */
	    if (Op == CMPM) {		/* Special routine for CMPM */
		Op |= Src.Rn | (Dest.Rn << 9);
		if (Src.Mode != ARPost)
		    Error (SrcLoc, ModeErr);
		if (Dest.Mode != ARPost)
		    Error (DestLoc, ModeErr);
	    } else {		/* Other two-register instructions */
		Op |= Src.Rn | (Dest.Rn << 9);
		if (RegMem3 IN AdrModeA) {
		    if (Src.Mode == DReg) {
			if (Dest.Mode != DReg)
			    Error (DestLoc, ModeErr);
		    } else if (Src.Mode == ARPre) {
			Op |= 0x0008;
			if (Dest.Mode != ARPre)
			    Error (DestLoc, ModeErr);
		    } else
			Error (SrcLoc, OperErr);
		} else {
		    if (Src.Mode == ARPost)
			if (Dest.Mode != ARPost)
			    Error (DestLoc, ModeErr);
			else
			    Error (SrcLoc, OperErr);
		}
	    }
	}
    }
    if (Data911 IN AdrModeA) {		/* Data in 9-11 (ADDQ/SUBQ) */
	quick = TRUE;
	if (Src.Mode == Imm)
	    if ((Src.Value > 0) && (Src.Value <= 8)) {
		if (Src.Value < 8)	/* Data of 8 is coded as 000. */
		    Op |= Src.Value << 9;
	    } else
		Error (SrcLoc, SizeErr);
	else
	    Error (SrcLoc, ModeErr);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (SrcAddrDiffs) Error (SrcLoc, RelErr);
    }

    if (CntR911 IN AdrModeA) {		/* Only Shift and Rotate use this. */
	if (Dest.Mode == DReg) {
	    Op = (Op & 0xF9FF) | Dest.Rn;
	    if (Size == Word) Op |= 0x0040;
	    if (Size == Long) Op |= 0x0080;
	    if (Src.Mode == DReg)
		Op |= 0x0020 | (Src.Rn << 9);
	    else if (Src.Mode == Imm) {
		quick = TRUE;
		if ((Src.Value > 0) && (Src.Value <= 8)) { /* Range check */
		    if (Src.Value < 8)	/* Data of 8 is coded as 000. */
			Op |= (Src.Value << 9);
		} else
		    Error (SrcLoc, SizeErr);
	    } else
		Error (SrcLoc, OperErr);
		/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
		if (SrcAddrDiffs) Error (SrcLoc, RelErr);
	} else if (Dest.Mode == Null) {
	    Op = (Op & 0xFFE7) | 0x00C0;
	    EffAdr (&Src, (mea | aea));
	} else
	    Error (SrcLoc, ModeErr);
    }

    if (Data03 IN AdrModeA) {		/* TRAP Vector in 0-3 */
	quick = TRUE;
	if (Src.Mode == Imm)
	    if ((Src.Value >= 0) && (Src.Value < 16))
		Op |= Src.Value;
	    else
		Error (SrcLoc, SizeErr);
	else
	    Error (SrcLoc, ModeErr);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (SrcAddrDiffs) Error (SrcLoc, RelErr);

	if (Dest.Mode != Null)
	    Error (DestLoc, OperErr);
    }

    if (Data07 IN AdrModeA) {		/* Data in 0-7 (MOVEQ) */
	quick = TRUE;
	Op |= (Src.Value & 0x00FFL) | (Dest.Rn << 9);
	/* Emit source address diffs -- Kevin Kofler, v.2.71.F3l */
	templong = AddrCnt + 1;	/* 8-bit absolute */
	AddrDiffs=SrcAddrDiffs;
	EmitAddrDiffs(templong, 1);
	SrcAddrDiffs=0;

	if (Src.Mode != Imm)
	    Error (SrcLoc, ModeErr);
	else if (Dest.Mode != DReg)
	    Error (DestLoc, ModeErr);
/*	else if ((Src.Value < -128) || (Src.Value > 127)) */
	else if ((SrcVal < -128) || (SrcVal > 127))
	    Error (SrcLoc, SizeErr);
/* use SrcVal, not Src.Value -- Kevin Kofler, v.2.71.F3l */
    }

    if (Pass2) {
	if (OpM68D IN AdrModeA) {	/* DReg in 6-8 (ADD/AND/OR/SUB) */
	    if (Dest.Mode == DReg) {
		Op |= (Dest.Rn << 9);
		if ((Src.Mode == ARDir) && (Size == Byte))
		    Error (SrcLoc, SizeErr);
	    } else /* Assume Src.Mode = DReg -- errors are trapped elsewhere. */
		Op |= (Src.Rn << 9) | 0x0100;

	    if (Size == Word) Op |= 0x0040;
	    if (Size == Long) Op |= 0x0080;
	}

	if (OpM68A IN AdrModeA) {	/* AReg in 6-8 (ADDA/CMPA/SUBA) */
	    if (Dest.Mode == ARDir)
		Op |= (Dest.Rn << 9);
	    else
		Error (DestLoc, ModeErr);

	    if (Size == Byte) Error (OpLoc+5, SizeErr);
	    if (Size == Word) Op |= 0x00C0;
	    if (Size == Long) Op |= 0x01C0;
	}

	if (OpM68C IN AdrModeA) {	/* CMP (Compare) */
	    if (Dest.Mode == DReg)
		Op |= (Dest.Rn << 9);
	    else
		Error (DestLoc, ModeErr);

	    if (Size == Byte) {
		if (Src.Mode == ARDir)
		    Error (OpLoc+4, SizeErr);
	    }
	    if (Size == Word) Op |= 0x0040;
	    if (Size == Long) Op |= 0x0080;
	}

	if (OpM68X IN AdrModeA) {	/* EOR (Exclusive or) */
	    if (Src.Mode == DReg)
		Op |= (Src.Rn << 9);
	    else
		Error (SrcLoc, ModeErr);

	    if (Size == Byte) Op |= 0x0100;
	    if (Size == Word) Op |= 0x0140;
	    if (Size == Long) Op |= 0x0180;
	}

	if (OpM68S IN AdrModeA) {	/* EXT (Sign extension) */
	    if (Src.Mode == DReg)
		Op |= Src.Rn;
	    else
		Error (SrcLoc, ModeErr);

	    if (Dest.Mode != Null)
		Error (DestLoc, OperErr);

	    if (Size == Byte) Error (OpLoc+4, SizeErr);
	    if (Size == Word) Op |= 0x0080;
	    if (Size == Long) Op |= 0x00C0;
	}

	if (OpM68R IN AdrModeA) {	/* MOVEP (Register/memory) */
	    if ((Src.Mode == DReg) && (Dest.Mode == ARDisp)) {
		if (Size == Byte) Error (OpLoc+6, SizeErr);
		if (Size == Word) Op |= 0x0180;
		if (Size == Long) Op |= 0x01C0;
		Op |= (Src.Rn << 9) | Dest.Rn;
	    } else if ((Src.Mode == ARDisp) && (Dest.Mode == DReg)) {
		if (Size == Byte) Error (OpLoc+6, SizeErr);
		if (Size == Word) Op |= 0x0100;
		if (Size == Long) Op |= 0x0140;
		Op |= Src.Rn | (Dest.Rn << 9);
	    } else
		Error (SrcLoc, ModeErr);
	}

	if (OpM37 IN AdrModeA) {	/* EXG (Exchange registers) */
	    if ((Src.Mode == DReg) && (Dest.Mode == DReg))
		Op |= 0x0040 | (Src.Rn << 9) | Dest.Rn;
	    else if ((Src.Mode == ARDir) && (Dest.Mode == ARDir))
		Op |= 0x0048 | (Src.Rn << 9) | Dest.Rn;
	    else if ((Src.Mode == ARDir) && (Dest.Mode == DReg))
		Op |= 0x0088 | (Dest.Rn << 9) | Src.Rn;
	    else if ((Src.Mode == DReg) && (Dest.Mode == ARDir))
		Op |= 0x0088 | (Src.Rn << 9) | Dest.Rn;
	    else
		Error (SrcLoc, ModeErr);
	}

	if (Bit811 IN AdrModeB) {	/* Bit operations using bits 8-11 */
	    if (Src.Mode == DReg)
		Op |= 0x0100 | (Src.Rn << 9);
	    else if (Src.Mode == Imm)
		Op |= 0x0800;
	    else
		Error (SrcLoc, ModeErr);
	}

	if (Size67 IN AdrModeB) {	/* Size in bits 6-7 */
	 /* if (Size == Byte) ; No action -- bits are already zero. */
	    if (Size == Word) Op |= 0x0040;
	    if (Size == Long) Op |= 0x0080;
	}

	if (Size6 IN AdrModeB) {	/* Size in bit 6 (MOVEM) */
	    if (Size == Byte) Error (OpLoc+6, SizeErr);
	 /* if (Size == Word) ; No action -- bit is already zero. */
	    if (Size == Long) Op |= 0x0040;
	}

	if (Sz1213A IN AdrModeB) {	/* Size in 12-13 (MOVE) */
	    if (Size == Byte) Op |= 0x1000;
	    if (Size == Word) Op |= 0x3000;
	    if (Size == Long) Op |= 0x2000;
	}

	if (Sz1213 IN AdrModeB) {	/* Size in 12-13 (MOVEA) */
	    if (Dest.Mode == DReg)	/* Can't be to adata register! */
		Error(DestLoc,ModeErr);	/* (Others are caught elsewhere). */
	    Op |= (Dest.Rn << 9);
	    if (Size == Byte) Error (OpLoc+6, SizeErr);
	    if (Size == Word) Op |= 0x3000;
	    if (Size == Long) Op |= 0x2000;
	}

	if (EA05a IN AdrModeB)		/* Effective address - all */
	    if ((Dest.Mode == DReg) || (Dest.Mode == ARDir))
		EffAdr (&Src, ea);
	    else
		Error (DestLoc, ModeErr);

	if (EA05b IN AdrModeB)	/* Eff. Addr. - all except ARDir */
	    if (Dest.Mode == DReg) {
		EffAdr (&Src, dea);
		Op |= (Dest.Rn << 9);
	    } else
		Error (DestLoc, ModeErr);

	if (EA05c IN AdrModeB)		/* BTST */
	    if (Src.Mode == DReg)
		EffAdr (&Dest, 0x0002);	/* All but ARDir */
	    else
		EffAdr (&Dest, 0x0802);	/* All but ARDir/Imm */

	if(EA05d IN AdrModeB) {	/* All but PC relative & immediate */
	    EffAdr (&Dest, aea);
	    if ((Dest.Mode == ARDir) && (Size == Byte))
		Error (OpLoc+5, SizeErr);
	}

	if(EA05e IN AdrModeB) {	/* All but ARDir, PC relative, Imm */
	    if (Dest.Mode == Null)
		EffAdr (&Src, (dea | aea));
	    else if (Src.Mode == Imm)
		EffAdr (&Dest, (dea | aea));
	    else if (Src.Mode == DReg) {
		if (AdrModeB == (Size67 | EA05e | Exten))
		    Error (SrcLoc, ModeErr);
		EffAdr (&Dest, (dea | aea));
	    } else
		Error (SrcLoc, ModeErr);
	}

	if (EA05f IN AdrModeB) {	/* JMP, JSR, LEA, and PEA */
	    EffAdr (&Src, cea);
	    if (Rx911 IN AdrModeA)
		if (Dest.Mode == ARDir)
		    Op |= (Dest.Rn << 9);   /* Address Reg. for LEA */
		else
		    Error(DestLoc,ModeErr); /* Must load Address Reg. */
	    else
		if (Dest.Mode != Null)
		    Error (DestLoc, OperErr);	/* No Dest. unless LEA */
	}

	if (EA05x IN AdrModeB) {	/* AND and OR */
	    if (Dest.Mode == DReg)
		EffAdr (&Src, dea);
	    else if (Src.Mode == DReg)
		EffAdr (&Dest, mea | aea);
	    else
		Error (SrcLoc, OperErr);
	}

	if (EA05y IN AdrModeB) {	/* ADD and SUB */
	    if (Dest.Mode == DReg) {
		EffAdr (&Src, ea);
		if ((Src.Mode == ARDir) && (Size == Byte))
		    Error (OpLoc+4, SizeErr);
	    } else if (Src.Mode == DReg)
		EffAdr (&Dest, (mea | aea));
	    else
		Error (SrcLoc, ModeErr);
	}
    }

    if (EA05z IN AdrModeB) {	    /* MOVEM */
	if (Pass2) {
	    if (Src.Mode == MultiM) {		/* Move to memory. */
		EffAdr (&Dest, (mea | aea | 0x0008));
		ExtL = Src.Value;
		i = (Dest.Mode == ARPre);	/* ExtL flip indicator */
	    } else if (Dest.Mode == MultiM) {	/* Move from memory. */
		Op |= 0x0400;			/* Set direction. */
		EffAdr (&Src, (mea | 0x0810));
		ExtL = Dest.Value;
		i = (Src.Mode == ARPre);
	    } else {
		Error (SrcLoc, OperErr);
		i = FALSE;
	    }
	    if (i) {				/* Flip ExtL if ARPre. */
		j = 0;
		for (i = 0; i < 8; i++)
		    j |= (ExtL & (1<<i)) << (15-i*2);
		for (i = 8; i < 16; i++)
		    j |= (ExtL & (1<<i)) >> (i*2-15);
		ExtL = j;
	    }
	}
	nO += 2;	/* Extension is part of OpCode. */
	InstSize += 2;
    }

    if (Pass2) {

	if (EA611 IN AdrModeB) {	/* Eff. Addr. in 6-11 (MOVE) */
	    if (Dest.Mode == CCR) {		/* MOVE to CCR */
		Op = 0x44C0;
		EffAdr (&Src, dea);
	    } else if (Dest.Mode == SR) {	/* MOVE to SR */
		Op = 0x46C0;
		EffAdr (&Src, dea);
	    } else if (Src.Mode	== SR) {	/* MOVE from SR */
		Op = 0x40C0;
		EffAdr (&Dest, dea | aea);
	    } else if (Dest.Mode == USP) {	/* MOVE to USP */
		Op = 0x4E60;
		if (Src.Mode == ARDir)
		    Op |= Src.Rn;
		else
		    Error (SrcLoc, ModeErr);
	    } else if (Src.Mode == USP) {	/* MOVE from USP */
		Op = 0x4E68;
		if (Dest.Mode == ARDir)
		    Op |= Dest.Rn;
		else
		    Error (DestLoc, ModeErr);
	    } else {			/* General MOVE instruction */
		EffAdr (&Src, (ea | xxx));
		if ((Size == Byte) && (Src.Mode == ARDir))
		    Error (SrcLoc, SizeErr);
		if (Src.Mode > 12)
		    Error (SrcLoc, ModeErr);
		if (((1<<(Dest.Mode-1)) IN (dea|aea)) || (Dest.Mode>12))
		    Error (DestLoc, ModeErr);
		else if (Dest.Mode < 8)	/* Register direct or indirect */
		    Op |= ((Dest.Mode - 1) << 6) | (Dest.Rn << 9);
		else		/* Absolute, PC relative, or immediate */
		    Op |= 0x01C0 | ((Dest.Mode - 8) << 9);
		OperExt (&Dest);	    /* Set up extension word. */
	    }
	}

	if ((Dest.Mode == CCR) && (Src.Mode == Imm)) {
	    if ((Size67 IN AdrModeB)
	    && (EA05e IN AdrModeB)
	    && (Exten IN AdrModeB))
		if (0x0400 IN Op)	/* not ANDI/EORI/ORI */
		    Error (DestLoc, ModeErr);
		else
		    Op = (Op & 0xFF00) | 0x003C;
	}

	if ((Dest.Mode == SR) && (Src.Mode == Imm)) {
	    if ((Size67 IN AdrModeB)
	    && (EA05e IN AdrModeB)
	    && (Exten IN AdrModeB))
		if (0x0400 IN Op)	/* not ANDI/EORI/ORI */
		    Error (DestLoc, ModeErr);
		else
		    Op = (Op & 0xFF00) | 0x007C;
	}
    }

    ObjOp = Op;
    InstSize += 2;
    nO += 2;
    if (nO > 2) {
	templong = ExtL;			/* Add extension word. */
	ObjOp = (ObjOp << 16) | (templong & 0x0000FFFFL);
    }
    if ((AdrModeA != 0) || (AdrModeB != 0)) {
	InstSize += (nS = GetInstModeSize (Src.Mode));
	ObjSrc = Src.Value;
	if ((Src.Mode == Imm) && (Size == Byte))
	    ObjSrc &= 0xFF;	/* Pad a byte value with zeros. */
	InstSize += (nD = GetInstModeSize (Dest.Mode));
	ObjDest = Dest.Value;
    }
    if (quick) {
	InstSize -= nS;		/* Source operand is in Op. */
	nS = 0;
    }

    if (Pass2) {
	if (nS!=0) {	/* Emit address diffs even when SrcOp is absolute.
	            	   -- Kevin Kofler, v.2.71.F3l */
/*	if ((nS!=0) && (Src.Hunk!=ABSHUNK)) {*/	/* SrcOp is relocatable. */
	    /* In global XREF mode, we need to handle ABSHUNK with NODEF properly. 
	       -- Kevin Kofler, v.2.71.F3t */
	    if (GlobalXREF && Src.Defn == NODEF && Src.Hunk == ABSHUNK && ReadSymTab (SrcOp))
	      Src.Hunk = Sym->Hunk;
	    if ((Src.Mode == AbsL)
	    || (Src.Mode == AbsW)
	    || (Src.Mode == ARDisp)
	    || (Src.Mode == PCDisp)
	    || (Src.Mode == Imm)) {
		templong = AddrCnt+nO;	/* 32- or 16-bit relocatable */
		/* Handle unoptimizable operands. -- Kevin Kofler, v.2.71.F3t */
		if (Src.Unopt) templong |= UNOPTIMIZABLE;
		if (Src.Hunk!=ABSHUNK) /* added by Kevin Kofler in v.2.71.F3l */
		PutRel (templong, Src.Hunk, nS, Src.Mode == PCDisp);
		/* Emit source address diffs -- Kevin Kofler, v.2.71.F3l */
		AddrDiffs=SrcAddrDiffs;
		EmitAddrDiffs(templong, nS);
		SrcAddrDiffs=0;
	    }
	    if ((Src.Mode==ARDisX) || (Src.Mode==PCDisX)) {
		templong = AddrCnt + nO + 1;	/* 8-bit relocatable */
		/* 8-bit sources are not optimizable. -- Kevin Kofler, v.2.71.F3s */
		templong |= UNOPTIMIZABLE;
		if (Src.Hunk!=ABSHUNK) /* added by Kevin Kofler in v.2.71.F3l */
		PutRel (templong, Src.Hunk, 1, Src.Mode == PCDisX);
		/* Emit source address diffs -- Kevin Kofler, v.2.71.F3l */
		AddrDiffs=SrcAddrDiffs;
		EmitAddrDiffs(templong, 1);
		SrcAddrDiffs=0;
	    }
	}
	if (nD!=0) {	/* Emit address diffs even when DestOp is absolute.
	            	   -- Kevin Kofler, v.2.71.F3l */
/*	if ((nD!=0) && (Dest.Hunk!=ABSHUNK)) {*/	/* DestOp is relocatable. */
	    /* In global XREF mode, we need to handle ABSHUNK with NODEF properly. 
	       -- Kevin Kofler, v.2.71.F3t */
	    if (GlobalXREF && Dest.Defn == NODEF && Dest.Hunk == ABSHUNK && ReadSymTab (DestOp))
	      Dest.Hunk = Sym->Hunk;
	    if ((Dest.Mode == AbsL)
	    || (Dest.Mode == AbsW)
	    || (Dest.Mode == ARDisp)
	    || (Dest.Mode == PCDisp)
	    || (Dest.Mode == Imm)) {
		templong = AddrCnt+nO+nS; /* 32- or 16-bit relocatable */
		/* Destinations are not optimizable. -- Kevin Kofler, v.2.71.F3s */
		templong |= UNOPTIMIZABLE;
		if (Dest.Hunk!=ABSHUNK) /* added by Kevin Kofler in v.2.71.F3l */
		PutRel (templong, Dest.Hunk, nD, Src.Mode == PCDisp);
		/* Emit destination address diffs -- Kevin Kofler, v.2.71.F3l */
		AddrDiffs=DestAddrDiffs;
		EmitAddrDiffs(templong, nD);
		DestAddrDiffs=0;
	    }
	    if ((Dest.Mode==ARDisX) || (Dest.Mode==PCDisX)) {
		templong = AddrCnt+nO+nS+1;	/* 8-bit relocatable */
		/* Destinations are not optimizable. -- Kevin Kofler, v.2.71.F3s */
		templong |= UNOPTIMIZABLE;
		if (Dest.Hunk!=ABSHUNK) /* added by Kevin Kofler in v.2.71.F3l */
		PutRel (templong, Dest.Hunk, 1, Src.Mode == PCDisX);
		/* Emit destination address diffs -- Kevin Kofler, v.2.71.F3l */
		AddrDiffs=DestAddrDiffs;
		EmitAddrDiffs(templong, 1);
		DestAddrDiffs=0;
	    }
	}
    }
    AddrAdv = InstSize;
	/* Get rid of leftover AddrDiffs lists -- Kevin Kofler, v.2.71.F3l */
	while (SrcAddrDiffs) {
	  struct AddrDiff *next=SrcAddrDiffs->Link;
	  free(SrcAddrDiffs);
	  SrcAddrDiffs=next;
	}
    while (DestAddrDiffs) {
      struct AddrDiff *next=DestAddrDiffs->Link;
      free(DestAddrDiffs);
      DestAddrDiffs=next;
    }
}



void PackFwdBranch (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Drops expired entries (more than 128 bytes old)
    from the forward branch optimization candidate table
    and the label fixup table.				*/
{
    register struct FwdBr *fp1, *fp2;
    register struct SymTab **fsp, **fsp2, **fsp3;

    fp2 = FwdBranch;
    while ((fp2 < FwdLim1) && (fp2->Loc < (AddrCnt - 128)))
	fp2++;			/* Find the first unexpired entry. */

    if (fp2 > FwdBranch) {		/* If we can drop anything, */
	fp1 = FwdBranch;		/*  shift active entries    */
	while (fp2 < FwdLim1) {		/*  over expired ones.      */
	    fp1->Loc    = fp2->Loc;
	    fp1->FwdSym = fp2->FwdSym;
	    fp1->Line   = fp2->Line;
	    fp1++;
	    fp2++;
	}
	FwdLim1 = fp1;	/* The table now ends here. */
    }

    if (FwdLim1 <= FwdBranch)
	FwdFixLimit = FwdBranchFix;	/* No outstanding branches */

    for (fsp = FwdBranchFix; fsp < FwdFixLimit; fsp++) {
	if ((*fsp)->Val < (AddrCnt - 128)) {
	    fsp2 = fsp3 = fsp;		/* Now drop expired label entries. */
	    fsp3++;
	    while (fsp3 < FwdFixLimit)
		*fsp2++ = *fsp3++;
	    FwdFixLimit--;
	}
    }
}
