/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*                Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*          Assembler directive processing - April 16, 1991	    */
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



int ObjDir (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Generates Object Code for Assembler Directives. */
{
    register char *s, *t;
    register int i, j;
    int  oploc;
    long templong;
    char tempop[MAXLINE], delim;
    struct SetFixup *sf;

    switch (Dir) {

    case Org:						/* ORG, RORG */
	PrntAddr = MakeHunk = TRUE;
	FwdLim1     = FwdBranch;	/* Clear forward branch controls. */
	FwdFixLimit = FwdBranchFix;
	templong = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 >= LineCount) {
	    Error (SrcLoc, FwdRef);	/* Illegal forward reference */
	    break;
	}
/*	if (OpCode[0] == 'R') {*/				/* RORG */
	if ((OpCode[0] == 'R')||(OpCode[0] == 'r')) {				/* RORG */
/* It can be both 'R' or 'r' -- Bugfix by Kevin Kofler in v.2.71.F3g. */
	    if (Hunk2 != ABSHUNK) {
		Error (SrcLoc, RelErr);	/* RORG needs an absolute value. */
		break;
	    }
	} else if ((!SFormat && (Hunk2 != CurrHunk))
	|| (SFormat && (Hunk2 != ABSHUNK))) {
	    Error (SrcLoc, RelErr);	/* We can't ORG out of a hunk. */
	    break;
	}

	if ((!Pass2 || (HunkType == HunkBSS))
	&& (templong < AddrCnt)	/* If we're ORGing to a lower address  */
	&& (AddrCnt > OrgHigh))	/*  and this is the highest we've been */
	    OrgHigh = AddrCnt;	/*  remember how far we got.           */

	AddrCnt = templong;	/* Update the location counter. */
	OrgFlag = TRUE;		/* Indicate object fixups are needed. */
	if (HunkType == HunkNone) {
	    DoSection ("", 0, "", 0, "", 0);	/* Start unnamed CODE section. */
	    MakeHunk = TRUE;
	}
	break;

    case Equ:						/* EQU */
	if (Label[0] == '\0')
	    Error (0, NeedLab);		/* We need a label. */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 == NODEF) {
	    if (GlobalXREF) {
		if (!Pass2) {
		    AddSymTab(SrcOp, 0L, 0L, LineCount, 0x101);
		    Hunk2 = Sym->Hunk = ~((long) Sym->Nam);
		}
	    }
	    else
		Error (SrcLoc, Undef);
	}
#if 0
	else if (DefLine2 >= LineCount)
	    Error (SrcLoc, FwdRef);		/* Illegal forward reference */
#endif
	Src.Hunk = Hunk2;
	PrntAddr = MakeHunk = TRUE;
	break;

    case DC:						/* DC */
/*	if ((Size == Word) || (Size == Long)) */
	if (((Size == Word) || (Size == Long)) && !Unaligned)
/* -u switch added by Kevin Kofler in v.2.71.F3k */
	    AddrCnt = AddrBndW (AddrCnt);
	s = Line + SrcLoc;
	while (!isspace(*s) && (*s != '\0') && (*s != ';')) {
	    oploc = s - Line;
	    if (((*s == '\'') || (*s == '"'))	/* String */
	    && (Size == Byte)) {
		delim = *s++;			/* Get the delimiter. */
		while (1) {
		    if (*s == '\0') {		/* No closing delimiter */
			Error (s-Line, NoStrEnd);
			break;
		    }
		    if (*s == delim) {		/* End of string? */
			if (*(++s) != delim)	/* Check next character. */
			    break;		/* End of string */
		    }	/* Otherwise it's an apostrophe in the string. */
		    Src.Hunk = ABSHUNK;		/* Absolute value */
		    ObjString[nX++] = *s++;	/* Current character */
		}
	    } else {			/* It's not a string constant. */
		s = GetField (s, SrcOp);
		ObjSrc = GetValue (SrcOp, oploc);	/* Value */
		/* In global XREF mode, we need to handle ABSHUNK with NODEF properly. 
		   -- Kevin Kofler, v.2.71.F3t */
		if (GlobalXREF && Src.Defn == NODEF && Hunk2 == ABSHUNK && ReadSymTab (SrcOp))
		  Hunk2 = Sym->Hunk;
		if ((Src.Hunk = Hunk2) != ABSHUNK) {	/* Hunk number */
		    templong = AddrCnt + nX;		/* Relocatable */
		    /* Constants are not optimizable. -- Kevin Kofler, v.2.71.F3s */
		    templong |= UNOPTIMIZABLE;
		    PutRel (templong, Hunk2, Size, 0);
		}
	    /* Emit address diffs -- Kevin Kofler, v.2.71.F3l */
	    templong = AddrCnt + nX;
	    /* Constants are not optimizable. -- Kevin Kofler, v.2.71.F3s */
	    templong |= UNOPTIMIZABLE;
	    EmitAddrDiffs(templong, Size);
		if (Size == 4) {
		    ObjString[nX++] = (ObjSrc >> 24) & 0x00FF;
		    ObjString[nX++] = (ObjSrc >> 16) & 0x00FF;
		}
		if (Size >= 2)
		    ObjString[nX++] = (ObjSrc >> 8) & 0x00FF;
		ObjString[nX++] = ObjSrc & 0x00FF;
		if (Size == 2)
		    templong = 0xFFFF0000L;
		else if (Size == 1)
		    templong = 0xFFFFFF00L;
		if (Size < 4)
		    if (((ObjSrc & templong) != 0)
		    && ((ObjSrc & templong) != templong))
			Error (s-Line, SizeErr);
	    }
	    if (*s == ',')
		s++;			/* Skip over separator. */
	}
	if ((!isspace(*s) && (*s != '\0') && (*s != ';'))
	|| (SrcLoc == 0))
	    Error (s-Line, OperErr);	/* Statement didn't end properly. */
	AddrAdv = InstSize = nX;
	PrntAddr = MakeHunk = TRUE;
	break;

    case DS:						/* DS */
	if (DestLoc != 0) {
	    Error (DestLoc, OperErr);	/* Only one operand is allowed. */
	    PrntAddr = MakeHunk = TRUE;
	    break;
	}
	AddrAdv = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 >= LineCount) {
	    Error (SrcLoc, FwdRef);	/* Illegal forward reference */
	    AddrAdv = 0;
	}
	if (Hunk2 != ABSHUNK)
	    Error (SrcLoc, AbsReq);	/* Count must be absolute. */

	if (Size == Word) {		/* Words */
/*	    AddrCnt = AddrBndW (AddrCnt); */
	    if (!Unaligned) AddrCnt = AddrBndW (AddrCnt); /* -u switch */
	    AddrAdv <<= 1;
	}
	if (Size == Long) {		/* Long words */
/*	    AddrCnt = AddrBndW (AddrCnt); */
	    if (!Unaligned) AddrCnt = AddrBndW (AddrCnt); /* -u switch */
	    AddrAdv <<= 2;
	}
	if (Pass2 && (HunkType != HunkBSS)) {	/* If this isn't      */
	    templong = AddrAdv;			/*  a BSS hunk,       */
	    while (templong >= 4) {		/*  generate zeros    */
		AppendSdata (0L, 4);		/*  to fill the area. */
		templong -= 4;
	    }
	    if (templong > 0) {
		i = templong;
		AppendSdata (0L, i);
	    }
	}
	PrntAddr = MakeHunk = TRUE;
	break;

    case Even:						/* EVEN */
	FwdLim1     = FwdBranch;	/* Clear forward branch controls. */
	FwdFixLimit = FwdBranchFix;
	AddrCnt = AddrBndW (AddrCnt);
	PrntAddr = MakeHunk = TRUE;
	break;

    case End:						/* END */
	/* bugfix by Kevin Kofler in v.2.71.F3c: END is not allowed in macros! */
	/* (I had to do this in order to correct a freeze bug with include files or
	   invalid op-code errors which only occurs when END is used in a macro.) */ 
	if (InF->UPtr) {Error (OpLoc, NoENDM); Dir=None; break;}
	/* bugfix by Kevin Kofler in v.2.71.F3c: END is not allowed in include files! */
	/* (That was buggy too. A68k simply does not expect anyone to use END
	    somewhere else than in the main program file.) */ 
	if (InFNum) {Error (OpLoc, EndErr); Dir=None; break;}
    /* (I am using the new "END statement not allowed here" error message.) */

	if (Pass2)
	    if (SrcOp[0] != '\0')
		EndAddr = GetValue (SrcOp, SrcLoc);
	    else
		EndAddr = 0;
	PrntAddr = MakeHunk = TRUE;
	break;

    case Xdef:						/* XDEF */
    case Public:					/* PUBLIC */
	if (SFormat)
	    Error (OpLoc, NotSFmt);		/* Not in S-format */
	s = Line + SrcLoc;
	while (!isspace(*s) && (*s != '\0') && (*s != ';')) {
	    oploc = s - Line;
	    s = GetField (s, SrcOp);		/* Get a symbol. */
	    if (ReadSymTab (SrcOp)) {
		if (!Pass2) {
		    if ((Sym->Flags & 0x101) == 0x101) {
			Sym->Flags &= ~0x101;
			Sym->Hunk = CurrHunk;
			Sym->Defn = NODEF;
		    }
		    if ((Sym->Flags & 0x61) == 0) {
			Sym->Flags |= 2;	/* Set XDEF flag. */
/*			if (OpCode[0] == 'P') {*/
			if ((OpCode[0] == 'P')||(OpCode[0] == 'p')) {
/* It can be both 'P' or 'p' -- Bugfix by Kevin Kofler in v.2.71.F3g. */
			    Sym->Flags |= 0x80;	/* It's defined as PUBLIC. */
			}
		    }
		    /*
		     * The following added by John Antonishek
		     * (provided a quick fix, may not be correct...)
		     * ant@cmr.ncsl.nist.gov 6/21/90
		     * Added to the official v.2.71.F3c by Kevin Kofler
		     */
		    else if (Sym->Flags == 1) { /* check if really XREF - condition
                                           added by Kevin Kofler in v.2.71.F3c */
		    /*
		     * added by John Antonishek
		     */
		    Sym->Flags &= ~1;       /* clear XREF flag. */
		    Sym->Flags |= 2;        /* Set XDEF flag. */
		    Sym->Defn = NODEF;
		    Sym->Hunk = CurrHunk;
		    }
		    /*
		     * end ant
		     */
		} else {
		    if (Sym->Defn != LineCount)	/* If not PUBLIC->XREF */
			AddRef (LineCount);	/*  it's a reference.  */
		    if (Sym->Defn == NODEF)
			Error (oploc, Undef);	/* It never got defined. */
		    else if (Sym->Flags & 0x60)
			Error (oploc, AddrErr);	/* Can't XDEF a register. */
		}
	    } else if (!Pass2) {		/* It's not yet defined. */
/*		if (OpCode[0] == 'P') { */		/* Treat PUBLIC as XREF. */
		if ((OpCode[0] == 'P')||(OpCode[0] == 'p')) {
/* It can be both 'P' or 'p' -- Bugfix by Kevin Kofler in v.2.71.F3g. */
		    AddSymTab (SrcOp, 0L, 0L ,LineCount, 0x81);
		    Sym->Hunk = ~((long) Sym->Nam);
		} else {
		    AddSymTab (SrcOp, 0L, CurrHunk, NODEF, 2);	/* XDEF */
		}
	    }
	    if (*s == ',')
		s++;				/* Skip over separator. */
	}
	break;

    case Xref:						/* XREF */
	if (SFormat)
	    Error (OpLoc, NotSFmt);		/* Not in S-format */
	s = Line + SrcLoc;
	while (!isspace(*s) && (*s != '\0') && (*s != ';')) {
	    oploc = s - Line;
	    s = GetField (s, SrcOp);
	    if (Pass2) {
		if (ReadSymTab (SrcOp)) {
		    if (Sym->Defn != LineCount) {
			AddRef (LineCount);	/* Ignore extraneous XREF. */
		    }
		}
	    } else {
		if (!ReadSymTab	(SrcOp)) {	/* Only if not defined */
		    AddSymTab (SrcOp, 0L, 0L, LineCount, 1);
		    Sym->Hunk = ~((long) Sym->Nam);
		}
	    }
	    if (*s == ',')
		s++;			/* Skip over separator. */
	}
	break;

    case Page:						/* PAGE */
	if (Pass2 && (LineCount > 1))	/* Ignore PAGE at start of file. */
	    LnCnt = LnMax;		/* Resume on a new page. */
	break;

    case DoList:					/* LIST */
	ListOff = FALSE;
	if (!Pass2 && !SuppList && (IncStart != 0)) {
	    IncStart = 0;			/* We can't      */
	    if (SkipLim->Set1 != NULL) {	/*  skip this    */
		SetFixLim = SkipLim->Set1;	/*  INCLUDE file */
		SetFixLim++;			/*  in pass 2    */
	    }					/*  (we must     */
	}					/*  list it).    */
	break;

    case NoList:					/* NOLIST */
	ListOff = TRUE;
	break;

    case Space:						/* SPC */
	if (Pass2 && !ListOff && !SuppList) {
	    if (SrcOp[0] != '\0')
		j = GetValue (SrcOp, SrcLoc);	/* Amount to space */
		/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
		if (AddrDiffs) Error (SrcLoc, RelErr);
	    else
		j = 1;				/* Default to one line. */
	    for (i = 0; i < j; i++) {
		if (LnCnt >= LnMax)
		    break;			/* Page overflow */
		xputs (&List, "\n");		/* Space one line. */
	    }
	}
	break;

    case Title:						/* TTL */
	s = Line + SrcLoc;
	t = TTLstring;
/*	while (*s && (s < (TTLstring+MAXLINE))) */
	while (*s && (t < (TTLstring+MAXLINE-1)))
/* It should be t, not s. Also make sure there is a byte left for the '\0'.
   Bugfix by Kevin Kofler in v.2.71.F3h */
	    *t++ = *s++;		/* Get title string. */
	*t = '\0';
	if (LineCount > 1) {
	    LnCnt = LnMax;		/* Skip to a new page. */
	} else {
	    if (Pass2
        && !SuppList /* Only if listing is enabled! - Kevin Kofler, v.2.71.F3h */
	    ) {
		xputs (&List, TTLstring);
		xputs (&List, "\n\n");
	    }
	    LnCnt += 2;			/* Don't skip at start of file. */
	}
	break;

    case Cnop:						/* CNOP */
	FwdLim1     = FwdBranch;	/* Clear forward branch controls. */
	FwdFixLimit = FwdBranchFix;
	i = TRUE;			/* "Error-free" flag */

	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 >= LineCount) {
	    Error (SrcLoc, FwdRef);	/* Illegal forward reference */
	    i = FALSE;
	}
	if (Hunk2 != ABSHUNK) {
	    Error (SrcLoc, AbsReq);	/* Must be absolute! */
	    i = FALSE;
	}

	ObjDest = GetValue (DestOp, DestLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (DestLoc, RelErr);
	if (DefLine2 >= LineCount) {
	    Error (DestLoc, FwdRef);	/* Illegal forward reference */
	    i = FALSE;
	}
	if (Hunk2 != ABSHUNK) {
	    Error (DestLoc, AbsReq);	/* Must be absolute! */
	    i = FALSE;
	}

	templong = ObjDest;
	while ((templong > 0) && ((templong & 1) == 0))
	    templong >>= 1;		/* Shift out low-order zeros. */
	if ((templong != 1) || (ObjDest == 1)) {
	    Error (DestLoc, OperErr);	/* DestOp must be a power of 2. */
	    i = FALSE;
	}
	if ((ObjSrc & 1) || (ObjSrc >= ObjDest)) {
	    Error (SrcLoc, OperErr);	/* SrcOp is odd or out of range. */
	    i = FALSE;
	}

	if (i) {			/* If no errors so far... */
	    AddrCnt = AddrBndW (AddrCnt);
	    templong = (AddrCnt & ~(ObjDest-1)) + ObjSrc;
	    if (templong < AddrCnt)
		templong += ObjDest;	/* We must advance to here. */
	    if ((templong - AddrCnt) < MAXLINE) {
		nX = templong - AddrCnt;
		for (j = 0; j < nX; ) {
		    ObjString[j++] = NOP / 256;
		    ObjString[j++] = NOP & 255;
		}
		AddrAdv = InstSize = nX;	/* Generate NOPs. */
		PrntAddr = MakeHunk = TRUE;
	    } else {
		Error (DestLoc, OperErr);	/* Too many NOPs */
	    }
	}
	break;

    case Include:					/* INCLUDE */
    case Incbin:					/* INCBIN */
	if (Pass2 && (Dir == Include)		/* If we can skip */
	&& (SkipIdx < SkipLim)			/*  this INCLUDE  */
	&& (LineCount == SkipIdx->Start)
    && !isError /* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3a */
    /* skip include file only if there are no errors to report! */
    ) {	                            /*  file in pass  */
	    LineCount = SkipIdx->Finish;	/*  2, do so.     */
	    MacCount = SkipIdx->MCount;
	    LabLine = SkipIdx->LLine; /* Fix LabLine -- bugfix by Kevin Kofler in
	                                                v.2.71.F3i */
	    if ((sf = SkipIdx->Set1) != NULL) {
		while ((sf >= SetFixLim) && (sf->Sym != NULL)) {
		    (sf->Sym)->Val  = sf->Val;	/* Fix up SET symbols. */
		    (sf->Sym)->Hunk = sf->Hunk;
		    sf--;
		}
	    }
	    SkipIdx++;
	    break;
	}
	if ((Quiet < 0) && (InF->UPtr == 0) && (Dir == Include))
	    ShowLine (InF->Line);	/* Show where we are. */
	s = Line + SrcLoc;
	if ((*s == '"') || (*s == '\''))
	    s++;			/* Ignore quotes. */
	t = tempop;
	while (!isspace(*s)
	&& (*s != '"')
	&& (*s != '\'')
	&& (*s != '\0'))
	    *t++ = *s++;
	*t = '\0';

	if (!OpenIncl (tempop, InclList)) {
	    Error (SrcLoc, NoIncl);	/* We couldn't open file. */
	    InclErrs = TRUE;
	    break;			/* Return to outer file. */
	}
	if (Dir == Incbin) {
	    while ((t = In.Buf + read (In.fd, In.Buf, BUFFSIZE)) > In.Buf) {
		for (s = In.Buf; s < t; s++) {
		    templong = *s;
		    AppendSdata (templong, 1);	/* Copy INCBIN file to output. */
		    AddrAdv++;			/* Bump location counter. */
		}
	    }
	    close (In.fd);			/* Close INCBIN file. */
	    In.fd = NOFD;
	    if (InF->UPtr == 0) {
		In.fd = open (InF->NPtr, 0);	/* Re-open the outer file. */
		lseek (In.fd, InF->Pos, 0);	/* Restore position in file. */
		In.Ptr = In.Lim = In.Buf;
	    }
	    break;
	}
	InFNum++;			/* Bump nesting level. */
	if (--InF < LowInF)
	    LowInF = InF;
	InF->UPtr = 0;			/* It's not a user macro. */
	InF->NPtr = NextFNS;		/* New stack pointer */
	InF->Line = 0;			/* Clear line counter. */
	InF->MCnt = MacCount;
	Heap2Space (strlen(tempop)+1);	/* Check for space. */
	strcpy (NextFNS, tempop);	/* File name */
	NextFNS	+= strlen (tempop) + 1;	/* Next available space */
	if (NextFNS > High2)
	    High2 = NextFNS;		/* Set high-water mark. */
	InF->NArg = -1;			/* Indicate it's not a macro. */

	if (!Pass2 && (SuppList || ListOff) && (IncStart == 0)) {
	    s = (char *) SkipLim + sizeof (struct SkipEnt);
	    if (s <= (char *) SetFixLim) {
		SkipLim->Set1 = NULL;	/* Save starting position */
		IncStart = LineCount;	/*  in case we can skip   */
		IncPtr = InF;		/*  this file in pass 2.  */
	    }
	}
	break;

    case Set:						/* SET */
	if (Label[0] == '\0')
	    Error (0, NeedLab);		/* Need a label */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 >= LineCount)
	    Error (SrcLoc, FwdRef);	/* Illegal forward reference */
	Src.Hunk = Hunk2;
	if (!ReadSymTab (Label))		/* Make a new entry. */
	    AddSymTab (Label, ObjSrc, Src.Hunk, LineCount, 4);
	else if (Sym->Flags & 4) {		/* Re-SET the symbol. */
	    Sym->Val = ObjSrc;			/* SET value */
	    Sym->Hunk = Src.Hunk;		/* Hunk number */
	    Sym->Defn = LineCount;		/* Statement number */
	}
	PrntAddr = MakeHunk = TRUE;

	if (!Pass2 && (IncStart != 0)) {
	    if ((sf = SkipLim->Set1) != NULL) {
		while (sf >= SetFixLim) {
		    if (sf->Sym != Sym) {
			sf--;
		    } else {
			sf->Val  = Sym->Val;	/* Update an        */
			sf->Hunk = Sym->Hunk;	/*  existing entry. */
			return (Set);
		    }
		}
	    }
	    sf = SetFixLim;
	    sf--;
	    s = (char *) SkipLim + sizeof (struct SkipEnt);
	    if (s > (char *) sf) {
		IncStart = 0;		/* There's no room for set symbol. */
		if (SkipLim->Set1 != NULL) {
		    SetFixLim = SkipLim->Set1;
		    SetFixLim++;
		}
	    } else {
		if (SkipLim->Set1 == NULL)
		    SkipLim->Set1 = sf;	/* First SET symbol in INCLUDE */
		sf->Sym  = Sym;
		sf->Val  = Sym->Val;	/* Save SET symbol value */
		sf->Hunk = Sym->Hunk;	/*  and hunk.            */
		SetFixLim = sf;
	    }
	}
	break;

    case Macro:						/* MACRO */
	if (Label[0] == '\0')
	    Error (0, NeedLab);		/* Need a label */

	s = Label;
	t = tempop;
	*t++ = ' ';			/* Prepend name with a */
	while (*s) {			/*  blank and convert  */
	    *t++ = /*toupper*/ (*s);	/*  it to upper case.  */
	    s++;
	}
	*t = '\0';

	if (!Pass2) {			/* Pass 1 */
	    if (!ReadSymTab (tempop))		/* Save MACRO name. */
		AddSymTab (tempop, 0L, 0L, LineCount, 8);
	    Sym->Hunk = (long) AddName(Line,1);	/* Save MACRO stmt. */
	} else {			/* Pass 2 */
	    ReadSymTab (tempop);
	    if (Sym->Defn != LineCount) {
		Error (LabLoc, DupMac);		/* Duplicate MACRO */
		AddRef (LineCount);
	    }
	    WriteListLine (&List);		/* Echo MACRO. */
	}

	i = 0;				/* IF nest counter */
	while (1) {			/* Process macro body. */
	    if (LineParts (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/)) {
		Error (OpLoc, NoENDM);	/* Premature EOF */
		i = 0;
		break;
	    }
	    if ((i += CountNest (OpCode)) < 0) {
		Error (OpLoc,ManyENDC);	/* Unmatched ENDC */
		i = 0;
	    }
	    if (!Pass2)
		AddName (Line, 2);	/* Store a line. */
	    if (strcasecmp (OpCode, "ENDM") == 0)
		break;			/* Main program echoes ENDM. */
	    if (Pass2)
		WriteListLine (&List);	/* Echo a line. */
	}
	if (i > 0)
	    Error (OpLoc, NoENDC);	/* ENDC is missing! */

	break;

    case IfEQ:						/* IFEQ */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc != 0)
	    SkipNest++;			/* Skip to the next ENDC. */
	break;

    case IfNE:						/* IFNE */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc == 0)
	    SkipNest++;
	break;

    case IfGT:						/* IFGT */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc <= 0)
	    SkipNest++;
	break;

    case IfGE:						/* IFGE */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc < 0)
	    SkipNest++;
	break;

    case IfLT:						/* IFLT */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc >= 0)
	    SkipNest++;
	break;

    case IfLE:						/* IFLE */
	ObjSrc = GetValue (SrcOp, SrcLoc);
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (ObjSrc > 0)
	    SkipNest++;
	break;

    case IfC:						/* IFC */
	if (strcmp (SrcOp, DestOp) != 0)
	    SkipNest++;
	break;

    case IfNC:						/* IFNC */
	if (strcmp (SrcOp, DestOp) == 0)
	    SkipNest++;
	break;

    case IfD:						/* IFD */
	if (ReadSymTab (SrcOp))
	    AddRef (LineCount);
	if (DefLine2 > LineCount)
	    SkipNest++;
	break;

    case IfND:						/* IFND */
	if (ReadSymTab (SrcOp))
	    AddRef (LineCount);
	if (DefLine2 <= LineCount)
	    SkipNest++;
	break;

    case EndC:						/* ENDC */
	break;				/* LineParts will take care of it. */

    case Section:					/* SECTION */
	if (SFormat)
	    Error (OpLoc, NotSFmt);	/* Not in S-format */
	tempop[0] = '\0';
	j = DestLoc + strlen (DestOp); /* bugfix by Kevin Kofler in v.2.71.F3c -
	                                  j needs to be computed! */
	s = Line + j;	/* Check for flags. */
/*	s = Line + DestLoc + strlen (DestOp); */	/* Check for flags. */
	if (*s == ',') {
	    s++;
	    j++; /* bugfix by Kevin Kofler in v.2.71.F3c */
	    GetField (s, tempop);	/* Get specification. */
	}
	DoSection (SrcOp, SrcLoc, DestOp, DestLoc, tempop, j);
	break;

    case CSeg:						/* CODE */
	if (SFormat)
	    Error (OpLoc, NotSFmt);	/* Not in S-format */
	DoSection (SrcOp, SrcLoc, "CODE", OpLoc, DestOp, DestLoc);
	break;				/* Treat as SECTION. */

    case DSeg:						/* DATA */
	if (SFormat)
	    Error (OpLoc, NotSFmt);	/* Not in S-format */
	DoSection (SrcOp, SrcLoc, "DATA", OpLoc, DestOp, DestLoc);
	break;				/* Treat as SECTION. */

    case BSS:						/* BSS */
	if (SFormat)
	    Error (OpLoc, NotSFmt);	/* Not in S-format */
	DoSection (SrcOp, SrcLoc, "BSS", OpLoc, DestOp, DestLoc);
	break;				/* Treat as SECTION. */

    case Idnt:						/* IDNT */
	s = Line + SrcLoc;
	if ((*s == '"') || (*s == '\''))
	    s++;			/* Ignore quotes. */
	t = IdntName;
	while (!isspace(*s)
	&& (*s != '"')
	&& (*s != '\'')
	&& (*s != '\0'))
	    *t++ = *s++;
	*t = '\0';
	break;

    case DCB:						/* DCB */
/*	if ((Size == Word) || (Size == Long)) */
	if (((Size == Word) || (Size == Long)) && !Unaligned)
/* -u switch added by Kevin Kofler in v.2.71.F3k */
	    AddrCnt = AddrBndW (AddrCnt);
	DupFact = GetValue (SrcOp, SrcLoc);	/* Replication factor */
	/* Address diffs not allowed here -- Kevin Kofler, v.2.71.F3l */
	if (AddrDiffs) Error (SrcLoc, RelErr);
	if (DefLine2 >= LineCount) {
	    Error (SrcLoc, FwdRef);	/* Illegal forward reference */
	    DupFact = 0;
	}
	if (Hunk2 != ABSHUNK) {
	    Error (SrcLoc, AbsReq);	/* Must be absolute! */
	    DupFact = 0;
	}
	ObjDest = GetValue (DestOp, DestLoc);	/* Value to replicate */
	Dest.Hunk = Hunk2;
	if (Size == 4) {
	    ObjString[nX++] = (ObjDest >> 24) & 0x00FF;
	    ObjString[nX++] = (ObjDest >> 16) & 0x00FF;
	}
	if (Size >= 2)
	    ObjString[nX++] = (ObjDest >> 8) & 0x00FF;
	ObjString[nX++] = ObjDest & 0x00FF;

	AddrAdv = InstSize = nX;
	/* In global XREF mode, we need to handle ABSHUNK with NODEF properly. 
	   -- Kevin Kofler, v.2.71.F3t */
	if (GlobalXREF && DefLine2 == NODEF && Hunk2 == ABSHUNK && ReadSymTab (DestOp))
	  Dest.Hunk = Hunk2 = Sym->Hunk;
	if (Dest.Hunk != ABSHUNK) {		/* Relocatable */
	    templong = AddrCnt;
	    /* Constants are not optimizable. -- Kevin Kofler, v.2.71.F3s */
	    templong |= UNOPTIMIZABLE;
	    for (i = 0; i < DupFact; i++) {
		PutRel (templong, Hunk2, Size, 0);	/* Generate a relocation     */
		templong += nX;				/*  entry for each constant. */
	    }
	}
    /* Emit address diffs -- Kevin Kofler, v.2.71.F3l */
    templong = AddrCnt;
    /* Constants are not optimizable. -- Kevin Kofler, v.2.71.F3s */
    templong |= UNOPTIMIZABLE;
	for (i = 1; i < DupFact; i++) {
	  /* We need to copy the address differences because EmitAddrDiffs eats them. */
	  struct AddrDiff *newaddrdiffs=0;
	  struct AddrDiff *oldaddrdiffs=AddrDiffs;
	  struct AddrDiff **newaddrdiffnext=&newaddrdiffs;
	  while (oldaddrdiffs) {
	    *newaddrdiffnext=malloc(sizeof(struct AddrDiff));
	    if (!*newaddrdiffnext) {
	      while (newaddrdiffs) {
	        struct AddrDiff *next=newaddrdiffs->Link;
	        free(newaddrdiffs);
	        newaddrdiffs=next;
	      }
	      quit_cleanup("Out of memory!\n");
	    }
	    memcpy(*newaddrdiffnext,oldaddrdiffs,sizeof(struct AddrDiff));
	    (*newaddrdiffnext)->Link=0;
	    oldaddrdiffs=oldaddrdiffs->Link;
	    newaddrdiffnext=&((*newaddrdiffnext)->Link);
	  }
	  EmitAddrDiffs(templong, Size);
	  AddrDiffs=newaddrdiffs;
	  templong += nX;
	}
	EmitAddrDiffs(templong, Size);
	PrntAddr = MakeHunk = TRUE;
	break;

    case Equr:						/* EQUR */
	if ((i = IsRegister (SrcOp, strlen (SrcOp))) < 0)
	    Error (SrcLoc, AddrErr);	/* Not a valid register. */
	if (Label[0] == '\0')
	    Error (0, NeedLab);		/* Need a label */
	else {
	    if (!ReadSymTab (Label))	/* Make a new entry. */
		AddSymTab (Label, (long) i, 0L, LineCount, 0x20);
	    GotEqur = TRUE;		/* We have at least one EQUR. */
	}
	break;

    case Reg:						/* REG */
	if (Label[0] == '\0')
	    Error (0, NeedLab);		/* Need a label */
	else {
	    if ((i = GetMultReg (SrcOp, SrcLoc)) == 0) {
		Error (SrcLoc, OperErr);
	    } else {
		if (!ReadSymTab	(Label)) {	/* Make a new entry. */
		    AddSymTab (Label, (long) i, 0L, LineCount, 0x40);
		    GotEqur = TRUE;	    /* We have at least one EQUR. */
		}
	    }
	}
	break;

    case Near:						/* NEAR */
	if (SrcOp[0] == '\0') {
	    SmallData = 4;		/* Default to register A4. */
	} else {			/* Get register specification. */
	    SmallData = IsRegister (SrcOp, strlen (SrcOp)) - 8;
	    if ((SmallData < 0) || (SmallData > 6)) {
		Error (SrcLoc,AddrErr);	/* It's not a valid register. */
		SmallData = -1;		/* Disable small-data model. */
	    }
	}
	AnyNear	= TRUE;	/* Remember that we use small data somewhere. */
	break;

    case Far:						/* FAR */
	SmallData = -1;		/* Reset small-data model flag. */
	break;

    default:
	break;
    }

    return (Dir);
}



void DoSection (name, nameloc, type, typeloc, flags, flagloc)
char *name, *type, *flags;
int nameloc, typeloc, flagloc;
/* Processes SECTION directive or equivalent. */
{
    static long HunkPos;	/* Seek address of start of section */
    register char *s, *t;
    long newflags, templong;
    char tempop[MAXLINE];

    PrntAddr = TRUE;

    if (HunkType != HunkNone) {
	FwdLim1     = FwdBranch;	/* Clear forward branch controls. */
	FwdFixLimit = FwdBranchFix;
    }

#if 0
    for (s = type; *s; s++)		/* Convert section type */
	*s = toupper (*s);		/*  to upper case. */
#endif
    if ((type[0] == '\0') || (strcasecmp (type, "CODE") == 0))
	newflags = HunkCode;		/* Code section */
    else if (strcasecmp (type, "DATA") == 0)
	newflags = HunkData;		/* Data section */
    else if (strcasecmp (type, "BSS") == 0)
	newflags = HunkBSS;		/* BSS section */
    else {
	Error (typeloc, OperErr);	/* Invalid type */
	strcpy (type, "CODE");
	newflags = HunkCode;		/* Make it CODE. */
    }
    newflags <<= 16;	/* Shift to high-order 16 bits. */

    if (flags[0]) {
#if 0
	for (s = flags; *s; s++)		/* Convert flags */
	    *s = toupper (*s);			/*  to upper case. */
#endif
	if (strcasecmp (flags, "CHIP") == 0)
	    newflags |= MEMF_CHIP;		/* CHIP memory */
	else if (strcasecmp (flags, "FAST") == 0)
	    newflags |= MEMF_FAST;		/* FAST memory */
	else
	    Error (flagloc, OperErr);		/* Invalid - ignore. */
    }

    sprintf (tempop,"  %4x",HunkSeq++);	/* Make section name unique. */
    s = name;
    if ((*s == '"') || (*s == '\''))
	s++;				/* Ignore quotes. */
    t = tempop + 6;
    while (!isspace(*s)
    && (*s != '"')
    && (*s != '\'')
    && (*s != '\0'))
	*t++ = *s++;			/* Concatenate section name. */
    *t = '\0';

    if (ReadSymTab (tempop)) {		/* Scan for section name. */
	if (HunkType == HunkNone) {
	    SectLine = 1;		/* Start of first section */
	} else {
	    if (Pass2) {
		AddRef (LineCount);
		DumpSdata (&Srec);	/* Finish the previous hunk. */
		if (!MakeHunk) {	/* If it was a null hunk, */
		    xwrite (&Srec);	/* overwrite it.  */
		    lseek (Srec.fd, HunkPos, 0);
		    Srec.Ptr = Srec.Buf;
		}
	    } else {
		AddrCnt = AddrBndL (AddrCnt);	/* Finish on long word. */
	    }
	    if (AddrCnt > OrgHigh)
		Sect->Val = AddrCnt;	/* End of old section */
	    else
		Sect->Val = OrgHigh;	/* We've ORGed higher. */
	    SectLine = LineCount;	/* Start of new section */
	}
	Sect = Sym;			/* Point to new section. */
	if (Dir != Org)
	    AddrCnt = Sym->Val;		/* Continuation */
	SectStart = OrgHigh = AddrCnt;
	OrgFlag = FALSE;
	TempAddr = StartAddr = AddrCnt;
	if (SFormat)
	    CurrHunk = ABSHUNK;		/* S-format is absolute. */
	else
	    CurrHunk = Sym->Hunk & 0x0000FFFFL;	/* Hunk number */
	HunkType = (Sym->Hunk & 0x3FFF0000L) >> 16;	/* Type */
	HunkFlags = Sym->Hunk & 0xC0000000L;		/* Flags */
	if (Pass2 && !SFormat) {		/* Start a new hunk. */
	    HunkPos = lseek (Srec.fd, 0L, 1);
	    HunkPos += Srec.Ptr - Srec.Buf;	/* It starts here. */
	    templong = HunkName;
	    xputl (&Srec, templong);
	    if (tempop[6])
		DumpName (&Srec, &tempop[6], 0L);	/* Hunk name */
	    else
		DumpName (&Srec, " ", 0L);
	    xputl (&Srec, HunkType | HunkFlags);	/* Hunk type */
	    LenPtr = Srec.Ptr;		/* Pointer to hunk length */
	    LenPos = lseek (Srec.fd, 0L, 1);
	    LenPos += LenPtr-Srec.Buf;	/* Hunk length goes here. */
	    xputl (&Srec, 0L);		/* For now, set it to zero. */
	}
	MakeHunk = FALSE;		/* We don't have anything yet. */
	if (AnyNear && (CurrHunk > 1))
	    Error (OpLoc, ManySect);	/* Too many hunks for small data! */
	return;
    }

    AddrCnt = AddrBndL (AddrCnt);	/* Finish on long word bounary. */

    if (Pass2) {
	Error (OpLoc, ManySect);	/* Table overflowed in pass 1! */
	return;
    }
    if (NextHunk >= ABSHUNK)		/* Set up a new table entry. */
	return;				/* Section table overflow */

    if (HunkType != HunkNone) {
	if (AddrCnt > OrgHigh)
	    Sect->Val = AddrCnt;	/* End of old section */
	else
	    Sect->Val = OrgHigh;	/* We've ORGed higher. */
	SectLine = LineCount;		/* Starting line number */
    } else {
	SectLine = 1;			/* Start of first section */
    }
    if (Dir != Org)
	AddrCnt = 0L;			/* Reset location counter. */
    SectStart = OrgHigh = AddrCnt;
    OrgFlag = FALSE;
    TempAddr = StartAddr = AddrCnt;
    HunkType = (newflags & 0x3FFF0000L) >> 16;	/* Type */
    HunkFlags = newflags & 0xC0000000L;		/* Flags */
    if (SFormat)
	CurrHunk = ABSHUNK;		/* S-format is absolute. */
    else
	CurrHunk = NextHunk++;		/* Bump next hunk number. */
    newflags |= CurrHunk;		/* Add hunk number to flags. */
    AddSymTab (tempop, AddrCnt, newflags, LineCount, 16);	/* New entry */
    Sect = Sym;				/* Pointer to new entry */
    MakeHunk = FALSE;			/* We don't have anything yet. */
    if (AnyNear && (CurrHunk > 1))
	Error (OpLoc, ManySect);	/* Too many hunks for small data! */
    return;
}
