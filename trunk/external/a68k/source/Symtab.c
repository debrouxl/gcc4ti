/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*                Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*             Symbol table manipulation - May 24, 1992		    */
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

long Value;	/* Passed from ReadSymTab to CalcValue */



int OpenIncl (name, dirlist) char *name, *dirlist;
/* Opens the file whose name is in "name".  The current
    directory is tried first.  If that fails, the directory
    names in "dirlist" (separated by commas) are then tried
    until either a file is found or the list is exhausted.
    If the file is found in a subdirectory, "name" is
    modified to include the entire path specification.
    If another input file is open when this routine is called,
    it is closed first; if the specified file cannot be opened,
    the original input file is re-opened and the current position
    within that file is restored.  This routine Returns TRUE if
    it successfully opens the specified file, FALSE if not.  */
/* NOTE: Semicolons are also accepted as separators. - Kevin Kofler (v.2.71.F3f) */
{
    register char *s, *t;
    char dirname[MAXLINE];

    if (InF->UPtr == 0) {		/* If we're reading from a file */
	InF->Pos = lseek (In.fd, 0L,1); /*  remember where we are.  */
	InF->Pos -= In.Lim - In.Ptr;
    }
    if (In.fd != NOFD)
	close (In.fd);		/* Close the inner file. */

    if ((In.fd = open (name, 0)) >= 0) {
	In.Ptr = In.Lim = In.Buf;
	if (Quiet < 0)
	    fprintf (stderr, "\n%s line ", name);
	return (TRUE);		/* We found it in the current directory. */
    }
    s = dirlist;
    while (*s) {
	s = GetField (s, dirname);
	t = dirname + strlen (dirname) - 1;
	if ((*t != '/') && (*t != ':'))
	    strcat (dirname, "/");	/* Slash after directory name */
	strcat (dirname, name);
	if ((In.fd = open (dirname, 0)) >= 0) {
	    In.Ptr = In.Lim = In.Buf;
	    strcpy (name, dirname);	/* Return entire path. */
	    if (Quiet < 0)
		fprintf (stderr, "\n%s line ", name);
	    return (TRUE);	/* We found it in a subdirectory. */
	}
	if (*s)
	    s++;		/* Skip over separator and try again. */
    }
    if (InF->UPtr == 0) {	/* We couldn't find it anywhere. */
	In.fd = open (InF->NPtr, 0);	/* Re-open the outer file, */
	lseek (In.fd, InF->Pos, 0);	/*  and restore current position. */
	In.Ptr = In.Lim = In.Buf;
    } else {
	In.fd = NOFD;
    }
    return (FALSE);
}



int LineParts (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Gets the next statement and extracts its component parts.
    If end of file is reached, and we're in a macro or include
    file, the file is closed and the next outermost file is
    continued.  If we have reached the end of the source file, or
    encounter an ENDM or MEXIT directive within a macro expansion,
    the current input file is closed and TRUE is returned.

    If we're in a user macro (indicated by UPtr being nonzero),
    we'll get the next statement from the save area in memory instead.

    Macro arguments, if any, are substituted.

    LineCount is incremented if a statement was successfully read.

    If this is the first call of this routine (i.e. LineCount is zero)
    and HeaderFN is not a null string, we'll return an INCLUDE statement
    requesting the specified header file, rather than reading the first
    statement from the source file.

    The following fields are set up:
	Line	- statement line image
	Label	- instruction label (without trailing colon)
	OpCode	- instruction mnemonic (converted to upper case)
	SrcOp	- first (source) operand
	DestOp	- second (destination) operand
	Size	- size from OpCode
	LabLoc	- displacement to start of instruction label
	OpLoc	- displacement to start of instruction mnemonic
	SrcLoc	- displacement to start of source operand
	DestLoc	- displacement to start of destination operand
	InFNum	- decremented if end of file is reached
	InF	- incremented if end of file is reached
	LabLine	- set to LineCount if this line is labeled
			(unless it's a local label)
								*/
{
    register int i;
    int eofflag;
    char *x;


    while (1) {	/* Repeat until we get something (not end of INCLUDE). */
	Line[0] = Label[0] = OpCode[0] = SrcOp[0] = DestOp[0] = '\0';
	LabLoc = OpLoc = SrcLoc = DestLoc = 0;
	Src.Mode = Dest.Mode = NOMODE;
	ErrLim = AddrAdv = InstSize = nO = nS = nD = nX = 0;
	PrntAddr = FwdShort = FALSE;
	DupFact = 1;

	if ((LineCount==0) && (HeaderFN[0])) {	/* Header file */
	    strcpy (Line, "        INCLUDE ");	/* Make an INCLUDE stmt. */
	    strcat (Line, HeaderFN);
	    strcat (Line, "        ;Generated for header file");
	    strcpy (OpCode, "INCLUDE");	/* Dummy op code */
	    OpLoc = 8;
	    strcpy (SrcOp, HeaderFN);	/* Dummy source operand */
	    SrcLoc = 16;
	    LineCount++;
	    return (FALSE);
	}

	if (InF->UPtr != 0) {			/* User macro input */
	    GetMacLine (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);
	    eofflag = FALSE;
	} else {				/* Normal file input */
	    eofflag = GetLine (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);
	}
	if ((Line[0] != '\0') && (Line[0] != '*') && (Line[0] != ';')) {
	    SubArgs (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Substitute macro arguments. */
	    GetParts (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Break Line into its component parts. */
	}

	/* ------ If we have reached the end of a macro or ------ */
	/* ------ include file, return to the calling file. ----- */

	i = eofflag;				/* End of file */

	if (i) {
	    if (SkipNest != 0) {
		Error (OpLoc, NoENDC);	/* ENDC is missing! */
		WriteListLine (&List);	/* It's not normally listed. */
		SkipNest = 0;
	    }
	} else if ((InF->NArg != -1)&&(Dir != Macro)) {	/* Macro exits */
	    if (strcasecmp (OpCode, "ENDM") == 0) {
		i = TRUE;
		(InF->Line)++;		/* Count ENDM directive. */
		if (SkipNest != 0) {
		    Error (OpLoc, NoENDC);  /* ENDC is missing! */
		    WriteListLine (&List);  /* It's not normally listed. */
		    SkipNest = 0;
		}
	    } else if (SkipNest == 0) {
		if (strcasecmp (OpCode, "MEXIT") == 0) {
		    i = TRUE;
		    (InF->Line)++;	/* Count MEXIT directive. */
		}
	    }
	}
	if (!i) {			/* Not end of file or macro */
	    if (PrevDir == MacCall) {
		if (strcasecmp (OpCode, "MACRO") == 0) {
		    (InF->Line)++;	/* Count macro header */
		    continue;		/*  and ignore it.    */
		}
	    }
	    if (SkipNest == 0) {	/* If we're not skipping, */
		break;			/*  we got something.     */
	    } else {
		(InF->Line)++;		/* Count skipped lines. */
		SkipNest += CountNest (OpCode);	/* Adjust SkipNest. */
		continue;
	    }
	}
	if (!Pass2 && (IncStart != 0) && (IncPtr == InF)) {
	    SkipLim->Start = IncStart;	/* End of skippable INCLUDE */
	    SkipLim->Finish=LineCount;	/* Save line numbers for pass 2. */
	    SkipLim->MCount = MacCount;	/* Save macro counter too. */
	    SkipLim->LLine = LabLine; /* Save LabLine too -- bugfix by Kevin Kofler
	                                                     in v.2.71.F3i */
	    if (SkipLim->Set1 != NULL) {
		SetFixLim--;
		x = (char *) SkipLim + sizeof (struct SkipEnt);
		if (x > (char *) SetFixLim) {
		    SetFixLim++;	/* No room for final entry */
		} else {
		    SetFixLim->Sym  = NULL;	/* Null entry        */
		    SetFixLim->Val  = 0;	/*  indicates end of */
		    SetFixLim->Hunk = 0;	/*  SET symbol list. */
		}
	    }
	    SkipLim++;
	    IncStart = 0;
	}
	if (InFNum == 0)
	    break;			/* End of source file */
	if (i = (InF->UPtr == 0)) {
	    if (Quiet < 0)
		fprintf (stderr, "%d\n", InF->Line);
	    close (In.fd);		/* Close inner file. */
	    In.fd = NOFD;
	}
	NextFNS = InF->NPtr;		/* Release space on name stack. */
	InFNum--;			/* Return to outer file. */
	InF++;
	if (InFNum < OuterMac)
	    OuterMac = 0;		/* End of outer macro */
	if (InF->UPtr == 0) {
	    if (i)
		ShowFile (FALSE);	/* Inner file -> outer file */
	    else if (InnrFMac) {
		ShowFile (FALSE);	/* Inner user macro -> file */
		InnrFMac = FALSE;
	    }
	    if (In.fd == NOFD) {
		In.fd = open (InF->NPtr, 0);
		lseek (In.fd, InF->Pos, 0);
		In.Ptr = In.Lim = In.Buf;
	    }
	} else if (i) {
	    InnrFMac = TRUE;	/* Inner file -> outer user macro */
	}
    }
    LineCount++;			/* Bump line counter. */
    (InF->Line)++;
    if ((Label[0] != '\0') && (Label[0] != '\\'))
	if ((Label[0] < '0') || (Label[0] > '9'))
	    LabLine = LineCount;	/* Save line number of label. */
    if (Quiet != 0) {
	i = (Quiet < 0 ? InF->Line : LineCount);
	if ((i % Quiet) == 0) {		/* Display progress. */
	    ShowLine (i);
	}
    }

    if (LineCount == DebugStart)
	fprintf (stderr, "%d\n", LineCount);
    if ((LineCount >= DebugStart) && (LineCount <= DebugEnd))
	printf ("%9lx %5d %s\n", AddrCnt, LineCount, Line);

    return (eofflag);
}



void GetMacLine (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Gets the next stored user macro line. */
{
    register char *s, *t;
    register struct NameChunk *np;

    s = InF->UPtr;
    if (*s == '\n') {		/* Continue in next chunk. */
	np = NameStart;
	while ((s < (char *) np)
	|| (s > ((char *) np + CHUNKSIZE)))
	    np = np->Link;	/* Find the chunk we're in. */
	InF->UPtr = (char *) (np->Link) + sizeof (struct NameChunk *);
	s = InF->UPtr;
    }
    t = Line;
    while (*t++ = *s++)	/* Copy InF->UPtr to Line, then bump InF->UPtr. */
	;		/*  (It's faster than doing a strcpy followed   */
    InF->UPtr = s;	/*  by a strlen to bump the pointer.)           */
}



int GetLine (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Gets the next line from the current input file and leaves it in Line.
    Returns TRUE if end of file has been reached, FALSE otherwise.	*/
{
    register char *s;
    register int c;
    register char *t, *m, *l;

    s = Line;
    t = In.Ptr;				/* Use a register for speed. */
    m = Line + MAXLINE - 1;		/* Limit of "Line" */
    while (1) {				/* Get Line, expanding tabs. */
	if (t >= In.Lim) {
	    t = In.Buf;			/* Re-fill the buffer. */
	    In.Lim = In.Buf + read (In.fd, In.Buf, BUFFSIZE);
	    if (In.Lim == In.Buf) {
		*s = '\0';		/* End of file */
		In.Ptr = t;
		return (s <= Line);	/* Last line might have no \n. */
	    }
	}
	if ((c = *t++) == '\n') {
	    break;
	}
#ifdef MSDOS
	if (c == 26) {
	    *s = '\0';			/* Catch MS-DOS EOF char. */
	    In.Ptr = t;
	    return (s <= Line);
	}
	if ((s < m) && (c != 13)) {	/* Ignore excess. */
#else
	if (s < m) {			/* Ignore excess. */
#endif
	    if ((c == '\t') && !KeepTabs) {
		l = Line + (((s - Line) + 8) & ~7);
		if (l > m)
		    l = m;		/* We tabbed off the end. */
		while (s < l)
		    *s++ = ' ';		/* Expand tabs. */
	    } else {
		*s++ = c;		/* Normal character */
	    }
	}
    }
    *s = '\0';			/* Terminate the string in Line. */
    In.Ptr = t;
    return (FALSE);
}



void SubArgs (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Macro argument substitution routine */
{
    int j;
    register char *s, *t, *x;
    char subline[MAXLINE];

    if (InF->NArg == -1)
	return;			/* Not a macro - leave Line alone. */

    s = Line;
    t = subline;
    while (*s) {
	if ((*t++ = *s++) != '\\') {
	    continue;
	}
	x = s--;
	t--;
	if (*x == '@') {	/* \@ - substitute macro number. */
	    x = t;
	    *t++ = '.';
/*	    *t++ = '_';    C.W. Fox - I replaced a . because of named locals */
	    j = InF->MCnt % 1000;
	    *t++ = j / 100 + '0';
	    j = j % 100;
	    *t++ = j / 10 + '0';
	    *t++ = j % 10 + '0';
	    strcpy (t, s+2);		/* Remainder of Line */
	    strcpy (Line, subline);	/* Replace Line. */
	    while (*t != '\\')		/* Check for more substitutions. */
		if (*t)
		    t++;
		else
		    return;		/* All done */
	    s = t - subline + Line;	/* Do the next substitution. */
	    continue;
	}
	if ((*x < '0') || (*x > '9')) {
	    s++;		/* False alarm - it's a   */
	    t++;		/*  named local variable. */
	    continue;
	}

	s++;
	*t = '\0';
	j = 0;			/* Get argument index. */
	while ((*s >= '0') && (*s <= '9')) {
	    j *= 10;
	    j += *s++ - '0';	/* Current digit */
	}
	if (j == 0)
	    strcpy (t, MacSize);	/* Macro call size */
	else if ((j > 0) && (j <= InF->NArg)) {
	    x = InF->NPtr;
	    while (j > 0) {		/* Find argument. */
		x += strlen (x) + 1;
		j--;
	    }
	    strcpy (t, x);		/* Insert it. */
	}
	while (*t)
	    t++;			/* Skip over replacement. */
	strcpy (t, s);			/* Remainder of Line */
	strcpy (Line, subline);		/* Replace Line. */
	while (*t != '\\')		/* Check for more substitutions. */
	    if (*t)
		t++;
	    else
		return;			/* All done */
	s = t - subline + Line;		/* Do the next substitution. */
    }
}



void GetParts (__NO_PARAMS__) /* using now void in v.2.71.F3d - Kevin Kofler */
/* Break up Line into its component parts. */
{
    register char *s, *x;

    Size = S0;
    s = Line;
    if (!isspace (*s)) {
	x = Label;
	while (!isspace (*s) && (*s != ';') && (*s != '\0'))
	    *x++ = *s++;		/* Get the label. */
	*x-- = '\0';
	while (*x == ':') {
	    *x-- = '\0';		/* Strip trailing colon(s). */
	    if (x < Label)
		break;
	}
    }
    while (OpLoc == 0) {
	while (isspace (*s))
	    s++;			/* Skip over to opcode. */
	if ((*s == ';') || (*s == '\0'))
	    return;			/* End of statement image */
	OpLoc = s - Line;
	x = OpCode;
	while (!isspace (*s) && (*s != ';') && (*s != '\0'))
	    *x++ = *s++;		/* Get the opcode. */
	*x-- = '\0';
	if (*x == ':') {		/* It's actually a label. */
	    if (Label[0]) {
		Error (OpLoc, MultLab);	/* Multiple labels */
	    } else {
		while ((x >= OpCode) && (*x == ':'))
		    *x-- = '\0';
		strcpy (Label, OpCode);	/* Get the label. */
	    }
	    OpLoc = 0;			/* Try again for opcode. */
	    OpCode[0] = '\0';
	}
    }
#if 0
    for (x = OpCode; *x; x++)		/* Convert OpCode  */
	*x = toupper(*x);		/*  to upper case. */
    x -= 2;
#endif
    x = OpCode + strlen(OpCode) - 2;
    if ((x < OpCode) || (*x != '.'))	/* If no explicit size is given */
/*	Size = Word;	*/		/*  default to Word (16 bits).  */
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
    {
	Size = Word;			/*  default to Word (16 bits).  */
    noExplicitSize=TRUE; /* remember that no explicit size was given! */
    }

    else {
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
    noExplicitSize=FALSE; /* remember that an explicit size was given! */
    
	*x++ = '\0';			/* Chop off size extension. */
	switch (toupper(*x)) {
	case 'B':			/* Byte */
	case 'S':			/* Short Branch */
	    Size = Byte;
	    break;
	case 'W':			/* Word */
	    Size = Word;
	    break;
	case 'L':			/* Long */
	    Size = Long;
	    break;
	default:
	    Error (OpLoc+x-OpCode, SizeErr);	/* Invalid size */
	    Size = Word;			/* Default to Word. */
	    break;
	}
    }
    while (isspace(*s))
	s++;				/* Skip over to operands. */
    if ((*s == ';') || (*s == '\0'))
	return;				/* There are no operands. */
    SrcLoc = s - Line;
    s = GetField (s, SrcOp);		/* Op1 (source) */
    if (*s == ',')
	s++;
    if (!isspace (*s) && (*s != '\0') && (*s != ';')) {
	DestLoc = s - Line;
	s = GetField (s, DestOp);	/* Op2 (destination) */
    }
}



void ShowFile (newline) int newline;
/* Shows the current file name if we're displaying all input modules.
    If "newline" is TRUE, go to a new line before displaying the name. */
{
    if ((Quiet < 0) && (InF->UPtr == 0))
	if (newline)
	    fprintf (stderr, "\n%s line ", InF->NPtr);
	else
	    fprintf (stderr, "%s line ", InF->NPtr);
}



void ShowLine (i) register int i;
/* Shows the current line number and backs up over it. */
{
    if (i >= 10000)
	fprintf (stderr, "%5d\b\b\b\b\b", i);
    else if (i >= 1000)
	fprintf (stderr, "%4d\b\b\b\b", i);
    else if (i >= 100)
	fprintf (stderr, "%3d\b\b\b", i);
    else if (i >= 10)
	fprintf (stderr, "%2d\b\b", i);
    else
	fprintf (stderr, "%1d\b", i);
    fflush (stderr);		/* Make sure it gets out. */
}



char *GetField (s, d) register char *s, *d;
/* Gets a field from "s", returns result in "d".
    Stops on the first comma, semicolon, or white space not
	enclosed within apostrophes or parentheses.
    Returns stopping location.
    If already at end of "s", "d" is set to null string. */
{
    register char c;
    register int parncnt, instring;

    instring = FALSE;
    parncnt = 0;

    while (c = *s) {
	if (instring) {
	    *d++ = c;
	} else {
	    if (isspace(c) || (c == ';'))
		break;
	    else if ((c == ',') && (parncnt == 0))
		break;
	    else {
		*d++ = c;
		if (c == '(')
		    parncnt++;
		else if (c == ')')
		    parncnt--;
	    }
	}
	if (c == '\'')
	    instring = !instring;
	s++;
    }
    *d = '\0';
    return (s);
}



long GetValue (operand, loc) char *operand; int loc;
/* Determines value of expression. */
/* Hunk2 is set to hunk number of result (ABSHUNK if absolute).
   If the expression consists solely of self-defining terms,
	DefLine2 is set to zero.  Otherwise, DefLine2 is set
	to the highest statement number in which any symbol in
	the expression was defined.  If the expression contains
	any undefined symbols, DefLine2 is set to NODEF.
    SingleFlag is set to TRUE if the expression consists of a
	single term, FALSE otherwise.
    The following code is based on a regular-to-Polish expression
	converter described in "A Guide to FORTRAN IV Programming"
	by Daniel D. McCracken (John Wiley & Sons, Inc. 1965,
	3rd printing August 1968).  However, rather than generating
	the entire Polish expression, this routine will evaluate
	and combine two terms as soon as an operator of lower
	precedence is encountered.				*/
{
    register char *o, *s;
    char tempop[MAXLINE];
    int  oloc, parncnt, nextprec, instring;
    struct TermStack *origterm;

    /* Get rid of leftover AddrDiffs list -- Kevin Kofler, v.2.71.F3l */
    while (AddrDiffs) {
      struct AddrDiff *next=AddrDiffs->Link;
      free(AddrDiffs);
      AddrDiffs=next;
    }

    instring = (unsigned int) '~';
    OpPrec[instring] = 9;	/* Fudge IsOperator for speed. */
    SingleFlag = TRUE;		/* Assume there's only a single term. */
    for (o = operand; *o; o++) {
	if (IsOperator (o)) {
	    SingleFlag = FALSE;	/* It's not a single term. */
	    break;
	}
    }
    instring = (unsigned int) '~';
    OpPrec[instring] = '\0';	/* Restore IsOperator. */
    if (SingleFlag)
	return(CalcValue(operand,loc));	/* Short cut for single term */

    Hunk2 = ABSHUNK;
    parncnt = DefLine2 = 0;
    o = (char *) (((long) NextFNS + 3L) & ~3L);
    origterm = Term = (struct TermStack *) o;	/* Term stack */
    Ops = (struct OpStack *) InF;		/* Operator stack */
    Ops--;
    ParseSpace (0);
    Ops->chr = ' ';		/* Prime the operator stack. */
    Ops->prec = -1;
    if ((char *) Ops < Low2)
	Low2 = (char *) Ops;

    /* Get all tokens.
	Terms are evaluated, and operator precedence is determined.
	Left and right parentheses are given a precedence of
	    1 and 2 respectively.
	Binary operators are given precedence values starting at 3.
	Unary plus is ignored.
	Unary minus is converted to zero minus the remainder of
	    of the expression - its precedence is set to 9 to
	    ensure that the simulated unary operator is evaluated
	    before the remainder of the expression.
	Logical not (~), being another unary operator, is converted
	    to -1 exclusive-ORed with the remainder of the expression.
	    Its precedence is also set to 9.				*/

    o = operand;			/* Current position in operand */

    while (1) {
	while (*o == '(') {		/* Left parenthesis */
	    Ops--;
	    ParseSpace (0);
	    Ops->chr  = '(';
	    Ops->prec = 1;
	    if ((char *) Ops < Low2)
		Low2 = (char *) Ops;
	    parncnt++;
	    o++;
	}
	if ((*o == '+') || (*o == '-') || (*o == '~')) {    /* Unary op */
	    if (*o != '+') {	/* Ignore unary plus. */
		Ops--;
		ParseSpace (sizeof (struct TermStack));
		Term->value   = (*o == '-') ? 0 : -1;	/* Dummy value */
		Term->hunk    = ABSHUNK;
		Term->oploc   = loc + (o - operand);
		Term->defline = 0;
		Term->isaddrdiff=0; /* added by Kevin Kofler in v.2.71.F3l */
		Term++;
		if ((char *) Term > High2)
		    High2 = (char *) Term;
		Ops->chr  = *o;		/* Now get the operator itself. */
		Ops->prec = 9;		/* Do it ASAP. */
		if ((char *) Ops < Low2)
		    Low2 = (char *) Ops;
	    }
	    o++;
	    if (*o == '(')
		continue;	/* Inner parenthesized expression */
	}
	oloc = loc + (o - operand);

	s = tempop;				/* Get a term. */
	if (*o == '*') {	/* It's a location counter reference, */
	    *s++ = *o++;	/*   not a multiplication operator!   */
	} else {
	    if (IsOperator (o) || (*o == '\0')) {
		Error (oloc, OperErr);	/* Unexpected operator or no terms */
		return (0L);
	    }
	    instring = (unsigned int) '~';
	    OpPrec[instring] = 9;	/* Fudge IsOperator for speed. */
	    instring = FALSE;
	    while (*o) {
		if (*o == '\'')
		    instring=!instring;	/* String delimiter */
		if (!instring && IsOperator (o))
		    break;		/* Found an operator - stop. */
		*s++ = *o++;		/* Get a character. */
	    }
	    instring = (unsigned int) '~';
	    OpPrec[instring] = '\0';	/* Restore IsOperator. */
	}
	*s = '\0';
	ParseSpace (sizeof (struct TermStack));
	Term->value   = CalcValue (tempop, oloc);
	Term->hunk    = Hunk2;
	Term->oploc   = oloc;
	Term->defline = DefLine2;
	Term->isaddrdiff=0; /* added by Kevin Kofler in v.2.71.F3l */
	Term++;
	if ((char *) Term > High2)
	    High2 = (char *) Term;
	Hunk2 = DefLine2 = 0;

	while (*o == ')') {		/* Right parenthesis */
	    if (parncnt == 0) {
		Error ((int) (loc + (o - operand)), OperErr);
		return (0L);
	    }
	    CondCalc (2);		/* Unstack what we can. */
	    if (Ops->chr == '(')
		Ops++;			/* Drop paired parentheses. */
	    else {
		Ops--;
		ParseSpace (0);
		Ops->chr  = ')';	/* Stack parenthesis for now. */
		Ops->prec = 2;
		if ((char *) Ops < Low2)
		    Low2 = (char *) Ops;
	    }
	    parncnt--;
	    o++;
	}
	if (*o) {
	    nextprec = IsOperator (o);
	    if ((nextprec == 0) || (*o == '(')) {
		Error ((int) (loc + (o - operand)), OperErr);
		return (0L);		/* We expected an operator. */
	    }
	    CondCalc (nextprec);	/* Unstack what we can. */
	    Ops--;
	    ParseSpace (0);
	    Ops->chr  = *o;		/* Stack the next operator. */
	    Ops->prec = nextprec;
	    if ((char *) Ops < Low2)
		Low2 = (char *) Ops;
	    if ((*o == '<') || (*o == '>'))
		o++;	/* Skip over two-character operator. */
	    o++;
	} else {
	    if (parncnt) {
		Error ((int) (loc + (o - operand)), OperErr);
		return (0L);	/* Too many left parentheses */
	    }
	    CondCalc (0);		/* Unstack what's left. */
	    if (--Term != origterm)	/* Should be only one term left */
		Error (Term->oploc, OperErr);		/* Parser bug? */
	    Hunk2    = Term->hunk;
	    DefLine2 = Term->defline;
	    return (Term->value);	/* Final value */
	}
    }
}



void CondCalc (newprec) int newprec;
/* As long as the top operator on the operator stack has a precedence
    greater than or equal to the contents of "newprec", this routine
    will pop the two top terms from the term stack, combine them
    according to the operator on the top of the operator stack (which
    is also popped), and push the result back onto the term stack. */
{
    while (Ops->prec >= newprec) {	/* Unstack an operator. */
	Term -= 2;
	if (Ops->chr == '+') {		/* Relocatable addition */
	    if (Term->hunk == ABSHUNK)
		Term->hunk = (Term+1)->hunk;	/* A+R */
	    else if ((Term+1)->hunk != ABSHUNK) {
		Error ((Term+1)->oploc,RelErr);	/* R+R - error */
		Term->hunk = ABSHUNK;		/* Make it absolute. */
	    }
	} else if (Ops->chr == '-') {		/* Subtraction */
	    if (Term->hunk == (Term+1)->hunk)
		Term->hunk = ABSHUNK;		/* R-R - absolute */
	    else if ((Term+1)->hunk != ABSHUNK) {   /* R-R across hunks  */
		Error ((Term+1)->oploc, RelErr);    /*  is an error -    */
		Term->hunk = ABSHUNK;		    /* make it absolute. */
	    }
	} else if ((Term->hunk != ABSHUNK)
	|| ((Term+1)->hunk != ABSHUNK)
	|| (Term->isaddrdiff)||((Term+1)->isaddrdiff) /* added by Kevin Kofler in
	                                                 v.2.71.F3l */
	) {
	    Error (Term->oploc,RelErr);		/* All other operations */
	    Term->hunk = ABSHUNK;		/*   must be absolute.  */
	}
	if ((Term+1)->defline > Term->defline)	/* Definition */
	    Term->defline = (Term+1)->defline;	/*  line nos. */

	switch (Ops->chr) {		/* Perform the operation. */
	case '+':
	    Term->value += (Term+1)->value;
	    break;
	case '-':
	    /* In AllReloc-mode, emit a pair of extended relocs here
	       -- Kevin Kofler, v.2.71.F3l */
	    /* ...unless the 2 addresses are actually the same.
	       -- Kevin Kofler, v.2.71.F3q */
	    if (AllRelocs && !SFormat && (Term+1)->hunk!=ABSHUNK
	        && Term->value!=(Term+1)->value) {
	      struct AddrDiff *next=AddrDiffs;
	      AddrDiffs=malloc(sizeof(struct AddrDiff));
	      AddrDiffs->Link=next;
	      AddrDiffs->phunk=AddrDiffs->nhunk=(Term+1)->hunk;
	      AddrDiffs->paddr=Term->value;
	      AddrDiffs->naddr=(Term+1)->value;
	      Term->value=0;
	      Term->isaddrdiff=1;
	      break;
	    }
	    Term->value -= (Term+1)->value;
	    break;
	case '*':
	    Term->value *= (Term+1)->value;
	    break;
	case '/':
	    if ((Term+1)->value)
		Term->value /= (Term+1)->value;
	    else
		Term->value = 0;	/* Don't divide by zero. */
	    break;
	case '&':
	    Term->value &= (Term+1)->value;
	    break;
	case '!':
	case '|':
	    Term->value |= (Term+1)->value;
	    break;
	case '<':
	    Term->value <<= (Term+1)->value;
	    break;
	case '>':
	    Term->value >>= (Term+1)->value;
	    break;
	case '~':
	    Term->value ^= (Term+1)->value;
	    break;
	default:
	    Error (Term->oploc, OperErr);	/* Parser bug? */
	    break;
	}
	Term++;
	Ops++;
    }
}



int IsOperator (o) register char *o;
/* Tests whether "o" points to a valid operator or parenthesis.
    Returns the precedence of the operator, or 0 if it isn't one. */
{
    register unsigned int i;

    i = (unsigned int) *o;
    i = (unsigned int) OpPrec[i];
    if (i != 6)
	return ((int) i);
    if (*(o+1) == *o)
	return ((int) i);	/* << or >> */
    else
	return (0);		/* False alarm */
}



long CalcValue (operand, loc) char *operand; int loc;
/* Evaluates a single term (called by GetValue).
    Hunk2 receives relative hunk number (ABSHUNK if absolute).
    If the value is a symbol, DefLine2 is set to the line number
	where it was defined, or NODEF if it is undefined.
	For self-defining terms, DefLine2 is set to zero.	*/
{
    register long result;	/* Result is calculated here. */
    register char *s, *numstart;
    register int  radix;
    int  neg, overflow;
    char maxdig;	/* Highest valid digit in current radix */
    char delim;		/* String delimiter (' or ") */

    Hunk2 = ABSHUNK;			/* Assume value is absolute */
    DefLine2 = 0;			/*  and self-defining.      */
    result = 0;
    overflow = FALSE;
    if (neg = (*operand == '-'))
	numstart = operand + 1;		/* Negative value */
    else
	numstart = operand;		/* Positive value */

    if ((*numstart >= '0') && (*numstart <= '9')) {
	radix = 10;		/* Decimal number */
	maxdig = '9';
    } else if (*numstart == '$') {
	radix = 16;		/* Hexadecimal number */
	maxdig = '9';
    } else if ((*numstart == '@') && (isdigit(*(numstart+1)))) {
	radix = 8;		/* Octal number */
	maxdig = '7';
    } else if (*numstart == '%') {
	radix = 2;		/* Binary number */
	maxdig = '1';
    } else
	radix = 0;		/* Not a number */

    if (radix != 0) {			/* Some sort of number */
	result = 0;
	if (radix != 10)
	    numstart++;			/* Allow for type character. */
	for (s = numstart; *s; s++) {
	    if (!overflow) {		/* If we haven't overflowed yet... */
		if (radix == 2) {		/* Check for binary overflow. */
		    if (result & 0x80000000L) {
			Error (loc, SizeErr);
			overflow = TRUE;
		    }
		} else if (radix == 8) {	/* Check for octal overflow. */
		    if (result & 0xE0000000L) {
			Error (loc, SizeErr);
			overflow = TRUE;
		    }
		} else if (radix == 16) {	/* Check for hex overflow. */
		    if (result & 0xF0000000L) {
			Error (loc, SizeErr);
			overflow = TRUE;
		    }
		} else {			/* Check for decimal overflow. */
		    if ((result > 429496729L)
		    || ((result == 429496729L) && (*s > '5'))) {
			Error (loc, SizeErr);
			overflow = TRUE;
		    }
		}
	    }
	    result *= radix;
	    if ((*s >= '0') && (*s <= maxdig)) {
		result += (*s - '0');
	    } else if (radix == 16) {
		if ((*s >= 'A') && (*s <= 'F'))
		    result += (*s - 'A' + 10);
		else if ((*s >= 'a') && (*s <= 'f'))
		    result += (*s - 'a' + 10);
		else
		    Error (loc + s - operand, OperErr);
	    } else if (!neg && (radix==10) && (*s=='$') && (*(s+1)=='\0')) {
		if (ReadSymTab (operand)) {	/* Look up local label. */
		    result = Value;		/* Get its value. */
		    AddRef (LineCount);		/* Add reference. */
		    if (Sym->Flags & 0x60)
			Error (loc, AddrErr);	/* Can't use a register equate! */
		} else {
		    Error (loc, Undef);		/* Undefined */
		}
		break;
	    } else {
		Error (loc + s - operand, OperErr);
	    }
	}
    } else if ((*operand == '\'') || (*operand == '"')) {
	delim = *operand;		/* Character value delimiter */
	result = 0;
	s = operand + 1;
	while (1) {
	    if (*s == '\0') {
		Error (loc+s-operand,NoStrEnd);	/* End is missing! */
		result = 0;
		break;
	    }
	    if (*s == delim) {		/* End of string? */
		if (*(++s) != delim)	/* Check next character. */
		    break;		/* End of string */
	    }		/* Otherwise it's an apostrophe in the string. */
	    if ((result & 0xFF000000L) && !overflow) {
		Error (loc+s-operand, SizeErr);	/* Result overflowed! */
		overflow = TRUE;
		result = 0;
	    }
	    if (!overflow)
		result = (result << 8) + *s;
	    s++;
	}
    } else if ((*operand == '*') && (*(operand+1) == '\0')) {
	result = AddrCnt;	/* Value of location counter */
	Hunk2 = CurrHunk;	/* Use current section's hunk number. */
    } else {
	if (ReadSymTab (operand)) {	/* Look up symbol. */
	    result = Value;		/* Get its value. */
	    if (Pass2 && (Sym->Defn == NODEF)) {
	        if (GlobalXREF) {
		    Sym->Hunk = ~((long) Sym->Nam);
		    Sym->Defn = LineCount;
		    Sym->Flags |= 1;
		    result = Value;
		    AddRef (LineCount);
		} else
		    Error (loc, Undef);
	    }
	    else if (Sym->Flags & 0x60)
		Error (loc, AddrErr);	/* Can't use a register equate! */
/* Avoid a segfault on invalid code. -- Kevin Kofler, v.2.71.F3v */
	    while (Sym && Sym->Hunk < 0) {			/*  Make sure we're  */
	        operand = (void *) ~((long) Sym->Hunk);	/*  referencing the  */
		if (operand == Sym->Nam) break;		/* real operand, not */
		ReadSymTab (operand);			/*     an equate     */
	    }
	    if (Sym)
	        AddRef (LineCount);		/* Add reference. */
	    else
	        {result = 0; Error (loc, RelErr);}
	} else if (strcasecmp (operand, "NARG") == 0) {
	    result = InF->NArg;		/* Number of arguments */
	    if (result == -1)
		result = 0;		/* No arguments outside macros */
	} else {
	    if (Pass2 && GlobalXREF) {	/* Make all undefined symbols XREF. */
		AddSymTab(operand, 0L, 0L, LineCount, 1);
		Sym->Hunk = ~((long) Sym->Nam);
		ReadSymTab (operand);		/* Look up symbol. */
		result = Value;			/* Get its value. */
		AddRef (LineCount);		/* Add reference. */
	    } else {
	        Error (loc, Undef);		/* Undefined */
	    }
	}
    }
    if (neg) {
	result = -result;		/* Negative value */
	if (Hunk2 != ABSHUNK)
	    Error (loc, RelErr);	/* Must be absolute! */
    }
    return (result);
}



void AddSymTab (label, value, hunk, line, flags)
char label[];
long value, hunk;
int line, flags;
/* Inserts a new entry to the symbol table and increments NumSyms.
    "Sym" will be set to point to the new entry.
    If the label is a local label (i.e. the first character is
    a numeric digit or a backslash), the current contents of LabLine
    will be converted to characters and appended to the label before
    it is added.  If the first character of the label is a backslash
    (i.e. a named local variable) a dollar sign will be appended
    to the label ahead of the value of LabLine.			*/
{
    char wlab[MAXLINE], wnum[6];
    register struct SymTab *chainsym, **hashindex;

    strcpy (wlab, label);
    if (((label[0] >= '0') && (label[0] <= '9')) || (label[0] == '\\')) {
	if (label[0] == '\\')
/*	if ((label[0] == '\\') || (label[0] == '.')) */
	    strcat (wlab, "$");
	sprintf (wnum, "%d", LabLine);	/* If it's a local label, */
	strcat (wlab, wnum);		/*    append LabLine.     */
    }

    Sym = SymLim;			/* Pointer to new entry */
    SymLim++;				/* Bump limit pointer. */
    if (((char *) SymLim - (char *) SymCurr) > CHUNKSIZE) {
	Sym = (struct SymTab *) malloc ((unsigned) CHUNKSIZE);
	if (Sym == NULL)
	    quit_cleanup ("Out of memory!\n");
	SymCurr->Link = Sym;		/* Link from previous chunk */
	SymCurr = Sym;			/* Make the new chunk current. */
	SymCurr->Link = NULL;		/* Clear forward pointer. */
	Sym++;				/* Skip over pointer entry. */
	SymLim = Sym;			/* New table limit */
	SymLim++;			/* Bump it. */
    }
    Sym->Link = NULL;		/* Clear hash chain link. */
    Sym->Nam = AddName(wlab,0);	/* Pointer to symbol */
    Sym->Val   = value;		/* Value */
    Sym->Hunk  = hunk;		/* Hunk number */
    Sym->Defn  = line;		/* Statement number */
    Sym->Flags = flags;		/* Flags */
    Sym->Ref1  = NULL;		/* Reference pointer */
    NumSyms++;			/* Count symbols. */

    hashindex = HashIt (wlab);	/* Get hash index. */
    if (*hashindex == NULL) {
	*hashindex = Sym;	/* First entry in this hash chain */
	return;
    }
    chainsym = *hashindex;
    while (chainsym->Link != NULL)
	chainsym = chainsym->Link;	/* Scan for end of hash chain. */
    chainsym->Link = Sym;		/* Insert new entry at the end. */
}



char *AddName (name, macflag) char *name; int macflag;
/* Adds the name in "name" to the name heap.
    "macflag" can take any of the following values:
	0 - normal name
	1 - first line of macro text - there must be room on the
		name heap for at least one character following "name".
	2 - additional lines of macro text - make sure there's room
		for an addition character (as in 1 above).  Also,
		if it is necessary to allocate a new chunk of memory,
		first place a newline character after the last entry
		in the old chunk.  This acts as a flag when retrieving
		lines of macro text during an expansion.
    This function returns a pointer to "name" on the name heap. */
{
    register char *s, *t;
    struct NameChunk *n;

    s = NameLim + strlen(name) + 1;	/* The new entry ends here. */
    if (macflag)			/* If this is a macro, */
	s++;				/*  allow for continuation flag. */
    if ((s - (char *) NameCurr) > CHUNKSIZE) {	/* If this chunk is full */
	if (macflag == 2)		/* If this is more macro text */
	    *NameLim = '\n';		/*  insert continuation flag. */
	n = (struct NameChunk *) malloc ((unsigned) CHUNKSIZE);
	if (n == NULL)
	    quit_cleanup ("Out of memory!\n");
	NameCurr->Link = n;		/* Link from previous chunk */
	NameCurr = n;			/* Make the new chunk current. */
	NameCurr->Link = NULL;		/* Clear forward pointer. */
	s = (char *) NameCurr;
	NameLim = s + sizeof (char *);	/* Skip over pointer entry. */
    }
    s = NameLim;
    t = name;
    while ((*s++ = *t++) != '\0')	/* Store name. */
	;
    t = NameLim;
    NameLim = s;			/* Update table limit. */
    return (t);
}



int ReadSymTab (label) char label[];
/* Searches the symbol table for the given label.
   If not found, points Sym to NULL and returns FALSE.
   If found, points Sym to the proper table entry,
     and sets up the following fields:
	Value    - value of symbol
	Hunk2    - hunk number in which symbol resides
		   ABSHUNK if value is absolute
		   ones complement of symbol table index if external
	DefLine2 - statement number in which symbol was defined
			(NODEF if undefined )
    If the label is a local label (i.e. the first character is
    numeric), the current contents of LabLine will be converted
    to characters and appended to the label before it is searched.
    (This matches the way AddSymTab added the label to the table.)
    If the first character of the label is a backslash (i.e. a
    named local variable) a dollar sign will be appended to the
    label ahead of the value of LabLine.			*/
{
    char wlab[MAXLINE], wnum[6];
    register struct SymTab **hashindex;

    strcpy (wlab, label);
    if (((label[0] >= '0') && (label[0] <= '9')) || (label[0] == '\\')) {
	if (label[0] == '\\')
/*	if ((label[0] == '\\') || (label[0] == '.')) */
	    strcat (wlab, "$");
	sprintf (wnum, "%d", LabLine);	/* If it's a local label, */
	strcat (wlab, wnum);		/*    append LabLine.     */
    }
    hashindex = HashIt (wlab);	/* Get hash index. */
    Sym = *hashindex;
    while (Sym != NULL) {
	if (strcmp (Sym->Nam, wlab) == 0) {
	    Value = Sym->Val;		/* We found it. */
	    Hunk2 = Sym->Hunk;
	    if (!(Sym->Flags & 9))
		Hunk2 &= 0x0000FFFFL;	/* Isolate hunk number. */
	    DefLine2 = Sym->Defn;
	    return (TRUE);
	}
	Sym = Sym->Link;
    }
    Value = 0;			/* We didn't find it - */
    Hunk2 = ABSHUNK;		/*  set value to absolute zero. */
    DefLine2 = NODEF;
    return (FALSE);
}



struct SymTab **HashIt (label) register char *label;
/* Returns a pointer to the hash table entry corresponding to "label". */
{
    register unsigned i;

    i = 0;
    while (*label) {
	i = ((i << 3) - i + *label++) % HashSize;
    }
    return (Hash + i);
}



struct SymTab *NextSym (sym) register struct SymTab *sym;
/* Returns a pointer to the next symbol table entry in memory,
    or NULL if there are no more symbol table entries.
    SymChunk and SymChLim must be properly set up.	*/
{
    register struct SymTab *sp;

    if (sym == NULL)
	return (NULL);		/* We're nowhere - get out. */
    sym++;
    sp = sym;
    sp++;
    if ((SymLim >= SymChunk) && (SymLim <= SymChLim))
	if (sym >= SymLim)
	    return (NULL);	/* End of symbol table entries */
    if (sp > SymChLim) {
	if ((SymChunk = SymChunk->Link) == NULL)
	    return (NULL);	/* End of the last chunk */
	SymChLim = (struct SymTab *) ((char *) SymChunk + CHUNKSIZE);
	sym = SymChunk;
	sym++;			/* First entry in the new chunk */
    }
    return (sym);
}



void AddRef (linenum) int linenum;
/* Adds "linenum" to the list of references
    for the symbol pointed to by Sym.	*/
{
    register int i;
    register struct Ref *ref, *prevref;

    if (!Pass2)
	return;			/* Pass 2 only! */
    if (!XrefList)
	return;			/* No cross-reference */
    prevref = NULL;
    ref = Sym->Ref1;
    while (ref) {		/* Chase pointers. */
	for (i = 0; i < MAXREF; i++) {	/* Scan reference entry */
	    if (ref->RefNum[i] == 0) {	/*  for an empty slot.  */
		ref->RefNum[i]=linenum;	/* Insert new line number. */
		return;
	    }
	}
	prevref = ref;			/* Remember where we were. */
	ref = ref->NextRef;		/* Link to the next entry. */
    }
    ref = RefLim;			/* Pointer to new entry */
    RefLim++;				/* Bump limit pointer. */
    if (((char *) RefLim - (char *) SymCurr) > CHUNKSIZE) {
	ref = (struct Ref *) malloc ((unsigned) CHUNKSIZE);
	if (ref == NULL) {
	    fprintf (stderr, "     \nOut of memory");
	    fprintf (stderr, " - cross-reference disabled.\n");
	    XrefList = FALSE;
	    return;
	}
	SymCurr->Link = (struct SymTab *) ref;	/* Link from prev. chunk */
	SymCurr = (struct SymTab *)ref;	/* Make the new chunk current. */
	SymCurr->Link = NULL;		/* Clear forward pointer. */
	ref++;				/* Skip over pointer entry. */
	RefLim = ref;			/* New table limit */
	RefLim++;			/* Bump it. */
    }
    ref->NextRef = NULL;		/* Pointer to next entry */
    ref->RefNum[0] = linenum;		/* First reference in new entry */
    for (i = 1; i < MAXREF; i++)
	ref->RefNum[i] = 0;		/* Clear remaining slots. */
    if (prevref == NULL)
	Sym->Ref1 = ref;		/* Link to first entry */
    else
	prevref->NextRef = ref;		/* Link to next entry */
}



int CountNest (s) register char *s;
/* Returns 1 if "s" contains any IF statement (i.e. IFEQ, etc.).
   Returns -1 if "s" contains "ENDC" or "ENDIF".
   Returns 0 in all other cases.		*/
{
   if (strcasecmp (s, "ENDC") == 0)
	return (-1);
   else if (strcasecmp (s, "ENDIF") == 0)
	return (-1);
   else if (toupper(*s++) != 'I')
	return (0);
   else if (toupper(*s++) != 'F')
	return (0);
   else if (strcasecmp (s, "EQ") == 0)
	return (1);
   else if (strcasecmp (s, "NE") == 0)
	return (1);
   else if (strcasecmp (s, "GT") == 0)
	return (1);
   else if (strcasecmp (s, "GE") == 0)
	return (1);
   else if (strcasecmp (s, "LT") == 0)
	return (1);
   else if (strcasecmp (s, "LE") == 0)
	return (1);
   else if (strcasecmp (s, "C" ) == 0)
	return (1);
   else if (strcasecmp (s, "NC") == 0)
	return (1);
   else if (strcasecmp (s, "D" ) == 0)
	return (1);
   else if (strcasecmp (s, "ND") == 0)
	return (1);
   else
	return (0);
}



void Heap2Space (n) int n;
/* Die if we can't find "n" bytes on the secondary heap. */
{
    if ((NextFNS + n) > (char *) InF) {
	printf ("\n%5d   %s\n", LineCount, Line);
	quit_cleanup ("Secondary heap overflow - assembly terminated.\n");
    }
}



void ParseSpace (n) int n;
/* Special version of Heap2Space for the expression parser */
{
    if (((char *) Term + n) > (char *) Ops) {
	printf ("\n%5d   %s\n", LineCount, Line);
	quit_cleanup ("Secondary heap overflow - assembly terminated.\n");
    }
}
