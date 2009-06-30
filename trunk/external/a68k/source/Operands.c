/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*                Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*                Operand processor - April 16, 1991		    */
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



int GetArgs (name) char *name;
/* Gets macro arguments and adds them to FNStack after adding "name".
    Returns the number of arguments added to the stack.
    Note that this might not be the full number of arguments
    provided if the stack overflowed.				*/
{
    register char *s, *t;
    int narg, instring;
    int instring2,instring3; /* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b */
    char currarg[MAXLINE];		/* Current argument */

    narg = strlen (name) + 1;
    Heap2Space (narg);			/* Find space for name. */
    strcpy (NextFNS, name);		/* Add name to stack. */
    NextFNS += narg;			/* Bump pointer. */
    if (NextFNS > High2)
	High2 = NextFNS;		/* Update the high-water mark. */

    narg = 0;				/* Argument counter */

    s = Line + SrcLoc;			/* Now scan Line. */
    while (!isspace(*s) && (*s != ';') && (*s != '\0')) {
	t = currarg;
	if (instring = (*s == '<'))	/* String delimiter */
	    s++;

/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b: */
	if ((instring2=(*s=='\''))||(instring3=(*s=='\"')))
	    *t++ = *s++;		/* Get a character. */

	while (1) {
	    if (*s == '\0')
		break;			/* End of line */
	    if (instring) {
		if (*s == '>') {
		    s++;
		    break;		/* End of string */
		}
/* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3b:
   do not break up quoted strings! */
	    } else if (instring2) {
	    if (*s=='\'') {
		    *t++ = *s++;		/* Get a character. */
		    if (*s!='\'') break;
        }
	    } else if (instring3) {
	    if (*s=='\"') {
		    *t++ = *s++;		/* Get a character. */
		    if (*s!='\"') break;
        }

	    } else {
		if ((*s == ',')		/* End of operand */
		|| isspace(*s)		/* End of all operands */
		|| (*s == ';'))		/* Start of comments */
		    break;
	    }
	    *t++ = *s++;		/* Get a character. */
	}
	*t++ = '\0';
	Heap2Space (t - currarg);	/* Check for space. */
	strcpy (NextFNS, currarg);	/* Store argument. */
	NextFNS += t - currarg;		/* Next available space */
	if (NextFNS > High2)
	    High2 = NextFNS;		/* High-water mark */
	narg++;				/* Count arguments. */
	if (*s == ',')
	    s++;			/* Skip over separator. */
    }
    return (narg);			/* Successful completion */
}



void EffAdr (EA, Bad) register struct OpConfig *EA; int Bad;
/* Adds effective address field to Op (BITSET representing opcode). */
{
    if ((1 << (EA->Mode - 1)) IN Bad) {
	Error (EA->Loc, ModeErr);	/* Invalid mode */
	return;
    } else if (EA->Mode > 12)		/* Special modes */
	return;
    else if (EA->Mode < 8)		/* Register direct or indirect */
	Op |= ((EA->Mode - 1) << 3) | EA->Rn;
    else
	Op |= 0x0038 | (EA->Mode - 8);	/* Absolute modes */
    OperExt (EA);
}



void OperExt (EA) register struct OpConfig *EA;
/* Calculate operand Extension word, and check range of operands. */
{
    switch (EA->Mode) {
	case AbsL:
	    break;	/* No range checking is needed. */
	case AbsW:
	case ARDisp:
	case PCDisp:
	    /* In all-reloc mode, we need to compute the distance here.
	       -- Kevin Kofler, v.2.71.F3o */
	    if ((EA->Mode == PCDisp && AllRelocs && EA->Hunk == CurrHunk) ?
	           ((EA->Value-AddrCnt-EA->PCConv < -32768)
	            || (EA->Value-AddrCnt-EA->PCConv > 32767)) :
	           ((EA->Value < -32768) || (EA->Value > 32767)))
/*	    if ((EA->Value < -32768) || (EA->Value > 32767)) */
		Error (EA->Loc, SizeErr);
	    break;
	case ARDisX:
	case PCDisX:
	    /* In all-reloc mode, we need to compute the distance here.
	       -- Kevin Kofler, v.2.71.F3o */
	    if ((EA->Mode == PCDisX && AllRelocs && EA->Hunk == CurrHunk) ?
	           ((EA->Value-AddrCnt-EA->PCConv < -128)
	            || (EA->Value-AddrCnt-EA->PCConv > 127)) :
	           ((EA->Value < -128) || (EA->Value > 127)))
/*	    if ((EA->Value < -128) || (EA->Value > 127)) */
		Error (EA->Loc, SizeErr);
	    EA->Value &= 0x00FF;			/* Displacement */
	    EA->Value |= EA->Xn << 12;			/* Index reg. */
	    if (EA->X == Areg)     EA->Value |= 0x8000;	/* Addr. Reg. */
	    if (EA->Xsize == Long) EA->Value |= 0x0800;	/* Long reg.  */
	    break;
	case Imm:
	    if (Size == Word) {
		if ((EA->Value < -32768) || (EA->Value > 65535L))
		    Error (EA->Loc, SizeErr);
	    } else if (Size == Byte)
		if ((EA->Value < -128) || (EA->Value > 255))
		    Error (EA->Loc, SizeErr);
	    break;
    }
}



void GetOperand (oper, op, pcconv)
char *oper; register struct OpConfig *op; int pcconv;
/* Finds mode and value for source or destination operand.
    If PC-relative addressing is permitted, "pcconv" gives the
    offset to the displacement word; otherwise "pcconv" is zero. */
{
    register char *s, *t;
    register int  i;
    char *opend;
    char UCoper[MAXLINE], tempop[MAXLINE];
    int  rloc;
    long templong;

    op->Value  = op->Defn = 0;
    op->Mode   = Null;
    op->X      = X0;
    op->Hunk   = ABSHUNK;
    op->Single = FALSE;
    op->Unopt  = FALSE; /* initialize Unopt field -- Kevin Kofler, v.2.71.F3t */

    if (*oper == '\0')
	return;				/* There is nothing to process. */

    s = oper;
    t = UCoper;
    while (*s) {
	*t++ = /*toupper*/ (*s);		/* Upper-case version */
	s++;
    }
    *t = '\0';
    opend = s - 1;			/* Last character of operand */

    if (*oper == '#') {			/* Immediate */
	s = oper + 1;			/* The value starts here. */
	if (*s == '~')
	    s++;			/* Skip over unary NOT. */
	op->Value  = GetValue (s, (op->Loc)+1);
	op->Mode   = Imm;
	op->Hunk   = Hunk2;
	op->Defn   = DefLine2;
	op->Single = SingleFlag;
	if (*(oper+1) == '~') {		/* Unary NOT of entire value */
	    if (Hunk2 != ABSHUNK
	    || AddrDiffs /* added by Kevin Kofler in v.2.71.F3l */
	    ) {
		Error (op->Loc + 2, RelErr);
		op->Hunk = ABSHUNK;	/* Must be absolute! */
	    }
	    op->Value = ~(op->Value);	/* Flip all bits. */
	    if (Size == Byte)
		op->Value &= 0xFFL;	/* Trim to 8 bits. */
	    else if (Size == Word)
		op->Value &= 0xFFFFL;	/* Trim to 16 bits. */
	}
	return;
    }

    i = IsRegister (oper, opend-oper+1);
    if (i >= 0) {
	op->Mode = (i & 8) ? ARDir : DReg;	/* Register type */
	op->Rn = i & 7;				/* Register number */
	return;
    } else if (i == -2) {
	op->Mode = MultiM;			/* Equated register list */
	op->Value = Sym->Val;
	return;
    } else if ((*oper == '(') && (*opend == ')')) {
	i = IsRegister (oper+1, opend-oper-1);
	if (i >= 8 && i <= 15) {
	    op->Mode = ARInd;		/* Address Register indirect */
	    op->Rn = i - 8;
	    return;
	} else if (i != -1) {
	    Error (op->Loc, AddrErr);	/* Data register is invalid! */
	    return;
	}	/* else may be parenthesized expression */
    } else if ((*oper == '(')		/* Post-increment */
    && (*opend == '+')
    && (*(opend-1) == ')')) {
	op->Mode = ARPost;
	op->Rn = GetAReg (oper+1, opend-oper-2, op->Loc + 1);
	return;
    } else if ((*oper == '-')		/* Pre-decrement */
    && (*opend == ')')
    && (*(oper+1) == '(')) {
	i = IsRegister (oper+2, opend-oper-2);
	if (i >= 8 && i <= 15) {
	    op->Mode = ARPre;
	    op->Rn = i - 8;
	    return;
	} else if (i > 0) {
	    Error (op->Loc, AddrErr);	/* Data register is invalid! */
	    return;
	}	/* else parenthesized expression with leading minus? */
    } else if (strcasecmp (UCoper, "SR") == 0) {
	op->Mode = SR;				/* Status Register */
	return;
    } else if (strcasecmp (UCoper, "CCR") == 0) {
	op->Mode = CCR;			/* Condition Code Register */
	return;
    } else if (strcasecmp (UCoper, "USP") == 0) {
	op->Mode = USP;			/* User Stack Pointer */
	return;
    }

    /* Try to split off displacement (if present).
	We'll assume we have a register expression if the operand
	ends with a parenthesized expression not preceded by an
	operator.  I know this code is a real kludge, but that's
	the result of the bloody syntax.  Thanks, Motorola.	*/

    s = opend;				/* Last character */
    if (i = (*s == ')'))		/* Trailing parenthesis? */
	while (*(--s) != '(')		/* Find left parenthesis. */
	    if (s <= oper)
		break;
    if (s <= oper)			/* Must not be at beginning. */
	i = FALSE;
    if (i) {
	if (s == (oper+1)) {
	    if (*oper == '-')
		i = FALSE;		/* Leading minus sign */
	} else {
	    t = s - 1;
	    if (*t == '*') {		/* Location counter? */
		t--;
		if (!IsOperator (t) || (*t == ')'))
		    i = FALSE;		/* No, it's multiplication. */
	    } else if (IsOperator (t) && (*t != ')')) {
		i = FALSE;		/* Preceded by an operator */
	    }
	}
    }

    if (i) {		/* Looks like a displacement mode */
	*s = '\0';
	op->Value = GetValue (oper, op->Loc);	/* Displacement */
	op->Hunk  = Hunk2;			/* Hunk number */
	op->Defn  = DefLine2;			/* Line where defined */
	*s++ = '(';				/* Restore parenthesis. */

	rloc = op->Loc + s - oper;	/* The register starts here. */
	s = GetField (s, tempop);	/* Get address register. */
	if (*s == '\0')			/* If there's no index register, */
	    tempop[strlen(tempop)-1] = '\0';	/* chop off parenthesis. */

	if ((tempop[2] == '\0')
	&& (toupper (tempop[0]) == 'P')
	&& (toupper (tempop[1]) == 'C')) {
	    op->Mode = PCDisp;			/* Program Counter */
	    op->PCConv = pcconv; /* added by Kevin Kofler in v.2.71.F3o */
	    if (op->Hunk == CurrHunk) {
		if (!AllRelocs) { /* Do not adjust displacement when outputting all
		                     relocs. -- Kevin Kofler, v.2.71.F3l
		        But don't return immediately. -- Kevin Kofler, v.2.71.F3m */
		op->Value -= (AddrCnt+pcconv);	/* Adjust displacement. */
		op->Hunk = ABSHUNK; }
	    }
	} else {
	    if ((op->Value == 0)	/* If displacement is zero   */
	    && (op->Hunk == ABSHUNK)	/*  and is absolute          */
	    && (op->Defn < LineCount)	/*  and is already defined   */
	    && !(OpM68R IN AdrModeA)	/*  and isn't for a MOVEP    */
	    && !AddrDiffs /* and we don't have address differences to emit
                         -- Kevin Kofler, v.2.71.F3q */
	    && !NoOpt)			/*  and we can optimize      */
		op->Mode = ARInd;	/*  forget the displacement. */
	    else
		op->Mode = ARDisp;	/* Address reg. w/displacement */
	    op->Rn = GetAReg (tempop, strlen (tempop), rloc);
	}
	if (*s != '\0') {		/* Index register is present. */
	    if (op->Mode == PCDisp)
		op->Mode = PCDisX;	/* Program Counter indexed */
	    else
		op->Mode = ARDisX;	/* Address Register indexed */
	    if (*s != ',')
		Error (op->Loc, AddrErr);	/* Bad separator */
	    s++;				/* Skip separator. */
	    rloc = op->Loc + s - oper;		/* Start of index */
	    s = GetField (s, tempop);		/* Get index register. */
	    t = tempop + strlen(tempop);
	    if (*s == '\0')
		*(--t) = '\0';			/* Chop parenthesis. */
	    else
		Error (rloc, AddrErr);		/* It better be there. */

	    t -= 2;
	    if ((t < tempop) || (*t != '.')) {
		op->Xsize = Word;	/* Size defaults to 16 bits. */
		t += 3;
	    } else {
		*t++ = '\0';			/* Chop off size code. */
		switch (toupper (*t)) {
		case 'W':			/* Word */
		    op->Xsize = Word;
		    break;
		case 'L':			/* Long */
		    op->Xsize = Long;
		    break;
		default:
		    Error (op->Loc+s-1-oper, SizeErr);	/* Invalid size */
		    op->Xsize = Word;		/* Make it word for now. */
		}
	    }
	    i = IsRegister (tempop,t-tempop-1);	/* Get register. */
	    op->Xn = i & 7;			/* Index register number */
	    if ((i >= 0) && (i <= 7))
		op->X = Dreg;			/* Data Register */
	    else if ((i >= 8) && (i <= 15))
		op->X = Areg;			/* Address Register */
	    else
		Error (rloc, AddrErr);		/* Invalid register */
	}

#if 0 /* Bugfix by Kevin Kofler (v.2.71.F3m): Removed this broken check
         because it does not make any sense and causes spurious error
         messages. */
	if ((op->Hunk >= 0) && (op->Hunk != ABSHUNK))
	    Error (op->Loc, RelErr);	/*  Relocatable displacement */
#endif

/* Bugfix by Kevin Kofler (v.2.71.F3p): Handle the non-ABSHUNK-case correctly.*/
/*	if ((op->Mode == PCDisX) && (op->Hunk >= 0) && (op->Hunk != ABSHUNK))*/
/* Bugfix by Kevin Kofler (v.2.71.F3r): Hunk may also be negative. */
	if ((op->Mode == PCDisX) && (op->Hunk != ABSHUNK))
	  op->Value++;

	return;
    }

    if ((i = GetMultReg (oper, op->Loc)) != 0) {
	op->Value = (long) i;
	op->Mode = MultiM;		/* Register list for MOVEM */
	return;
    }

    if ((*oper == '(')		/* Operands of the form (xxxx).W or (xxxx).L */
    && (*(opend-2) == ')')
    && (*(opend-1) == '.')
    && ((toupper(*opend) == 'W') || (toupper(*opend) == 'L'))) {
	*(opend-1) = '\0';	/* Temporarily cut off length specifier. */
	op->Value  = GetValue (oper, op->Loc);	/* Get operand value. */
	op->Hunk   = Hunk2;
	op->Defn   = DefLine2;
	op->Single = SingleFlag;
	if (toupper(*opend) == 'W')
	    op->Mode = AbsW;	/* Absolute word */
	else
	  {
	    op->Mode = AbsL;	/* Absolute long */
	    /* Harcoded .l means the operand is unoptimizable. -- Kevin Kofler, v.2.71.F3t */
	    op->Unopt = TRUE;
	  }
	*(opend-1) = '.';	/* Restore original operand. */
	return;
    }

    op->Value  = GetValue (oper, op->Loc);	/* Plain old expression */
    op->Hunk   = Hunk2;
    op->Defn   = DefLine2;
    op->Single = SingleFlag;
    op->Mode   = AbsL;		/* Assume absolute long addressing. */

    if (NoOpt)
	return;			/* Do no optimizing. */

    if (DefLine2 < LineCount) {		/* Backward reference */

	if (Hunk2 < 0) {
	    return;		/* External - leave as absolute long. */

	} else if (Hunk2 == CurrHunk) {	/* Reference to current hunk */
	    if (pcconv) {
		templong = op->Value-(AddrCnt+pcconv);	/* PC disp. */
		if ((templong >= -32768) && (templong <= 32767)) {
		    op->Mode = PCDisp;	/* Convert to PC relative mode. */
		    op->PCConv = pcconv; /* added by Kevin Kofler in v.2.71.F3o */
		    if (AllRelocs) return; /* Do not adjust displacement when outputting
		                              all relocs. -- Kevin Kofler, v.2.71.F3l */
		    op->Value=templong;	/* Adjust displacement. */
		    op->Hunk = ABSHUNK;
		}
	    }

	} else if (Hunk2 == ABSHUNK) {	/* Absolute value */
	    if ((op->Value >= -32768) && (op->Value <= 32767))
		op->Mode = AbsW;	/* Absolute word */

	} else if ((SmallData != -1)
	&& (op->Value>=0) && (op->Value<=65535L)) {
	    op->Mode = ARDisp;		/* Make it a data reference     */
	    op->Rn = SmallData;		/*  through specified register. */
	    op->Value -= DataOffset;	/* Adjust displacement. */
	    op->Hunk = ABSHUNK;
	}
	return;			/* Could default to absolute long. */

    } else if (SmallData==-1) {	/* Fwd. reference - if not small data, */
	return;			/*  leave as absolute long addressing. */

    } else if (Brnch IN AdrModeA) {
	return;			/* Branches are handled elsewhere. */

    } else if (!Pass2) {	/* Forward reference, pass 1 */
	op->Mode = ARDisp;	/* Assume displacement       */
	op->Rn = SmallData;	/*  from specified register. */
	op->Hunk = ABSHUNK;
	return;

    } else {			/* On pass 2 we know what it is. */

	if (Hunk2 < 0) {
	    Error (op->Loc,FwdRef);	/* External - must be 32 bits. */
	    op->Mode = AbsW;		/* Force absolute word anyway. */

	} else if (Hunk2 == CurrHunk) {	/* It's in the current hunk. */
	    op->Mode = PCDisp;		/* Convert to PC relative mode. */
	    op->Value -= AddrCnt + pcconv;	/* Adjust displacement. */
	    op->Hunk = ABSHUNK;
	    if (!pcconv || (op->Value < -32768) || (op->Value > 32767))
		Error (op->Loc,FwdRef);	/* It doesn't fit! */

	} else if (Hunk2 == ABSHUNK) {	/* It's absolute. */
	    op->Mode = AbsW;		/* It has to fit in a word. */
	    if ((op->Value < -32768) || (op->Value > 32767))
		Error (op->Loc,FwdRef);	/* It doesn't fit! */

	} else {
	    op->Mode = ARDisp;		/* Assume data reference        */
	    op->Rn = SmallData;		/*  through specified register. */
	    op->Value -= DataOffset;	/* Adjust displacement. */
	    if ((op->Value < -32768) || (op->Value > 32767))
		Error (op->Loc,FwdRef);	/* It doesn't fit! */
	}
    }
}



int GetMultReg (oper, loc) char *oper; int loc;
/* Builds a register mask for the MOVEM instruction.
    Returns the mask in the low-order portion of its value if
    "oper" is a valid multiple-register list; otherwise returns 0. */
{
    register char *s, *t;
    register int  j;
    int t1, t2;		/* Temporary variables for registers */
    int range;		/* We're processing a range of registers. */
    int multext;	/* The result is built here. */

    multext = 0;
    range = FALSE;
    s = oper;
    if (IsOperator (s))
	return (0);			/* Starts with an operator! */

    while (1) {
	for (t = s; *t; t++) {
	    if ((*t == '-') || (*t == '/')) {
		break;
	    }
	}
	if ((multext == 0) && (*t == '\0'))
	    return (0);			/* Reject single term. */
	if ((t2 = IsRegister (s, (int)(t-s))) < 0)
	    return (0);			/* Not a recognizable register */

	if (!range) {
	    multext |= (1 << t2);	/* Single register */
	    t1 = t2;			/* Save number in case it's a range. */
	} else {			/* Range of registers */
	    range = FALSE;
	    if (t1 > t2) {
		j = t1;			/* Swap registers if backwards. */
		t1 = t2;
		t2 = j;
	    }
	    for (j = t1; j <= t2; j++)
		multext |= (1 << j);	/* Mark all registers in range. */
	    if (*t == '-')
		return (0);		/* Invalid range */
	}
	if (*t == '\0')
	    break;			/* Normal end of operand */
	if (*t++ == '-')
	    range = TRUE;		/* Range indicator */
	if (*t == '\0')
	    return (0);			/* Premature end of operand */
	s = t;
    }
    return (multext);
}



int GetAReg (op, len, loc) char *op; int len, loc;
/* Validate an address register specification.
    Valid specifications are A0 through A7, SP, or an EQUR label.
    The address register number will be returned if it is valid.
    Otherwise, Error will be called, using "loc" for the error
    location (this is its only use), and zero (A0) will be returned. */
{
    register int i;

    i = IsRegister (op, len);		/* Get register number. */
    if ((i >= 8) && (i <= 15))
	return (i - 8);			/* Valid address register */
    else {
	Error (loc, AddrErr);		/* Not an address register */
	return (0);			/* Set to A0. */
    }
}



int IsRegister (op, len) char *op; int len;
/* Check whether the current operand is an address or data register.
    Valid specifications are D0 through D7, A0 through A7, SP,
    or any symbol equated to a register with the EQUR directive.
    Return values:
	0 through 7 - data registers 0 through 7 respectively
	8 through 15 - address registers 0 through 7 respectively
	-1 - not a recognizable register
	-2 - Equated register list for MOVEM instruction (REG) */
{
    char tempop[MAXLINE];
    register char *s;
    register int  i;

    if (len == 2) {		/* Two-character specification */
	i = toupper (*op);
	s = op + 1;
	if ((i == 'S') && (toupper (*s) == 'P')) {
	    return (15);		/* Stack Pointer */
	} else if ((*s >= '0') && (*s <= '7')) {
	    if (i == 'A') {
		return (*s - '0' + 8);	/* Address Register */
	    } else if (i == 'D') {
		return (*s - '0');	/* Data Register */
	    }
	}
    }
    if (!GotEqur)			/* If we have no EQURs to check */
	return (-1);			/*  don't waste any time here.  */
    for (i = 0, s = op; i < len; i++) {
	if (IsOperator (s))
	    return (-1);		/* It sure isn't a label. */
	tempop[i] = *s++;
    }
    tempop[i] = '\0';
    if (ReadSymTab (tempop)) {
	if (Sym->Flags & 0x60) {
	    AddRef (LineCount);		/* Found a register or list. */
	    return ((Sym->Flags & 0x20) ? (int) Sym->Val : -2);
	}
    }
    return (-1);			/* Not a recognizable register */
}



int GetInstModeSize (Mode) register int Mode;
/* Determines the size for the various instruction modes. */
{
    switch (Mode) {
	case ARDisp:
	case ARDisX:
	case PCDisp:
	case PCDisX:
	case AbsW:
	    return (2);
	case AbsL:
	    return (4);
	case MultiM:
	    return (0);		/* Accounted for by code generator */
	case Imm:
	    if (Size == Long)
		return (4);
	    else
		return (2);
	default:
	    return (0);
    }
}
