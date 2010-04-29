/*------------------------------------------------------------------*/
/*								    */
/*			MC68000 Cross Assembler			    */
/*								    */
/*		  Copyright 1985 by Brian R. Anderson		    */
/*								    */
/*                    Main program - May 23, 1992		    */
/*								    */
/*   This program may be copied for personal, non-commercial use    */
/*   only, provided that the above copyright notice is included	    */
/*   on all copies of the source code.  Copying for any other use   */
/*   without the consent of the author is prohibited.		    */
/*								    */
/*------------------------------------------------------------------*/
/*								    */
/*              Originally published (in Modula-2) in		    */
/*          Dr. Dobb's Journal, April, May, and June 1986.          */
/*								    */
/*       AmigaDOS conversion copyright 1991 by Charlie Gibbs.	    */
/*								    */
/*------------------------------------------------------------------*/


#define PRIMARY
#include "A68kdef.h"
#include "A68kglb.h"

char *Version = "2.71.F3z (July 29, 2009)";

int main (argc,argv) int argc; char *argv[];
{
    char ListFN[MAXFN], EquateFN[MAXFN]; /* File names */
    int  makeequ;			/* Make an equate file. */
    int  keepobj;			/* Keep object file with errors. */
    int  endfile;			/* End-of-file flag */
    long maxheap2;			/* Maximum secondary heap size */
/*    int  cmderror, dummy; */
    int  cmderror; /* using now void in v.2.71.F3d - Kevin Kofler */
    long codesize, datasize, bsssize;
    int  *intptr;
    long templong;
    char tempchar[MAXLINE];
    register int i, j;
    struct SymTab **hashptr, **fsp;
    struct FwdBr *fp1, *fp2, *fp3;
    char *vopt[16]; int vopt_num; /* added by Paul Froissart in v.2.71.F3c
                                     variables for the -v switch */
    char *p;		/* Temporary - added by Paul Froissart in v.2.71.F3c
                       variable for the -r switch */

/*    dummy = 0; */  /* using now void in v.2.71.F3d - Kevin Kofler */
    Hash = NULL;	/* Clear all memory pointers - */
    SymStart = NULL;	/*  we haven't allocated anything yet. */
    NameStart = NULL;
    RelStart = NULL;
    Heap2 = NULL;
    SymSort = NULL;
    vopt_num=0; /* number of -v switches -  added by Paul Froissart
                                            in v.2.71.F3c */
    optim=-1;	/* All optimizations are allowed (variable for the -r switch)
                   added by Paul Froissart in v.2.71.F3c*/
    In.fd = Eq.fd = List.fd = Srec.fd = NOFD;	/* No files are open yet. */
    In.Buf = Eq.Buf = List.Buf = Srec.Buf = NULL;

    cmderror = FALSE;		/* Clear command-line error flag. */
    InclErrs = FALSE;
    SourceFN[0] = '\0';			/* We don't have source name yet. */
    HeaderFN[0] = EquateFN[0] = '\0';	/* No header or equate files yet */
    makeequ = FALSE;
    ListFN[0] = SrecFN[0] = '\0';	/* Indicate default file names. */
    InclList[0] = '\0';		/* Clear the include directory list. */
    IdntName[0] = '\0';		/* Clear program unit name. */
    DataOffset = 32768L;	/* Default small data offset */
    LnMax = 60;			/* Default page size */
    Quiet = 100;		/* Show progress every 100 lines. */
    strcpy (MacSize, "W");	/* Macro call size (\0) */
    XrefList = DumpSym = GotEqur = KeepTabs = keepobj = FALSE;
    Unaligned = FALSE; /* added by Kevin Kofler in v.2.71.F3k */
    AllRelocs = FALSE; /* added by Kevin Kofler in v.2.71.F3l */
    SuppList = TRUE;		/* Default to no listing file. */
    HashStats = FALSE;		/* Default to no hashing statistics. */
    HashSize = DEFHASH;		/* Hash table size default */
    maxheap2 = DEFHEAP2;	/* Secondary heap size default */
    DebugStart = NODEF /*32767*/; DebugEnd = 0;	/* Disable debug displays. */
    
    isError = FALSE; /* bugfix by Kevin Kofler for the TIGCC team in v.2.71.F3a */

    for (i = 0; i < 256; i++)
	OpPrec[i] = '\0';	/* Set up the operator precedence table. */
    i = (unsigned int) '('; OpPrec[i] = 1;
    i = (unsigned int) ')'; OpPrec[i] = 2;
    i = (unsigned int) '+'; OpPrec[i] = 3;
    i = (unsigned int) '-'; OpPrec[i] = 3;
    i = (unsigned int) '*'; OpPrec[i] = 4;
    i = (unsigned int) '/'; OpPrec[i] = 4;
    i = (unsigned int) '&'; OpPrec[i] = 5;
    i = (unsigned int) '!'; OpPrec[i] = 5;
    i = (unsigned int) '|'; OpPrec[i] = 5;
    i = (unsigned int) '<'; OpPrec[i] = 6;
    i = (unsigned int) '>'; OpPrec[i] = 6;

    for (i = 1; i < argc; i++) {	/* Analyze the command line. */
	if (argv[i][0] != '-') {
	    if (SourceFN[0] == '\0')
		strcpy (SourceFN, argv[i]);	/* Source file name */
	    else if (SrecFN[0] == '\0')
		strcpy (SrecFN, argv[i]);	/* Object file name */
	    else if (ListFN[0] == '\0')
		strcpy (ListFN, argv[i]);	/* Listing file name */
	    else {
		fprintf (stderr, "Too many file names.\n");
		cmderror = TRUE;
	    }
	} else {
	    switch (toupper(argv[i][1])) {
	    case 'A':			/* Output all relocs. */
		AllRelocs = TRUE;
		cmderror |= checkswitch (argv[i], "all relocs");
		break;
	    case 'D':			/* Dump the symbol table. */
		DumpSym = TRUE;
		strcpy (DumpSymList, &argv[i][2]);	/* Selections */
		break;
	    case 'E':			/* Equate file name */
		makeequ = TRUE;
		if (getfilename (EquateFN, &argv[i][2], "Equate", FALSE))
		    cmderror = keepobj = TRUE;
		break;
	    case 'F':			/* Dump the symbol table. */
		FwdProc = TRUE;
		cmderror |= checkswitch (argv[i], "forward reference");
		break;
	    case 'G':			/* XREF all unknown globals */
		GlobalXREF = TRUE;
		cmderror |= checkswitch (argv[i], "automatic XREF");
		break;
	    case 'H':			/* Header file name */
		if (getfilename (HeaderFN, &argv[i][2], "Header", TRUE))
		    cmderror = keepobj = TRUE;
		break;
	    case 'I':			/* Include directories */
		if (argv[i][2]) {
		    if (InclList[0])
			strcat (InclList, ",");	/* Add to previous list */
		    strcat (InclList, &argv[i][2]);
		} else {
		    fprintf (stderr, "Include directory list is missing.\n");
		    cmderror = keepobj = TRUE;
		}
		break;
	    case 'K':			/* Keep object code file. */
		keepobj = TRUE;
		cmderror |= checkswitch (argv[i], "object file keep");
		break;
	    case 'X':			/* Cross-reference listing */
		XrefList = TRUE;	/* Falls through to case 'L': */
	    case 'L':			/* Produce a listing file. */
		SuppList = FALSE;
		if (getfilename (ListFN, &argv[i][2], "List", FALSE))
		    cmderror = keepobj = TRUE;
		break;
	    case 'M':			/* Offset to small data base */
		if (argv[i][2] == '\0') {
		    fprintf (stderr, "Small data offset is missing.\n");
		    cmderror = keepobj = TRUE;
		    break;
		}
		if (!isdigit (argv[i][2])) {
		    fprintf (stderr, "Small data offset is invalid.\n");
		    cmderror = TRUE;
		    break;
		}
		DataOffset = CalcValue (&argv[i][2], 0);
		break;
	    case 'N':			/* Suppress optimization. */
		NoOpt = TRUE;
		cmderror |= checkswitch (argv[i], "optimization suppress");
		break;
	    case 'O':			/* Object file name */
		if (getfilename (SrecFN, &argv[i][2], "Object", TRUE))
		    cmderror = keepobj = TRUE;
		break;
	    case 'P':			/* Page depth */
		if (argv[i][2] == '\0') {
		    fprintf (stderr, "Page depth is missing.\n");
		    cmderror = keepobj = TRUE;
		    break;
		}
		if (!isdigit (argv[i][2])) {
		    fprintf (stderr, "Page depth is invalid.\n");
		    cmderror = TRUE;
		    break;
		}
		if ((LnMax = CalcValue (&argv[i][2], 0)) < 10) {
		    fprintf (stderr, "Page depth is invalid.\n");
		    cmderror = TRUE;
		}
		break;
	    case 'Q':			/* Quiet console display */
		if (argv[i][2] == '\0') {
		    Quiet = 0;
		    break;
		}
		if (!isdigit (argv[i][2])) {
		    fprintf (stderr, "Quiet interval is invalid.\n");
		    cmderror = TRUE;
		    break;
		}
		Quiet = CalcValue (&argv[i][2], 0);
		break;
 	    case 'R':			/* Remove optimization - added by Paul Froissart
                               in v.2.71.F3c */
 		p=&argv[i][2];
 		while (*p) switch (tolower(*p++)) {
 		    case 'a': optim &= ~OPTIM_ADDA; break;
 		    case 'l': optim &= ~OPTIM_LEA; break;
 		    case 'm': optim &= ~OPTIM_MOVEM; break;
 		    default: cmderror |= checkswitch (argv[i],"optimization removal");
            /* a slightly dirty, but correct way to catch invalid options */
 		}
 		break;
	    case 'S':			/* Motorola S-format */
		SFormat = TRUE;
		cmderror |= checkswitch (argv[i], "S-format");
		break;
	    case 'T':			/* Keep tabs in listing. */
		KeepTabs = TRUE;
		cmderror |= checkswitch (argv[i], "tab");
		break;
	    case 'U':			/* Allow unaligned DC/DCB/DS.W/L and code. */
		Unaligned = TRUE;
		cmderror |= checkswitch (argv[i], "unaligned");
		break;
 	    case 'V':		/* Set value - added by Paul Froissart in v.2.71.F3c
                           also checks if more than 16 -v switches (Kevin
                           Kofler) */
		if (vopt_num<=15) {
		vopt[vopt_num++]=&argv[i][2];
		if (!argv[i][2]) { cmderror=TRUE; fprintf(stderr,"Invalid SET value switch (-v).\n"); }
		} else {
		cmderror=TRUE; fprintf(stderr,"Too many SET value switches (-v).\n");
		}
		break;
	    case 'W':			/* Work storage size(s) */
		if (argv[i][2] == '\0') {
		    fprintf (stderr, "Work storage size is missing.\n");
		    cmderror = keepobj = TRUE;
		    break;
		}
/*		if (argv[i][2] != ',') {*/
		if ((argv[i][2] != ',') && (argv[i][2] != ';')) {
		/* also allow ; as separator - Kevin Kofler, v.2.71.F3f */
		    GetField (argv[i]+2, tempchar);
		    if (!isdigit (tempchar[0])) {
			fprintf (stderr, "Hash table size is invalid.\n");
			cmderror = TRUE;
			break;
		    }
		    HashSize = CalcValue (tempchar, 0);
		    if (HashSize >= 16384) {
			fprintf (stderr, "Hash table size is too big.\n");
			cmderror = TRUE;
		    }
		}
		for (j = 2; argv[i][j]; j++) {
/*		    if (argv[i][j] == ',') {*/
		    if ((argv[i][j] == ',') || (argv[i][j] == ';')) {
		    /* also allow ; as separator - Kevin Kofler, v.2.71.F3f */
		    /* Find secondary size. */
			if (!isdigit (argv[i][j+1])) {
			    fprintf (stderr, "Secondary size is invalid.\n");
			    cmderror = TRUE;
			    break;
			}
			maxheap2 = CalcValue (&argv[i][j+1], 0);
			if (maxheap2 < MAXLINE)
			    maxheap2 = MAXLINE;
			maxheap2 &= ~3L;
			break;
		    }
		}
		break;
	    case 'Y':			/* Display hashing statistics. */
		HashStats = TRUE;
		cmderror |= checkswitch (argv[i], "hash statistics");
		break;
	    case 'Z':			/* Debug option */
		DebugStart = 0;
		DebugEnd = NODEF /*32767*/;
/*		if (argv[i][2] != ',') {*/
		if ((argv[i][2] != ',') && (argv[i][2] != ';')) {
		/* also allow ; as separator - Kevin Kofler, v.2.71.F3f */
		/* Debug dump starts here. */
		    GetField (argv[i]+2, tempchar);
		    if (!isdigit (tempchar[0])) {
			fprintf (stderr, "Debug start line is invalid.\n");
			cmderror = TRUE;
			break;
		    }
		    DebugStart = CalcValue (tempchar, 0);
		}
		for (j = 2; argv[i][j]; j++) {
/*		    if (argv[i][j] == ',') {*/
		    if ((argv[i][j] == ',') || (argv[i][j] == ';')) {
		    /* also allow ; as separator - Kevin Kofler, v.2.71.F3f */
		    /* Debug dump ends here. */
			if (!isdigit (argv[i][j+1])) {
			    fprintf (stderr, "Debug end line is invalid.\n");
			    cmderror = TRUE;
			    break;
			}
			DebugEnd = CalcValue (&argv[i][j+1], 0);
			if (DebugEnd == 0)
			    DebugEnd = NODEF /*32767*/;
		    }
		}
		break;
	    default:
		fprintf (stderr, "Unrecognized switch: %c\n", argv[i][1]);
		cmderror = TRUE;
		break;
	    }
	}
    }

    if (makeequ)
	defaultfile (EquateFN, ".equ");	/* Default equate file name */
    if (!SuppList)
	defaultfile (ListFN, ".lst");	/* Default list file name */
    else			/* If there's no listing, don't bother */
	KeepTabs = TRUE;	/*  expanding tabs - it's faster.      */
    if (SFormat)
	defaultfile (SrecFN, ".s");	/* Default S-format file name */
    else
	defaultfile (SrecFN, ".o");	/* Default object file name */

/* Check for duplicate file names. */

    if (SourceFN[0]) {
	cmderror |= checkdupfile (SourceFN, "Source", EquateFN, "equate");
	cmderror |= checkdupfile (SourceFN, "Source", ListFN, "listing");
	cmderror |= checkdupfile (SourceFN, "Source", SrecFN, "object");
    } else {
	fprintf (stderr, "Source file name is missing.\n");
	cmderror = TRUE;
    }
    if (EquateFN[0]) {
	cmderror |= checkdupfile (EquateFN, "Equate", ListFN, "listing");
	cmderror |= checkdupfile (EquateFN, "Equate", SrecFN, "object");
    }
    if (ListFN[0]) {
	cmderror |= checkdupfile (ListFN, "Listing", SrecFN, "object");
    }

/*	Open files.	*/

    if (!cmderror) {				/* Source file */
	if ((In.Buf = (char *) malloc (BUFFSIZE)) == NULL)
	    quit_cleanup ("Out of memory!\n");
#ifdef MSDOS
	_fmode = O_BINARY;
#endif
	if ((In.fd = open (SourceFN, 0)) < 0) {
	    fprintf (stderr, "Unable to open source file.\n");
	    In.fd = NOFD;
	    cmderror = TRUE;
	}
	In.Ptr = In.Lim = In.Buf;
    }
#ifdef MSDOS
    _fmode = O_TEXT;
#endif
    if (!cmderror && EquateFN[0])		/* Equate file */
	cmderror |= xopen (EquateFN, &Eq, "equate");

    if (!cmderror && !SuppList)			/* Listing file */
	cmderror |= xopen (ListFN, &List, "listing");

#ifdef MSDOS
    if (!SFormat)
	_fmode = O_BINARY;
#endif
    if (!cmderror)				/* Object code file */
	cmderror |= xopen (SrecFN, &Srec, "object code");
#ifdef MSDOS
    _fmode = O_BINARY;
#endif

    if (cmderror) {
	fprintf (stderr, "\n");
	fprintf (stderr, "68000 Assembler - version %s\n", Version);
	fprintf (stderr,
		 "Copyright 1985 by Brian R. Anderson\n"
		 "AmigaDOS conversion copyright 1991 by Charlie Gibbs.\n"
		 "Adapted for use with Fargo by David Ellsworth.\n"
#ifdef ST_VERSION
		 "Atari ST conversion copyright 1990 by Colin Fox and Darren Schebek\n\n"
#endif ST_VERSION
#ifdef WIN32
         "Win32 port copyright 2001-2005 by Kevin Kofler for the TIGCC project\n"
#endif
         "Bugfixes and additions by Julien Muchembled, Paul Froissart and Kevin Kofler\n\n"
		 "Usage: a68k <source file>\n"
		 "            [-a]                    [-p<page depth>]\n"
		 "            [-d[[!]<prefix>]]       [-q[<quiet interval>]]\n"
		 "            [-e[<equate file>]]     [-r[a][l][m]]\n"
		 "            [-f]                    [-s]\n"
		 "            [-g]                    [-t]\n"
		 "            [-h<header file>]       [-u]\n"
		 "            [-i<include dirlist>]   [-v<name>[,<value>]]\n"
		 "            [-k]                    "
		 "[-w[<hash size>][,<heap size>]]\n"
		 "            [-l[<listing file>]]    [-x]\n"
		 "            [-m<small data offset>] [-y]\n"
		 "            [-n]                    "
		 "[-z[<debug start>][,<debug end>]]\n"
		 "            [-o<object file>]\n\n"
		 "Heap size default:  -w"
		 "%ld,%ld\n", (long) DEFHASH, (long) DEFHEAP2);
	SrecFN[0] = '\0';	/* Don't scratch the object file! */
	quit_cleanup ("\n");
    }

    if (Quiet != 0) {
	printf ("68000 Assembler - version %s\n", Version);
	printf ("Copyright 1985 by Brian R. Anderson\n"
		"AmigaDOS conversion copyright 1991 by Charlie Gibbs.\n"
		"Adapted for use with Fargo by David Ellsworth.\n"
#ifdef ST_VERSION
		"Atari ST conversion copyright 1990 by Colin Fox and Darren Schebek\n\n"
#endif ST_VERSION
#ifdef WIN32
		"Win32 port copyright 2001-2005 by Kevin Kofler for the TIGCC project\n"
#endif
		"Bugfixes and additions by Julien Muchembled, Paul Froissart and Kevin Kofler\n\n"
		"Assembling %s\n\n", SourceFN);
    }

/* Allocate initial symbol table chunks. */

    templong = sizeof (struct SymTab *) * HashSize;
    Hash = (struct SymTab **) malloc ((unsigned) templong);
    if (Hash == NULL)
	quit_cleanup ("Out of memory!\n");
    for (hashptr = Hash, i = 0; i < HashSize; hashptr++, i++)
	*hashptr = NULL;	/* Clear the hash table. */

    SymStart = (struct SymTab *) malloc ((unsigned) CHUNKSIZE);
    if (SymStart == NULL)
	quit_cleanup ("Out of memory!\n");
    SymCurr = SymStart;			/* Make the first chunk current. */
    SymCurr->Link = NULL;		/* Clear forward pointer. */
    SymLim = SymCurr;
    SymLim++;				/* Start of names */

    NameStart = (struct NameChunk *) malloc ((unsigned) CHUNKSIZE);
    if (NameStart == NULL)
	quit_cleanup ("Out of memory!\n");
    NameCurr = NameStart;		/* Make the first chunk current. */
    NameCurr->Link = NULL;		/* Clear forward pointer. */
    NameLim = (char *) NameCurr + sizeof (char *);  /* Start of names */

/* Allocate the relocation attribute table. */

    RelStart = (struct RelTab *) malloc ((unsigned) CHUNKSIZE);
    if (RelStart == NULL)
	quit_cleanup ("Out of memory!\n");
    RelCurr = RelStart;			/* Relocation table */
    RelCurr->Link = NULL;		/* No additional chunks */
    RelLast = NULL;			/* There are no entries yet. */
    RelLim = RelStart;
    RelLim++;				/* First unused space */

/* Allocate the secondary heap (input files and parser stack). */

    Heap2 = malloc ((unsigned) maxheap2);
    if (Heap2 == NULL)
	quit_cleanup ("Out of memory!\n");

/* Allocate the INCLUDE skip table. */

    SkipLim = (struct SkipEnt *) malloc ((unsigned) INCSKSIZ);
    if (SkipLim == NULL)
	quit_cleanup ("Out of memory!\n");
    SkipIdx = SkipLim;
    SetFixLim = (struct SetFixup *) ((char *) SkipLim + INCSKSIZ);
    IncStart = 0;

/* Allocate the forward branch optimization log. */

    if (NoOpt) {
	FwdStart = NULL;
    } else {
	FwdStart = (struct FwdTable *) malloc ((unsigned) FWDSIZE);
	if (FwdStart == NULL)
	    quit_cleanup ("Out of memory!\n");
	FwdCurr = FwdStart;	/* Make the first chunk current. */
	FwdCurr->Link = NULL;	/* Clear forward pointer. */
	FwdLim2 = (int *) ((char *) FwdCurr + sizeof (struct FwdTable *));
	FwdPtr = FwdLim2;	/* Current position in pass 2 */
    }

/*-------------------------------------------------------------------

    Begin Pass 1.
								   */
    Pass2 = FALSE;
    startpass ('1', maxheap2);
    NumSyms = 0;	/* There's nothing in the symbol table yet. */
    NextHunk = 0L;	/* Start in hunk zero. */
    LowInF = InF;	/* Initialize secondary heap usage pointers. */
    High2 = NextFNS;
    Low2  = (char *) LowInF;
    FwdLim1     = FwdBranch;	/* Forward branch controls */
    FwdFixLimit = FwdBranchFix;

    /* Define ".A68K" as a SET symbol with an absolute value of 1.
	This allows programs to identify this assembler.	*/
    AddSymTab (".A68K", 1L, (long) ABSHUNK, 0, 4);  /* All spellings */
    AddSymTab (".A68k", 1L, (long) ABSHUNK, 0, 4);
    AddSymTab (".a68K", 1L, (long) ABSHUNK, 0, 4);
    AddSymTab (".a68k", 1L, (long) ABSHUNK, 0, 4);

    /* If -a is given, define "__ld_all_relocs" as an XDEF symbol to a dummy label.
	This allows linkers to identify such object files. -- Kevin Kofler, v.2.71.F3l	*/
    if (AllRelocs) AddSymTab ("__ld_all_relocs", 0L, 0L, 1, 2);

    /* Add -v variables - added by Paul Froissart in 2.71.F3c: */
    while (vopt_num) {
    	char *s=vopt[--vopt_num]; long val; /* decrement vopt_num at the
    	                               beginning - Kevin Kofler, v.2.71.F3c */
/*    	while (*s && *s!=',') s++;*/
    	while ((*s) && (*s!=',') && (*s!=';') && (*s!='=')) s++;
    	/* also allow ; and = as separators - Kevin Kofler, v.2.71.F3f */
    	if (s==vopt[vopt_num]) /* exit(0x39); */
    	quit_cleanup ("Invalid SET value switch (-v).\n");
    	/* exit cleanly and with an error message - Kevin Kofler, v.2.71.F3c */
    	if (!*s) val=1; else { *s++=0; val=CalcValue(s,0); }
    	AddSymTab(vopt[vopt_num],val,(long)ABSHUNK,0,4);
    	/* vopt_num--; */ /* see above - Kevin Kofler, v.2.71.F3c */
    }

    endfile = FALSE;
    Dir = None;
    while (!endfile && (Dir != End)) {
	PrevDir = Dir;			/* Save previous directive. */
	endfile = LineParts (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Get a statement. */
pass1cont: /* necessary for optional END - Paul Froissart, v.2.71.F3c */
	GetObjectCode (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);		/* Process the statement. */

	if (IncStart != 0) {
	    if ((OpCode[0] != '\0') && (Dir < SkipDir)) {
		IncStart = 0;			/* We can't      */
		if (SkipLim->Set1 != NULL) {	/*  skip this    */
		    SetFixLim = SkipLim->Set1;	/*  INCLUDE file */
		    SetFixLim++;		/*  in pass 2.   */
		}
	    }
	}
	if ((HunkType == HunkNone) && (AddrAdv != 0)) {
	    DoSection ("", 0, "", 0, "", 0);	/* Start unnamed CODE section. */
	    MakeHunk = TRUE;
	}
	if ((Label[0] != '\0')			/* If the statement is labeled */
	&& (Dir != Set) && (Dir != Equr) && (Dir != Reg)) {
	    if (!ReadSymTab (Label)) {		/* Make a new entry. */
		AddSymTab (Label, AddrCnt, CurrHunk, LineCount, 0);
	    } else if ((Sym->Flags & 1)		/* If dup., ignore... */
	    || (Sym->Defn == NODEF)) {		/* else fill in... */
		Sym->Val = AddrCnt;		/* Current loc. */
		Sym->Hunk = CurrHunk;		/* Hunk number */
		Sym->Defn = LineCount;		/* Statement number */
		Sym->Flags &= ~1;		/* Clear XREF flag. */
		if (Sym->Flags & 0x80) {	/* If it's PUBLIC, */
		    Sym->Flags |= 2;		/*  make it XDEF. */
		}
	    }
        else isError=TRUE; /* bugfix by Kevin Kofler for the TIGCC team in
                              v.2.71.F3a
                              if duplicate, set "error detected" flag */
	    if (Dir == Equ) {
		Sym->Val = ObjSrc;		/* Equated value */
		Sym->Hunk = Src.Hunk;		/* Hunk number */
	    }
	    if (!NoOpt && (Dir != Equ)) {	/* Forward optimization */
		PackFwdBranch (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);		/* Drop expired entries. */
		fp1 = FwdBranch;
		while (fp1 < FwdLim1) {		/* Scan forward branches. */
		    if (fp1->FwdSym == Sym) {	/* It branched here. */
			if (fp1->Loc != (AddrCnt-4)) {	/* Don't make zero displacement! */
			    if (fp1->Line < LineCount)
				Sym->Val -= 2;	/* Move the label back. */
			    AddrCnt -= 2;	/* Shorten the program. */
			    for (fsp = FwdBranchFix; fsp < FwdFixLimit; fsp++) {
				if (fp1->Loc<(*fsp)->Val) {	/* Adjust labels  */
				    (*fsp)->Val -= 2;		/*  within range. */
				}
			    }
			    if ((char *) FwdLim2 >= ((char *) FwdCurr + FWDSIZE)) {
				FwdCurr->Link =	/* Get a new chunk. */
				    (struct FwdTable *) malloc ((unsigned) FWDSIZE);
				if (FwdCurr->Link == NULL)
				    quit_cleanup ("Out of memory!\n");
				FwdCurr = FwdCurr->Link;
				FwdCurr->Link = NULL; /* Make sure it does not link to anything
				It is a freshly allocated block AT THE END of the linked list!
				bugfix submitted by [Pollux] (Paul Froissart) for 2.71.F3a */
				FwdLim2 = (int *)
				    ((char *) FwdCurr + sizeof (struct FwdTable *));
			    }
			    *FwdLim2++ = fp1->Line; /* Flag this branch in pass 2. */
			}
			fp3 = fp2 = fp1;
			fp3++;
			while (fp3 < FwdLim1) {	/* Remove processed entry. */
			    fp2->Loc    = fp3->Loc - 2;	/* Locations shifted too! */
			    fp2->FwdSym = fp3->FwdSym;
			    fp2->Line   = fp3->Line;
			    fp2++;
			    fp3++;
			}
			FwdLim1--;	/* Decrement table limit pointer. */
			fp1--;		/* Offset increment below. */
		    }
		    fp1++;		/* Check the next entry. */
		}
		if (FwdLim1 > FwdBranch) {	/* Store labels within    */
		    *FwdFixLimit++ = Sym;	/*  range of fwd. branch. */
		}
	    }
	}
	AddrCnt	+= AddrAdv * DupFact;	/* Advance the location counter. */

	if (endfile) { /* END is now optional */
	    strcpy(Line," end"); endfile=0;	/* Enhancement by Paul Froissart */
	    SubArgs(/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/); GetParts(/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/*     in version v2.71.F3c      */
	    goto pass1cont;
	}

    }
    if ((HunkType == HunkNone) && (NumSyms != 0)) { /* Dummy section   */
	DoSection ("", 0, "", 0, "", 0);	    /*  to get XDEF    */
	MakeHunk = TRUE;			    /*  symbols if any */
    }
    if (HunkType != HunkNone)
	if (AddrCnt > OrgHigh)
	    Sect->Val = AddrCnt;	/* End of the last section */
	else
	    Sect->Val = OrgHigh;	/* We've ORGed higher. */

    if (InclErrs)
	quit_cleanup ("Fatal errors - assembly aborted\n");

    if (Quiet > 0)
	fprintf (stderr, "%d\n", LineCount);
    else if (Quiet < 0)
	fprintf (stderr, "%d\n\n", InF->Line);



/*----------------------------------------------------------------

	Begin Pass 2.
								*/
    Pass2 = TRUE;
    lseek (In.fd, 0L, 0);		/* "Rewind" the source file. */
    In.Ptr = In.Lim = In.Buf;
    startpass ('2', maxheap2);
/*    RefLim = (struct Ref *) SymLim; */	/* Cross-reference table */
    RefLim = (struct Ref *) (SymLim+1);
/* don't overwrite the last symbol - Kevin Kofler, v.2.71.F3h */

/* Calculate the total size of each section type,
    reset all section pointers to the beginning, and
    write all absolute symbols to an equate file if desired. */

    codesize = datasize = bsssize = 0;
    if (EquateFN[0]) {
	xputs (&Eq, "* Equate file for ");
	xputs (&Eq, SourceFN);
	xputs (&Eq, "\n* Created by");
	xputs (&Eq, " A68k version ");
	xputs (&Eq, Version);
	xputs (&Eq, "\n");
    }
    Sym = SymChunk = SymStart;
    Sym++;
    SymChLim = (struct SymTab *) ((char *) SymChunk + CHUNKSIZE);
    while (Sym) {
	if (Sym->Flags & 0x10) {
	    templong = (Sym->Val + 3) & ~3L;		/* Hunk size */
	    j = (Sym->Hunk & 0x3FFF0000L) >> 16;	/* Hunk type */
	    if (j == HunkCode)		/* Accumulate sizes by type. */
		codesize += templong;
	    else if (j == HunkData)
		datasize += templong;
	    else
		bsssize += templong;
	    Sym->Val = 0L;		/* Back to start of all sections */
	}
	if (EquateFN[0]) {
	    if (((Sym->Hunk & 0x00007FFFL) == ABSHUNK)
	    && ((Sym->Flags == 0) || (Sym->Flags == 2))) {
		xputs (&Eq, Sym->Nam);
		xputs (&Eq, "\tEQU\t$");
		LongPut (&Eq, Sym->Val, 4);
		xputs (&Eq, "\n");
	    }
	}
	Sym = NextSym (Sym);	/* Try for another symbol table entry. */
    }
    if (EquateFN[0])
	xclose (&Eq);

/* Write sign-on messages for listing file. */

    LnCnt = LnMax;
    PgCnt = 0;
    if (!SuppList) {
	CheckPage (&List, FALSE);		/* Print headings. */
	xputs (&List, "68000 Assembler - version ");
	xputs (&List, Version);
	xputs (&List, "\nCopyright 1985 by Brian R. Anderson.\n");
	xputs (&List, "AmigaDOS conversion copyright 1991");
	xputs (&List, " by Charlie Gibbs.\n\n");
	LnCnt += 4;
    }

    StartSrec (&Srec, IdntName);	/* Write object header record. */

/*	Process the second pass.	*/

    endfile = FALSE;
    Dir = None;
    while (!endfile && (Dir != End)) {
	PrevDir = Dir;			/* Save previous directive. */
	endfile = LineParts (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Get a statement. */

	/* if (!endfile) { */
pass2cont: /* necessary for optional END - Paul Froissart, v.2.71.F3c */

	    GetObjectCode (/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/* Process the statement. */
	    if (Label[0] != '\0') {	/* If statement is labeled, */
		ReadSymTab (Label);	/*  check for duplicate defn. */
        if (Sym) /* Check if Sym actually exists -- bugfix by Kevin Kofler for
                    the TIGCC Team in v.2.71.F3a. */
        { /* (TIGCC) */
 		if (Sym->Defn != LineCount) {
		    AddRef (LineCount);	/* Got one - flag as reference. */
		    if (Dir == Set) {
			if ((Sym->Flags & 4) == 0)
			    Error (LabLoc, SymDup); /* Can't SET normal label. */
		    } else {
			Error (LabLoc, SymDup);	/* Ordinary duplicate */
		    }
		} else if (Dir == Set) {
		    AddRef (LineCount);	/* Flag all SETs as references. */
		} else {
		    if (Sym->Val != AddrCnt)
			if ((Dir != Equ) && (Dir != Equr) && (Dir != Reg))
			    Error (0, Phase);	/* Assembler error */
		}
        } /* (TIGCC) */
        else /* (TIGCC) */
        Error (0, Phase); /* (TIGCC) */
	    }
	    WriteListLine (&List);
/*	    WriteSrecLine (&Srec); */
	    WriteSrecLine (); /* removed useless parameter - Kevin Kofler, v.2.71.F3d */
	    AddrCnt += AddrAdv * DupFact;	/* Advance the location counter. */

/*	} else {
	    Error (0, EndErr);		** END statement is missing. **
	    WriteListLine (&List); */
	if (endfile) { /* END is now optional */
	    strcpy(Line," end"); endfile=0;	/* Enhancement by Paul Froissart */
	    SubArgs(/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/); GetParts(/*dummy*/ /*(using now void in v.2.71.F3d - Kevin Kofler)*/);	/*     in version v2.71.F3c      */
	    goto pass2cont;

	}
    }
    if ((HunkType == HunkNone) && (NumSyms != 0)) { /* Dummy section   */
	DoSection ("", 0, "", 0, "", 0);	    /*  to get XDEF    */
	MakeHunk = TRUE;			    /*  symbols if any */
    }

/*----------------------------------------------------------------

	Clean up.
								*/

    if (HunkType != HunkNone)
	if (AddrCnt > OrgHigh)
	    Sect->Val = AddrCnt;	/* End of the last section */
	else
	    Sect->Val = OrgHigh;	/* We've ORGed higher. */

    if (Quiet > 0)
	fprintf (stderr, "%d", LineCount);	/* Final line number */
    else if (Quiet < 0)
	fprintf (stderr, "%d\n", InF->Line);
    fflush (stderr);			/* Make sure it gets out. */

    close (In.fd);		/* We're finished with the source file. */
    In.fd = NOFD;
    free (In.Buf);
    In.Buf = NULL;

    EndSdata (&Srec, EndAddr);	/* Write remaining data and end record. */
    xclose (&Srec);		/* We're finished with the object file. */
    if ((ErrorCount != 0) && (!keepobj))
	unlink (SrecFN);	/* Scratch it if there were errors. */

    RelCurr = RelStart;
    RelStart = NULL;
    while (RelCurr != NULL) {
	RelLim = RelCurr;
	RelCurr = RelCurr->Link;
	free (RelLim);		/* Free the relocation table. */
    }

    if (Heap2 != NULL) {
	free (Heap2);		/* Free the secondary heap. */
	Heap2 = NULL;
    }

    if (XrefList)
	WriteSymTab (&List);	/* List the symbol table. */

/* Display final error count. */

    if (Quiet != 0)
	fprintf (stderr, "\nEnd of assembly - ");
    if (!SuppList)
	xputs (&List, "\nEnd of assembly - ");
    if (ErrorCount == 0) {
	if (Quiet != 0)
	    fprintf (stderr, "no errors were found.\n");
	if (!SuppList)
	    xputs (&List, "no errors were found.\n");
    } else if (ErrorCount == 1) {
	if (Quiet != 0)
	    fprintf (stderr, "1 error was found.\n");
	if (!SuppList)
	    xputs (&List, "1 error was found.\n");
    } else {
	if (Quiet != 0)
	    fprintf (stderr, "%d errors were found.\n", ErrorCount);
	if (!SuppList) {
	    sprintf (tempchar, "%d errors were found.\n", ErrorCount);
	    xputs (&List, tempchar);
	}
    }

/* Display heap usage. */

    if (Quiet != 0)
	fprintf (stderr, "Heap usage:  -w%ld", HashSize);
    if (!SuppList) {
	sprintf (tempchar, "Heap usage:  -w%ld", HashSize);
	xputs (&List, tempchar);
    }
    templong = (long) (High2 - Heap2);
    if (Low2 < (char *) LowInF)
	templong += (long) (Heap2 + maxheap2 - Low2);
    else
	templong += (long) (Heap2 + maxheap2 - (char *) LowInF);
    if (Quiet != 0)
	fprintf (stderr, ",%ld\n", templong);
    if (!SuppList) {
	sprintf (tempchar, ",%ld\n", templong);
	xputs (&List, tempchar);
    }

/* Display the total size of all section types. */

    if (Quiet != 0) {
	fprintf (stderr, "Total hunk sizes:  %lx code, ", codesize);
	fprintf (stderr, "%lx data, %lx BSS\n", datasize, bsssize);
    }
    if (!SuppList) {
	sprintf (tempchar, "Total hunk sizes:  %lx code, ", codesize);
	xputs (&List, tempchar);
	sprintf (tempchar, "%lx data, %lx BSS\n", datasize, bsssize);
	xputs (&List, tempchar);
    }

/* Display hashing statistics if required. */

    if (HashStats && (NumSyms != 0)) {
	printf ("\n");
	printf ("HASH CHAIN STATISTICS - %d symbols\n\n", NumSyms);
	templong = (NumSyms + 1) * sizeof (int);
	HashCount = (int *) malloc ((unsigned) templong);
	if (HashCount == NULL)
	    quit_cleanup ("Out of memory!\n");

	printf ("Length     No. of chains\n");
	printf ("------     -------------\n");
	intptr = HashCount;
	for (i = 0; i <= NumSyms; i++)
	    *(intptr++) = 0;	/* Clear hash chain length counters. */

	hashptr = Hash;
	for (i = 0; i < HashSize; i++) {
	    j = 0;
	    if ((Sym = *hashptr) != NULL) {
		j++;		/* This chain has at least one entry. */
		while ((Sym = Sym->Link) != NULL) {
		    j++;	/* Count entries in the chain. */
		}
	    }
	    intptr = HashCount + j;
	    (*intptr)++;	/* Bump counter by chain length. */
	    hashptr++;
	}
	intptr = HashCount;
	for (i = 0; i <= NumSyms; i++) {
	    if (*intptr)
		printf ("%4d          %4d\n", i, *intptr);
	    intptr++;
	}
	free (HashCount);		/* Free hash statistics table. */
	HashCount = NULL;
    }

/* All done! */
    if (!SuppList) {
	xputs (&List, "\f");	/* One last page eject */
	xclose (&List);		/* We're finished with the listing file. */
    }
    quit_cleanup ("");		/* Normal termination */
}



/*======================================================================*/
/*									*/
/*              Subroutines used by the main program			*/
/*									*/
/*======================================================================*/



int getfilename (name, arg, desc, needit)
char *name, *arg, *desc;
int needit;
/* If "name" is not a duplicate, copies "arg" to it, else flags
    duplicate using "desc".  If "needit" is TRUE, also flags
    an error if "arg" is a null string.
    Returns TRUE if an error is found, FALSE otherwise. */
{
    if (*name) {
	fprintf (stderr, "%s file is declared more than once.\n", desc);
	return (TRUE);
    }
    if (*arg) {
	strcpy (name, arg);
	return (FALSE);
    }
    if (needit) {
	fprintf (stderr, "%s file name is missing\n", desc);
	return (TRUE);
    }
    return (FALSE);
}



int checkswitch (sw, name) char *sw, *name;
/* Displays an error message and returns TRUE if the argument
    pointed to by "s" is more than two characters long.
    Just returns FALSE otherwise.				*/
{
    if (strlen (sw) > 2) {
	fprintf (stderr, "Invalid %s switch (%s).\n", name, sw);
	return (TRUE);
    } else {
	return (FALSE);
    }
}



void defaultfile (name, ext) char *name, *ext;
/* If "name" is a null string, search for the last period in "name"
    (if any) and append "ext".
    If "name" doesn't contain a period, append a period and "ext". */
{
    char *s;

    if (*name == '\0') {	/* If name isn't specified... */
	strcpy (name,SourceFN);	/* Start with source file name. */
	s = name+strlen(name);	/* Scan backwards for period. */
	while (--s > name) {
	    if (*s == '.') {
		*s = '\0';	/* Chop off name extension. */
		break;
	    }
	}
	strcat (name, ext);	/* Add name extension. */
    }
}



int checkdupfile (name1, desc1, name2, desc2)
char *name1, *desc1, *name2, *desc2;
/* If "name1" is the same as "name2", display an error message using
    "desc1" and "desc2" and return TRUE.  Otherwise, return FALSE. */
{
    if (strcmp (name1, name2) == 0) {
	fprintf (stderr,
	    "%s and %s file names are the same.\n", desc1, desc2);
	return (TRUE);
    } else {
	return (FALSE);
    }
}



void startpass (pchar, maxheap2) char pchar; long maxheap2;
/* Set up to start the next pass. */
{
    if (Quiet > 0) {
	fprintf (stderr, "PASS %c line ", pchar);
	fflush (stderr);
    } else if (Quiet < 0) {
	fprintf (stderr, "PASS %c\n", pchar);
    }
    NextFNS = Heap2;
    InF = (struct InFCtl *) (Heap2 + maxheap2);
    InF--;
    InFNum = OuterMac = SkipNest = InF->Pos = InF->MCnt = 0;
    InF->Line = 0;
    InF->UPtr = 0;
    InF->NPtr = NextFNS;
    InF->NArg = -1;
    InF->MCnt = 0;
    strcpy (NextFNS, SourceFN);
    ShowFile (FALSE);			/* Show source file name. */
    NextFNS += strlen (SourceFN) + 1;
    LineCount = LabLine = MacCount = ErrorCount = 0;
    AddrCnt = CurrHunk = SectStart = EndAddr = 0L;
    HunkType = HunkNone;		/* We're not in a hunk yet. */
    HunkFlags = SectLine = HunkSeq = 0;
    ListOff = MakeHunk = InnrFMac = FALSE;
    SmallData = -1;
    TTLstring[0] = '\0';		/* Clear the title string. */
}



void quit_cleanup (s) char *s;
/* Clean up and exit.  If "s" doesn't point to a null string, print the
    string as an error message, remove the partially-formed object
    file if it exists, and exit with an error code.			*/
{
    if (In.fd != NOFD)			/* Close all files... */
	close (In.fd);
    if (In.Buf != NULL)			/*  and free buffers. */
	free (In.Buf);
    if (Srec.fd != NOFD)
	xclose (&Srec);
    if (List.fd != NOFD)
	xclose (&List);
    if (Eq.fd != NOFD)
	xclose (&Eq);

    if (Hash != NULL)
	free (Hash);			/* Free the hash table. */

    SymCurr = SymStart;
    while (SymCurr != NULL) {
	SymLim = SymCurr;
	SymCurr = SymCurr->Link;
	free (SymLim);			/* Free the symbol table. */
    }

    NameCurr = NameStart;
    while (NameCurr != NULL) {
	NameLim = (char *) NameCurr;
	NameCurr = NameCurr->Link;
	free (NameLim);			/* Free the name table. */
    }

    RelCurr = RelStart;
    while (RelCurr != NULL) {
	RelLim = RelCurr;
	RelCurr = RelCurr->Link;
	free (RelLim);			/* Free the relocation table. */
    }

    FwdCurr = FwdStart;
    while (FwdCurr != NULL) {
	FwdLim2 = (int *) FwdCurr;
	FwdCurr = FwdCurr->Link;
	free (FwdLim2);			/* Free the forward branch log. */
    }

    if (Heap2 != NULL)
	free (Heap2);			/* Free the secondary heap. */

    if (SymSort != NULL)
	free (SymSort);			/* Free symbol table sort area. */

    if (HashCount != NULL)
	free (HashCount);		/* Free hash statistics table. */

    /* Get rid of leftover AddrDiffs list -- Kevin Kofler, v.2.71.F3l */
    while (AddrDiffs) {
      struct AddrDiff *next=AddrDiffs->Link;
      free(AddrDiffs);
      AddrDiffs=next;
    }

    if (*s) {				/* If we have an error message, */
	if (SrecFN[0])
	    unlink (SrecFN);		/*  scratch the object file,    */
	fprintf (stderr, "%s", s);	/*  display the error message,  */
	exit (20);			/*  and die. */
    } else {
	exit (ErrorCount ? 10 : 0);	/* Normal termination */
    }
}
