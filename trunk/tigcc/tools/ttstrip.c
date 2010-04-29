/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttstrip.c
* initial date:    13/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     strips PC header and trailing checksum from an TI executable
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttstrip may also be
  redistributed or sold without source code, for any purpose. (The Lesser
  General Public License restrictions do apply in other respects; for example,
  they cover modification of the program.) This exception notice must be
  removed on modified copies of this file.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef __TTSTRIP__
#define __TTSTRIP__

// if EMBEDDED_USE is defined, than we use this sourcefile from within another
// sourcefile
#ifndef EMBEDDED_USE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.8"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
#ifndef EMBEDDED_USE
    PRINT_ID("TTStrip");
#endif
    fprintf(USAGE_OUT, "Usage: ttstrip [-quiet] <infile> <outfile>\n\n"                     \
                       "       -quiet ... don't output standard messages\n\n"               \
                       "       extracts the raw binary content of a TI file by stripping\n" \
                       "       the header and trailing checksum which are only necessary\n" \
                       "       for the link software to send the data to the calc.\n"       \
                       "       NOTE: after stripping these informations it is no longer\n"  \
                       "       possible to distinguish between a TI89 and a TI92p file.\n"  \
                       "       So be careful!\n\n");
}


//=============================================================================
// its a main ...
//=============================================================================
int main(int argc,char *argv[]) {
#else
int TTStrip(int argc,char *argv[]) {
#endif
    char*          infile    = NULL;
    char*          outfile   = NULL;
    FILE*          ifp;
    FILE*          ofp;
    char           sig[9];
    int            length;
    int            striplen;
    int            n;
    int            quiet = 0;

    // check for too less arguments
    if (argc < 3) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-quiet"))  quiet = 1;
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }
        else if (!infile)    infile    = argv[n];
        else if (!outfile)   outfile   = argv[n];
        else {
#ifndef EMBEDDED_USE
            PrintUsage();
#endif
            return 1;
        }
    }

    // check if all necessary arguments are supplied
    if (!infile || !outfile) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

#ifndef EMBEDDED_USE
    if (!quiet) PRINT_ID("TTStrip");
#endif


    if (!(ifp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    memset(sig,0,9);
    fread(sig,1,8,ifp);
    fseek(ifp,0,SEEK_END);
    length = ftell(ifp);
    length -= 88;   // 86 bytes for header + 2 bytes of trailing checksum
    rewind(ifp);
    fseek(ifp,86,SEEK_SET);

    if (strcmp(sig,SIGNATURE_TI89) && strcmp(sig,SIGNATURE_TI92P)) {
        fprintf(stderr,"WARNING: neither TI89 nor TI92 inputfile\n");
    }

    if (!(ofp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
        fclose(ifp);
        return 1;
    }

    striplen = length;
    while (length--) fputc(fgetc(ifp),ofp);

    fclose(ifp);
    fclose(ofp);

    if (!quiet) fprintf(stdout,"%d bytes written to %s\n",striplen,outfile);

    return 0;
}
#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.8  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.7  2002/03/14 08:59:47  tnussb
// (1) new flag "-quiet" added (suppress standard messages)
// (2) usage text completely rewritten
//
// Revision 1.6  2002/03/04 14:32:42  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.5  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.4  2000/11/28 00:04:21  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.3  2000/08/23 20:21:29  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.2  2000/08/13 20:23:15  Thomas Nussbaumer
// usage text modified
//
// Revision 1.1  2000/08/13 16:26:02  Thomas Nussbaumer
// initial version
//
//
//
