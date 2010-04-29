/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttbin2bin.c
* initial date:    21/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     converts a TI89 binary to a TI92p binary or
*                  a TI92p binary to a TI89 binary by just patching the
*                  header information
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttbin2bin may also be
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

// if EMBEDDED_USE is defined, than we use this sourcefile from within another
// sourcefile

#ifndef EMBEDDED_USE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"
#include "strhead.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.7"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTBin2Bin");
    fprintf(USAGE_OUT, "Usage: ttbin2bin [-quiet] <infile>\n\n"  \
                       "       quiet  ... don't output standard messages\n\n"\
                       "       converts TI89 binary to TI92p binary or TI92p binary to TI89 binary\n"  \
                       "       by just patching the header information. the generated outputfile\n" \
                       "       will have the same name, but a different extension.\n" \
                       "       NOTE1: this should be only applied to datafiles or executables\n" \
                       "              which uses no compiled in calc depending constants\n" \
                       "       NOTE2: the program auto-detects the infile type by the extension\n" \
                       "              if the extension starts with '.89' it will treated as \n"\
                       "              TI89 inputfile. if it starts with '.9x' it will be treated\n" \
                       "              as TI92p inputfile.\n\n");
}


//=============================================================================
// ... its not much in this file beside the main ...
//=============================================================================
int main(int argc,char *argv[]) {
#else
int TTBin2Bin(int argc,char *argv[]) {
#endif
    char* infile;
    char  outfile[1024];
    FILE* ifp;
    FILE* ofp;
    char  tmp[1024];
    int   calctype = -1;
    int   n;
    char  lastchar;
    int   quiet = 0;

    // check for too less arguments
    if (argc != 2 && argc != 3) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

    if (argc == 3) {
        if (!strcmp(argv[1],"-quiet")) quiet = 1;
        else {
#ifndef EMBEDDED_USE
            PrintUsage();
#endif
            return 1;
        }
        infile = argv[2];
    }
    else {
        infile = argv[1];
    }

#ifndef EMBEDDED_USE
    if (!quiet) PRINT_ID("TTBin2Bin");
#endif

    n = strlen(infile);
    if (n<5) {
        fprintf(stderr,"ERROR: inputfile name too short\n");
        return 1;
    }

    if (!strncmp(&infile[n-4],".89",3))      calctype = CALC_TI89;
    else if (!strncmp(&infile[n-4],".9x",3)) calctype = CALC_TI92P;
    else {
        fprintf(stderr,"ERROR: no extension or not starting with '.89' or '.9x'\n");
        return 1;
    }

    lastchar = infile[n-1];

    if (!(ifp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    if (fread(tmp,8,1,ifp) != 1) {
        fprintf(stderr,"ERROR: cannot read 8 characters from inputfile %s\n",infile);
        fclose(ifp);
        return 1;
    }

    if (strncmp(tmp,calctype == CALC_TI89 ? SIGNATURE_TI89 : SIGNATURE_TI92P,8)) {
        fprintf(stderr,"ERROR: calctype signature does not match\n");
        fclose(ifp);
        return 1;
    }

    strncpy(outfile,infile,n-3);
    sprintf(&outfile[n-3],calctype == CALC_TI89 ? "9x%c" : "89%c",lastchar);

    if (!(ofp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outfile %s\n",outfile);
        fclose(ifp);
        return 1;
    }

    if (fwrite(calctype == CALC_TI89 ? SIGNATURE_TI92P : SIGNATURE_TI89,8,1,ofp) != 1) {
        fprintf(stderr,"ERROR: problem while writing to outfile %s\n",outfile);
        fclose(ifp);
        fclose(ofp);
        return 1;
    }

    //-----------------------------------------------------------------------
    // I know this copy loop is slow - but who cares on near GHz computers ??
    //-----------------------------------------------------------------------
    do {
        n = fgetc(ifp);
        if (n == EOF) break;
        fputc(n,ofp);
    }
    while (1);

    if (!quiet) fprintf(stdout,"%ld bytes written to outputfile %s.\n",ftell(ofp),outfile);

    fclose(ifp);
    fclose(ofp);

    return 0;
}

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.7  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.6  2002/03/14 11:35:19  tnussb
// new commandline parameter -quiet (suppress standard messages)
//
// Revision 1.5  2002/03/04 14:32:41  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.4  2002/02/07 09:49:36  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.3  2000/11/28 00:06:33  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.2  2000/08/23 19:28:08  Thomas Nussbaumer
// adapted to automatic version printing (revtools.h)
//
// Revision 1.1  2000/08/21 17:12:27  Thomas Nussbaumer
// initial version
//
//
//
