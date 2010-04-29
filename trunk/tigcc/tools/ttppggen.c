/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttppggen.c
* initial date:    28/09/2000
* authors:         thomas.nussbaumer@gmx.net
* description:     executes all steps to convert a TI binary to a PPG file
*                  (PPG == packed program used in combination with ttstart)
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttppggen may also be
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

#include "tt.h"          // generic defines
#include "ttversion.h"   // TI-68k Developer Utilities version info
#include "revtools.h"    // automatic cvs version extraction


#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.8"

#define EMBEDDED_USE
#include "ttbin2oth.c"
#include "packhead.h"    // compressed header definition
#include "ttpack.c"
#include "ttstrip.c"
#undef EMBEDDED_USE

//#define NO_KERNEL_PROGS 1   // disables generation of kernel-dependent ppgs


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTPPGGen");
#ifdef NO_KERNEL_PROGS
    fprintf(USAGE_OUT, "Usage: ttppggen [flags] <infile> <varname>\n\n"\
                       "-quiet  ... don't output standard messages\n"\
                       "-nowarn ... optional flag to turn off signature missmatch warning\n\n"\
                       "infile  ... a valid 89z or 9xz NOSTUB program\n"\
                       "varname ... name of output ppg (without extension)\n\n"\
                       "NOTE: the type of file (89y or 9xy) which will be generated depends\n"\
                       "      on the extension of the given infile\n\n");
#else
    fprintf(USAGE_OUT, "Usage: ttppggen [flags] <infile> <varname>\n\n"\
                       "-quiet  ... don't output standard messages\n"\
                       "-nowarn ... optional flag to turn off signature missmatch warning\n\n"\
                       "infile  ... a valid 89z or 9xz program (NOSTUB or kernel-dependent)\n"\
                       "varname ... name of output ppg (without extension)\n\n"\
                       "NOTE: the type of file (89y or 9xy) which will be generated depends\n"\
                       "      on the extension of the given infile\n\n");
#endif
}


//=============================================================================
// outputs a message with a special prefix
//=============================================================================
void OutMsg(char*s) {
    fprintf(stdout,"%s\n",s);
}


//=============================================================================
// delete temporary files
//=============================================================================
void CleanUp() {
    remove("tmp_ppg.bin");
    remove("tmp_ppg.pck");
}


//=============================================================================
// as usual: the main entry point ...
//=============================================================================
int main(int argc, char* argv[]) {
    char* infilename   = NULL;
    char* varname      = NULL;
    //char* description  = 0;
    int   ti89_version = 0;
    int   no_warn      = 0;
    int   quiet        = 0;
    char  buffer[1000];
    int   n;
    char* tool_params[6];
    FILE* fp;
    long  inputsize;
    long  outputsize;

    if (argc < 3) {
        PrintUsage();
        return 1;
    }

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-quiet"))       quiet   = 1;
        else if (!strcmp(argv[n], "-nowarn")) no_warn = 1;
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }
        else if (!infilename) infilename = argv[n];
        else if (!varname)    varname    = argv[n];
        else {
#ifndef EMBEDDED_USE
            PrintUsage();
#endif
            return 1;
        }
    }

    if (!infilename || !varname) {
        PrintUsage();
        return 1;
    }

    if (!quiet) PRINT_ID("TTPPGGen");

    //-------------------------------------------------------------------------
    // check infilename for minimum length (1 character + ".89z" or ".9xz"
    //-------------------------------------------------------------------------
    n = strlen(infilename);
    if (n<5) {
        fprintf(stderr,"ERROR: inputfile name too short\n");
        return 1;
    }

    //-------------------------------------------------------------------------
    // check extension of infilename
    //-------------------------------------------------------------------------
    if (!strncmp(&infilename[n-4],".89z",4))      ti89_version = 1;
    else if (!strncmp(&infilename[n-4],".9xz",4)) ti89_version = 0;
    else {
        fprintf(stderr,"ERROR: infile has neither extension '.89z' nor '.9xz'\n");
        return 1;
    }

    //-------------------------------------------------------------------------
    // check signature of infile
    //-------------------------------------------------------------------------
    if (!(fp = fopen(infilename,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infilename);
        return 1;
    }

    if (fread(buffer,8,1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot read 8 characters from inputfile %s\n",infilename);
        fclose(fp);
        return 1;
    }

    if (!no_warn) {
        if (strncmp(buffer,ti89_version ? SIGNATURE_TI89 : SIGNATURE_TI92P,8)) {
            fprintf(stderr,"WARNING: calctype signature does not match with extension\n");
            //   it's just a warning now, because if a file is loaded back from the
            //   calc to the pc, for both calcs the same signature will be used ... argghh TI
        }
    }

    //-------------------------------------------------------------------------
    // check if infile is a DOORS program which cannot be used with ExePack
    //-------------------------------------------------------------------------
    fseek(fp,92,SEEK_SET);
    if (fread(buffer,4,1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot extract possible DOORS signature from inputfile %s\n",infilename);
        fclose(fp);
        return 1;
    }

#ifdef NO_KERNEL_PROGS
    if (!strncmp(buffer,"68kP",4) || !strncmp(buffer,"68kL",4)) {
        fprintf(stderr,"ERROR: Only NOSTUB programs can be used with ExePack !!\n");
        fclose(fp);
        return 1;
    }
#else
    if (!strncmp(buffer,"68kL",4)) {
        fprintf(stderr,"ERROR: Only programs (no libs) can be used with ExePack !!\n");
        fclose(fp);
        return 1;
    }
#endif

    // get input size
    fseek(fp,0,SEEK_END);
    inputsize = ftell(fp);

    fclose(fp);

    //--------------------------------------------------------
    // stripping pc header
    //--------------------------------------------------------
    if (!quiet) fprintf(stdout,"stripping pc header .....\n");
    tool_params[0] = "";
    tool_params[1] = "-quiet";
    tool_params[2] = infilename;
    tool_params[3] = "tmp_ppg.bin";
    if (TTStrip(4,tool_params)) {
        CleanUp();
        return 1;
    }

    //--------------------------------------------------------
    // packing temporary file
    //--------------------------------------------------------
    if (!quiet) fprintf(stdout,"packing result .....\n");
    tool_params[0] = "";
    tool_params[1] = "tmp_ppg.bin";
    tool_params[2] = "tmp_ppg.pck";
    if (TTPack(3,tool_params)) {
        CleanUp();
        return 1;
    }

    //--------------------------------------------------------
    // encapsulate packed file into OTH
    //--------------------------------------------------------
    if (!quiet) fprintf(stdout,"convert packed file to OTH file .....\n");

    if (ti89_version) tool_params[2] = "-89";
    else              tool_params[2] = "-92";

    tool_params[0] = "";
    tool_params[1] = "-quiet";
    tool_params[3] = "ppg";
    tool_params[4] = "tmp_ppg.pck";
    tool_params[5] = varname;

    if (TTBin2OTH(6,tool_params)) {
        CleanUp();
        return 1;
    }

    // get output size
    if (ti89_version) sprintf(buffer,"%s.89y",varname);
    else              sprintf(buffer,"%s.9xy",varname);

    if (!quiet) {
        if ((fp=fopen(buffer,"rb"))) {
            fseek(fp,0,SEEK_END);
            outputsize = ftell(fp);
            fclose(fp);
            fprintf(stdout,"%s (size=%ld) written to %s (size=%ld) - ratio=%lf\n",
                    infilename,inputsize,buffer,outputsize,(double)outputsize/(double)inputsize);
        }
        else {
            fprintf(stderr,"ERROR: cannot open successully generated file? Should NEVER happen\n");
            CleanUp();
            return 1;
        }
    }

    CleanUp();
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
// Revision 1.8  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.7  2002/03/13 21:32:25  tnussb
// (1) parameter handling of -quiet added
// (2) use "-quiet" as parameter for ttbin2oth and ttstrip calls
// (3) printing of statistics if successfully completes added
// (4) checking of inputfile extension fixed
// (5) usage text modified
//
// Revision 1.6  2002/03/04 14:35:02  tnussb
// No use of external TIGCC Tools anymore, but using now compiled-in versions.
// This way this tool can be distributed by its own without the need of a
// TIGCC Tools Suite installation.
//
// Revision 1.5  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.4  2002/02/07 09:15:45  tnussb
// conversion of kernelbased programs enabled
//
// Revision 1.2  2000/11/28 00:08:29  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.1  2000/10/01 15:01:59  Thomas Nussbaumer
// initial version
//
