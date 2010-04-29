/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttextract.c
* initial date:    13/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     extracts a part of a file which starts after the starttoken
*                  and ends just before the endtoken
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttextract may also be
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
#include <string.h>
#include <ctype.h>

#include "ttversion.h"
#include "revtools.h"


#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.8"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTExtract");
    fprintf(USAGE_OUT, "Usage: ttextract [flags] <infile> <outfile> <starttoken> [<endtoken>]\n\n"\
                       "       -quiet .... don't output standard messages (unsets -v)\n"          \
                       "       -v     .... output additional messages (unsets -quiet)\n"          \
                       "       -sh    .... treat start token as hexcode\n"                        \
                       "       -eh    .... treat end token as hexcode\n\n"                        \
                       "       Extract part of infile which starts right after the start token.\n"\
                       "       If an endtoken is given it ends just before the end token.\n"      \
                       "       NOTE1: If you use -sh or -eh the given tokens have to be plain\n"  \
                       "              hexcodes WITHOUT any prefix like \"0x\" or \"0h\"\n"        \
                       "       NOTE2: if you want the extracted data to be correct, you have to\n"\
                       "              use -fno-unit-at-a-time, as long as this switch is\n"       \
                       "              supported by GCC\n\n");
}


//=============================================================================
// converts hex digit to number
//=============================================================================
unsigned char ConvHexDigit(unsigned char c) {
    c = tolower(c);
    if (c >= '0' && c <= '9') return c -'0';
    return c-'a'+10;
}


//=============================================================================
// find given token in file
//=============================================================================
long FindToken(FILE* fp, char* token, int treat_as_hex) {
    int length = strlen(token);
    int i;
    int index = 0;
    int input;
    int found = 0;

    if (treat_as_hex) {
        if (length % 2 == 1) {
            fprintf(stderr,"ERROR: invalid length (%d) for hextoken (%s)\n",length,token);
            return -1;
        }
        for (i=0;i<length;i+=2) {
            if (isxdigit(token[i]) && isxdigit(token[i+1])) {
                token[i/2] = ConvHexDigit(token[i]) * 16 + ConvHexDigit(token[i+1]);
            }
            else {
                fprintf(stderr,"ERROR: invalid character in hextoken (%s)\n",token);
                return -1;
            }
        }
        length /= 2;
    }

    rewind(fp);

    // find start ....
    while ((input = fgetc(fp)) != EOF) {
        if (input == (unsigned char)token[index]) {
            index++;
            if (index == length) {
                found = 1;
                break;
            }
        }
        else {
            index = 0;
            if (input == token[0]) {
                index++;
                if (index == length) {
                    found = 1;
                    break;
                }
            }
        }
    }

    return (found) ? ftell(fp) : -2;
}


//=============================================================================
// what should I say?
//=============================================================================
int main(int argc, char* argv[]) {
    FILE *infp;
    FILE *outfp;
    char *infile     = NULL;
    char *outfile    = NULL;
    char *starttoken = NULL;
    char *endtoken   = NULL;
    long  startpos;
    long  endpos;
    int   endtokenlength = 0; // 1.40: added initialization, hope it is correct.
    long  count;
    int   n;
    int   hexstart = 0;
    int   hexend   = 0;
    int   quiet    = 0;
    int   verbose  = 0;

    if (argc < 4) {
        PrintUsage();
        return 1;
    }

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-sh"))         hexstart   = 1;
        else if (!strcmp(argv[n], "-eh"))    hexend     = 1;
        else if (!strcmp(argv[n], "-quiet")) quiet=1,verbose=0;
        else if (!strcmp(argv[n], "-v"))     verbose=1,quiet=0;
        else if (!infile)                    infile     = argv[n];
        else if (!outfile)                   outfile    = argv[n];
        else if (!starttoken)                starttoken = argv[n];
        else if (!endtoken)                  endtoken   = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    if (!infile || !outfile || !starttoken) {
        PrintUsage();
        return 1;
    }

    if (!quiet) PRINT_ID("TTExtract");

    if (endtoken) endtokenlength = strlen(endtoken);

    if (verbose) fprintf(stdout,"opening input file %s ...\n",infile);
    if (!(infp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    startpos = FindToken(infp,starttoken,hexstart);

    if (startpos == -1) {  // error
        fclose(infp);
        return 1;
    }
    if (startpos == -2) { // not found
        fprintf(stderr,"ERROR: position of start token not found\n");
        return 1;
    }

    if (verbose) fprintf(stdout,"starttoken (%s) found at position=%ld\n",starttoken,startpos);

    if (endtoken) {
        endpos = FindToken(infp,endtoken,hexend);

        if (endpos == -1) {
            fclose(infp);
            return 1;
        }
        if (endpos == -2) {
            fprintf(stderr,"ERROR: position of end token not found\n");
            return 1;
        }

        if (verbose) fprintf(stdout,"endtoken (%s) found at position=%ld\n",endtoken,endpos);

        if (hexend) endpos -= endtokenlength/2;
        else        endpos -= endtokenlength;

        if (endpos <= startpos) {
            fprintf(stderr,"ERROR: endpos <= startpos\n");
            return 1;
        }
    }
    else {
        fseek(infp,0,SEEK_END);
        endpos = ftell(infp);
        if (verbose) fprintf(stdout,"using end-of-file as endposition (%ld)\n",endpos);
    }


    if (verbose) fprintf(stdout,"opening output file %s ...\n",outfile);
    if (!(outfp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outfile %s\n",argv[2]);
        fclose(infp);
        return 1;
    }


    fseek(infp,startpos,SEEK_SET);
    count = endpos - startpos;
    if (verbose) fprintf(stdout,"extracting %ld bytes ...\n",count);

    for (n=0;n<count;n++) fputc(fgetc(infp),outfp);

    fclose(infp);
    fclose(outfp);

    if (!quiet) fprintf(stdout,"%ld bytes written to %s\n",count,outfile);

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
// Revision 1.7  2005/08/22 Lionel Debroux.
// Added a note about -fno-unit-at-a-time with GCC 4.0+ in the usage.
//
// Revision 1.6  2002/03/15 13:10:03  tnussb
// usage text extended
//
// Revision 1.5  2002/03/15 13:00:51  tnussb
// (1) closing of files added to end of program
// (2) treatment of -v (verbose) added
// (3) treatment of -quiet (suppress standard messages) added
// (4) bug fixed (if no endtoken was given, a wrong endpos was evaluated)
//
// Revision 1.4  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.3  2000/11/28 00:04:21  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.2  2000/08/23 19:55:02  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/13 20:20:55  Thomas Nussbaumer
// initial version
//
//
