/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttbin2hex.c
* initial date:    13/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     converts binary file to C hex dump
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttbin2hex may also be
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
#include <stdlib.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.10"

//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTBin2Hex");
    fprintf(USAGE_OUT, "Usage: ttbin2hex [flags] <infile> <outfile>\n\n"              \
                       "       -quiet      ....  don't output standard messages\n"    \
                       "       -slash      ....  append backslash at end of line\n"   \
                       "       -c <count>  ....  number of items per line\n"          \
                       "       -b1         ....  generate unsigned char array\n"      \
                       "       -b2         ....  generate unsigned int  array\n"      \
                       "       -b4         ....  generate unsigned long array\n"      \
                       "       -a <name>   ....  generate an array with given name\n" \
                       "       -ss <nr>    ....  skip number of bytes at begin of inputfile\n"\
                       "       -se <nr>    ....  skip number of bytes at end of inputfile\n"\
                       "       -lb <nr>    ....  insert an additionally linebreak every nr items\n"\
                       "       infile      ....  name of infile\n"                    \
                       "       outfile     ....  name of outfile (use \"-\" for stdout)\n\n"  \
                       "       converts binary file to C array data\n\n");
}


//=============================================================================
// ... another "just a main routine" tool ...
//=============================================================================
int main(int argc,char *argv[]) {
    char*          infile    = NULL;
    char*          outfile   = NULL;
    FILE*          ifp;
    FILE*          ofp;
    int            bytes_per_item = 1;
    int            items_per_line = DEFAULT_ITEMS_PER_LINE;
    int            n;
    int            data[4];
    int            appendslash = 0;
    long           insize;
    long           bytes_processed;
    long           loop;
    long           itemcount;
    char*          arrayname  = NULL;
    int            quiet      = 0;
    int            skip_start = 0;
    int            skip_end   = 0;
    int            empty_line = 0;

    // check for too less arguments
    if (argc < 3) {
        PrintUsage();
        return 1;
    }


    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-slash"))      appendslash    = 1;
        else if (!strcmp(argv[n], "-quiet")) quiet          = 1;
        else if (!strcmp(argv[n], "-b1"))    bytes_per_item = 1;
        else if (!strcmp(argv[n], "-b2"))    bytes_per_item = 2;
        else if (!strcmp(argv[n], "-b4"))    bytes_per_item = 4;
        else if (!strcmp(argv[n], "-c")) {
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (sscanf(argv[n+1],"%d",&items_per_line) != 1) {
                    PrintUsage();
                    return 1;
                }
                if (items_per_line < 1 || items_per_line > 200) {
                    fprintf(stderr,"ERROR: invalid size for items per line (must be >=1 and <=200)\n");
                    return 1;
                }
                n++;
            }
        }

        else if (!strcmp(argv[n], "-ss")) {
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (sscanf(argv[n+1],"%d",&skip_start) != 1) {
                    PrintUsage();
                    return 1;
                }
                if (skip_start < 0) {
                    fprintf(stderr,"ERROR: invalid count for option -ss\n");
                    return 1;
                }
                n++;
            }
        }

        else if (!strcmp(argv[n], "-se")) {
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (sscanf(argv[n+1],"%d",&skip_end) != 1) {
                    PrintUsage();
                    return 1;
                }
                if (skip_end < 0) {
                    fprintf(stderr,"ERROR: invalid count for option -se\n");
                    return 1;
                }
                n++;
            }
        }

        else if (!strcmp(argv[n], "-a")) {
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                arrayname = argv[n+1];
                n++;
            }
        }

        else if (!strcmp(argv[n], "-lb")) {
            if (n == argc -1) {
                PrintUsage();
                return 1;
            }
            else {
                if (sscanf(argv[n+1],"%d",&empty_line) != 1) {
                    PrintUsage();
                    return 1;
                }
                n++;
            }
        }

        else if (!infile)  infile  = argv[n];
        else if (!outfile) outfile = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    if (!infile || !outfile) {
        PrintUsage();
        return 1;
    }

    if (!(ifp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    if (!strcmp(outfile,"-")) {
       ofp = stdout;
    }
    else {
        if (!quiet) PRINT_ID("TTBin2Hex");
        if (!(ofp = fopen(outfile,"w"))) {
            fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
            fclose(ifp);
            return 1;
        }
    }

    fseek(ifp,0,SEEK_END);
    insize = ftell(ifp);
    rewind(ifp);

    bytes_processed = 0;
    itemcount       = 0;

    insize-=skip_start;
    insize-=skip_end;

    if (insize<0) {
        fprintf(stderr,"ERROR: no bytes left to process\n");
        fclose(ifp);
        fclose(ofp);
        return 1;
    }

    if (skip_start > 0) fseek(ifp,skip_start,SEEK_SET);

    if (arrayname) {
        int correct = (insize%bytes_per_item) ? 1 : 0;
        switch(bytes_per_item) {
            case 1:
               fprintf(ofp,"unsigned char %s[%ld] = {%c\n",arrayname,insize/bytes_per_item+correct,(appendslash)?'\\':' ');
               break;
            case 2:
               fprintf(ofp,"unsigned short %s[%ld] = {%c\n",arrayname,insize/bytes_per_item+correct,(appendslash)?'\\':' ');
               break;
            case 4:
               fprintf(ofp,"unsigned long %s[%ld] = {%c\n",arrayname,insize/bytes_per_item+correct,(appendslash)?'\\':' ');
               break;
        }
    }

    for (loop = 0;loop<insize/bytes_per_item;loop++) {
        switch(bytes_per_item) {
            case 1:
               data[0] = fgetc(ifp);
               fprintf(ofp,"0x%02x", data[0]);
               break;
            case 2:
               data[0] = fgetc(ifp);
               data[1] = fgetc(ifp);
               fprintf(ofp,"0x%02x%02x",data[0],data[1]);
               break;
            case 4:
               data[0] = fgetc(ifp);
               data[1] = fgetc(ifp);
               data[2] = fgetc(ifp);
               data[3] = fgetc(ifp);
               fprintf(ofp,"0x%02x%02x%02x%02x",data[0],data[1],data[2],data[3]);
               break;
        }
        bytes_processed+=bytes_per_item;
        itemcount++;

        if (bytes_processed < insize) fputc(',',ofp);
        if (itemcount == items_per_line) {
            if (appendslash) fputc('\\',ofp);
            fputc('\n',ofp);
            itemcount=0;
        }

        if (empty_line) {
            if (!((loop+1) % empty_line)) {
                if (appendslash) fputc('\\',ofp);
                fputc('\n',ofp);
                itemcount=0;
            }
        }


    }


    if (insize%bytes_per_item) {
        for (n=insize%bytes_per_item;n<bytes_per_item;n++) data[n]=0;

        switch(bytes_per_item) {
            case 1: fprintf(ofp,"0x%02x",            data[0]);                         break;
            case 2: fprintf(ofp,"0x%02x%02x",        data[0],data[1]);                 break;
            case 4: fprintf(ofp,"0x%02x%02x%02x%02x",data[0],data[1],data[2],data[3]); break;
        }

        if (appendslash) fputc('\\',ofp);
        fputc('\n',ofp);
    }

    if (arrayname) {
       fputc('}',ofp);
       fputc(';',ofp);
       fputc('\n',ofp);
    }

    fclose(ifp);
    if (ofp != stdout) {
        fclose(ofp);
        loop = insize/bytes_per_item;
        if (insize%bytes_per_item) loop++;
        if (!quiet) fprintf(stdout,"%ld items with %d byte(s) written to %s\n",loop,bytes_per_item,outfile);
    }

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
// Revision 1.10  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.9  2002/05/07 15:53:38  tnussb
// (1) option -a fixed. previously the array length was off by one when the
//     input length was not a multiple of the itemsize
// (2) option -lb added. Now additionally line breaks can be added after every
//     n items.
//
// Revision 1.8  2002/04/17 10:46:45  tnussb
// commandline options -ss and -se added
//
// Revision 1.7  2002/03/15 13:17:30  tnussb
// (1) treatment of -quiet (suppress standard messages) added
//
// Revision 1.6  2002/02/07 09:49:36  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.5  2001/06/20 13:08:01  Thomas Nussbaumer
// (1) handling of output redirection to stdout (outfilename == "-")
// (2) handling of "-a arrayname" to generate a complete C array of name arrayname
//
// Revision 1.4  2001/02/05 20:42:49  Thomas Nussbaumer
// (1) adding no comma after last item anymore
// (2) -slash parameter implemented (adding a backslash at end of line)
//
// Revision 1.3  2000/11/28 00:03:04  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.2  2000/08/23 19:35:47  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/13 20:20:55  Thomas Nussbaumer
// initial version
//
//
//
//
