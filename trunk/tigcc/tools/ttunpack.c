/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttunpack.c
* initial date:    18/08/2000
* authors:         albert@cs.tut.fi
*                  thomas.nussbaumer@gmx.net
* description:     unpacking program
*
* -----------------------------------------------------------------------------
*
* based on code from Pasi 'Albert' Ojala, albert@cs.tut.fi
*
* heavily reduced to fit to the needs by thomas.nussbaumer@gmx.net
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttunpack may also be
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

#include "unpack.c"

#include "tt.h"          // generic defines
#include "ttversion.h"   // TI-68k Developer Utilities version info
#include "revtools.h"
#include "ttunpack.h"    // errorcodes definition
#include "packhead.h"    // compressed header definition

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.8"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttunpack [-<flags>] <infile> <outfile>\n"   \
                       "       -hti      treat input as hex-text input\n"  \
                       "       -hto      generate hex-text output\n"       \
                       "       -t        just test (generates no outfile)\n");
}


#define NO_HEX_CHARACTER 255


//=============================================================================
// converts hex character to int
//=============================================================================
unsigned char hex2int(unsigned char c) {
    c = tolower(c);

    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= '0' && c <= '9') return c - '0';
    return NO_HEX_CHARACTER;
}


//=============================================================================
// converts hextext to binary
//=============================================================================
int ConvertText2Bin(unsigned char* ib,int origlen) {
    int           pos;
    int           cnt = 0;
    int           searchforendofline = 0;
    int           len_after_convert  = 0;
    unsigned char val = 0; // 1.40: added initialization.
    unsigned char actual;

    len_after_convert = 0;
    for (pos = 0; pos < origlen;pos++) {
        if (searchforendofline) {
            if (ib[pos] == '\n') searchforendofline = 0;
            continue;
        }
        if (ib[pos] == '/') {
            cnt = 0;
            if (pos < origlen-1 && ib[pos+1] == '/') searchforendofline = 1;
            continue;
        }

        actual = hex2int(ib[pos]);
        if (actual == NO_HEX_CHARACTER) {
            cnt = 0;
            continue;
        }
        if (cnt == 0) {
            val = actual*16;
            cnt++;
        }
        else {
            val += actual;
            cnt=0;
            ib[len_after_convert++] = val;
        }
    }
    return(len_after_convert);
}


//=============================================================================
// our main reason why we are here ...
//=============================================================================
int main(int argc, char* argv[]) {
    unsigned char* src;
    unsigned char* dest;
    FILE*          fp;
    int            result;
    PackedHeader*  ch;
    int            textinput  = 0;
    int            textoutput = 0;
    int            justtest   = 0;
    int            i;
    int            origsize;
    int            bufsize;
    int            newlen;
    int            compsize;
    char*          infilename  = NULL;
    char*          outfilename = NULL;

    PRINT_ID("TTUnpack");

    for (i=1;i<argc;i++) {
        if (!strcmp(argv[i], "-hti"))      textinput   = 1;
        else if (!strcmp(argv[i], "-hto")) textoutput  = 1;
        else if (!strcmp(argv[i], "-t"))   justtest    = 1;
        else if (!infilename)              infilename  = argv[i];
        else if (!outfilename)             outfilename = argv[i];
        else {
            PrintUsage();
            return 1;
        }
    }

    //-------------------------------------------
    // check if infile and out file are specified
    //-------------------------------------------
    if (!infilename || (!justtest && !outfilename)) {
        PrintUsage();
        return 1;
    }

    //----------------------------------------------------------------
    // NOTE: The code is prepared to handle piping, but piping doesn't
    //       work correct in all cases on a Win98 system, so I have
    //       disabled it
    //----------------------------------------------------------------
    if (infilename) {
        if (!(fp = fopen(infilename, "rb"))) {
            fprintf(stderr, "ERROR: cannot open input file %s\n", infilename);
            return 1;
        }
    }
    else {
        fprintf(stderr, "assuming stdin as text input.\nCtrl-C to abort, Ctrl-Z for EOF.\n");
        fp = stdin;
    }

    /* Read in the data */
    compsize = 0;
    bufsize  = 0;
    src      = NULL;
    while (1) {
        if (bufsize < compsize + 1024) {
            unsigned char *tmp = realloc(src, bufsize + 1024);
            if (!tmp) {
                fprintf(stderr, "ERROR: cannot allocate buffer\n");
                free(src);
                if (infilename) fclose(fp);
                return 1;
            }
            src = tmp;
            bufsize += 1024;
        }
        newlen = fread(src + compsize, 1, 1024, fp);
        if (newlen <= 0) break;
        compsize += newlen;
    }

    if (infilename) fclose(fp);

    //-----------------------------------------------------------
    // convert the input buffer from hex text to binary
    // if the user asks for it
    //-----------------------------------------------------------
    if (textinput) compsize = ConvertText2Bin(src,compsize);

    if (compsize > 65535) {
        fprintf(stderr, "ERROR: inputdata is too long (>65535 bytes)\n");
        free(src);
        return 1;
    }

    ch = (PackedHeader*)src;

    if ((compsize != (ch->compsize_lo | (ch->compsize_hi << 8))) ||
        ch->magic1 != MAGIC_CHAR1 ||
        ch->magic2 != MAGIC_CHAR2)
    {
        fprintf(stderr, "ERROR: format mismatch in input (csize=%d|m1=%c|m2=%c)\n",
                compsize,ch->magic1,ch->magic2);
        free(src);
        return 1;
    }

    origsize = ch->origsize_lo | (ch->origsize_hi << 8);

    if (!(dest = (unsigned char*)malloc(origsize))) {
        fputs("ERROR: cannot allocate output buffer\n",stderr);
        free(src);
        return 1;
    }

    result=UnPack(src,dest);
    if (result) {
        fprintf(stderr,"ERROR: cannot decompress (code %03d)\n",result);
        free(src);
        free(dest);
        return 1;
    }

    if (!justtest) {
        if ((outfilename && (fp = fopen(outfilename, "wb"))) || (fp = stdout)) {
            if (textoutput) {
                int loopy;
                for (loopy=0;loopy<origsize;loopy++) {
                    if (loopy < origsize - 1) fprintf(fp,"0x%02x,",dest[loopy]);
                    else                      fprintf(fp,"0x%02x",dest[loopy]);
                    if (loopy && (!(loopy % DEFAULT_ITEMS_PER_LINE))) fputc('\n',fp);
                }
            }
            else {
                fwrite(dest, origsize, 1, fp);
                if (outfilename) fclose(fp);
                fprintf(stderr,"Decompressed %u bytes.\n\n",origsize);
            }
            result = 0;
            if (fp != stdout) fclose(fp);
        }
        else {
            fprintf(stderr,"ERROR: cannot open outputfile %s\n\n",outfilename);
            result = 1;
        }
    }

    free(src);
    free(dest);
    return result;
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
// Revision 1.7  2002/02/13 17:03:18  tnussb
// -t option fixed
//
// Revision 1.6  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.5  2001/02/05 20:46:02  Thomas Nussbaumer
// using now local unpack.c file instead shared one
//
// Revision 1.4  2000/11/28 00:05:12  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.3  2000/08/23 20:25:43  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.2  2000/08/20 15:31:58  Thomas Nussbaumer
// (1) using now shared unpack.c file
// (2) flag -t added (just test, no output generation)
//
// Revision 1.1  2000/08/18 20:19:51  Thomas Nussbaumer
// initial version
//
//
