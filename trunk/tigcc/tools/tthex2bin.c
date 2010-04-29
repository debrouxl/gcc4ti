/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       tthex2bin.c
* initial date:    18/03/2002
* author:          thomas.nussbaumer@gmx.net
* description:     converts textfile with hex data into binary file
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of tthex2bin may also be
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
#include <ctype.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.3"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTHex2Bin");
    fprintf(USAGE_OUT, "Usage: tthex2bin [flags] <infile> <outfile>\n\n"                         \
                       "       -quiet      ....  don't output standard messages\n"               \
                       "       -binary     ....  infile contains binary numbers\n"               \
                       "       infile      ....  name of input file (use '-' for stdin)\n"       \
                       "       outfile     ....  name of output file\n\n"                        \
                       "       Converts text file with hex data to binary file. The numbers\n"   \
                       "       in the inputfile must be prefixed with \"0x\" and must have\n"    \
                       "       ALL the same length (2,4 or 8 hexdigits). Valid separators are\n" \
                       "       spaces, commas, linebreaks and TABs.\n"                           \
                       "       Empty lines are valid. Lines starting with \"//\" or \"#\" are\n" \
                       "       ignored (comment lines). Example of an inputline:\n"              \
                       "           0x1234,0x5678,0x9ABC,0xdfab\n"                                \
                       "       Binary mode: binary numbers have to be prefixed with \"0b\".\n"   \
                       "       Only binary numbers with 8, 16 or 32 digits are valid.\n"         \
                       "       You cannot mix binary and hexnumbers in one inputfile. Example:\n"\
                       "           0b11110000 0b10101010 0b11111111\n\n");
}

#define IS_COMMA_OR_WHITESPACE_NOTNULL(i) (((i) == '\t' || (i) == ' ' || (i) == ',' || (i) == '\n' || (i) == '\r') && (i))
//=============================================================================
// skips white spaces and commas of buffer ... return pointer to new position
//=============================================================================
unsigned char* SkipWhiteSpaces(unsigned char* buffer) {
    while (IS_COMMA_OR_WHITESPACE_NOTNULL(*buffer)) buffer++;
    return buffer;
}

//=============================================================================
// read a hex number of form 0xHHHHHHH .... where H are hexdigits
//=============================================================================
unsigned char* ReadHexNumber(unsigned char* buffer,unsigned long* data,int *len) {
   unsigned char b[65536];

   *len = 0;
   if (!*buffer) return NULL;
   if (*buffer != '0' || *(buffer+1) != 'x') {
       *len = -1;
       fprintf(stderr,"ERROR: missing prefix \"0b\" of binary number [%s]\n",buffer);
       return NULL;
   }

   buffer+=2;

   while (isxdigit(*buffer)) {
       b[*len] = *buffer++;
       (*len)++;
   }
   b[*len] = 0;

   if ((*len) %2) {
       fprintf(stderr,"ERROR: odd numbers (%d) of digits not allowed in hexnumber\n",*len);
       *len = -1;
       return NULL;
   }

   *data = strtoul((char *)b,NULL,16);
   *len /= 2;
   return buffer;
}

#define isbdigit(c) (c=='0' || c=='1')

//=============================================================================
// read a binary number of form 0xb1101010 s
//=============================================================================
unsigned char* ReadBinNumber(unsigned char* buffer,unsigned long* data,int *len) {
   unsigned char b[65536];

   *len = 0;
   if (!*buffer) return NULL;
   if (*buffer != '0' || *(buffer+1) != 'b') {
       *len = -1;
       fprintf(stderr,"ERROR: missing prefix \"0b\" of binary number\n");
       return NULL;
   }

   buffer+=2;

   while (isbdigit(*buffer)) {
       b[*len] = *buffer++;
       (*len)++;
   }
   b[*len] = 0;

   if ((*len) % 8) {
       fprintf(stderr,"ERROR: length of binary number (%d) not a multiple of 8\n",*len);
       *len = -1;
       return NULL;
   }

   *len /= 8;
   *data = strtoul((char *)b,NULL,2);
   return buffer;
}


//=============================================================================
// convert hexcode input into binary output
//=============================================================================
long ConvertHex2Bin(FILE* ifp, FILE* ofp,int binary) {
    unsigned char  buffer[65536];
    long           bytes_processed = 0;
    int            nr_bytes = 0;
    unsigned long  data;
    int            nr_tmp;
    unsigned char* ptr;

    while (fgets((char *)buffer,65535,ifp)) {
        ptr = SkipWhiteSpaces(buffer);
        if (!*ptr) continue;
        if ((*ptr == '/' && *(ptr+1) == '/') || *ptr == '#') continue;
        do {
            ptr = SkipWhiteSpaces(ptr);
            if (!*ptr) break;

            //printf("converting [%s]",ptr);
            ptr = (binary) ? ReadBinNumber(ptr,&data,&nr_tmp) : ReadHexNumber(ptr,&data, &nr_tmp);
            //printf("nr=0x%08X prev_bytes=%d act_bytes=%d\n",data,nr_bytes,nr_tmp);

            if (nr_tmp == -1) return -1;
            if (nr_tmp == 0) break;

            if (!nr_bytes && !nr_tmp) continue;

            if (!nr_bytes) {
                nr_bytes = nr_tmp;
            }
            else if (nr_bytes != nr_tmp) {
                fprintf(stderr,"ERROR: missmatch in length of hexnumbers\n");
                return -1;
            }

            if (nr_bytes == 1) {
                fputc(data & 0xff,ofp);
            }
            else if (nr_bytes == 2) {
                fputc((data >> 8) & 0xff,ofp);
                fputc(data & 0xff,ofp);
            }
            else if (nr_bytes == 4) {
                fputc((data >> 24) & 0xff,ofp);
                fputc((data >> 16) & 0xff,ofp);
                fputc((data >> 8) & 0xff,ofp);
                fputc(data & 0xff,ofp);
            }
            else {
                if (binary) {
                    fprintf(stderr,"ERROR: only hexnumbers with 2, 4, or 8 digits are valid\n");
                }
                else {
                    fprintf(stderr,"ERROR: only binarynumbers with 8, 16, or 32 digits are valid\n");
                }
                return -1;
            }
            bytes_processed+=nr_bytes;
        }
        while (1);
    }

    return bytes_processed;
}


//=============================================================================
// ... another "just a main routine" tool ...
//=============================================================================
int main(int argc,char *argv[]) {
    char* infile  = NULL;
    char* outfile = NULL;
    FILE* ifp;
    FILE* ofp;
    int   quiet       = 0;
    int   binary_mode = 0;
    long  bytes_processed;
    int   n;

    // check for too less arguments
    if (argc < 3) {
        PrintUsage();
        return 1;
    }


    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-quiet"))       quiet       = 1;
        else if (!strcmp(argv[n], "-binary")) binary_mode = 1;
        else if (!infile)                     infile      = argv[n];
        else if (!outfile)                    outfile     = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    if (!infile || !outfile) {
        PrintUsage();
        return 1;
    }

    if (!strcmp(infile,"-")) {
        if (!quiet) fprintf(stdout,"using stdin as input\n");
        ifp = stdin;
    }
    else {
        if (!(ifp = fopen(infile,"r"))) {
            fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
            return 1;
        }
    }

    if (!quiet) PRINT_ID("TTBin2Hex");

    if (!(ofp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
        if (ifp != stdin) fclose(ifp);
        return 1;
    }

    bytes_processed = ConvertHex2Bin(ifp,ofp,binary_mode);
    if (ifp != stdin) fclose(ifp);
    fclose(ofp);

    if (bytes_processed == -1) {
        remove(outfile);
        return 1;
    }

    if (!quiet) fprintf(stdout,"%ld bytes written to %s\n",bytes_processed,outfile);
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
// Revision 1.3  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.2  2002/03/19 09:41:22  tnussb
// new mode "convert binary numbers" added
//
// Revision 1.1  2002/03/18 12:54:23  tnussb
// initial version
//
//
