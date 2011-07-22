/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttbin2txt.c
* initial date:    09/07/2011
* author:          lionel_debroux@yahoo.fr
* description:     enwraps inputfile into TI89 and TI92 text files
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttbin2txt may also be
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
#include <stddef.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"
#include "strhead.h"


#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.1"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttbin2txt <-s89 | -s92> <infile> <name> [folder]\n\n"  \
                       "       -s89  ....  generate TI89 text\n"                \
                       "       -s92  ....  generate TI92 text\n"                \
                       "       -f    ....  don't print a warning if the text contains at least one 0x00\n");
}


//=============================================================================
// this routine is very similar to the Data2Str routine, but it will create a
// text variable
//=============================================================================
int Data2Text(FILE*          fp,
              int            calctype,
              char*          folder,
              char*          name,
              unsigned int   length,
              unsigned char* data,
              int            force)
{

    StrHeader      header;
    unsigned int   i;
    unsigned int   checksum;
    unsigned char* ptr;
    int            size   = sizeof(StrHeader)+length+7;
    unsigned char  c;
    static int     contains0x00 = 0;

    memset(&header,0,sizeof(StrHeader));

    if (calctype == CALC_TI89) strncpy(header.signature,SIGNATURE_TI89,8);
    else                       strncpy(header.signature,SIGNATURE_TI92P,8);

    header.fill1[0] = 1;
    header.fill1[1] = 0;

    if (!folder) strcpy(header.folder,DEFAULT_FOLDER);
    else         strncpy(header.folder,folder,8);

    header.fill2[0] = 0x01;
    header.fill2[1] = 0x00;
    header.fill2[2] = 0x52;
    header.fill2[3] = 0x00;
    header.fill2[4] = 0x00;
    header.fill2[5] = 0x00;
    strncpy(header.name,name,8);
    header.type[0] = 0x0c;  // 0c
    header.type[1] = 0x00;
    header.type[2] = 0x03;
    header.type[3] = 0x00;

    // Don't set header.size for now, it will be updated later.

    header.fill3[0] = 0xA5;
    header.fill3[1] = 0x5A;
    header.fill3[2] = 0x00;
    header.fill3[3] = 0x00;
    header.fill3[4] = 0x00;
    header.fill3[5] = 0x00;

    size -= sizeof(StrHeader);
    size -= 2;

    checksum = 0;

    ptr = (unsigned char*)&header;

    for (i=0;i<sizeof(StrHeader);i++) {
        fputc(*ptr,fp);
        ptr++;
    }

    // offset in text
    fputc(0,fp);
    fputc(2,fp);

    checksum+=2;

    // header of first line
    fputc(' ',fp);

    checksum+=' ';

    for (i=0;i<length;i++) {
        c = *data++;
        if ((c == 0x00) && (!force) && (!contains0x00)) {
            fprintf(stderr,"WARNING: data contains 0x00: TI-Connect may truncate the text\n");
            contains0x00 = 1;
        }

        if ((c == '\r') || (c == '\n')) { // newline
            fputc('\r',fp);
            checksum+='\r';
            size++;
            c=' '; // header of the next line, here an additional space (no commands)
        }
        fputc(c,fp);
        checksum+=c;
    }

    fputc(0,fp);
    fputc(0xE0,fp);

    checksum+=0xE0;

    // Update the checksum with the file's size.
    checksum += (size >> 8) & 0xff;
    checksum += size & 0xff;
    // Write the checksum
    fputc(checksum%256,fp);
    fputc(checksum/256,fp);

    // Write the file size embedded in the file.
    fseek(fp,offsetof(StrHeader,datasize),SEEK_SET);
    fputc((size >> 8) & 0xff,fp);
    fputc(size & 0xff,fp);

    size += sizeof(StrHeader);
    size += 2;

    // Write the file size embedded in the header.
    fseek(fp,offsetof(StrHeader,size),SEEK_SET);
    fputc(size & 0xff,fp);
    fputc((size >> 8) & 0xff,fp);
    fputc((size >> 16) & 0xff,fp);
    fputc((size >> 24) & 0xff,fp);

    return size + sizeof(StrHeader) + 2;
}


//=============================================================================
// an old friend ...
//=============================================================================
int main(int argc,char *argv[]) {
    char*          infile    = NULL;
    char*          folder    = NULL;
    char*          name      = NULL;
    int            calctype  = -1;
    FILE*          ifp;
    FILE*          ofp;
    char           outfile[1024];
    unsigned char* buffer;
    int            length;
    int            n;
    int            force = 0;


    PRINT_ID("TTBin2Txt");

    // check for too less arguments
    if (argc < 4) {
        PrintUsage();
        return 1;
    }

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-s89"))         calctype  = CALC_TI89;
        else if (!strcmp(argv[n], "-s92"))    calctype  = CALC_TI92P;
        else if (!strcmp(argv[n], "-f"))      force     = 1;
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }
        else if (!infile)                     infile    = argv[n];
        else if (!name)                       name      = argv[n];
        else if (!folder)                     folder    = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    // check if all necessary arguments are supplied
    if (!infile || !name || calctype == -1) {
        PrintUsage();
        return 1;
    }

    if (!(ifp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    if (calctype == CALC_TI89) sprintf(outfile,"%s.89t",name);
    else                       sprintf(outfile,"%s.9xt",name);

    if (!(ofp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
        fclose(ifp);
        return 1;
    }

    // read infile into buffer
    fseek(ifp,0,SEEK_END);
    length = ftell(ifp);
    buffer = (unsigned char*)malloc(length);
    if (!buffer) {
        fprintf(stderr,"ERROR: cannot allocate memory (%d bytes)\n",length);
        fclose(ifp);
        fclose(ofp);
        return 1;
    }
    rewind(ifp);
    fread(buffer,1,length,ifp);
    fclose(ifp);

    length = Data2Text(ofp,calctype,folder,name,(unsigned int)length,buffer,force);

    free(buffer);
    fclose(ofp);

    fprintf(stderr,"%d bytes written to %s\n",length,outfile);

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
// Revision 1.1  2011/07/09           Lionel Debroux
// initial version
//
//
//
