/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttchecksum.c
* initial date:    21/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     corrects checksum of .89s/.89y/.9xs/9xy files
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttchecksum may also be
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
#include "strhead.h"


#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.7"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttchecksum <infile>\n\n"\
                       "       corrects the checksum of 89z/89s/89y/9xz/9xs/9xy calculator files.\n\n"\
                       "       NOTE: if the embedded filelength doesn't match the real filelength\n"\
                       "             (for example: caused by an invalid FTP transfer) the file\n"\
                       "             cannot be corrected.\n");
}


//=============================================================================
// an old friend ...
//=============================================================================
int main(int argc,char *argv[]) {
    FILE*         fp;
    unsigned long len;
    unsigned long embedded_len;
    unsigned char ftype;
    unsigned char chk1;
    unsigned char chk2;
    unsigned int  ochecksum;
    unsigned int  checksum;
    //unsigned char h[4];
    unsigned long loop;
    StrHeader     header;

    PRINT_ID("TTChecksum");

    if (argc !=2) {
        PrintUsage();
        return 1;
    }

    // open inputfile
    if (!(fp = fopen(argv[1],"rb+"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",argv[1]);
        return 1;
    }

    // read TI header
    if (fread(&header,sizeof(StrHeader),1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot read TI fileheader from %s\n",argv[1]);
        fclose(fp);
        return 1;
    }

    // check for valid TI signature
    if (strncmp(header.signature,"**TI",4)) {
        fprintf(stderr,"ERROR: file doesn't start with '**TI'\n");
        fclose(fp);
        return 1;
    }

    // goto end of file
    if (fseek(fp,-3,SEEK_END)) {
        fprintf(stderr,"ERROR: cannot seek to filetype\n");
        fclose(fp);
        return 1;
    }

    // read filetype, checksum bytes and get length of file
    ftype = fgetc(fp);
    chk1  = fgetc(fp);
    chk2  = fgetc(fp);
    len   = (unsigned long)ftell(fp);

    // calculate embedded file length
    embedded_len = ((((unsigned long)(header.size[3]))<<24) +
                   (((unsigned long)(header.size[2]))<<16) +
                   (((unsigned long)(header.size[1]))<<8) +
                   ((unsigned long)(header.size[0])));

    // if real length missmatches embedded length -> cannot correct this problem
    if (embedded_len != len) {
        fprintf(stderr,"FATAL: embedded len (%lu) doesn't match real length (%lu).\nCannot correct this file.\n",len,embedded_len);
        fclose(fp);
        return 1;
    }

    // calculate checksum from checksum bytes
    ochecksum = (chk2 << 8) + chk1;

    // print some details about the file
    fprintf(stderr,"length=%lu type=%02x checksum=%u (0x%02x%02x)\n",len,ftype,ochecksum,chk1,chk2);

    if (ftype != 0x2d && ftype != 0xf8 && ftype != 0xf3) {
        fprintf(stderr,"ERROR: file neither STR, ASM nor OTH type\n");
        fclose(fp);
        return 1;
    }

    // evaluate checksum by our own ...
    fseek(fp,sizeof(StrHeader)-2,SEEK_SET);
    checksum = 0;
    for (loop=sizeof(StrHeader)-2;loop<len-2;loop++) {
        checksum += fgetc(fp);
    }

    chk1     = checksum%256;
    chk2     = checksum/256;
    checksum = (chk2 << 8) + chk1;

    // compare original checksum and our checksum ...
    if (checksum == ochecksum) {
        fprintf(stderr,"checksum already ok. will not patch.\n");
    }
    else {
        fprintf(stderr,"checksum wrong. correcting it to %u (0x%02x%02x)\n",checksum,chk1,chk2);

        fseek(fp,-2,SEEK_END);
        fputc(chk1,fp);
        fputc(chk2,fp);
    }

    fclose(fp);
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
// Revision 1.6  2002/03/04 10:21:42  tnussb
// compares now embedded filelength with real filelength
//
// Revision 1.5  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.4  2001/01/06 10:01:09  Thomas Nussbaumer
// operates now on 89z and 9xz files, too
//
// Revision 1.3  2000/11/28 00:07:32  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.2  2000/08/23 19:49:17  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/21 17:12:27  Thomas Nussbaumer
// initial version
//
//
//
//
