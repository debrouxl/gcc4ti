/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttchecksum.c
* initial date:    21/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     prints infos about 89s/89y/9xs/9xy files
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttinfo may also be
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
#define FILE_REVISION "1.6"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttinfo <infile>\n\n"  \
                       "       prints infos about a 89z/89s/89y/9xz/9xs/9xy file\n");
}


//=============================================================================
// ... it gets boring ...
//=============================================================================
int main(int argc,char *argv[]) {
    FILE*         fp;
    long          len;
    unsigned char ftype;
    unsigned char chk1;
    unsigned char chk2;
    unsigned int  checksum;
    char tmp[100];
    unsigned long calcvar;
    StrHeader     sh;


    PRINT_ID("TTInfo");

    if (argc !=2) {
        PrintUsage();
        return 1;
    }

    if (!(fp = fopen(argv[1],"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",argv[1]);
        return 1;
    }

    if (fread(&sh,sizeof(StrHeader),1,fp) != 1) {
        fprintf(stderr,"ERROR: file is too short\n");
        fclose(fp);
        return 1;
    }

    if (strncmp(sh.signature,"**TI",4)) {
        fprintf(stderr,"ERROR: file doesn't start with '**TI'\n");
        fclose(fp);
        return 1;
    }

    if (fseek(fp,-3,SEEK_END)) {
        fprintf(stderr,"ERROR: cannot seek to filetype\n");
        fclose(fp);
        return 1;
    }

    ftype = fgetc(fp);
    chk1  = fgetc(fp);
    chk2  = fgetc(fp);
    len   = ftell(fp);

    checksum = (chk2 << 8) + chk1;

    if (ftype != 0x2d && ftype != 0xf8 && ftype != 0xf3) {
        fprintf(stderr,"ERROR: file neither of STR nor of OTH type\n");
        fclose(fp);
        return 1;
    }

    printf("filename:  %s\n",argv[1]);
    printf("filesize:  %ld bytes\n",len);
    printf("filetype:  %s (0x%02x)\n",(ftype == 0x2D) ? "STR" : (ftype == 0xf8) ? "OTH" : "ASM",ftype);

    if (ftype == 0xf8) {
        int c1,c2,c3;
        fseek(fp,-7,SEEK_END);
        c1 = fgetc(fp);
        c2 = fgetc(fp);
        c3 = fgetc(fp);
        printf("extension: %c%c%c\n",c1,c2,c3);
    }

    strncpy(tmp,sh.signature,8);
    tmp[8] = 0;
    printf("signature: %s\n",tmp);
    strncpy(tmp,sh.folder,8);
    printf("folder:    %s\n",tmp);
    strncpy(tmp,sh.name,8);
    printf("varname:   %s\n",tmp);
    strncpy(tmp,sh.desc,40);
    tmp[40] = 0;
    printf("desc:      %s\n",tmp);

	calcvar  = sh.size[0];
	calcvar += (((unsigned long)sh.size[1]) << 8);
	calcvar += (((unsigned long)sh.size[2]) << 16);
	calcvar += (((unsigned long)sh.size[3]) << 24);

    printf("size:      %lu (0x%02x%02x%02x%02x)\n",calcvar,sh.size[0],sh.size[1],sh.size[2],sh.size[3]);
	calcvar = sh.datasize[0] << 8;
	calcvar+= sh.datasize[1];
    printf("datasize:  %lu (0x%02x%02x)\n",calcvar,sh.datasize[0],sh.datasize[1]);
    printf("checksum:  %u (0x%02x%02x)\n\n",checksum,chk1,chk2);

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
// Revision 1.6  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.5  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.4  2001/01/13 20:43:17  Thomas Nussbaumer
// can operate now on 89z and 9xz files, too
//
// Revision 1.3  2000/11/28 00:06:33  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.2  2000/08/23 20:02:22  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/21 17:12:27  Thomas Nussbaumer
// initial version
//
//
//
//
