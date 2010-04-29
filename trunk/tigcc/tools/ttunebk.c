/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttunebk.c
* initial date:    11/04/2001
* authors:         albert@cs.tut.fi
*                  thomas.nussbaumer@gmx.net
* description:     unpacks ebooks
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

  As a special exception, UNMODIFIED copies of ttunebk may also be
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
#include "ttarchive.h"   // ttarchive definitions

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.3"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttunebk <infile> <outfile>\n\n"\
                       "       exports the complete text of an ebook to a plain textfile\n\n");
}



//=============================================================================
// our main reason why we are here ...
//=============================================================================
int main(int argc, char* argv[]) {
    unsigned char* src;
    FILE*          fp;
    char*          infilename  = NULL;
    char*          outfilename = NULL;
    unsigned long           insize;
    int            nr_entries;
    int            i;

    PRINT_ID("TTUnEbk");

    for (i=1;i<argc;i++) {
        if (!infilename)       infilename  = argv[i];
        else if (!outfilename) outfilename = argv[i];
        else {
            PrintUsage();
            return 1;
        }
    }

    //-------------------------------------------
    // check if infile and out file are specified
    //-------------------------------------------
    if (!infilename || !outfilename) {
        PrintUsage();
        return 1;
    }

    if (!(fp = fopen(infilename, "rb"))) {
        fprintf(stderr, "ERROR: cannot open input file %s\n", infilename);
        return 1;
    }

    fseek(fp,0,SEEK_END);
    insize = ftell(fp);
    rewind(fp);

    // treatment of PC header ...
    insize -= 88;
    fseek(fp,86,SEEK_SET);


    if (!(src = (unsigned char*)malloc(insize))) {
        fprintf(stderr, "ERROR: cannot allocate buffer\n");
        fclose(fp);
        return 1;
    }

    if (fread(src, 1, insize, fp) != insize) {
        fprintf(stderr, "ERROR: cannot read %ld bytes from %s\n",insize,infilename);
        fclose(fp);
        free(src);
        return 1;
    }

    fclose(fp);

    if (!IsTTArchive(src+2)) {
        fprintf(stderr, "ERROR: %s is not a valid eBook\n",infilename);
        free(src);
        return 1;
    }

    nr_entries = GetNrEntries(src+2);

    //printf("%s contains %d entries ...\n",infilename,nr_entries);

    if (!(fp = fopen(outfilename, "wb"))) {
        fprintf(stderr, "ERROR: cannot open output file %s\n", outfilename);
        return 1;
    }

    for (i=1;i<nr_entries;i++) {
        TTAEntry*      entry = GetEntryInfo(src+2,i);
        unsigned char* data  = GetEntryStart(src+2,entry);
        PackedHeader*  ch    = (PackedHeader*)data;
        unsigned int origsize;
        unsigned char* dest;
        int            result;

        printf("processing entry %d ...\n",i);

        if (ch->magic1 != MAGIC_CHAR1 || ch->magic2 != MAGIC_CHAR2) {
            fprintf(stderr, "ERROR: format mismatch of part %d (skipping)\n",i);
            continue;
        }

        origsize = ch->origsize_lo | (ch->origsize_hi << 8);
        if (!(dest = (unsigned char*)malloc(origsize))) {
             fprintf(stderr,"ERROR: cannot allocate %d bytes for output buffer\n",origsize);
             free(src);
             fclose(fp);
             return 1;
        }

        result=UnPack(data,dest);
        if (result) {
            fprintf(stderr,"ERROR: cannot decompress (code %03d)\n",result);
            free(src);
            free(dest);
            fclose(fp);
            return 1;
        }

        if (fwrite(dest,1,origsize,fp) != origsize) {
            fprintf(stderr,"ERROR: cannot decompress (code %03d)\n",result);
            free(src);
            free(dest);
            fclose(fp);
            return 1;
        }
        printf("%d bytes written to %s\n",origsize,outfilename);
        free(dest);
    }

    free(src);
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
// Revision 1.3  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.2  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.1  2001/04/11 22:59:40  Thomas Nussbaumer
// initial version
//
//
