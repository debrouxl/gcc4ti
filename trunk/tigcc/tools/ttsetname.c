/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttsetname.c
* initial date:    18/10/2002
* author:          thomas.nussbaumer@@gmx.net
* description:     sets oncalc name and folder
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttsetname may also be
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
#define FILE_REVISION "1.2"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTSetName");
    fprintf(USAGE_OUT, "Usage: ttsetname [flags] file\n\n" \
                       "       -quit          ...  don't output standard messages\n"                    \
                       "       -name   string ...  set oncalc name to given string (max. 8 chars)\n"    \
                       "       -folder string ...  set oncalc folder to given string (max. 8 chars)\n\n"\
                       "       sets oncalc name and folder of given TI89/TI92p file\n");
}


//=============================================================================
// ... just a stupid main ...
//=============================================================================
int main(int argc,char *argv[]) {
    char*          file   = NULL;
    char*          folder = NULL;
    char*          name   = NULL;
    FILE*          fp;
    int            n;
    int            quiet = 0;
    StrHeader      buffer;

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-quiet")) quiet = 1;

        else if (!strcmp(argv[n], "-folder")) {
            if (n == argc -1) {
                fprintf(stderr,"ERROR: missing parameter for option '-folder'\n");
                return 1;
            }
            if (strlen(argv[n+1])>8) {
                fprintf(stderr,"ERROR: folder too long (max. 8 characters)\n");
                return 1;
            }
            folder = argv[n+1];
            n++;
        }
        else if (!strcmp(argv[n], "-name")) {
            if (n == argc -1) {
                fprintf(stderr,"ERROR: missing parameter for option '-name'\n");
                return 1;
            }

            if (strlen(argv[n+1])>8) {
                fprintf(stderr,"ERROR: name too long (max. 8 characters)\n");
                return 1;
            }
            name = argv[n+1];
            n++;
        }
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }
        else if (!file) file = argv[n];
        else {
            PrintUsage();
            return 1;
        }
    }

    // check if all necessary arguments are supplied
    if (!file || (!name && !folder)) {
        PrintUsage();
        return 1;
    }

    if (!quiet) PRINT_ID("TTBin2OTH");

    if (!(fp = fopen(file,"rb+"))) {
        fprintf(stderr,"ERROR: cannot open file %s\n",file);
        return 1;
    }

    if (fread(&buffer,sizeof(StrHeader),1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot read signature\n");
        fclose(fp);
        return 1;
    }

    if (strncmp(buffer.signature,SIGNATURE_TI89,8) && strncmp(buffer.signature,SIGNATURE_TI92P,8)) {
        fprintf(stderr,"ERROR: neither TI89 nor TI92p file\n");
        fclose(fp);
        return 1;
    }

    if (name) {
        memset(buffer.name,0,8);
        strncpy(buffer.name,name,8);
    }

    if (folder) {
        memset(buffer.folder,0,8);
        strncpy(buffer.folder,folder,8);
    }

    rewind(fp);
    if (fwrite(&buffer,sizeof(StrHeader),1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot write to %s\n",file);
        fclose(fp);
        return 1;
    }
    fclose(fp);

    if (!quiet) {
        if (name) {
            fprintf(stderr,"file %s: new oncalc name = %s\n",file,name);
        }
        if (folder) {
            fprintf(stderr,"file %s: new oncalc folder = %s\n",file,folder);
        }
    }

    return 0;
}

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE########################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.2  2009/01/25           Lionel Debroux
// Initial import in the TI-68k Developer Utilities (this tool used to be in TI-Chess)
// Adapt to new version display (revtools.h).
//
