/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttdos2ebk.c
* initial date:    20/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     prepares a text file for ebook
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttdos2ebk may also be
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

// if EMBEDDED_USE is defined, than we use this sourcefile from within another
// sourcefile

#ifndef EMBEDDED_USE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ttversion.h"
#include "revtools.h"
#include "ttebkmeta.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.9"


//=============================================================================
// print the usage
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttdos2ebk [-v] <infile> <outfile>\n\n"\
                       "       -v ... print verbose messages\n\n"\
                       "       converts a plain dos text to an ebook conform text\n"\
                       "       by discarding all 0x0D and by handling the following\n"\
                       "       inlined metatags:\n\n"\
                       "       <mt ff>     ... inserts a form feed\n"\
                       "       <mt hr>     ... inserts a horizontal ruler\n"\
                       "       <mt left>   ... text alignment left\n"\
                       "       <mt right>  ... text alignment right\n"\
                       "       <mt center> ... text alignment right\n\n"\
                       "       and the following preprocessing metatags:\n\n"\
                       "       <mt r=on>   ... replace single 0x0A by space ON\n"\
                       "       <mt r=off>  ... replace single 0x0A by space OFF\n"\
                       "       <mt c=on>   ... collapse spaces ON\n"\
                       "       <mt c=off>  ... collapse spaces OFF\n"\
                       "       <mt t=on>   ... metatag translation ON\n"\
                       "       <mt t=off>  ... metatag translation OFF\n");
}

#endif

#define DEFAULT_COLLAPSE   1
#define DEFAULT_REPLACE    1
#define DEFAULT_TRANSLATE  1
#define DEFAULT_DBLSPACE   0


typedef struct {
   char*         metatag;
   unsigned char sbcc;
} ConvEntry;

//#define CONV_ENTRIES  13
#define IDX_TRANS_OFF 10
#define IDX_TRANS_ON   9

//-----------------------------------------------------------------------------
// mapping table for meta tags
//-----------------------------------------------------------------------------
ConvEntry ctable[] = {
    {"<mt ff>",      SBCC_FF },
    {"<mt hr>",      SBCC_HR },
    {"<mt left>",    SBCC_LEFT },
    {"<mt right>",   SBCC_RIGHT },
    {"<mt center>",  SBCC_CENTER },
    {"<mt r=on>",    SBCC_REPLACE_ON },
    {"<mt r=off>",   SBCC_REPLACE_OFF },
    {"<mt c=on>",    SBCC_COLLAPSE_ON },
    {"<mt c=off>",   SBCC_COLLAPSE_OFF },
    {"<mt t=on>",    SBCC_TRANS_ON },
    {"<mt t=off>",   SBCC_TRANS_OFF },
    {"<mt ds=on>",   SBCC_DOUBLESPACE_ON },
    {"<mt ds=off>",  SBCC_DOUBLESPACE_OFF },
    {0,0}};


//=============================================================================
// reads 1 line from src into dest and appends a zero
//=============================================================================
long ReadLine(char* src, char* dest, char* end) {
     long icount = 0;
     while (*src != 0x0A && src < end && icount < 4094) *dest++ = *src++,icount++;
     *dest=0;
     return icount;
}

int state_translate = DEFAULT_TRANSLATE;


//=============================================================================
// handles metatag replacement for one line
//=============================================================================
void TranslateLine(char* src,char* dest) {
    int i=0;
    char* tmp;

    strcpy(dest,src);

    while (ctable[i].metatag) {
        if (state_translate || i==IDX_TRANS_ON || i==IDX_TRANS_OFF) {
            char* location = strstr(dest,ctable[i].metatag);

            if (location) {
                if (i == IDX_TRANS_OFF || i == IDX_TRANS_ON) {
                    if (i==IDX_TRANS_OFF)     state_translate = 0;
                    else if (i==IDX_TRANS_ON) state_translate = 1;
                    tmp = location + strlen(ctable[i].metatag);
                }
                else {
                    *location   = CCI;
                    *(location+1) = ctable[i].sbcc;
                    tmp = location + strlen(ctable[i].metatag);
                    location+=2;
                }
                while (*tmp) *location++ = *tmp++;
                *location = 0;
                i--;
            }
        }
        i++;
    }
}


//=============================================================================
// replaces all meta tags of within a buffer
//=============================================================================
long Translate(char* src,char* dest,long ilength) {
    char  b[4096];
    char  b2[4096];
    long           len,clen,ilen = 0;
    char* end = src + ilength;

    while (src < end) {
        len  = ReadLine(src,b,end);
        TranslateLine(b,b2);
        clen = strlen(b2);
        memcpy(dest,b2,clen);
        dest += clen;
        *dest++ = 0x0A;
        ilen += clen + 1;
        src += len + 1;
        //printf("%s\n",b2);
    }

    return ilen;
}


//=============================================================================
// copies a maximum of length bytes from src to dest, but skipping 0x0D bytes
//
// returns number of bytes copied
//=============================================================================
long RemoveAll0D(char* src, char* dest,long length) {
     long i;
     long cnt = 0;

     for (i=0;i<length;i++) {
         unsigned char c = *src++;
         if (c != 0x0D) {
             *dest++ = c;
              cnt++;
         }
     }
     return cnt;
}


//=============================================================================
// handles complete preprocessing including space collapsing and break
// replacement
//=============================================================================
long Preprocess(char* src,char* dest,long length,int verbose) {
    long i;
    long space_count = 0;
    long break_count = 0;

    long collapse_count = 0;
    long replace_count  = 0;

    int state_replace  = DEFAULT_REPLACE;
    int state_collapse = DEFAULT_COLLAPSE;
    int state_dblspace = DEFAULT_DBLSPACE;

    int removed_codes = 0;
    char* orig_dest = dest;

    for (i=0;i<length;i++) {
        unsigned char c = *src++;

        if (c == 0x0A) {
            break_count++;
            if (space_count) {
                if (state_collapse) {
                    *dest++ = ' ';
                    collapse_count += space_count - 1;
                }
                else {
                    while (space_count--) *dest++ = ' ';
                }
                space_count = 0;
            }
            continue;
        }

        if (c == ' ') {
            space_count++;
            if (state_dblspace) space_count++; // NEW
            if (break_count) {
                if (state_replace) {
                    if (break_count == 1) {
                       space_count++;
                       if (state_dblspace) space_count++; // NEW
                       replace_count++;
                    }
                    else {
                        while (break_count--) *dest++ = 0x0A;
                    }
                }
                else {
                    while (break_count--) *dest++ = 0x0A;
                }
                break_count = 0;
            }
            continue;
        }

        if (c == CCI) {
            c = *src++;
            i++;
            switch(c) {
                case SBCC_DOUBLESPACE_ON:
                    removed_codes++;
                    state_dblspace = 1;
                    break;
                case SBCC_DOUBLESPACE_OFF:
                    removed_codes++;
                    state_dblspace = 0;
                    break;
                case SBCC_COLLAPSE_ON:
                    removed_codes++;
                    if (space_count) {
                        if (state_collapse) {
                            *dest++ = ' ';
                            collapse_count += space_count - 1;
                        }
                        else {
                            while (space_count--) *dest++ = ' ';
                        }
                        space_count = 0;
                    }
                    state_collapse = 1;
                    break;
                case SBCC_COLLAPSE_OFF:
                    removed_codes++;
                    if (space_count) {
                        if (state_collapse) {
                            *dest++ = ' ';
                            collapse_count += space_count - 1;
                        }
                        else {
                            while (space_count--) *dest++ = ' ';
                        }
                        space_count = 0;
                    }
                    state_collapse = 0;
                    break;
                case SBCC_REPLACE_ON:
                    removed_codes++;
                    if (break_count) {
                        if (state_replace) {
                            if (break_count == 1) {
                                space_count++;
                                if (state_dblspace) space_count++; // NEW
                                replace_count++;
                            }
                            else {
                                while (break_count--) *dest++ = 0x0A;
                            }
                        }
                        else {
                            while (break_count--) *dest++ = 0x0A;
                        }
                        break_count = 0;
                    }
                    state_replace  = 1;
                    break;
                case SBCC_REPLACE_OFF:
                    removed_codes++;
                    if (break_count) {
                        if (state_replace) {
                            if (break_count == 1) {
                                space_count++;
                                if (state_dblspace) space_count++; // NEW
                                replace_count++;
                            }
                            else {
                                while (break_count--) *dest++ = 0x0A;
                            }
                        }
                        else {
                            while (break_count--) *dest++ = 0x0A;
                        }
                        break_count = 0;
                    }
                    state_replace  = 0;
                    break;
                default:
                    *dest++ = CCI;
                    *dest++ = c;
                    break;
            }
            continue;
        }

        //------------------------------------------------------
        // If we'll come here we have to flush spaces and breaks
        //------------------------------------------------------

        if (break_count) {
            if (state_replace) {
                if (break_count == 1) {
                   space_count++;
                   if (state_dblspace) space_count++; // NEW
                   replace_count++;
                }
                else {
                    while (break_count--) *dest++ = 0x0A;
                }
            }
            else {
                while (break_count--) *dest++ = 0x0A;
            }
            break_count = 0;
        }

        if (space_count) {
            if (state_collapse) {
                *dest++ = ' ';
                collapse_count += space_count - 1;
            }
            else {
                while (space_count--) *dest++ = ' ';
            }
            space_count = 0;
        }

        *dest++ = c;
    }


    //-----------------------------------------------
    // once more a little bit flushing
    //-----------------------------------------------
    if (break_count) {
        if (state_replace) {
            if (break_count == 1) {
               space_count++;
               if (state_dblspace) space_count++; // NEW
               replace_count++;
            }
            else {
                while (break_count--) *dest++ = 0x0A;
            }
        }
        else {
            while (break_count--) *dest++ = 0x0A;
        }
        break_count = 0;
    }

    if (space_count) {
        if (state_collapse) {
            *dest++ = ' ';
            collapse_count += space_count - 1;
        }
        else {
            while (space_count--) *dest++ = ' ';
        }
        space_count = 0;
    }

    if (verbose) fprintf(stderr,"replace_count=%ld collapse_count=%ld removed_codes=%d\n",replace_count,collapse_count,removed_codes);
    //return length-collapse_count-removed_codes*2;
    return dest - orig_dest;
}


//=============================================================================
// nomen omen est
//=============================================================================
#ifndef EMBEDDED_USE
int main(int argc, char* argv[]) {
#else
int TTDos2Ebk(int argc,char* argv[]) {
#endif
    FILE*          fp = NULL;
    long           size;
    char* buf1 = NULL;
    char* buf2 = NULL;
    char* infile  = 0;
    char* outfile = 0;
    //int            replace = 1;
    int            verbose = 0;
    int            i;

#ifndef EMBEDDED_USE
    PRINT_ID("TTDos2EBK");
#endif

    if (argc < 3) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

    // parse arguments
    for (i=1; i<argc; i++) {
        if (!strcmp(argv[i], "-v")) verbose = 1;
        else if (!infile)  infile  = argv[i];
        else if (!outfile) outfile = argv[i];
        else {
#ifndef EMBEDDED_USE
            PrintUsage();
#endif
            return 1;
        }
    }

    if (!infile || !outfile) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }


    if (!(fp = fopen(infile,"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",infile);
        return 1;
    }

    fseek(fp,0,SEEK_END);
    size = ftell(fp);
    rewind(fp);

    if (verbose) fprintf(stderr,"inputsize is %ld bytes\n",size);

    if (!(buf1 = malloc(size*2))) {   // size*2 necessary for double-space
        fprintf(stderr,"ERROR: cannot allocate %ld bytes\n",size);
        fclose(fp);
        free(buf2);
        free(buf1);
        return 1;
    }
    if (!(buf2 = malloc(size*2))) {   // size*2 necessary for double-space
        fprintf(stderr,"ERROR: cannot allocate %ld bytes\n",size);
        fclose(fp);
        free(buf2);
        free(buf1);
        return 1;
    }

    if (fread(buf1,size,1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot read content of inputfile %s\n",infile);
        fclose(fp);
        free(buf2);
        free(buf1);
        return 1;
    }
    fclose(fp);

    if (!(fp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
        free(buf2);
        free(buf1);
        return 1;
    }

    size = Translate(buf1,buf2,size);
    if (verbose) fprintf(stderr,"size after translation = %ld\n",size);

    size = RemoveAll0D(buf2,buf1,size);
    if (verbose) fprintf(stderr,"after removing all 0x0D is size %ld bytes\n",size);


    size = Preprocess(buf1,buf2,size,verbose);

    if (fwrite(buf2,size,1,fp) != 1) {
        fprintf(stderr,"ERROR: cannot write %ld bytes to outputfile %s\n",size,outfile);
    }
    else {
        if (verbose) fprintf(stderr,"%ld bytes written to file %s\n",size,outfile);
    }

    fclose(fp);
    free(buf2);
    free(buf1);

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
// Revision 1.9  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.8  2002/03/06 16:08:52  tnussb
// typo in PrintUsage() fixed
//
// Revision 1.7  2002/03/04 14:32:41  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.6  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.5  2000/11/28 00:06:33  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.4  2000/10/01 15:06:54  Thomas Nussbaumer
// support for more tags and some bugs fixed
//
// Revision 1.3  2000/08/27 23:54:55  Thomas Nussbaumer
// metatag handling support added
//
// Revision 1.2  2000/08/23 19:51:38  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/20 15:45:02  Thomas Nussbaumer
// initial version
//
//
//
