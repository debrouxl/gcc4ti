/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttsplit.c
* initial date:    23/08/2000
* authors:         thomas.nussbaumer@gmx.net
* description:     splits a file into two or more files
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttsplit may also be
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
#include <ctype.h>


#include "tt.h"          // generic defines
#include "ttversion.h"   // TI-68k Developer Utilities version info
#include "revtools.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.7"


//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    fprintf(USAGE_OUT, "Usage: ttsplit [flags] <-t | -b> <offsetfile> <infile>\n"\
                       "       -t          ...  treat offsets as linenumbers (starting at 1)\n"\
                       "       -b          ...  treat offsets as byteoffsets (starting at 0)\n"\
                       "       -s <string> ...  use inline split tag instead of offset file\n"\
                       "       -v          ...  output additionally messages\n\n"\
                       "offsets are read from file offsetfile, which must contain offsets only\n"\
                       "in ascending numbers (maximum: 999 offsets). each offset is treated as\n"\
                       "the start of a new file. the first offset is the start of the second (!!!)\n"\
                       "file and so on.\n"\
                       "The outputfile names are generated from the inputfilename using\n"\
                       "the following pattern:\n"\
                       "<inputname>p<nr>.<inputextension>\n"\
                       "nr ranges from 000 to 999\n\n");
}

#endif



//=============================================================================
// split text input file using a inline tag on a single line
//=============================================================================
unsigned int SplitWithTag(char* inputname,char* splittag,int verbose) {
    char*         dot_position = strchr(inputname,'.');
    char          extension[300];
    char          prefix[300];
    unsigned long dpl;
    size_t        len;
    FILE*         fp;
    FILE*         ofp = 0;
    unsigned int  i;
    unsigned int  generated_files = 0;
    long          pos = 0;
    char          ofname[1000];
    char          buffer[10000];

    buffer[9999] = 0;


    if (!(fp = fopen(inputname,"r"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",inputname);
        return 0;
    }

    if (!dot_position) {
        extension[0] = 0;
        strcpy(prefix,inputname);
    }
    else {
        dpl = dot_position - inputname;
        if (!dpl) prefix[0] = 0;
        else {
            strncpy(prefix,inputname,dpl);
            prefix[dpl] = 0;
        }
        len = strlen(inputname);

        if (len - dpl == 0) {
            extension[0] = 0;
        }
        else {
            strncpy(extension,&inputname[dpl],len - dpl);
            extension[len - dpl] = 0;
        }
    }

    if (verbose) {
        fprintf(stderr,"using prefix    = [%s]\n",prefix);
        fprintf(stderr,"using extension = [%s]\n",extension);
    }

    pos = 1;
    i   = 0;

    sprintf(ofname,"%sp%03d%s",prefix,i,extension);
    if (!(ofp = fopen(ofname,"w"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",ofname);
        return generated_files;
    }

    do {
        if (fgets(buffer,9999,fp) == 0) {
            fclose(fp);
            fclose(ofp);
            return ++generated_files;
        }

        // check if line contains splittag
        if (strstr(buffer,splittag)) {
            fclose(ofp);
            generated_files++;
            sprintf(ofname,"%sp%03d%s",prefix,i+1,extension);
            if (!(ofp = fopen(ofname,"w"))) {
                fprintf(stderr,"ERROR: cannot open outputfile %s\n",ofname);
                return generated_files;
            }
            i++;
            continue;
        }

        if (fputs(buffer,ofp) == EOF) {
            fprintf(stderr,"ERROR: cannot write more lines to file %s\n",ofname);
            fclose(fp);
            fclose(ofp);
            return generated_files;
        }
    }
    while(1);

    return 0;
}


//=============================================================================
// extract a part in binary mode
//=============================================================================
int ExtractBinary(FILE* ifp,char* outname,unsigned long start,unsigned long length)
{
    FILE* ofp;

    int input;

    fprintf(stderr,"extracting to %s from start=%lu (length=%lu)\n",outname,start,length);

    if (!(ofp = fopen(outname,"wb"))) {
        fprintf(stderr,"ERROR: cannot open output filename %s\n",outname);
        return 0;
    }

    if (fseek(ifp,start,SEEK_SET)) {
        fprintf(stderr,"ERROR: cannot seek to binary offset %lu\n",start);
        fclose(ofp);
        return 0;
    }
    while (length--) {
        input = fgetc(ifp);
        if (input == EOF) {
            fprintf(stderr,"ERROR: cannot read more data from inputfile\n");
            fclose(ofp);
            return 0;
        }
        if (fputc(input,ofp) == EOF) {
            fprintf(stderr,"ERROR: cannot read write data to outputfile\n");
            fclose(ofp);
            return 0;
        }
    }

    fclose(ofp);
    return 1;
}


//=============================================================================
// split the input file
//=============================================================================
unsigned int Split2Parts(char* inputname,int use_textmode,
                         long* offsets, unsigned int offset_count)
{
    char*         dot_position = strchr(inputname,'.');
    char          extension[300];
    char          prefix[300];
    unsigned long dpl;
    size_t        len;
    FILE*         fp;
    FILE*         ofp = 0;
    unsigned int  i;
    unsigned int  generated_files = 0;
    long          pos = 0;
    char          ofname[1000];
    char          buffer[10000];

    buffer[9999] = 0;

    if (use_textmode) {
        fp = fopen(inputname,"r");
    }
    else {
        fp = fopen(inputname,"rb");
    }

    if (!fp) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",inputname);
        return 0;
    }

    if (!dot_position) {
        extension[0] = 0;
        strcpy(prefix,inputname);
    }
    else {
        dpl = dot_position - inputname;
        if (!dpl) prefix[0] = 0;
        else {
            strncpy(prefix,inputname,dpl);
            prefix[dpl] = 0;
        }
        len = strlen(inputname);

        if (len - dpl == 0) {
            extension[0] = 0;
        }
        else {
            strncpy(extension,&inputname[dpl],len - dpl);
            extension[len - dpl] = 0;
        }
    }

    fprintf(stderr,"using prefix    = [%s]\n",prefix);
    fprintf(stderr,"using extension = [%s]\n",extension);

    if (!use_textmode) {
        sprintf(ofname,"%sp000%s",prefix,extension);
        if (!ExtractBinary(fp,ofname,0,offsets[0])) {
            return generated_files;
        }
        generated_files++;

        for (i=0;i<offset_count-1;i++) {
            sprintf(ofname,"%sp%03d%s",prefix,i+1,extension);
            if (!ExtractBinary(fp,ofname,offsets[i],offsets[i+1]-offsets[i])) {
                return generated_files;
            }
            generated_files++;
        }

        fseek(fp,0,SEEK_END);
        pos = ftell(fp);
        sprintf(ofname,"%sp%03d%s",prefix,i+1,extension);
        if (!ExtractBinary(fp,ofname,offsets[offset_count-1],pos - offsets[offset_count-1])) {
             return generated_files;
        }
        generated_files++;

        return generated_files;
    }

    // here comes the textmode extraction ...
    pos = 1;
    i   = 0;

    sprintf(ofname,"%sp%03d%s",prefix,i,extension);
    if (!(ofp = fopen(ofname,"w"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",ofname);
        return generated_files;
    }



    do {
        if (   pos > 1
            && i < offset_count
            && pos == offsets[i]
        ) {
            fclose(ofp);
            generated_files++;
            sprintf(ofname,"%sp%03d%s",prefix,i+1,extension);
            if (!(ofp = fopen(ofname,"w"))) {
                fprintf(stderr,"ERROR: cannot open outputfile %s\n",ofname);
                return generated_files;
            }
            i++;
        }
        if (fgets(buffer,9999,fp) == 0) {
            fclose(fp);
            fclose(ofp);
            return ++generated_files;
        }
        pos++;
        if (fputs(buffer,ofp) == EOF) {
            fprintf(stderr,"ERROR: cannot write more lines to file %s\n",ofname);
            fclose(fp);
            fclose(ofp);
            return generated_files;
        }
    }
    while(1);

    return 0;
}


//=============================================================================
// parse offsets from file
//=============================================================================
unsigned int ReadOffsets(char* filename,long* offsets) {
    FILE*        fp    = fopen(filename,"r");
    unsigned int count = 0;
    int          read;

    if (!fp) {
        fprintf(stderr,"ERROR: cannot open offsetfile %s\n",filename);
        return 0;
    }

    do {
        read = fscanf(fp,"%ld",&offsets[count]);
        if (!read) {
            fclose(fp);
            fprintf(stderr,"ERROR: parsing offset[%u] from %s failed\n",count,filename);
            return 0;
        }
        if (read == EOF) {
            fclose(fp);
            return count;
        }


        // From 1.30-: offsets[count] < offsets[count] (???)
        if (count > 0 && offsets[count] < offsets[count - 1]) {
            fprintf(stderr,"ERROR: offsets are not ascending\n");
            fclose(fp);
            return 0;
        }
        if (offsets[count] == 0) {
            fprintf(stderr,"ERROR: offset 0 is invalid\n");
            fclose(fp);
            return 0;
        }
        count++;
        if (count==999) {
            fclose(fp);
            fprintf(stderr,"WARNING: read 1000 offsets. Dropping the rest.\n");
            return count;
        }
    }
    while (1);

    return 0;
}


//=============================================================================
// nomen omen est
//=============================================================================
#ifndef EMBEDDED_USE
int main(int argc, char* argv[]) {
#else
int TTSplit(int argc, char* argv[]) {
#endif
    long  offsets[1000]; // 1.40: from unsigned long.
    unsigned int   offset_count;
    int   i;
    unsigned int   retval;
    int            textmode   = -1;
    int            verbose    = 0;
    char*          splittag   = 0;
    char*          offsetname = 0;
    char*          infilename = 0;

#ifndef EMBEDDED_USE
    PRINT_ID("TTSplit");
#endif

    for (i=1; i<argc; i++) {
        if (!strcmp(argv[i], "-b"))      textmode = 0;
        else if (!strcmp(argv[i], "-t")) textmode = 1;
        else if (!strcmp(argv[i], "-v")) verbose  = 1;
        else if (!strcmp(argv[i], "-s")) {
            if (i == argc - 1) {
#ifndef EMBEDDED_USE
                PrintUsage();
#endif
                return 1;
            }
            i++;
            splittag = argv[i];
        }
        else {
            if (!splittag && !offsetname)  offsetname = argv[i];
            else if (!infilename)          infilename = argv[i];
        }
    }

    if (!infilename || (!splittag && !offsetname) || textmode == -1) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

    if (!splittag) {
        if (!(offset_count = ReadOffsets(offsetname,offsets))) return 1;

        if (textmode && offsets[0] <= 1) {
            fprintf(stderr,"ERROR: offset with value 0 or 1 is not allowed for textmode\n");
            return 1;
        }
        retval = Split2Parts(infilename,textmode,offsets,offset_count);

    }
    else {
        retval = SplitWithTag(infilename,splittag,verbose);
        if (!retval) {
            fprintf(stderr,"ERROR: splitting inputfile %s failed\n",infilename);
            return 1;
        }
        if (verbose) fprintf(stderr,"successfully generated %u files\n",retval);
        return 0;
    }

    if (retval == offset_count+1) {
        fprintf(stderr,"generated ONLY %u out of %u files\n",retval,offset_count+1);
    }
    else {
        if (verbose) fprintf(stderr,"successfully generated %u out of %u files\n",retval,retval);
    }

    return (retval == offset_count+1) ? 0 : 1;
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
// Revision 1.6  2002/03/04 14:32:42  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.5  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.4  2000/11/28 00:07:32  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.3  2000/08/28 18:10:18  Thomas Nussbaumer
// nasty bug (uninitialized variable) fixed which causes program to return
// != 0 in good cases, too
//
// Revision 1.2  2000/08/26 19:00:36  Thomas Nussbaumer
// inline split tag mode added to make ebook generation easier
//
// Revision 1.1  2000/08/23 20:18:10  Thomas Nussbaumer
// initial version (quite a hack)
//
//
//
