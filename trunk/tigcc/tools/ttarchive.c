/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttarchive.c
* initial date:    16/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     archive generation tool
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttarchive may also be
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

#ifndef __TTARCHIVE__
#define __TTARCHIVE__

// if EMBEDDED_USE is defined, than we use this sourcefile from within another
// sourcefile

#ifndef EMBEDDED_USE

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>

#include "ttversion.h"
#include "revtools.h"
#include "tt.h"
#include "strhead.h"
#include "ttarchive.h"
#include "bin2oth.c"
#include "packhead.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.11"

//-----------------------------------------------------------------------------
//
// format of TTA (TIGCC Tools Archive):
// ------------------------------------
//
// unsigned char magic1;            ... must be 't'
// unsigned char magic2;            ... must be 't'
// unsigned char magic3;            ... must be 'a'
// unsigned char version;           ... version of fileformat (0 at the moment)
// unsigned int nr_entries;         ... count of entries
// TTAEntry     info[nr_entries];   ... header information of all entries
// <entry data>                     ... here comes the data of the entries
//
// TTAEntry is defined as:
//
// typedef struct {
//    unsigned int offset;   // offset to the entry data
//    unsigned int length;   // length of entry
//    char         name[8];  // entry name
//    unsigned int misc[2];  // info from cfg file (may be queried by a prg)
// } TTAEntry;
//
//-----------------------------------------------------------------------------

//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTArchive");
    fprintf(USAGE_OUT, "Usage: ttarchive [flags] <cfgfile> <varname> [folder]\n\n"\
                       "       -89            .... generate TI89 variable\n"\
                       "       -92            .... generate TI92p variable\n"\
                       "       -v             .... verbose (unsets -quiet)\n"\
                       "       -quiet         ...  don't output standard messages (unsets -v)\n"\
                       "       -e <extension> .... use special extension\n\n"\
                       "       cfgfile        .... configuration file to use\n"\
                       "       varname        .... on-calc variable name of output\n"\
                       "       folder         .... on-calc destination folder (OPTIONAL)\n\n"\
                       "       generates an archive file. The contents of the archive\n"\
                       "       have to be specified in a configfile where each line contains\n"\
                       "       one file which should be added to the archive.\n"\
                       "       An archive file can hold compressed or uncompressed entries.\n"\
                       "       If an entry should be compressed add tag \"compress\" at the\n"\
                       "       end of the config line.\n\n"\
                       "       [-------- format of one line in the configfile --------]\n"\
                       "       <file_to_store> <varname> <attr1> <attr2> [compress]\n\n"\
                       "       file_to_store ... name including path to inputfile\n"\
                       "       varname       ... name used to find data in archive (max. 8 chars)\n"\
                       "       attr1 attr2   ... misc. attributes which may be queried from program\n"\
                       "                           (0..65536)\n"\
                       "       compress      ... if this tag is set on the end of the line the\n"\
                       "                         given inputfile will be compressed before adding\n"\
                       "                         it to the archive\n\n");

}
#endif

#ifndef EMBEDDED_USE
#define EMBEDDED_USE
#include "ttpack.c"
#undef EMBEDDED_USE
#else
#include "ttpack.c"
#endif

//=============================================================================
// this function parses the configfile and generates a complete RAM image of
// the final output
//
// its quite a hack. normally I hate such spaghetti code, but it was done in
// 10 minutes and works. why should I touch it again ?
//=============================================================================
unsigned char* ProcessConfigFile(char* cfgname,unsigned long* psize,int verbose) {
    FILE*          fp = fopen(cfgname,"r");
    FILE*          tmpfp;
    char           fname[200];
    char           name[200];
    char           inputline[1024];
    unsigned int   misc1;
    unsigned int   misc2;
    unsigned int   parsed;
    unsigned int   lines;
    unsigned long  flength;
    unsigned int   offset;
    TTAEntry*      newptr;
    unsigned char* newptr2;
    TTAEntry*      entries = NULL;
    unsigned char* data    = NULL;
    unsigned char  description[TTA_DESC_LENGTH];
    int            input;
    char           option[200];
    int            do_packing;
    char*          tmp_packname = "tmppack.pck";

    //----------------------------------
    // check if file is opened correctly
    //----------------------------------
    if (!fp) {
        fprintf(stderr,"ERROR: cannot open configfile %s\n",cfgname);
        return 0;
    }

    lines  = 0;
    offset = 0;

    //----------------------------------------------------
    // check if there is a description in the configfile
    //
    // the description of a TTA file may only be specified
    // in the first line of the configfile.
    // if the first line starts with '#' the next max.20
    // character are taken as description of the TTArchive
    //----------------------------------------------------
    memset(description,0,TTA_DESC_LENGTH);
    input = fgetc(fp);
    if (input == '#') {
        int i = 0;
        do {
            input = fgetc(fp);
            if (input != EOF) description[i++] = input;
        }
        while (input != EOF && input != 0x0D && input != 0x0A && i < TTA_DESC_LENGTH);

        if (i > 0) description[i-1] = 0;
    }
    rewind(fp);

    if (verbose) {
        if (description[0]) fprintf(stderr,"description = [%s]\n",description);
        else                fprintf(stderr,"description = ***none***\n");
    }

    input = 0;

    while (!feof(fp)) {
        if (!input && description[0]) { // skip first line if it is a description
            fgets(inputline,1023,fp);
            input = 1;
            continue;
        }
        if (fgets(inputline,1023,fp)==NULL) break;
        parsed = sscanf(inputline,"%s %s %u %u %s \n",fname,name,&misc1,&misc2,option);
        if ((signed int)parsed == EOF) break;
        lines++;
        if (parsed != 4 && parsed != 5) {
            fprintf(stderr,"ERROR in line %d: invalid number of parameters (%d)\n",lines,parsed);
            fprintf(stderr,"input was [%s]\n",inputline);
            fclose(fp);
            if (entries) free(entries);
            if (data)    free(data);
            return 0;
        }

        if (parsed == 5 && !strcmp(option,"compress")) do_packing = 1;
        else                                           do_packing = 0;

        newptr = (TTAEntry*)realloc(entries,sizeof(TTAEntry)*lines);
        if (!newptr) {
            fprintf(stderr,"ERROR: cannot allocate memory\n");
            fclose(fp);
            if (entries) free(entries);
            if (data)    free(data);
            return 0;
        }
        entries = newptr;

        if (do_packing) {
            char* ttpack_params[4];
            ttpack_params[0] = "";
            ttpack_params[1] = "-quiet";
            ttpack_params[2] = fname;
            ttpack_params[3] = tmp_packname;

            if (verbose) printf("[line %02d] process packing %s -> %s ...\n",lines,fname,tmp_packname);

            if (TTPack(4,ttpack_params)) {
                fprintf(stderr,"ERROR in line %d: cannot compress file %s to %s\n",lines,fname,tmp_packname);
                if (entries) free(entries);
                if (data)    free(data);
                fclose(fp);
                return 0;
            }

            strcpy(fname,tmp_packname);
        }
        else {
            if (verbose) printf("[line %02d] dont process packing on %s\n",lines,fname);
        }

        tmpfp = fopen(fname,"rb");
        if (!tmpfp) {
            fprintf(stderr,"ERROR in line %d: cannot open file %s\n",lines,fname);
            if (entries) free(entries);
            if (data)    free(data);
            fclose(fp);
            if (do_packing) remove(tmp_packname);
            return 0;
        }

        fseek(tmpfp,0,SEEK_END);
        flength = ftell(tmpfp);
        if (flength+offset+lines*sizeof(TTAEntry) > TT_MAX_OTHDATA) {
            fprintf(stderr,"ERROR in line %d: will not fit anymore in archive (%lu > %d)\n",
                    lines,flength+offset+lines*sizeof(TTAEntry),TT_MAX_OTHDATA);
            if (entries) free(entries);
            if (data)    free(data);
            fclose(tmpfp);
            fclose(fp);
            if (do_packing) remove(tmp_packname);
            return 0;
        }

        fseek(tmpfp,0,SEEK_SET);
        newptr2 = realloc(data,offset+flength);
        if (!newptr2) {
            fprintf(stderr,"ERROR in line %d: cannot allocate memory\n",lines);
            if (entries) free(entries);
            if (data)    free(data);
            fclose(tmpfp);
            fclose(fp);
            if (do_packing) remove(tmp_packname);
            return 0;
        }

        data = newptr2;
        if (fread(data+offset,flength,1,tmpfp) != 1) {
            fprintf(stderr,"ERROR in line %d: reading input data failed\n",lines);
            if (entries) free(entries);
            if (data)    free(data);
            fclose(tmpfp);
            fclose(fp);
            if (do_packing) remove(tmp_packname);
            return 0;
        }

        fclose(tmpfp);
        if (do_packing) remove(tmp_packname);

        strncpy(entries[lines-1].name,name,8);
        entries[lines-1].offset[0]  = offset >> 8;
        entries[lines-1].offset[1]  = offset & 0xff;
        entries[lines-1].length[0]  = flength >> 8;
        entries[lines-1].length[1]  = flength & 0xff;
        entries[lines-1].misc1[0]   = misc1 >> 8;
        entries[lines-1].misc1[1]   = misc1 & 0xff;
        entries[lines-1].misc2[0]   = misc2 >> 8;
        entries[lines-1].misc2[1]   = misc2 & 0xff;

        if (verbose) printf("[line %02d] written - offset=%05u var=%s (%u %u)\n",lines,offset,name,misc1,misc2);
        offset+=flength;
    }

    fclose(fp);

    if (!entries) return 0;

    newptr2 = malloc(sizeof(TTAEntry)*lines + offset +6);

    if (!newptr2) {
        free(entries);
        free(data);
        return 0;
    }

    *newptr2     = 't';  // write magic characters and format version
    *(newptr2+1) = 't';
    *(newptr2+2) = 'a';
    *(newptr2+3) = 0;

    *(newptr2+4) = lines>>8;
    *(newptr2+5) = lines & 0xff;


    memcpy(newptr2+6,entries,sizeof(TTAEntry)*lines);
    memcpy(newptr2+6+sizeof(TTAEntry)*lines,data,offset);

    *psize = sizeof(TTAEntry)*lines + offset + 6;

    if (entries) free(entries);
    if (data)    free(data);

    if (description[0]) {
        if (*psize + TTA_DESC_LENGTH + 3 > TT_MAX_OTHDATA) {
            fprintf(stderr,"ERROR: no place for description. generation aborted.\n");
            free(newptr2);
            return 0;
        }
        data = realloc(newptr2,*psize + TTA_DESC_LENGTH + 3);
        if (!data) {
            fprintf(stderr,"ERROR: cannot allocate memory.\n");
            free(newptr2);
            return 0;
        }

        newptr2 = data;
        data = newptr2+*psize;
        *data++ = 0;
        memcpy(data,description,20);
        data+=20;
        *data++=0;
        *data=0xad;   // ad == archive description
        *psize += TTA_DESC_LENGTH + 3;
    }

    return newptr2;
}


//=============================================================================
// nomen omen est
//=============================================================================
#ifndef EMBEDDED_USE
int main(int argc,char *argv[]) {
#else
int TTArchive(int argc,char *argv[]) {
#endif
    char*          folder    = NULL;
    char*          name      = NULL;
    char*          cfgname   = NULL;
    int            calctype  = -1;
    FILE*          ofp;
    char           outfile[1024];
    unsigned char* buffer;
    unsigned long  length;
    unsigned char* outbuffer;
    unsigned int   outlength;
    unsigned int   retval;
    char* extension = "tta";  // default extension
    int            n;
    int            verbose = 0;
    int            quiet   = 0;


    // check for too less arguments
    if (argc < 4) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

    // parse arguments
    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-89"))         calctype = CALC_TI89;
        else if (!strcmp(argv[n], "-92"))    calctype = CALC_TI92P;
        else if (!strcmp(argv[n], "-quiet")) verbose  = 0,quiet = 1;
        else if (!strcmp(argv[n], "-v"))     verbose  = 1,quiet = 0;
        else if (!strcmp(argv[n],"-e")) {
            if (n==argc-1) {
#ifndef EMBEDDED_USE
                PrintUsage();
#endif
                return 1;
            }
            else {
                int length;
                n++;
                extension = argv[n];
                length = strlen(extension);
                if (length < 1 || length > 4) {
                    fprintf(stderr,"ERROR: extension have to be between 1 and 4 characters\n");
                    return 1;
                }
            }
        }
        else if (argv[n][0] == '-') {
            fprintf(stderr,"ERROR: invalid option %s",argv[n]);
            return 1;
        }
        else if (!cfgname) cfgname = argv[n];
        else if (!name)    name    = argv[n];
        else if (!folder)  folder  = argv[n];
        else {
#ifndef EMBEDDED_USE
            PrintUsage();
#endif
            return 1;
        }
    }

    // check if all necessary arguments are supplied
    if (!cfgname || !name || calctype == -1) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }


#ifndef EMBEDDED_USE
    if (!quiet) PRINT_ID("TTArchive");
#endif


    if (calctype == CALC_TI89) sprintf(outfile,"%s.89y",name);
    else                       sprintf(outfile,"%s.9xy",name);

    buffer = ProcessConfigFile(cfgname,&length,verbose);
    if (!buffer) return 1;


    if (!(ofp = fopen(outfile,"wb"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",outfile);
        return 1;
    }

    outbuffer = DataBuffer2OTHBuffer(calctype,folder,name,extension,length,buffer,&outlength);

    retval = 0;

    if (!outbuffer) {
        retval = 1;
    }
    else {
        if (fwrite(outbuffer,outlength,1,ofp) != 1) {
            fprintf(stderr,"ERROR: cannot write %u bytes to %s\n",outlength,outfile);
            retval = 1;
        }
        else {
            if (!quiet) fprintf(stdout,"%u bytes written to %s\n",outlength,outfile);
        }
        free(outbuffer);
    }

    free(buffer);
    fclose(ofp);

    return retval;
}

#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.11 2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.10 2002/03/14 10:45:49  tnussb
// (1) by adding the word "compress" at the end of a line in the configuration
//     file, ttarchive will automatically compress this file now, before adding
//     it to the archive
// (2) new flag "-quiet" added (suppress standard messages)
// (3) specifying of extensions between 1 and 4 characters is possible now
// (4) usage text adapted
//
// Revision 1.9  2002/03/04 14:32:41  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.8  2002/02/07 09:49:36  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.7  2000/11/28 00:05:12  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.6  2000/08/26 12:57:32  Thomas Nussbaumer
// outputs now sizes if ebook gets to long
//
// Revision 1.5  2000/08/23 19:22:59  Thomas Nussbaumer
// (1) using now macros for automatic version display (revtools.h)
// (2) minor comments added
// (3) usage information now near top of file to find it quicker
//
// Revision 1.4  2000/08/23 01:10:57  Thomas Nussbaumer
// (1) using now bin2oth.c for wrapping
// (2) handling of archive description (20 bytes) added
//
// Revision 1.3  2000/08/20 16:25:21  Thomas Nussbaumer
// missing initialization of extension pointer added
//
// Revision 1.2  2000/08/20 15:29:36  Thomas Nussbaumer
// flag -e <extension> added (use personalized in calc extension)
//
// Revision 1.1  2000/08/16 23:05:59  Thomas Nussbaumer
// initial version
//
//
//
//
