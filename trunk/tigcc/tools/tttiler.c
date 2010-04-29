/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       tttiler.c
* initial date:    24/11/2000
* authors:         thomas.nussbaumer@gmx.net
* description:     splits a binary image file (ImageStudio) into tiles
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of tttiler may also be
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
    fprintf(USAGE_OUT, "Usage: tttiler [-h|-v|-ht|-vt] <infile> <width> <height> <t_width> <t_height> <entries> <outfile> (array_name)\n"\
                       "       -h         ... tile from left to right and up to down\n"\
                       "       -v         ... tile from up   to down  and left to right\n"\
                       "       -ht        ... like -h, but generates complete test program\n"\
                       "       -vt        ... like -v, but generates complete test program\n"\
                       "       infile     ... binary image file generated with ImageStudio\n"\
                       "       width      ... width  of infile in pixel\n"\
                       "       height     ... height of infile in pixel\n"\
                       "       t_width    ... width  of one tile in pixel (<=32)\n"\
                       "       t_height   ... height of one tile in pixel\n"\
                       "       entries    ... number of data fields per line (0<entries<=20)\n"\
                       "       outfile    ... output filename (use - as name for output to stdout)\n"\
                       "       array_name ... optional name of array to generate\n\n"\
                       "       splits binary image file generated with ImageStudio in single\n"\
                       "       sprites (tiles) and generates C source code\n"\
                       "       including the necessary macros to access them.\n\n"\
                       "NOTE:  only 8, 16 and 32 pixel wide tiles are generated.\n"\
                       "       If the tilewidth is not exactly one of these widths,\n"\
                       "       the sprites are filled up to the right with empty pixels\n"\
                       "NOTE2: the width of the input file have to be a multiple of 8\n"\
                       "       otherwise ImageStudio and tttiler will not treat it correctly\n\n");
}


#define DIR_HORIZONTAL 0
#define DIR_VERTICAL   1

#define MODE_MONO      0
#define MODE_GRAY4     1

#define TESTPRG_ON     0
#define TESTPRG_OFF    1


//-----------------------------------------------------------------------------
// globals
//
// to reduce the function parameters we are using almost only globals
// (bad idea in general, but perfect for rapid prototyping)
//-----------------------------------------------------------------------------
FILE*          outfp;
int            width;
int            height;
int            tilewidth;
int            tileheight;
int            direction;
int            mode;
int            hcount;
int            vcount;
long           insize;
unsigned char* buffer;
int            dest_width;
int            padding;
int            entries;
int            padding_mask;
char*          arrayname;
char           str_height[255];
char           str_nr[255];
char           str_macro[255];

//=============================================================================
// converts given string into uppercase
//=============================================================================
void ToUppercase(char* str) {
    while (*str) {
        *str = (char)toupper(*str);
        str++;
    }
}

//=============================================================================
// extract a single tile
//=============================================================================
void WriteTile(long offset,int x,int y) {
    int h;
    int bytes_per_line = width/8;

    unsigned char* addr = buffer + offset + y*bytes_per_line + x/8;
    unsigned int  data1 = 0;
    //unsigned int  data2 = 0;
    unsigned int  shift = x % 8;

    for (h=0;h<tileheight;h++) {
        switch(dest_width) {
            case  8:
                data1 =  (*addr & 0xff) << 8;
                data1 |= (*(addr+1) & 0xff);
                data1 = (data1 >> (8-shift)) & padding_mask;
                fprintf(outfp,"0x%02x",data1);
                break;
            case 16:
                data1 =  (*addr & 0xff) << 16;
                data1 |= (*(addr+1) & 0xff) << 8;
                data1 |= (*(addr+2) & 0xff);
                data1 = (data1 >> (8-shift)) & padding_mask;
                fprintf(outfp,"0x%04x",data1);
                break;
            default:
                data1 =  (*addr & 0xff) << 24;
                data1 |= (*(addr+1) & 0xff) << 16;
                data1 |= (*(addr+2) & 0xff) << 8;
                data1 |= (*(addr+3) & 0xff);

                if (shift) {
                    data1 = (data1 << shift) & padding_mask;
                    data1 |= (*(addr+4) & 0xff) >> (8-shift);
                }

                data1 &= padding_mask;
                fprintf(outfp,"0x%08x",data1);
                break;
        }
        if (h == tileheight-1)       {}//fprintf(outfp,"");
        else if (!((h+1) % entries)) fprintf(outfp,",\n");
        else                         fprintf(outfp,",");

        addr += bytes_per_line;
    }
}


//=============================================================================
// process a complete plane starting at given offset
//=============================================================================
void ProcessPlane(long offset) {
    int x;
    int y;

    if (direction == DIR_HORIZONTAL) {
        for (y=0;y<vcount;y++) {
            for (x=0;x<hcount;x++) {
                fprintf(outfp,"// tile %d\n",y*hcount+x);
                WriteTile(offset,x*tilewidth,y*tileheight);
                if ((x == hcount - 1) && (y == vcount - 1)) {
                    fprintf(outfp,"\n");
                }
                else {
                    fprintf(outfp,",\n");
                }
            }
        }
    }
    else {
        for (x=0;x<hcount;x++) {
            for (y=0;y<vcount;y++) {
                fprintf(outfp,"// tile %d\n",x*vcount+y);
                WriteTile(offset,x*tilewidth,y*tileheight);
                if ((x == hcount - 1) && (y == vcount - 1)) {
                    fprintf(outfp,"\n");
                }
                else {
                    fprintf(outfp,",\n");
                }
            }
        }
    }
}



//=============================================================================
// write test program - part 1
//=============================================================================
void WriteTestProgram1() {
    fprintf(outfp,"#define SAVE_SCREEN\n");
    fprintf(outfp,"#include <tigcclib.h>\n");
    fprintf(outfp,"short _ti89,_ti92plus;\n\n\n");
}

//=============================================================================
// write test program - part 2
//=============================================================================
void WriteTestProgram2() {
    fprintf(outfp,"\n\nvoid _main(void) {\n");
    fprintf(outfp,"    short x,y;\n");
    fprintf(outfp,"    short tilewidth  = %d;\n",tilewidth);
    fprintf(outfp,"    short vcount     = %d;\n",vcount);
    fprintf(outfp,"    short hcount     = %d;\n",hcount);

    if (mode == MODE_GRAY4) {
        fprintf(outfp,"    short i;\n");
        fprintf(outfp,"    if (!GrayOn()) return;\n");
        fprintf(outfp,"    memset(GetPlane(0),0,LCD_SIZE);\n");
        fprintf(outfp,"    memset(GetPlane(1),0,LCD_SIZE);\n");
    }
    else {
        fprintf(outfp,"    memset(LCD_MEM,0,LCD_SIZE);\n");
    }

    if (direction == DIR_HORIZONTAL) {
        fprintf(outfp,"    for (y=0;y<vcount;y++) {\n");
        fprintf(outfp,"        for (x=0;x<hcount;x++) { \n");
        fprintf(outfp,"            short nr = y*hcount+x;\n");
    }
    else {
        fprintf(outfp,"    for (x=0;x<hcount;y++) {\n");
        fprintf(outfp,"        for (y=0;y<vcount;y++) { \n");
        fprintf(outfp,"            short nr = x*vcount+y);\n");
    }

    if (mode == MODE_GRAY4) {
        fprintf(outfp,"            for (i=0;i<2;i++) {\n");
        fprintf(outfp,"                Sprite%d(x*tilewidth,y*%s,%s,%s(i,nr),GetPlane(i),SPRT_OR);\n",dest_width,str_height,str_height,str_macro);
        fprintf(outfp,"            }\n");
    }
    else {
        fprintf(outfp,"            Sprite%d(x*tilewidth,y*%s,%s,%s(nr),LCD_MEM,SPRT_OR);\n",dest_width,str_height,str_height,str_macro);
    }

    fprintf(outfp,"        }\n");
    fprintf(outfp,"    }\n");

    fprintf(outfp,"    ngetchx();\n");
    if (mode == MODE_GRAY4) fprintf(outfp,"    GrayOff();\n");
    fprintf(outfp,"}\n\n");
}


//=============================================================================
// nomen omen est
//=============================================================================
int main(int argc, char* argv[]) {
    FILE* infp;
    int   i;
    int   testprg;

    // write it only when not in "output to stdout" mode !!!
    if (!(argc >= 9 && !strcmp(argv[8],"-"))) {
        PRINT_ID("TTTiler");
    }

    if (argc == 1) {
        PrintUsage();
        return 1;
    }

    if (argc != 9 && argc != 10) {
        fprintf(stderr,"ERROR: invalid number of parameters\n");
        PrintUsage();
        return 1;
    }

    //-------------------------------------------------------------------------
    // parse direction flag
    //-------------------------------------------------------------------------
    if (!strcmp(argv[1], "-h"))       direction = DIR_HORIZONTAL, testprg = TESTPRG_OFF;
    else if (!strcmp(argv[1], "-v"))  direction = DIR_VERTICAL,   testprg = TESTPRG_OFF;
    else if (!strcmp(argv[1], "-ht")) direction = DIR_HORIZONTAL, testprg = TESTPRG_ON;
    else if (!strcmp(argv[1], "-vt")) direction = DIR_VERTICAL,   testprg = TESTPRG_ON;
    else {
        fprintf(stderr,"ERROR: neither -h nor -v and neither -ht nor -vt specified\n");
        PrintUsage();
        return 1;
    }


    //-------------------------------------------------------------------------
    // parse width/height/tilewidth/tileheight/entries
    //-------------------------------------------------------------------------
    if (sscanf(argv[3],"%d",&width) != 1) {
        fprintf(stderr,"ERROR: cannot parse width=%s\n",argv[3]);
        PrintUsage();
        return 1;
    }
    if (width % 8) {
        fprintf(stderr,"ERROR: width not multiple of 8 (width=%d)\n",width);
        PrintUsage();
        return 1;
    }
    if (width <= 0) {
        fprintf(stderr,"ERROR: negative or zero width not allowed (width=%d)\n",width);
        PrintUsage();
        return 1;
    }


    if (sscanf(argv[4],"%d",&height) != 1) {
        fprintf(stderr,"ERROR: cannot parse height=%s\n",argv[4]);
        PrintUsage();
        return 1;
    }
    if (height <= 0) {
        fprintf(stderr,"ERROR: negative or zero height not allowed (height=%d)\n",height);
        PrintUsage();
        return 1;
    }

    if (sscanf(argv[5],"%d",&tilewidth) != 1) {
        fprintf(stderr,"ERROR: cannot parse tilewidth=%s\n",argv[5]);
        PrintUsage();
        return 1;
    }
    if (tilewidth <= 0) {
        fprintf(stderr,"ERROR: negative or zero tilewidth not allowed (tilewidth=%d)\n",tilewidth);
        PrintUsage();
        return 1;
    }
    if (tilewidth > 32) {
        fprintf(stderr,"ERROR: tilewidth larger than 32 not allowed (tilewidth=%d)\n",tilewidth);
        PrintUsage();
        return 1;
    }
    if (tilewidth <= 8)       dest_width = 8;
    else if (tilewidth <= 16) dest_width = 16;
    else                      dest_width = 32;

    padding = dest_width - tilewidth;

    padding_mask = 0;
    for (i=0;i<padding;i++) padding_mask |= 1 << i;
    padding_mask = ~padding_mask;

    if (tilewidth <= 8)       padding_mask &= 0xff;
    else if (tilewidth <= 16) padding_mask &= 0xffff;


    if (tilewidth > width) {
        fprintf(stderr,"ERROR: tilewidth larger than width\n");
        PrintUsage();
        return 1;
    }

    if (sscanf(argv[6],"%d",&tileheight) != 1) {
        fprintf(stderr,"ERROR: cannot parse tileheight=%s\n",argv[6]);
        PrintUsage();
        return 1;
    }
    if (tileheight <= 0) {
        fprintf(stderr,"ERROR: negative or zero tileheight not allowed (tileheight=%d)\n",tileheight);
        PrintUsage();
        return 1;
    }
    if (tileheight > height) {
        fprintf(stderr,"ERROR: tileheight larger than height\n");
        PrintUsage();
        return 1;
    }

    if (sscanf(argv[7],"%d",&entries) != 1) {
        fprintf(stderr,"ERROR: cannot parse entries=%s\n",argv[7]);
        PrintUsage();
        return 1;
    }
    if (entries <= 0 || entries > 20) {
        fprintf(stderr,"ERROR: invalid value for entries (entries=%d)\n",entries);
        PrintUsage();
        return 1;
    }

    hcount = width  / tilewidth;
    vcount = height / tileheight;

    //-------------------------------------------------------------------------
    // open inputfile
    //-------------------------------------------------------------------------
    if (!(infp = fopen(argv[2],"rb"))) {
        fprintf(stderr,"ERROR: cannot open inputfile %s\n",argv[2]);
        PrintUsage();
        return 1;
    }

    //-------------------------------------------------------------------------
    // get and check input size
    //-------------------------------------------------------------------------
    if (fseek(infp,0,SEEK_END)) {
        fprintf(stderr,"ERROR: cannot find end of inputfile %s\n",argv[2]);
        PrintUsage();
        fclose(infp);
        return 1;
    }

    insize = ftell(infp);
    if (insize == -1L) {
        fprintf(stderr,"ERROR: cannot evaluate size of inputfile %s\n",argv[2]);
        PrintUsage();
        fclose(infp);
        return 1;
    }
    rewind(infp);

    if (insize == (long)height*(long)width/8) {
        mode = MODE_MONO;
    }
    else if (insize == (long)height*(long)width/4) {
        mode = MODE_GRAY4;
    }
    else {
        fprintf(stderr,"ERROR: filesize (%ld) doesn't correspond with height/width\n",insize);
        PrintUsage();
        fclose(infp);
        return 1;
    }

    //-------------------------------------------------------------------------
    // allocate buffer, read in all bytes and close input file
    //-------------------------------------------------------------------------
    if (!(buffer = malloc(insize))) {
        fprintf(stderr,"ERROR: cannot allocate %ld bytes for input data\n",insize);
        PrintUsage();
        fclose(infp);
        return 1;
    }

    if (fread(buffer,insize,1,infp) != 1) {
        fprintf(stderr,"ERROR: cannot read %ld bytes from inputfile %s\n",insize,argv[2]);
        PrintUsage();
        fclose(infp);
        free(buffer);
        return 1;
    }

    fclose(infp);

    //-------------------------------------------------------------------------
    // open outputfile
    //-------------------------------------------------------------------------
    if (!strcmp(argv[8],"-")) {
        outfp = stdout;
    }
    else if (!(outfp = fopen(argv[8],"w"))) {
        fprintf(stderr,"ERROR: cannot open outputfile %s\n",argv[8]);
        PrintUsage();
        free(buffer);
        return 1;
    }

    if (argc == 10) arrayname = argv[9];
    else            arrayname = "data";

    if (outfp != stdout) {
        printf("infile     = %s\n",argv[2]);
        printf("filesize   = %ld\n",insize);
        printf("direction  = %s\n",((direction==DIR_HORIZONTAL) ? "HORIZONTAL":"VERTICAL"));
        printf("mode       = %s\n",((mode==MODE_MONO) ? "MONO":"GRAY4"));
        printf("width      = %d\n",width);
        printf("height     = %d\n",height);
        printf("tilewidth  = %d\n",tilewidth);
        printf("tileheight = %d\n",tileheight);
        printf("hcount     = %d\n",hcount);
        printf("vcount     = %d\n",vcount);
        printf("tilecount  = %d\n",hcount * vcount);
        printf("destwidth  = %d\n",dest_width);
        printf("padding    = %d\n",padding);
        printf("paddmask   = 0x%08x\n",padding_mask);
        printf("outfile    = %s\n",argv[8]);
        printf("arrayname  = %s\n",arrayname);
    }

    sprintf(str_nr,"TILENUMBER_%s",arrayname);
    ToUppercase(str_nr);
    sprintf(str_height,"TILEHEIGHT_%s",arrayname);
    ToUppercase(str_height);
    sprintf(str_macro,"TILEADDR_%s",arrayname);
    ToUppercase(str_macro);
    if (testprg == TESTPRG_ON) WriteTestProgram1();

    //-------------------------------------------------------------------------
    // write header
    //-------------------------------------------------------------------------
    fprintf(outfp,"// number of tiles in array %s\n",arrayname);
    fprintf(outfp,"#define %s %d\n\n",str_nr,hcount*vcount);
    fprintf(outfp,"// height of one tile in array %s\n",arrayname);
    fprintf(outfp,"#define %s %d\n\n",str_height,tileheight);

    if (mode == MODE_GRAY4) {
        fprintf(outfp,"// macro returns the address of tile nr of plane p from array %s\n",arrayname);
        fprintf(outfp,"#define %s(p,nr) &(%s[p][%s*nr])\n\n",str_macro,arrayname,str_height);
    }
    else {
        fprintf(outfp,"// macro returns the address of tile nr from array %s\n",arrayname);
        fprintf(outfp,"#define %s(nr) &(%s[%s*nr])\n\n",str_macro,arrayname,str_height);
    }

    switch(dest_width) {
        case 8:  fprintf(outfp,"unsigned char %s",arrayname); break;
        case 16: fprintf(outfp,"unsigned short %s",arrayname); break;
        default: fprintf(outfp,"unsigned long %s",arrayname); break;
    }

    if (mode == MODE_GRAY4) fprintf(outfp,"[2][%s*%s] = {{\n",str_nr,str_height);
    else                    fprintf(outfp,"[%s*%s] = {\n",str_nr,str_height);

    ProcessPlane(0);
    if (mode == MODE_GRAY4) {
        fprintf(outfp,"},\n// ---- plane 2 ----\n{\n"/*,hcount*vcount*tileheight*/);
        ProcessPlane(insize/2);
        fprintf(outfp,"}");
    }

    fprintf(outfp,"};\n");

    if (testprg == TESTPRG_ON) WriteTestProgram2();

    if (outfp != stdout) fclose(outfp);
    free(buffer);

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
// Revision 1.6  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.5  2001/06/20 13:05:41  Thomas Nussbaumer
// (1) tilenumber output corrected
// (2) suppressing following comma after the last tile of a plane
// (3) writing now macros to access the tiles
// (4) handling of output redirection to stdout (outfilename == "-")
// (5) customizable arrayname (commandline parameter)
// (6) test program output adapted to TIGCC 0.90 (int -> short)
//
// Revision 1.4  2000/11/28 00:08:29  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.3  2000/11/27 23:25:46  Thomas Nussbaumer
// bug fix: generating now correct test program for monochrome tiles, too
//
// Revision 1.2  2000/11/26 21:35:02  Thomas Nussbaumer
// (1) some debug outputs removed
// (2) 16 pixel-wide sprites mode fixed
//
// Revision 1.1  2000/11/26 20:01:02  Thomas Nussbaumer
// initial version
//
//
//
