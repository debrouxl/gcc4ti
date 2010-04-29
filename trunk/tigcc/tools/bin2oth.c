/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       bin2oth.c
* initial date:    22/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     converts a data buffer into an oth buffer and checks
*                  for maximal length, too
*
* NOTE:  this routine is used in many tools, therefore it is implemented in a
*        separately C file and included in the tools
*
* NOTE2: !!!! THIS SOURCECODE DEPENDS ON MINIMUM 32BIT INTEGERS !!!!
*
******************************************************************************/

/*
  This file is part of ttbin2oth, one of the TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttbin2oth may also be
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

#ifndef __BIN2OTH__
#define __BIN2OTH__

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "tt.h"
#include "strhead.h"

#define TT_MAX_MEMBLOCK 65520
#define TT_MAX_OTHDATA  (TT_MAX_MEMBLOCK - 2 - 7)

//=============================================================================
// returns a pointer to a newly allocated memory block which holds the
// OTH file contents ready to flush into a file
//
// the length of the newly allocated memory block will be stored in outlength
//
// NOTE: the length of the extension may vary between 1 and 4 characters
//       (no checks are performed on the type of the character)
//=============================================================================
unsigned char* DataBuffer2OTHBuffer(int            calctype,
                                    char*          folder,
                                    char*          varname,
                                    char*          extension,
                                    unsigned int   inlength,
                                    unsigned char* indata,
                                    unsigned int*  outlength)
{
    StrHeader*     psh;
    unsigned int   checksum;
    unsigned char* ptr;
    unsigned int   size;
    unsigned char* output;
    int            ext_len;

    if (!extension) {
        fprintf(stderr,"ERROR: no extension given\n");
        return 0;
    }

    ext_len = strlen(extension);
    if (ext_len < 1 || ext_len > 4) {
        fprintf(stderr,"ERROR: invalid OTH extension [%s]\n",extension);
        return 0;
    }


    if (inlength > TT_MAX_OTHDATA) {
        fprintf(stderr,"ERROR: length (%u) exceeds max. user data size (%u)\n",inlength,TT_MAX_OTHDATA);
        return 0;
    }

    size = sizeof(StrHeader)+inlength+2+3+ext_len; // 6 == OTH bytes , 2 == checksum

    if (!(output = (unsigned char*)malloc(size))) {
        fprintf(stderr,"ERROR: cannot allocate %u bytes of memory\n",size);
        return 0;
    }

    *outlength = size;
    psh = (StrHeader*)output;
    memset(psh,0,sizeof(StrHeader));

    //-------------------------------------------------------------------
    // fill up all the static fields
    //-------------------------------------------------------------------
    psh->fill1[0]=1;psh->fill1[1]=0;
    psh->fill2[0]=0x01;psh->fill2[1]=0x00;psh->fill2[2]=0x52;
    psh->fill2[3]=0x00;psh->fill2[4]=0x00;psh->fill2[5]=0x00;
    psh->fill3[0]=0xA5;psh->fill3[1]=0x5A;psh->fill3[2]=0x00;
    psh->fill3[3]=0x00;psh->fill3[4]=0x00;psh->fill3[5]=0x00;
    psh->type[0]=0x1c;psh->type[1]=0x00;psh->type[2]=0x00;psh->type[3]=0x00;

    //-------------------------------------------------------------------
    // fill in the magic marker string depending on given calc type
    //-------------------------------------------------------------------
    if (calctype == CALC_TI89) strncpy(psh->signature,SIGNATURE_TI89,8);
    else                       strncpy(psh->signature,SIGNATURE_TI92P,8);

    //-------------------------------------------------------------------
    // fill in folder and variable name
    // if folder name pointer is NULL, use DEFAULT_FOLDER ("main")
    //-------------------------------------------------------------------
    if (!folder) strncpy(psh->folder,DEFAULT_FOLDER,8);
    else         strncpy(psh->folder,folder,8);

    strncpy(psh->name,varname,8);

    //-------------------------------------------------------------------
    // size holds the complete output size == filelength including header
    //-------------------------------------------------------------------
    psh->size[0] = (unsigned char)(size & 0xff);
    psh->size[1] = (unsigned char)((size >> 8)  & 0xff);
    psh->size[2] = (unsigned char)((size >> 16) & 0xff);
    psh->size[3] = (unsigned char)((size >> 24) & 0xff);

    //-------------------------------------------------------------------
    // data size will hold user data size + OTH tag related bytes
    //-------------------------------------------------------------------
    size -= sizeof(StrHeader);
    size -= 2;
    psh->datasize[0] = (unsigned char)((size >> 8) & 0xff);
    psh->datasize[1] = (unsigned char)(size & 0xff);

    //-------------------------------------------------------------------
    // copy complete indata
    //-------------------------------------------------------------------
    memcpy(output + sizeof(StrHeader),indata,inlength);

    //-------------------------------------------------------------------
    // append OTH bytes
    //-------------------------------------------------------------------
    ptr    = output + sizeof(StrHeader) + inlength;
    *ptr++ = 0;

    switch (ext_len) {
        case 1:
            *ptr++ = *extension;
            *ptr++ = 0;
            break;
        case 2:
            *ptr++ = *extension++;
            *ptr++ = *extension;
            *ptr++ = 0;
            break;
        case 3:
            *ptr++ = *extension++;
            *ptr++ = *extension++;
            *ptr++ = *extension;
            *ptr++ = 0;
            break;
        case 4:
            *ptr++ = *extension++;
            *ptr++ = *extension++;
            *ptr++ = *extension++;
            *ptr++ = *extension;
            *ptr++ = 0;
            break;
    }
    *ptr++ = 0xF8;

    size = *outlength - sizeof(StrHeader);
    ptr  = psh->datasize;

    checksum = 0;
    while (size--) checksum += *ptr++;

    output[*outlength-2] = (unsigned char)(checksum & 0xff);
    output[*outlength-1] = (unsigned char)((checksum >> 8) & 0xff);

    return output;
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
// Revision 1.8  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.7  2002/05/13 15:17:43  tnussb
// static header information fixed (thanx to Sebastian again)
//
// Revision 1.6  2002/05/13 14:09:56  tnussb
// TT_MAX_OTHDATA ... corrected (thanx to Sebastian Reichelt)
//
// Revision 1.5  2002/03/14 09:00:56  tnussb
// checking for define __BIN2OTH__ added at begin of file
//
// Revision 1.4  2002/03/13 22:05:41  tnussb
// handles now between 1 and 4 characters for the extension
//
// Revision 1.3  2002/02/07 09:49:36  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.2  2000/08/23 19:37:14  Thomas Nussbaumer
// changes due to headerfile and define renaming
//
// Revision 1.1  2000/08/23 01:09:44  Thomas Nussbaumer
// initial version (extracted and clearified from ttbin2oth.c)
//
//
//
