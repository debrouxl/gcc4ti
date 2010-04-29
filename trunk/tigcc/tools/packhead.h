/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       packhead.h
* initial date:    14/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     header definition of compressed data
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

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

#ifndef __PACKHEAD_H__
#define __PACKHEAD_H__


#define MAGIC_CHAR1     'T'
#define MAGIC_CHAR2     'P'

#define MAX_RLE_ENTRIES 31

// size = 16 bytes
typedef struct {
    unsigned char origsize_lo; // original size lowbyte
    unsigned char origsize_hi; // original size highbyte
    unsigned char magic1;      // must be equal to MAGIC_CHAR1
    unsigned char magic2;      // must be equal to MAGIC_CHAR2
    unsigned char compsize_lo; // compressed size lowbyte
    unsigned char compsize_hi; // compressed size lowbyte
    unsigned char esc1;        // escape >> (8-escBits)
    unsigned char notused3;
    unsigned char notused4;
    unsigned char esc2;        // escBits
    unsigned char gamma1;      // maxGamma + 1
    unsigned char gamma2;      // (1<<maxGamma)
    unsigned char extralz;     // extraLZPosBits
    unsigned char notused1;
    unsigned char notused2;
    unsigned char rleentries;  // rleUsed
} PackedHeader;


#define GetUnPackedSize(p)  (unsigned int)((p)->origsize_lo | ((p)->origsize_hi << 8))
#define IsPacked(p)         ((p)->magic1 == MAGIC_CHAR1 && (p)->magic2 == MAGIC_CHAR2)


typedef struct {
    unsigned char value[MAX_RLE_ENTRIES];
} RLEEntries;


#endif


//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.3  2000/08/20 15:24:28  Thomas Nussbaumer
// macros to get unpacked size and to check if packed added
//
// Revision 1.2  2000/08/16 23:08:55  Thomas Nussbaumer
// magic characters changed to TP ... t(igcc tools) p(acked file)
//
// Revision 1.1  2000/08/14 22:49:57  Thomas Nussbaumer
// initial version
//
//
