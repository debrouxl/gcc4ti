/******************************************************************************
*
* project name:    TI-68k Developer Utilities (formerly TIGCC Tools Suite)
* file name:       ttunpack.h
* initial date:    14/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     defines of errorcodes of decompression routine and its
*                  declaration
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities and ExtGraph.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttunpack may also be
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

#ifndef __TTUNPACK_H__
#define __TTUNPACK_H__

#define ERRPCK_OKAY             0
#define ERRPCK_NOESCFOUND     248
#define ERRPCK_ESCBITS        249
#define ERRPCK_MAXGAMMA       250
#define ERRPCK_EXTRALZP       251
#define ERRPCK_NOMAGIC        252
#define ERRPCK_OUTBUFOVERRUN  253
#define ERRPCK_LZPOSUNDERRUN  254

int _tt_Decompress(unsigned char *src, unsigned char *dest);
#define UnPack _tt_Decompress

#endif


//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.2  2000/08/20 15:26:21  Thomas Nussbaumer
// prefix of unpack routine (_tt_) corrected
//
// Revision 1.1  2000/08/14 22:49:57  Thomas Nussbaumer
// initial version
//
//
