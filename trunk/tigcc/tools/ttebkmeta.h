/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttebkmeta.h
* initial date:    25/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     meta tags and control codes definitions
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of ttebkgen may also be
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

#ifndef __TTEBKMETA_H__
#define __TTEBKMETA_H__

#define CCI 0x07  // control code indicator

#define SBCC_FF         0x01  // formfeed
#define SBCC_HR         0x02  // horizontal ruler
#define SBCC_LEFT       0x03  // alignment left
#define SBCC_RIGHT      0x04  // alignment right
#define SBCC_CENTER     0x05  // alignment center

//--------------------------------------------------
// the runlength indicator is NOT a real single
// byte control key !!!
//--------------------------------------------------
#define SBCC_RLI        0xff  // run length indicator

//--------------------------------------------------
// the following codes are just used during
// preprocessing. they will not be in the final
// ebook text
//--------------------------------------------------
#define SBCC_TRANS_ON         0xe0
#define SBCC_TRANS_OFF        0xe1
#define SBCC_REPLACE_ON       0xe2
#define SBCC_REPLACE_OFF      0xe3
#define SBCC_COLLAPSE_ON      0xe4
#define SBCC_COLLAPSE_OFF     0xe5
#define SBCC_DOUBLESPACE_ON   0xe6
#define SBCC_DOUBLESPACE_OFF  0xe7


#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.2  2000/10/01 14:59:22  Thomas Nussbaumer
// generic commit
//
// Revision 1.1  2000/08/27 23:51:17  Thomas Nussbaumer
// initial version
//
//

