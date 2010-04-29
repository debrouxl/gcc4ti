/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       tthelp.c
* initial date:    21/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     prints tools list
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of tthelp may also be
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

#include "ttversion.h"
#include "revtools.h"

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.12"


//=============================================================================
// the shortest "tool", but still helpful if you forget things as easy as I do
//=============================================================================
int main(void) {
    PRINT_ID("TTHelp");
    fprintf(USAGE_OUT, "ttarchive   ... archive generation\n"\
                       "ttbin2bin   ... TI89 <-> TI92p binary conversion\n"\
                       "ttbin2hex   ... binary file to C array data\n"\
                       "ttbin2oth   ... binary file to special type calcfile\n"\
                       "ttbin2str   ... binary file to string calcfile\n"\
                       "ttchecksum  ... corrects checksum of 89z/89s/89y/9xz/9xs/9xy files\n"\
                       "[ttdasm]    ... REMOVED: use dasm-tigcc instead\n"\
                       "ttdos2ebk   ... dos text to ebook conversion\n"\
                       "ttebkgen    ... ebook generator\n"\
                       "ttextract   ... extract from binary using start and endtoken\n"\
                       "tthelp      ... prints tool list - this tool\n"\
                       "tthex2bin   ... converts textfile with hex or binary numbers into a binary file\n"\
                       "ttinfo      ... prints infos about 89z/89s/89y/9xz/9xs/9xy files\n"\
                       "ttpack      ... packer\n"\
                       "ttppggen    ... PPG (packed programs) generator\n"\
                       "ttsetname   ... sets the file name that appears on the calculator\n"\
                       "ttsplit     ... splits a file in parts\n"\
                       "ttstrip     ... strips header and trailing checksum from calcfile\n"\
                       "tttiler     ... generates single sprites from a binary image file\n"\
                       "ttunarchive ... extracts entries from TTArchives or list content\n"\
                       "ttunebk     ... exports complete text of ebook to textfile\n"\
                       "ttunpack    ... unpacker\n\n");
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
// Revision 1.12 2009/01/25           Lionel Debroux
// Add ttsetname, remove ttdasm.
// Adapt to new version display (revtools.h).
//
// Revision 1.11 2002/03/19 09:40:02  tnussb
// tthex2bin added
//
// Revision 1.10 2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.9  2002/02/07 09:13:14  tnussb
// changes for Tools Suite Release 1.00
//
// Revision 1.8  2001/04/11 23:01:40  Thomas Nussbaumer
// ttunebk added
//
// Revision 1.7  2001/03/21 21:23:13  Thomas Nussbaumer
// using now USAGE_OUT for help output
//
// Revision 1.6  2001/01/06 10:01:56  Thomas Nussbaumer
// ttchecksum info changed
//
// Revision 1.5  2000/11/26 20:00:35  Thomas Nussbaumer
// help line for tttiler added
//
// Revision 1.4  2000/10/01 15:05:25  Thomas Nussbaumer
// more tools added
//
// Revision 1.3  2000/08/25 18:14:44  Thomas Nussbaumer
// ttebkgen added
//
// Revision 1.2  2000/08/23 19:59:52  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.1  2000/08/21 17:12:27  Thomas Nussbaumer
// initial version
//
//
//
