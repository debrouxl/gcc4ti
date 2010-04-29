/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttarchive.h
* initial date:    20/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     structures, definitions and macros to use ttarchive
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

#ifndef __TTARCHIVE_H__
#define __TTARCHIVE_H__

//-----------------------------------------------------------------------------
// entry of archive structure
//-----------------------------------------------------------------------------
typedef struct {
   unsigned char offset[2];  // offset to the entry data (high byte first)
   unsigned char length[2];  // length of entry
   char          name[8];    // entry name
   unsigned char misc1[2];   // info from cfg file (may be queried by a program)
   unsigned char misc2[2];   // info from cfg file (may be queried by a program)
} TTAEntry;


//-----------------------------------------------------------------------------
// smart macros to access ttarchive
//
// pa    ... pointer to archive start address
// idx   ... index of entry
// entry ... pointer to TTAEntry structure
//
// NOTE: No checking is done in the macros !!
//-----------------------------------------------------------------------------

#define IsTTArchive(pa)         (*(pa) == 't' && *((pa)+1) == 't' && *((pa)+2) == 'a' && *((pa)+3) == 0)
#define GetNrEntries(pa)        ((unsigned int)(*((pa)+4) << 8) | (unsigned int)(*((pa)+5)))
#define GetEntryInfo(pa,idx)    (TTAEntry*)((pa) + 6 + sizeof(TTAEntry)*(unsigned int)(idx))
#define GetEntryStart(pa,entry) ((pa) + 6 + (unsigned int)(sizeof(TTAEntry)*GetNrEntries(pa)) + \
                                (((unsigned int)entry->offset[0]) << 8 | (unsigned int)entry->offset[1]))
#define GetEntrySize(entry)     (((unsigned int)entry->length[0]) << 8 | (unsigned int)entry->length[1])

#define TTA_DESC_LENGTH 20

#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.4  2002/02/07 09:35:11  tnussb
// Macro GetEntrySize added (used in ttunarchive)
//
// Revision 1.3  2000/11/18 16:05:08  Thomas Nussbaumer
// bug in GetEntryStart macro fixed
//
// Revision 1.2  2000/08/23 01:05:31  Thomas Nussbaumer
// TTA_DESC_LENGTH added
//
// Revision 1.1  2000/08/20 15:23:18  Thomas Nussbaumer
// initial version
//
//
//
