/******************************************************************************
*
* project name:    TIGCC Tools Suite
* file name:       revtools.h
* initial date:    23/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     macros for automatic handling of version number output
*                  which is in sync with the CVS version number
*
* examine one of the pctools source codes to see how it works ;-)
*
******************************************************************************/

/*
  This file is part of the TIGCC Tools Suite.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of some TIGCC Tools Suite utilities
  may also be redistributed or sold without source code, for any purpose. (The
  Lesser General Public License restrictions do apply in other respects; for
  example, they cover modification of the program.) Please refer to the main
  source file for the individual utility as to whether this is the case for a
  particular tool. This exception notice must be removed on modified copies of
  this file.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

//-----------------------------------------------------------------------------
// [Usage]
//
// (1) include this file into your tool
//
//
// (2) AFTER (!!) all includes put the following lines into your source code:
//
// #ifdef CVS_FILE_REVISION
// #undef CVS_FILE_REVISION
// #endif
// ----------------------------------------------------------------------------
// DON'T EDIT THE NEXT REVISION BY HAND! THIS IS DONE AUTOMATICALLY BY THE
// CVS SYSTEM !!!
// ----------------------------------------------------------------------------
// #define CVS_FILE_REVISION "$Revision: 1.2 $"
//
// It must be placed AFTER (!!) all includes otherwise the macros may be
// expanded wrong where you use them. For example: if you have added the above
// lines at the top of your file and include afterwards a file which does
// the same, the version of the included file will be used further due to the
// #undef CVS_FILE_REVISION. Everything clear?
//
//
// (3) to output the cvs revision number you can now to a simple:
//
// printf(CVSREV_PRINTPARAMS)
//
// if your file has, for example, the revision 1.3 the following string will
// be printed:  v1.03
//
// the prefix before the subversion is used to get equal-sized output strings
// for mainversions between 1 ... 9 and subversion between 1 .. 99
//
// if your program leaves that range and you need to have equal-sized output
// strings you have to implement it by your own by using the
// CVSREV_MAIN and CVSREV_SUB macros which returns the main and sub versions
// as plain integers. if there is something wrong with your revision string
// CVSREV_MAIN and/or CVSREV_SUB will deliver 0, which is no valid CVS main
// or subversion number.
//
// i will suggest that you use the CVS system. its simple to handle, keeps
// track of your revisions and their history and with the smart macros below
// you haven't to worry anymore about the version output strings of your
// program. if you want a special version number you can force CVS at every
// time to give this version number to your program (0 is not allowed as
// main and subversion number - thats the only pity)
//
// within this tools suite every tool will use the automatic version handling
// but the tool suite version number itself will be handled "by hand".
// this number shouldn't change that quickly as with the tools.
//
//-----------------------------------------------------------------------------
#ifndef __REV_TOOLS_H__
#define __REV_TOOLS_H__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

//-----------------------------------------------------------------------------
// just used internally
//-----------------------------------------------------------------------------
#define CVS_TRUNC_PREFIX    ((strlen((CVS_FILE_REVISION))<=11) ? 0 : (CVS_FILE_REVISION+11))
#define CVS_FIND_COMMA      (strchr(CVS_TRUNC_PREFIX,'.'))
#define CVSREV_MAIN         (int)(!(CVS_TRUNC_PREFIX) ? 0 : atoi(CVS_TRUNC_PREFIX))
#define CVSREV_SUB          (int)(!(CVS_TRUNC_PREFIX) ? 0 : (!(CVS_FIND_COMMA) ? 0 : atoi(CVS_FIND_COMMA+1)))

//-----------------------------------------------------------------------------
// NOTE: THE FOLLOWING MACRO WILL ONLY HANDLE MAIN VERSION < 10 AT CONSTANT
//       LENGTH !!!
//       (subversions from 1 .. 99 are mapped to 01 .. 99)
//
// the following macro may be used to setup a printf(),sprintf() or fprintf()
// call
//-----------------------------------------------------------------------------
#define CVSREV_PRINTPARAMS  "v%d.%02d",CVSREV_MAIN,CVSREV_SUB


#define PRINT_ID(name)  {fprintf(stdout,"\n");fprintf(stdout, name" ");\
                         fprintf(stdout,CVSREV_PRINTPARAMS);\
                         fprintf(stdout," - TIGCC Tools Suite v"TTV_MAIN TTV_SUB"\n" \
                                       "(c) thomas.nussbaumer@gmx.net "__DATE__" "__TIME__"\n\n");}






#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
