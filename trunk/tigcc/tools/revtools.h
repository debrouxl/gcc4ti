/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       revtools.h
* initial date:    23/08/2000
* author:          thomas.nussbaumer@gmx.net
* description:     macros for automatic handling of version number output
*
* examine one of the pctools source codes to see how it works ;-)
*
******************************************************************************/

/*
  This file is part of TI-68k Developer Utilities.

  This file is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  As a special exception, UNMODIFIED copies of revtools may also be
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

#ifndef __REV_TOOLS_H__
#define __REV_TOOLS_H__

#include <stdio.h>

#define PRINT_ID(name)  {fprintf(stdout,"\n");fprintf(stdout, name" ");\
                         fprintf(stdout,FILE_REVISION);\
                         fprintf(stdout," - TI-68k Developer Utilities v"TTV_MAIN TTV_SUB"\n" \
                                       "(c) thomas.nussbaumer@gmx.net "__DATE__" "__TIME__"\n\n");}



#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
