/* 
  LzmaDecode.h
  LZMA Decoder interface

  LZMA SDK 4.05 Copyright (c) 1999-2004 Igor Pavlov (2004-08-25)
  Modifications for TIGCC Copyright (C) 2004 Kevin Kofler
  http://www.7-zip.org/

  LZMA SDK is licensed under two licenses:
  1) GNU Lesser General Public License (GNU LGPL)
  2) Common Public License (CPL)
  It means that you can select one of these two licenses and 
  follow rules of that license.

  SPECIAL EXCEPTION:
  Igor Pavlov, as the author of this code, expressly permits you to 
  statically or dynamically link your code (or bind by name) to the 
  interfaces of this file without subjecting your linked code to the 
  terms of the CPL or GNU LGPL. Any modifications or additions 
  to this file, however, are subject to the LGPL or CPL terms.
*/

#ifndef __LZMADECODE_H
#define __LZMADECODE_H

#define UInt32 unsigned long
#define CProb unsigned short

#define LZMA_RESULT_OK 0
#define LZMA_RESULT_DATA_ERROR 1
#define LZMA_RESULT_NOT_ENOUGH_MEM 2

#define LZMA_BASE_SIZE 1062
#define LZMA_LIT_SIZE 768

/* 
bufferSize = (LZMA_BASE_SIZE + (LZMA_LIT_SIZE << (lc + lp)))* sizeof(CProb)
bufferSize += 100 in case of _LZMA_OUT_READ
by default CProb is unsigned short, 
but if specify _LZMA_PROB_32, CProb will be UInt32(unsigned int)
*/

int LzmaDecode(
    unsigned char *inStream,
    unsigned char *outStream, UInt32 outSize);

#endif
