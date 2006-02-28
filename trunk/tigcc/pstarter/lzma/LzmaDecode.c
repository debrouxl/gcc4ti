/*
  LzmaDecode.c
  LZMA Decoder
  
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

#include "LzmaDecode.h"

#define Byte unsigned char

#define kNumTopBits 24
#define kTopValue ((UInt32)1 << kNumTopBits)

#define kNumBitModelTotalBits 11
#define kBitModelTotal (1 << kNumBitModelTotalBits)
#define kNumMoveBits 5

typedef struct _CRangeDecoder
{
  Byte *Buffer;
  UInt32 Range;
  UInt32 Code;
} CRangeDecoder;

#define ReadByte (*rd->Buffer++)

static inline void RangeDecoderInit(CRangeDecoder *rd,
    Byte *stream)
{
  int i;
  rd->Buffer = stream;
  rd->Code = 0;
  rd->Range = (0xFFFFFFFF);
  for(i = 0; i < 5; i++)
    rd->Code = (rd->Code << 8) | ReadByte;
}

#define RC_INIT_VAR UInt32 range = rd->Range; UInt32 code = rd->Code;        
#define RC_FLUSH_VAR rd->Range = range; rd->Code = code;
#define RC_NORMALIZE if (range < kTopValue) { range <<= 8; code = (code << 8) | ReadByte; }

static inline UInt32 RangeDecoderDecodeDirectBits(CRangeDecoder *rd, int numTotalBits)
{
  RC_INIT_VAR
  UInt32 result = 0;
  int i;
  for (i = numTotalBits; i > 0; i--)
  {
    /* UInt32 t; */
    range >>= 1;

    result <<= 1;
    if (code >= range)
    {
      code -= range;
      result |= 1;
    }
    /*
    t = (code - range) >> 31;
    t &= 1;
    code -= range & (t - 1);
    result = (result + result) | (1 - t);
    */
    RC_NORMALIZE
  }
  RC_FLUSH_VAR
  return result;
}

static __attribute__((regparm)) int RangeDecoderBitDecode(CProb *prob, CRangeDecoder *rd)
{
  UInt32 bound = (rd->Range >> kNumBitModelTotalBits) * *prob;
  if (rd->Code < bound)
  {
    rd->Range = bound;
    *prob += (kBitModelTotal - *prob) >> kNumMoveBits;
    if (rd->Range < kTopValue)
    {
      rd->Code = (rd->Code << 8) | ReadByte;
      rd->Range <<= 8;
    }
    return 0;
  }
  else
  {
    rd->Range -= bound;
    rd->Code -= bound;
    *prob -= (*prob) >> kNumMoveBits;
    if (rd->Range < kTopValue)
    {
      rd->Code = (rd->Code << 8) | ReadByte;
      rd->Range <<= 8;
    }
    return 1;
  }
}

static __attribute__((regparm)) int RangeDecoderBitTreeDecode(CProb *probs, int numLevels, CRangeDecoder *rd)
{
  int mi = 1;
  int i;
  for(i = numLevels; i > 0; i--)
  {
    mi = (mi + mi) + RangeDecoderBitDecode(probs + mi, rd);
  }
  return mi - (1 << numLevels);
}

static inline int RangeDecoderReverseBitTreeDecode(CProb *probs, int numLevels, CRangeDecoder *rd)
{
  int mi = 1;
  int i;
  int symbol = 0;
  for(i = 0; i < numLevels; i++)
  {
    int bit = RangeDecoderBitDecode(probs + mi, rd);
    mi = mi + mi + bit;
    symbol |= (bit << i);
  }
  return symbol;
}

static inline Byte LzmaLiteralDecode(CProb *probs, CRangeDecoder *rd)
{ 
  int symbol = 1;
  do
  {
    symbol = (symbol + symbol) | RangeDecoderBitDecode(probs + symbol, rd);
  }
  while (symbol < 0x100);
  return symbol;
}

static inline Byte LzmaLiteralDecodeMatch(CProb *probs, CRangeDecoder *rd, Byte matchByte)
{ 
  int symbol = 1;
  do
  {
    int bit;
    int matchBit = (matchByte >> 7) & 1;
    matchByte <<= 1;
    bit = RangeDecoderBitDecode(probs + ((1 + matchBit) << 8) + symbol, rd);
    symbol = (symbol << 1) | bit;
    if (matchBit != bit)
    {
      while (symbol < 0x100)
      {
        symbol = (symbol + symbol) | RangeDecoderBitDecode(probs + symbol, rd);
      }
      break;
    }
  }
  while (symbol < 0x100);
  return symbol;
}

#define kNumPosBitsMax 1
#define kNumPosStatesMax (1 << kNumPosBitsMax)

#define kLenNumLowBits 3
#define kLenNumLowSymbols (1 << kLenNumLowBits)
#define kLenNumMidBits 3
#define kLenNumMidSymbols (1 << kLenNumMidBits)
#define kLenNumHighBits 8
#define kLenNumHighSymbols (1 << kLenNumHighBits)

#define LenChoice 0
#define LenChoice2 (LenChoice + 1)
#define LenLow (LenChoice2 + 1)
#define LenMid (LenLow + (kNumPosStatesMax << kLenNumLowBits))
#define LenHigh (LenMid + (kNumPosStatesMax << kLenNumMidBits))
#define kNumLenProbs (LenHigh + kLenNumHighSymbols) 

static __attribute__((regparm)) int LzmaLenDecode(CProb *p, CRangeDecoder *rd, int posState)
{
  if(RangeDecoderBitDecode(p + LenChoice, rd) == 0)
    return RangeDecoderBitTreeDecode(p + LenLow +
        (posState << kLenNumLowBits), kLenNumLowBits, rd);
  if(RangeDecoderBitDecode(p + LenChoice2, rd) == 0)
    return kLenNumLowSymbols + RangeDecoderBitTreeDecode(p + LenMid +
        (posState << kLenNumMidBits), kLenNumMidBits, rd);
  return kLenNumLowSymbols + kLenNumMidSymbols + 
      RangeDecoderBitTreeDecode(p + LenHigh, kLenNumHighBits, rd);
}

#define kNumStates 12

#define kStartPosModelIndex 4
#define kEndPosModelIndex 14
#define kNumFullDistances (1 << (kEndPosModelIndex >> 1))

#define kNumPosSlotBits 6
#define kNumLenToPosStates 4

#define kNumAlignBits 4
#define kAlignTableSize (1 << kNumAlignBits)

#define kMatchMinLen 2

#define IsMatch 0
#define IsRep (IsMatch + (kNumStates << kNumPosBitsMax))
#define IsRepG0 (IsRep + kNumStates)
#define IsRepG1 (IsRepG0 + kNumStates)
#define IsRepG2 (IsRepG1 + kNumStates)
#define IsRep0Long (IsRepG2 + kNumStates)
#define PosSlot (IsRep0Long + (kNumStates << kNumPosBitsMax))
#define SpecPos (PosSlot + (kNumLenToPosStates << kNumPosSlotBits))
#define Align (SpecPos + kNumFullDistances - kEndPosModelIndex)
#define LenCoder (Align + kAlignTableSize)
#define RepLenCoder (LenCoder + kNumLenProbs)
#define Literal (RepLenCoder + kNumLenProbs)

int LzmaDecode(
    unsigned char *inStream,
    unsigned char *outStream, UInt32 outSize)
{
  UInt32 numProbs = Literal + ((UInt32)LZMA_LIT_SIZE);
  CProb p[LZMA_BASE_SIZE+LZMA_LIT_SIZE];
  CRangeDecoder rd;
  UInt32 i;
  int state = 0;
  int previousIsMatch = 0;
  Byte previousByte = 0;
  UInt32 rep0 = 1, rep1 = 1, rep2 = 1, rep3 = 1;
  UInt32 nowPos = 0;
  UInt32 posStateMask = 1;
  int len = 0;
  for (i = 0; i < numProbs; i++)
    p[i] = kBitModelTotal >> 1; 
  RangeDecoderInit(&rd, inStream);

  while(nowPos < outSize)
  {
    int posState = (int)(nowPos & posStateMask);
    if (RangeDecoderBitDecode(p + IsMatch + (state << kNumPosBitsMax) + posState, &rd) == 0)
    {
      CProb *probs = p + Literal;

      if (state < 4) state = 0;
      else if (state < 10) state -= 3;
      else state -= 6;
      if (previousIsMatch)
      {
        Byte matchByte;
        matchByte = outStream[nowPos - rep0];
        previousByte = LzmaLiteralDecodeMatch(probs, &rd, matchByte);
        previousIsMatch = 0;
      }
      else
        previousByte = LzmaLiteralDecode(probs, &rd);
      outStream[nowPos++] = previousByte;
    }
    else             
    {
      previousIsMatch = 1;
      if (RangeDecoderBitDecode(p + IsRep + state, &rd))
      {
        if (RangeDecoderBitDecode(p + IsRepG0 + state, &rd) == 0)
        {
          if (RangeDecoderBitDecode(p + IsRep0Long + (state << kNumPosBitsMax) + posState, &rd) == 0)
          {
            if (nowPos == 0)
              return LZMA_RESULT_DATA_ERROR;
            state = state < 7 ? 9 : 11;
            previousByte = outStream[nowPos - rep0];
            outStream[nowPos++] = previousByte;
            continue;
          }
        }
        else
        {
          UInt32 distance;
          if(RangeDecoderBitDecode(p + IsRepG1 + state, &rd) == 0)
            distance = rep1;
          else 
          {
            if(RangeDecoderBitDecode(p + IsRepG2 + state, &rd) == 0)
              distance = rep2;
            else
            {
              distance = rep3;
              rep3 = rep2;
            }
            rep2 = rep1;
          }
          rep1 = rep0;
          rep0 = distance;
        }
        len = LzmaLenDecode(p + RepLenCoder, &rd, posState);
        state = state < 7 ? 8 : 11;
      }
      else
      {
        int posSlot;
        rep3 = rep2;
        rep2 = rep1;
        rep1 = rep0;
        state = state < 7 ? 7 : 10;
        len = LzmaLenDecode(p + LenCoder, &rd, posState);
        posSlot = RangeDecoderBitTreeDecode(p + PosSlot +
            ((len < kNumLenToPosStates ? len : kNumLenToPosStates - 1) << 
            kNumPosSlotBits), kNumPosSlotBits, &rd);
        if (posSlot >= kStartPosModelIndex)
        {
          int numDirectBits = ((posSlot >> 1) - 1);
          rep0 = ((2 | ((UInt32)posSlot & 1)) << numDirectBits);
          if (posSlot < kEndPosModelIndex)
          {
            rep0 += RangeDecoderReverseBitTreeDecode(
                p + SpecPos + rep0 - posSlot - 1, numDirectBits, &rd);
          }
          else
          {
            rep0 += RangeDecoderDecodeDirectBits(&rd, 
                numDirectBits - kNumAlignBits) << kNumAlignBits;
            rep0 += RangeDecoderReverseBitTreeDecode(p + Align, kNumAlignBits, &rd);
          }
        }
        else
          rep0 = posSlot;
        rep0++;
      }
      if (rep0 == (UInt32)(0))
      {
        /* it's for stream version */
        /* len = -1; */
        break;
      }
      if (rep0 > nowPos)
      {
        return LZMA_RESULT_DATA_ERROR;
      }
      len += kMatchMinLen;
      do
      {
        previousByte = outStream[nowPos - rep0];
        outStream[nowPos++] = previousByte;
        len--;
      }
      while(len > 0 && nowPos < outSize);
    }
  }

  return LZMA_RESULT_OK;
}
