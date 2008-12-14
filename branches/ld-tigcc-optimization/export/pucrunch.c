/* pucrunch.c: Pucrunch compression code

   Pucrunch 1997-2005 by Pasi 'Albert' Ojala, a1bert@iki.fi
   Copyright (C) 2000 Thomas Nussbaumer
   Copyright (C) 2007 Kevin Kofler

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

#include "../generic.h"
#include "../formats/packhead.h" // compressed header definition
#include "../formats/tios.h"
#include "export.h"
#include "pucrunch.h"

#define VERBOSE_OUT stdout


#ifndef min
#define min(a,b) ((a<b)?(a):(b))
#endif

#if defined(__GNUC__) && __GNUC__ >= 4
#define OFFSETOF __builtin_offsetof
#else
#define OFFSETOF(type,member) ((size_t) &(((type*)0)->member))
#endif


#define LRANGE          (((2<<maxGamma)-3)*256) /* 0..125, 126 -> 1..127 */
#define MAXLZLEN        (2<<maxGamma)
#define MAXRLELEN       (((2<<maxGamma)-2)*256) /* 0..126 -> 1..127 */
#define DEFAULT_LZLEN   LRANGE


static unsigned short *rle, *elr, *lzlen, *lzpos;
static int *length, inlen;
static unsigned char *indata, *mode, *newesc;
static unsigned short *backSkip;


enum MODE {
    LITERAL = 0,
    LZ77    = 1,
    RLE     = 2,
    MMARK   = 4
};

static int lzopt          = 0;
static int maxGamma       = 7;
static int reservedBytes  = 2;
static int escBits        = 2;
static int escMask        = 0xc0;
static int extraLZPosBits = 0;
static int rleUsed        = 31;


static unsigned char rleLen[256];
static int lenValue[256];
static int lrange, maxlzlen, maxrlelen;

static int gainedEscaped = 0;
static int gainedRle = 0, gainedSRle = 0, gainedLRle = 0;
static int gainedLz = 0, gainedRlecode = 0;

static int timesEscaped = 0, timesNormal = 0;
static int timesRle = 0, timesSRle = 0, timesLRle = 0;
static int timesLz = 0;

static int lenStat[8][4];

static unsigned char rleValues[32] = {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int rleHist[256];




#define OUT_SIZE 65536
static unsigned char outBuffer[OUT_SIZE];
static int outPointer = 0;
static int bitMask    = 0x80;


static void TTPackInit(void) {
    int i;

    rleValues[0] = 1;
    for (i=1;i<32;i++) rleValues[i] = 0;

    for (i=0;i<256;i++) {
        rleLen[i]   = 0;
        lenValue[i] = 0;
        rleHist[i]  = 0;
    }

    for (i=0;i<OUT_SIZE;i++) outBuffer[i] = 0;

    lrange = 0, maxlzlen = 0, maxrlelen = 0;


    gainedEscaped = 0;
    gainedRle = 0, gainedSRle = 0, gainedLRle = 0;
    gainedLz = 0, gainedRlecode = 0;

    timesEscaped = 0, timesNormal = 0;
    timesRle = 0, timesSRle = 0, timesLRle = 0;
    timesLz = 0;

    lrange         = 0;
    maxlzlen       = 0;
    maxrlelen      = 0;
    outPointer     = 0;
    bitMask        = 0x80;
    lzopt          = 0;
    maxGamma       = 7;
    reservedBytes  = 2;
    escBits        = 2;
    escMask        = 0xc0;
    extraLZPosBits = 0;
    rleUsed        = 31;
    rle            = NULL;
    elr            = NULL;
    lzlen          = NULL;
    lzpos          = NULL;
    length         = NULL;
    inlen          = 0;
    indata         = NULL;
    mode           = NULL;
    newesc         = NULL;
    backSkip       = NULL;
}



//=============================================================================
// the packing code
//=============================================================================
static int SavePack(unsigned char *data, int size, EXP_FILE *fp, int escape,
                    unsigned char *rleValues, int extraLZPosBits)
{
    int  i;

    if (!data)   return 10;

    /* Save without decompressor */

    PackedHeader   cth;
    RLEEntries     re;

    cth.origsize_lo  = inlen & 0xff;
    cth.origsize_hi  = (inlen >> 8);
    cth.magic1       = MAGIC_CHAR1;
    cth.magic2       = MAGIC_CHAR2;
    cth.compsize_lo  = (size + rleUsed + sizeof(PackedHeader)) & 0xff;
    cth.compsize_hi  = (size + rleUsed + sizeof(PackedHeader)) >> 8;
    cth.esc1         = (escape >> (8-escBits));
    cth.notused3     = 0; // just to make sure it has a defined value
    cth.notused4     = 0; // just to make sure it has a defined value
    cth.esc2         = escBits;
    cth.gamma1       = maxGamma + 1;
    cth.gamma2       = (1 << maxGamma);
    cth.extralz      = extraLZPosBits;
    cth.notused1     = 0; // just to make sure it has a defined value
    cth.notused2     = 0; // just to make sure it has a defined value
    cth.rleentries   = rleUsed;

    for(i=0; i<rleUsed; i++) re.value[i] = rleValues[i+1];

    {
        int packedSize = sizeof(PackedHeader) + cth.rleentries + size + 6;
        ExportWriteTI2(fp, packedSize);                 // write size bytes
        ExportWrite(fp, &cth, 1, sizeof(PackedHeader)); // write header
        ExportWrite(fp, &re,  1, cth.rleentries);       // write rle values
        ExportWrite(fp, data, size, 1);                 // write compressed data

        /* Now fix the on-computer size */
        if (!OutputBin && !OutputBinMainOnly) {
            long position = ExportTell(fp);
            HI4 packedSizeEnc;
            packedSize += 2 + sizeof (TIOS_HOST_FILE_HEADER) + sizeof (TIOS_HOST_FILE_FOOTER);
            WriteHI4(packedSizeEnc, packedSize);
            ExportSeek(fp, OFFSETOF(TIOS_HOST_FILE_HEADER, FileSize));
            fwrite(&packedSizeEnc, 4, 1, fp->File.File);
            ExportSeek(fp, position);
        }
    }

    return 0;
}



//=============================================================================
//
//=============================================================================
static void FlushBits(void) {
    if (bitMask != 0x80) outPointer++;
}


//=============================================================================
//
//=============================================================================
static void PutBit(int bit) {
    if (bit && outPointer < OUT_SIZE) outBuffer[outPointer] |= bitMask;
    bitMask >>= 1;

    if (!bitMask) {
        bitMask = 0x80;
        outPointer++;
    }
}


//=============================================================================
//
//=============================================================================
static void PutValue(int value) {
    int bits = 0, count = 0;

    while (value>1) {
        bits = (bits<<1) | (value & 1); /* is reversed compared to value */
        value >>= 1;
        count++;
        PutBit(1);
    }

    if (count<maxGamma) PutBit(0);

    while (count--) {
        PutBit((bits & 1));     /* output is reversed again -> same as value */
        bits >>= 1;
    }
}



//--------------------------------------------
// why not initializing value lenValue[0] ????
//--------------------------------------------
//=============================================================================
//
//=============================================================================
static void InitValueLen(void) {
    int i;

    // could be heavily optimized, but isn't necessary
    for (i=1; i<256; i++) {
        int count = 0;

        if (i<2)        count = 0;  /* 1       */
        else if (i<4)   count = 1;  /* 2-3     */
        else if (i<8)   count = 2;  /* 4-7     */
        else if (i<16)  count = 3;  /* 8-15    */
        else if (i<32)  count = 4;  /* 16-31   */
        else if (i<64)  count = 5;  /* 32-63   */
        else if (i<128) count = 6;  /* 64-127  */
        else if (i<256) count = 7;  /* 128-255 */

        lenValue[i] = 2*count;
        if (count<maxGamma) lenValue[i]++;
    }
}


#define LenValue(a) (lenValue[a])


//=============================================================================
//
//=============================================================================
static void PutNBits(int byte, int bits) {
    while (bits--)
        PutBit((byte & (1<<bits)));
}




//=============================================================================
//
//=============================================================================
static int OutputNormal(int *esc, unsigned char *data, int newesc) {
    timesNormal++;
    if ((data[0] & escMask) == *esc) {
        PutNBits((*esc>>(8-escBits)), escBits); /* escBits>=0 */
        PutBit(0);
        PutBit(1);
        PutBit(0);

        *esc = newesc;
        PutNBits((*esc>>(8-escBits)), escBits); /* escBits>=0 */
        PutNBits(data[0], 8-escBits);

        gainedEscaped += escBits + 3;
        timesEscaped++;
        return 1;
    }
    PutNBits(data[0], 8);
    return 0;
}


//=============================================================================
//
//=============================================================================
static void OutputEof(int *esc) {
    /* EOF marker */
    PutNBits((*esc>>(8-escBits)), escBits);     /* escBits>=0 */
    PutValue(3);        /* >2 */
    PutValue((2<<maxGamma)-1);  /* Maximum value */
    FlushBits();
}


//=============================================================================
//
//=============================================================================
static void PutRleByte(int data) {
    int index;

    for (index = 1; index < 32; index++) {
        if (data == rleValues[index]) {
            if (index==1)       lenStat[0][3]++;
            else if (index<=3)  lenStat[1][3]++;
            else if (index<=7)  lenStat[2][3]++;
            else if (index<=15) lenStat[3][3]++;
            else if (index<=31) lenStat[4][3]++;

            gainedRlecode += 8 - LenValue(index);

            PutValue(index);
            return;
        }
    }
    PutValue(32 + (data>>3));

    gainedRlecode -= LenValue(32+(data>>3)) + 3;

    PutNBits(data, 3);

    lenStat[5][3]++;
}




//=============================================================================
//
//=============================================================================
static void InitRleLen(void) {
    int i;

    for (i=0; i<256; i++) rleLen[i] = LenValue(32 + 0) + 3;
    for (i=1; i<32; i++) rleLen[rleValues[i]] = LenValue(i);
}

#define LenRleByte(d) (rleLen[d])


//=============================================================================
//
//=============================================================================
static int LenRle(int len, int data) {
    int out = 0;

    do {
        if (len == 1) {
            out += escBits + 3 + 8;
            len = 0;
        }
        else if (len <= (1<<maxGamma)) {
            out += escBits + 3 + LenValue(len-1) + LenRleByte(data);
            len = 0;
        }
        else {
            int tmp = min(len, maxrlelen);
            out += escBits + 3 + maxGamma + 8 +
                        LenValue(((tmp-1)>>8)+1) + LenRleByte(data);
            len -= tmp;
        }
    } while (len);
    return out;
}


//=============================================================================
//
//=============================================================================
static int OutputRle(int *esc, unsigned char *data, int rlelen) {
    int len = rlelen, tmp;

    while (len) {
        if (len >= 2 && len <= (1<<maxGamma)) {
            /* Short RLE */
            if (len==2)        lenStat[0][2]++;
            else if (len<=4)   lenStat[1][2]++;
            else if (len<=8)   lenStat[2][2]++;
            else if (len<=16)  lenStat[3][2]++;
            else if (len<=32)  lenStat[4][2]++;
            else if (len<=64)  lenStat[5][2]++;
            else if (len<=128) lenStat[6][2]++;
            else if (len<=256) lenStat[6][2]++;

            PutNBits((*esc>>(8-escBits)), escBits);     /* escBits>=0 */
            PutBit(0);
            PutBit(1);
            PutBit(1);
            PutValue(len-1);
            PutRleByte(*data);

            tmp = 8*len -escBits -3 -LenValue(len-1) -LenRleByte(*data);
            gainedRle += tmp;
            gainedSRle += tmp;

            timesRle++;
            timesSRle++;
            return 0;
        }
        if (len<3) {
            while (len--)
                OutputNormal(esc, data, *esc);
            return 0;
        }

        if (len <= maxrlelen) {
            /* Run-length encoding */
            PutNBits((*esc>>(8-escBits)), escBits);     /* escBits>=0 */

            PutBit(0);
            PutBit(1);
            PutBit(1);

            PutValue((1<<maxGamma) + (((len-1)&0xff)>>(8-maxGamma)));

            PutNBits((len-1), 8-maxGamma);
            PutValue(((len-1)>>8) + 1);
            PutRleByte(*data);

            tmp = 8*len -escBits -3 -maxGamma -8 -LenValue(((len-1)>>8)+1)
                -LenRleByte(*data);
            gainedRle += tmp;
            gainedLRle += tmp;

            timesRle++;
            timesLRle++;
            return 0;
        }

        /* Run-length encoding */
        PutNBits((*esc>>(8-escBits)), escBits); /* escBits>=0 */

        PutBit(0);
        PutBit(1);
        PutBit(1);

        PutValue((1<<maxGamma) + (((maxrlelen-1)&0xff)>>(8-maxGamma)));

        PutNBits((maxrlelen-1) & 0xff, 8-maxGamma);
        PutValue(((maxrlelen-1)>>8)+1);
        PutRleByte(*data);

        tmp = 8*maxrlelen -escBits -3 -maxGamma -8
            -LenValue(((maxrlelen-1)>>8)+1) -LenRleByte(*data);
        gainedRle += tmp;
        gainedLRle += tmp;
        timesRle++;
        timesLRle++;
        len -= maxrlelen;
        data += maxrlelen;
    }
    return 0;
}


//=============================================================================
//
//=============================================================================
static int LenLz(int lzlen, int lzpos) {
    if (lzlen==2) {
        if (lzpos <= 256) return escBits + 2 + 8;
        else              return 100000;
    }

    return escBits + 8 + extraLZPosBits +
           LenValue(((lzpos-1)>>(8+extraLZPosBits))+1) +
           LenValue(lzlen-1);
}


//=============================================================================
//
//=============================================================================
static int OutputLz(int *esc, int lzlen, int lzpos, int curpos) {
    if (lzlen==2)        lenStat[0][1]++;
    else if (lzlen<=4)   lenStat[1][1]++;
    else if (lzlen<=8)   lenStat[2][1]++;
    else if (lzlen<=16)  lenStat[3][1]++;
    else if (lzlen<=32)  lenStat[4][1]++;
    else if (lzlen<=64)  lenStat[5][1]++;
    else if (lzlen<=128) lenStat[6][1]++;
    else if (lzlen<=256) lenStat[7][1]++;

    if (lzlen >= 2 && lzlen <= maxlzlen) {
        int tmp;

        PutNBits((*esc>>(8-escBits)), escBits); /* escBits>=0 */

        tmp = ((lzpos-1)>>(8+extraLZPosBits))+2;
        if (tmp==2)         lenStat[0][0]++;
        else if (tmp<=4)    lenStat[1][0]++;
        else if (tmp<=8)    lenStat[2][0]++;
        else if (tmp<=16)   lenStat[3][0]++;
        else if (tmp<=32)   lenStat[4][0]++;
        else if (tmp<=64)   lenStat[5][0]++;
        else if (tmp<=128)  lenStat[6][0]++;
        else if (tmp<=256)  lenStat[6][0]++;

        if (lzlen==2) {
            PutValue(lzlen-1);
            PutBit(0);
            if (lzpos > 256) Warning(NULL, "lzpos at %d too long (%d) for lzlen==2\n", curpos, lzpos);
        }
        else {
            PutValue(lzlen-1);
            PutValue( ((lzpos-1) >> (8+extraLZPosBits)) +1);
            PutNBits( ((lzpos-1) >> 8), extraLZPosBits);
        }
        PutNBits(((lzpos-1) & 0xff) ^ 0xff, 8);

        gainedLz += 8*lzlen -LenLz(lzlen, lzpos);
        timesLz++;
        return 3;
    }
    Warning(NULL, "lzlen too short/long (%d)\n", lzlen);
    return lzlen;
}



/* Non-recursive version */
/* NOTE! IMPORTANT! the "length" array length must be inlen+1 */

//=============================================================================
//
//=============================================================================
static int OptimizeLength(int optimize) {
    int i;

    length[inlen] = 0;          /* one off the end, our 'target' */
    for (i=inlen-1; i>=0; i--) {
        int r1 = 8 + length[i+1], r2, r3;

        if (!lzlen[i] && !rle[i]) {
            length[i] = r1;
            mode[i] = LITERAL;
            continue;
        }

        /* If rle>maxlzlen, skip to the start of the rle-maxlzlen.. */
        if (rle[i] > maxlzlen && elr[i] > 1) {
            int z = elr[i];

            i -= elr[i];

            r2 = LenRle(rle[i], indata[i]) + length[i+ rle[i]];
            if (optimize) {
                int ii, mini = rle[i], minv = r2;

                int bot = rle[i] - (1<<maxGamma);
                if (bot < 2)
                    bot = 2;

                for (ii=mini-1; ii>=bot; ii--) {
                    int v = LenRle(ii, indata[i]) + length[i + ii];
                    if (v < minv) {
                        minv = v;
                        mini = ii;
                    }
                }
                if (minv != r2) {
                    lzopt += r2 - minv;
                    rle[i] = mini;
                    r2 = minv;
                }
            }
            length[i] = r2;
            mode[i] = RLE;

            for (; z>=0; z--) {
                length[i+z] = r2;
                mode[i+z] = RLE;
            }
            continue;
        }
        r3 = r2 = r1 + 1000; /* r3 >= r2 > r1 */

        if (rle[i]) {
            r2 = LenRle(rle[i], indata[i]) + length[i+ rle[i]];

            if (optimize) {
                int ii, mini = rle[i], minv = r2;

                /* Check only the original length and all shorter
                   lengths that are power of two.

                   Does not really miss many 'minimums' this way,
                   at least not globally..

                   Makes the assumption that the Elias Gamma Code is
                   used, i.e. values of the form 2^n are 'optimal' */
                ii = 2;
                while (rle[i] > ii) {
                    int v = LenRle(ii, indata[i]) + length[i + ii];
                    if (v < minv) {
                        minv = v;
                        mini = ii;
                    }
                    ii <<= 1;
                }
                if (minv != r2) {
                    lzopt += r2 - minv;
                    rle[i] = mini;
                    r2 = minv;
                }
            }
        }
        if (lzlen[i]) {
            r3 = LenLz(lzlen[i], lzpos[i]) + length[i + lzlen[i]];

            if (optimize && lzlen[i]>2) {
                int ii, mini = lzlen[i], minv = r3;
                int topLen = LenLz(lzlen[i], lzpos[i])
                    - LenValue(lzlen[i]-1);

                /* Check only the original length and all shorter
                   lengths that are power of two.

                   Does not really miss many 'minimums' this way,
                   at least not globally..

                   Makes the assumption that the Elias Gamma Code is
                   used, i.e. values of the form 2^n are 'optimal' */
                ii = 4;
                while (lzlen[i] > ii) {
                    int v = topLen + LenValue(ii-1) + length[i + ii];
                    if (v < minv) {
                        minv = v;
                        mini = ii;
                    }
                    ii <<= 1;
                }
                /*
                  Note:
                  2-byte optimization checks are no longer done
                  with the rest, because the equation gives too long
                  code lengths for 2-byte matches if extraLzPosBits>0.
                  */
                /* Two-byte rescan/check */
                if (backSkip[i] && backSkip[i] <= 256) {
                    /* There are previous occurrances (near enough) */
                    int v = LenLz(2, (int)backSkip[i]) + length[i + 2];

                    if (v < minv) {
                        minv = v;
                        mini = 2;
                        lzlen[i] = mini;
                        r3 = minv;
                        lzpos[i] = (int)backSkip[i];
                    }
                }
                if (minv != r3 && minv < r2) {
                    lzopt += r3 - minv;
                    lzlen[i] = mini;
                    r3 = minv;
                }
            }
        }

        if (r2 <= r1) {
            if (r2 <= r3) {
                length[i] = r2;
                mode[i]   = RLE;
            }
            else {
                length[i] = r3;
                mode[i]   = LZ77;
            }
        }
        else {
            if (r3 <= r1) {
                length[i] = r3;
                mode[i]   = LZ77;
            }
            else {
                length[i] = r1;
                mode[i]   = LITERAL;
            }
        }
    }
    return length[0];
}


/*
    The algorithm in the OptimizeEscape() works as follows:
    1) Only unpacked bytes are processed, they are marked
       with MMARK. We proceed from the end to the beginning.
       Variable A (old/new length) is updated.
    2) At each unpacked byte, one and only one possible
       escape matches. A new escape code must be selected
       for this case. The optimal selection is the one which
       provides the shortest number of escapes to the end
       of the file,
        i.e. A[esc] = 1+min(A[0], A[1], .. A[states-1]).
       For other states A[esc] = A[esc];
       If we change escape in this byte, the new escape is
       the one with the smallest value in A.
    3) The starting escape is selected from the possibilities
       and mode 0 is restored to all mode 3 locations.

 */

//=============================================================================
//
//=============================================================================
static int OptimizeEscape(int *startEscape, int *nonNormal) {
    int  i, j, states = (1<<escBits);
    long minp = 0, minv = 0, other = 0;
    long a[256]; /* needs int/long */
    long b[256]; /* Remembers the # of escaped for each */
    int  esc8 = 8-escBits;

    for (i=0; i<256; i++) b[i] = a[i] = -1;

    if (states>256) {
        Warning(NULL, "Escape optimize: only 256 states (%d)!\n",states);
        return 0;
    }

    /* Mark those bytes that are actually outputted */
    for (i=0; i<inlen; ) {
        switch (mode[i]) {
            case LZ77:
                other++;
                i += lzlen[i];
                break;

            case RLE:
                other++;
                i += rle[i];
                break;

            /*case LITERAL:*/
            default:
                mode[i++] = MMARK; /* mark it used so we can identify it */
                break;
        }
    }

    for (i=inlen-1; i>=0; i--) {
        /* Using a table to skip non-normal bytes does not help.. */
        if (mode[i] == MMARK) {
            int k = (indata[i] >> esc8);

            /* Change the tag values back to normal */
            mode[i] = LITERAL;

            /*
                k are the matching bytes,
                minv is the minimum value,
                minp is the minimum index
             */

            newesc[i] = (minp << esc8);
            a[k] = minv + 1;
            b[k] = b[minp] + 1;
            if (k==minp) {
                /* Minimum changed -> need to find a new minimum */
                /* a[k] may still be the minimum */
                minv++;
                for (k=states-1; k>=0; k--) {
                    if (a[k] < minv) {
                        minv = a[k];
                        minp = k;
                        /*
                            There may be others, but the first one that
                            is smaller than the old minimum is equal to
                            any other new minimum.
                         */
                        break;
                    }
                }
            }
        }
    }

    /* Select the best value for the initial escape */
    if (startEscape) {
        i = inlen;      /* make it big enough */
        for (j=states-1; j>=0; j--) {
            if (a[j] <= i) {
                *startEscape = (j << esc8);
                i = a[j];
            }
        }
    }
    if (nonNormal)
        *nonNormal = other;
    return b[startEscape ? (*startEscape>>esc8) : 0];
}


//=============================================================================
// Initialize the RLE byte code table according to all RLE's found so far O(n)
//=============================================================================
static void InitRle(void) {
    int p, mr, mv, i;

    for (i=1; i<32; i++) {
        mr = -1;
        mv = 0;

        for (p=0; p<256; p++) {
            if (rleHist[p] > mv) {
                mv = rleHist[p];
                mr = p;
            }
        }
        if (mv>0) {
            rleValues[i] = mr;
            rleHist[mr] = -1;
        } else
            break;
    }
    InitRleLen();
}


//=============================================================================
// Initialize the RLE byte code table according to RLE's actually used O(n)
//=============================================================================
static void OptimizeRle(int flags) {
    int p, mr, mv, i;

    if (flags & F_STATS) fprintf(VERBOSE_OUT, "RLE Byte Code Re-Tune, RLE Ranks:\n");

    for (p=0; p<256; p++) rleHist[p] = 0;

    for (p=0; p<inlen; ) {
        switch (mode[p]) {
            case LZ77:
                p += lzlen[p];
                break;

            case RLE:
                rleHist[indata[p]]++;
                p += rle[p];
                break;

            default:
                p++;
                break;
        }
    }

    for (i=1; i<32; i++) {
        mr = -1;
        mv = 0;

        for (p=0; p<256; p++) {
            if (rleHist[p] > mv) {
                mv = rleHist[p];
                mr = p;
            }
        }
        if (mv>0) {
            rleValues[i] = mr;
            if (flags & F_STATS) {
                fprintf(VERBOSE_OUT, " %2d.0x%02x %-3d ", i, mr, mv);
                if (!((i - 1) % 6)) fprintf(VERBOSE_OUT, "\n");
            }
            rleHist[mr] = -1;
        }
        else {
            break;
        }
    }
    rleUsed = i-1;

    if (flags & F_STATS)
        if (((i - 1) % 6)!=1) fprintf(VERBOSE_OUT, "\n");
    InitRleLen();
}


//=============================================================================
//
//=============================================================================
static int PackLz77(int lzsz, int flags, int *startEscape)
{
    int i, j, outlen, p, headerSize;
    int escape;
    unsigned char *hashValue;
    unsigned char *a;
    int k;

    unsigned short *lastPair;
    unsigned short err_occured = 0;

    int rescan = 0;


    if (lzsz < 0 || lzsz > lrange) {
        Warning(NULL, "LZ range must be from 0 to %d (was %d). Set to %d.\n",
                      lrange, lzsz, lrange);
        lzsz = lrange;
    }
    if (lzsz > 65535) {
        Warning(NULL,
                "LZ range must be from 0 to 65535 (was %d). Set to 65535.\n",
                lzsz);
        lzsz = 65535;
    }
    if (!lzsz) Warning(NULL, "Zero LZ range. Only RLE packing used.\n");

    InitRleLen();
    length = (int *)calloc(sizeof(int), inlen + 1);
    mode   = (unsigned char  *)calloc(sizeof(unsigned char),  inlen);
    rle    = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    elr    = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    lzlen  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    lzpos  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    newesc    = (unsigned char  *)calloc(sizeof(unsigned char),  inlen);
    backSkip  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    hashValue = (unsigned char  *)malloc(inlen);
    lastPair  = (unsigned short *)calloc(sizeof(unsigned short), 256*256);


    /* error checking */
    if (!length || !mode || !rle || !elr || !lzlen || !lzpos || !newesc ||
        !lastPair || !backSkip
        || !hashValue)
    {
        Error(NULL, "Memory allocation failed!\n");
        err_occured = 1;
        goto errorexit;
    }

    i = 0;
    j = 0;
    a = indata + inlen;
    for (p=inlen-1; p>=0; p--) {
        k = j;
        j = i;
        i = *--a;       /* Only one read per position */
        hashValue[p] = i*3 + j*5 + k*7; /* 7.95 % */
    }

    /* Detect all RLE and LZ77 jump possibilities */
    for (p=0; p<inlen; p++) {
        if (flags & F_VERBOSE) {
            if (!(p & 2047)) {
                fprintf(VERBOSE_OUT, "\r%d ", p);
                fflush(VERBOSE_OUT);     /* for SAS/C */
            }
        }
        /* check run-length code - must be done, LZ77 search needs it! */
        if (rle[p] <= 0) {
            unsigned char *a = indata + p;
            int val = *a++; /* if this were uchar, it would go to stack..*/
            int top = inlen - p;
            int rlelen = 1;

            /* Loop for the whole RLE */
            while (rlelen<top && *a++ == (unsigned char)val) rlelen++;

            if (rlelen>=2) {
                rleHist[indata[p]]++;

                for (i=rlelen-1; i>=0; i--) {
                    rle[p+i] = rlelen-i;
                    elr[p+i] = i;       /* For RLE backward skipping */
                }

            }
        }

        /* check LZ77 code */
        if (p+rle[p]+1<inlen) {
            int bot = p - lzsz, maxval, maxpos, rlep = rle[p];
            unsigned char hashCompare = hashValue[p];

            /*
                There's always 1 equal byte, although it may
                not be marked as RLE.
             */
            if (rlep <= 0)
                rlep = 1;
            if (bot < 0)
                bot = 0;
            bot += (rlep-1);

            /*
                First get the shortest possible match (if any).
                If there is no 2-byte match, don't look further,
                because there can't be a longer match.
             */
            i = (int)lastPair[ (indata[p]<<8) | indata[p+1] ] -1;
            if (i>=0 && i>=bot) {
                /* Got a 2-byte match at least */
                maxval = 2;
                maxpos = p-i;

                /*
                    A..AB       rlep # of A's, B is something else..

                    Search for bytes that are in p + (rlep-1), i.e.
                    the last rle byte ('A') and the non-matching one
                    ('B'). When found, check if the rle in the compare
                    position (i) is long enough (i.e. the same number
                    of A's at p and i-rlep+1).

                    There are dramatically less matches for AB than for
                    AA, so we get a huge speedup with this approach.
                    We are still guaranteed to find the most recent
                    longest match there is.
                 */

                i = (int)lastPair[(indata[p+(rlep-1)]<<8) | indata[p+rlep]] -1;
                while (i>=bot /* && i>=rlep-1 */) {   /* bot>=rlep-1, i>=bot  ==> i>=rlep-1 */

                    /* Equal number of A's ? */
                    if (!(rlep-1) || rle[i-(rlep-1)]==rlep) {   /* 'head' matches */
                        /* rlep==1 ==> (rlep-1)==0 */
                        /* ivanova.run: 443517 rlep==1,
                           709846 rle[i+1-rlep]==rlep */

                        /*
                            Check the hash values corresponding to the last
                            two bytes of the currently longest match and
                            the first new matching(?) byte. If the hash
                            values don't match, don't bother to check the
                            data itself.
                         */
                        if (
                            hashValue[i+maxval-rlep-1] == hashCompare
                           ) {
                            unsigned char *a = indata + i+2;    /* match  */
                            unsigned char *b = indata + p+rlep-1+2;/* curpos */
                            int topindex = inlen-(p+rlep-1);

                            /* the 2 first bytes ARE the same.. */
                            j = 2;
                            while (j < topindex && *a++==*b++)
                                j++;

                            if (j + rlep-1 > maxval) {
                                int tmplen = j+rlep-1, tmppos = p-i+rlep-1;

                                if (tmplen > maxlzlen)
                                    tmplen = maxlzlen;

                                /* Accept only versions that really are shorter */
                                if (tmplen*8 - LenLz(tmplen, tmppos) >
                                    maxval*8 - LenLz(maxval, maxpos)) {
                                    maxval = tmplen;
                                    maxpos = tmppos;
                                    hashCompare = hashValue[p+maxval-2];
                                }
                                if (maxval == maxlzlen)
                                    break;
                            }
                        }
                    }
                    if (!backSkip[i])
                        break; /* No previous occurrances (near enough) */
                    i -= (int)backSkip[i];
                }

                /*
                    If there is 'A' in the previous position also,
                    RLE-like LZ77 is possible, although rarely
                    shorter than real RLE.
                 */
                if (p && rle[p-1] > maxval) {
                    maxval = rle[p-1] - 1;
                    maxpos = 1;
                }
                /*
                    Last, try to find as long as possible match
                    for the RLE part only.
                 */
                if (maxval < maxlzlen && rlep > maxval) {
                    bot = p - lzsz;
                    if (bot < 0)
                        bot = 0;

                    /* Note: indata[p] == indata[p+1] */
                    i = (int)lastPair[indata[p]*257] -1;
                    while (/* i>= rlep-2 &&*/ i>=bot) {
                        if (elr[i] + 2 > maxval) {
                            maxval = min(elr[i] + 2, rlep);
                            maxpos = p - i + (maxval-2);
                            if(maxval == rlep)
                                break; /* Got enough */
                        }
                        i -= elr[i];
                        if (!backSkip[i])
                            break; /* No previous occurrances (near enough) */
                        i -= (int)backSkip[i];
                    }
                }
                if (p+maxval > inlen) {
                    Warning(NULL, "Error @ %d, lzlen %d, pos %d - exceeds inlen\n",p, maxval, maxpos);
                    maxval = inlen - p;
                }
                if (maxpos<=256 || maxval > 2) {
                    if (maxpos < 0) Warning(NULL, "Error @ %d, lzlen %d, pos %d\n",p, maxval, maxpos);
                    lzlen[p] = (maxval<maxlzlen)?maxval:maxlzlen;
                    lzpos[p] = maxpos;
                }
            }
        }

        /* Update the two-byte history ('hash table') &
           backSkip ('linked list') */
        if (p+1<inlen) {
            int index = (indata[p]<<8) | indata[p+1];
            int ptr = p - (lastPair[index]-1);

            if (ptr > p || ptr > 0xffff)
                ptr = 0;

            backSkip[p] = ptr;
            lastPair[index] = p+1;
        }
    }

    if (flags & F_VERBOSE) {
        fprintf(VERBOSE_OUT, "\rChecked: %d \n", p);
        fflush(VERBOSE_OUT);
    }


    /* Initialize the RLE selections */
    InitRle();

    /* Check the normal bytes / all ratio */
    {
        int mb, mv;

        if (flags & F_VERBOSE) {
            fprintf(VERBOSE_OUT, "Selecting the number of escape bits.. ");
            fflush(VERBOSE_OUT);
        }

        /*
            Absolute maximum number of escaped bytes with
            the escape optimize is 2^-n, where n is the
            number of escape bits used.

            This worst case happens only on equal-
            distributed normal bytes (01230123..).
            This is why the typical values are so much smaller.
         */

        mb = 0;
        mv = 8*OUT_SIZE;
        for (escBits=1; escBits<9; escBits++) {
            int escaped, other = 0, c;

            escMask = (0xff00>>escBits) & 0xff;

            /* Find the optimum path for selected escape bits (no optimize) */
            OptimizeLength(0);

            /* Optimize the escape selections for this path & escBits */
            escaped = OptimizeEscape(&escape, &other);

            /* Compare value: bits lost for escaping -- bits lost for prefix */
            c = (escBits+3)*escaped + other*escBits;
            if (flags & F_STATS) {
                fprintf(VERBOSE_OUT, " %d:%d", escBits, c);
                fflush(VERBOSE_OUT); /* for SAS/C */
            }
            if (c < mv) {
                mb = escBits;
                mv = c;
            } else {
                /* minimum found */
                break;
            }
            if (escBits==4 && (flags & F_STATS)) fprintf(VERBOSE_OUT, "\n");
        }
        if (mb==1) {    /* Minimum was 1, check 0 */
            int escaped;

            escBits = 0;
            escMask = 0;

            /* Find the optimum path for selected escape bits (no optimize) */
            OptimizeLength(0);
            /* Optimize the escape selections for this path & escBits */
            escaped = OptimizeEscape(&escape, NULL);

            if ((flags & F_STATS)) {
                fprintf(VERBOSE_OUT, " %d:%d", escBits, 3*escaped);
                fflush(VERBOSE_OUT); /* for SAS/C */
            }
            if (3*escaped < mv) {
                mb = 0;
                /* mv = 3*escaped; */
            }
        }
        if ((flags & F_STATS)) fprintf(VERBOSE_OUT, "\n");

        if (flags & F_VERBOSE) fprintf(VERBOSE_OUT, "Selected %d-bit escapes\n", mb);
        escBits = mb;
        escMask = (0xff00>>escBits) & 0xff;
    }


    if (flags & F_VERBOSE) {
        fprintf(VERBOSE_OUT, "Optimizing LZ77 and RLE lengths...");
        fflush(VERBOSE_OUT);
    }

    /* Find the optimum path (optimize) */
    OptimizeLength(1);
    if (flags & F_STATS) {
        fprintf(VERBOSE_OUT, " gained %d units.\n", lzopt/8);
    }
    else {
        if (flags & F_VERBOSE) fprintf(VERBOSE_OUT, "\n");
    }

    {
        long lzstat[5] = {0,0,0,0,0}, i, cur = 0, old = extraLZPosBits;

        if (flags & F_VERBOSE) {
            fprintf(VERBOSE_OUT, "Selecting LZPOS LO length.. ");
            fflush(VERBOSE_OUT);
        }

        for (p=0; p<inlen; ) {
            switch (mode[p]) {
            case LZ77:
                extraLZPosBits = 0;
                lzstat[0] += LenLz(lzlen[p], lzpos[p]);
                extraLZPosBits = 1;
                lzstat[1] += LenLz(lzlen[p], lzpos[p]);
                extraLZPosBits = 2;
                lzstat[2] += LenLz(lzlen[p], lzpos[p]);
                extraLZPosBits = 3;
                lzstat[3] += LenLz(lzlen[p], lzpos[p]);
                extraLZPosBits = 4;
                lzstat[4] += LenLz(lzlen[p], lzpos[p]);
                p += lzlen[p];
                break;
            case RLE:
                p += rle[p];
                break;

            default:
                p++;
                break;
            }
        }
        for (i=0; i<5; i++) {
            if (flags & F_STATS) fprintf(VERBOSE_OUT, " %ld:%ld", i + 8, lzstat[i]);

            if (lzstat[i] < lzstat[cur]) cur = i;
        }
        extraLZPosBits = cur;

        if (flags & F_STATS) fprintf(VERBOSE_OUT, "\n");

        if (flags & F_VERBOSE) {
            fprintf(VERBOSE_OUT, "Selected %d-bit LZPOS LO part\n",extraLZPosBits + 8);
            if (cur != old) fprintf(VERBOSE_OUT,"Note: Using option -p%ld you may get better results.\n",cur);
        }
        /* Find the optimum path (optimize) */
        if (extraLZPosBits != old) OptimizeLength(1);
    }
    {
        long stat[4] = {0,0,0,0};

        for (p=0; p<inlen; ) {
            switch (mode[p]) {
            case LZ77:
                if ((lzpos[p] >> 8)+1 > (1<<maxGamma))
                    stat[3]++;
                if (lzlen[p] > (1<<maxGamma))
                    stat[0]++;
                p += lzlen[p];
                break;

            case RLE:
                if (rle[p] > (1<<(maxGamma-1))) {
                    if (rle[p] <= (1<<maxGamma))
                        stat[1]++;

                }
                p += rle[p];
                break;

            default:
                p++;
                break;
            }
        }
        /* TODO: better formula.. */
        if (maxGamma < 7 && stat[0] + stat[1] + stat[3] > 10) {
            if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"Note: Using option -m%d you may get better results.\n",maxGamma+1);
        }
        if (maxGamma > 5 && stat[0] + stat[1] + stat[3] < 4) {
            if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"Note: Using option -m%d you may get better results.\n",maxGamma-1);
        }
    }

    /* Optimize the escape selections */
    OptimizeEscape(&escape, NULL);
    if (startEscape) *startEscape = escape;
    OptimizeRle(flags); /* Retune the RLE selections */

    if (flags & F_VERBOSE) {
        int oldEscape = escape;
        if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"normal RLE  LZLEN LZPOS(absolute)\n\n");

        for (p=0; p<inlen; ) {
            switch (mode[p]) {
            case LZ77:
                mode[p - lzpos[p]] |= MMARK; /* Was referred to by lz77 */
                p += lzlen[p];
                break;
            case RLE:
                p += rle[p];
                break;
        /*  case LITERAL:
            case MMARK:*/
            default:
                p++;
                break;
            }
        }

        j = 0;
        for (p=0; p<inlen; p++) {
            switch (mode[p]) {
            case MMARK | LITERAL:
            case LITERAL:
                if (flags & F_VERBOSE) {
                    if (j==p) fprintf(VERBOSE_OUT,">");
                    else      fprintf(VERBOSE_OUT," ");
                }
                if (j==p) {
                    if (flags & F_VERBOSE) {
                        fprintf(VERBOSE_OUT,"*001*  %03d   %03d  %04x(%04x)  %02x %s %02x",
                                rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                (mode[p] & MMARK)?"#":" ", newesc[p]);
                    }
                    if ((indata[p] & escMask) == escape) {
                        escape = newesc[p];
                        if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"<<");
                    }
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"\n");
                    j += 1;
                } else {
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,"*001*  %03d   %03d  %04x(%04x)  %02x %s %02x\n",
                                                   rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                                   (mode[p] & MMARK)?"#":" ", newesc[p]);
                }
                break;
            case MMARK | LZ77:
            case LZ77:
                if (j==p) {
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,">");
                    j += lzlen[p];
                } else
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT," ");
                if (flags & F_VERBOSE) fprintf(VERBOSE_OUT," 001   %03d  *%03d* %04x(%04x)  %02x %s\n",
                                               rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                               (mode[p] & MMARK)?"#":" ");
                break;
            case MMARK | RLE:
            case RLE:
                if (j==p) {
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,">");
                    j += rle[p];
                } else
                    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT," ");
                if (flags & F_VERBOSE) fprintf(VERBOSE_OUT," 001  *%03d*  %03d  %04x(%04x)  %02x %s\n",
                                               rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                               (mode[p] & MMARK)?"#":" ");
                break;
            default:
                j++;
                break;
            }
            mode[p] &= ~MMARK;
        }
        escape = oldEscape;
    }

    for (p=0; p<inlen; ) {
        switch (mode[p]) {
        case LITERAL: /* normal */
            length[p] = outPointer;

            OutputNormal(&escape, indata+p, newesc[p]);
            p++;
            break;

        case LZ77: /* lz77 */

            /* Not possible for smaller backSkip table
               (the table is overwritten during previous use) */
            /* Re-search matches to get the closest one */
            if (lzopt && lzlen[p] > 2 && lzlen[p] > rle[p]) {
                int bot = p - lzpos[p] + 1, i;
                unsigned short rlep = rle[p];

                if (!rlep)
                    rlep = 1;
                if (bot < 0)
                    bot = 0;
                bot += (rlep-1);

                i = p - (int)backSkip[p];
                while (i>=bot /* && i>=rlep-1 */) {
                    /* Equal number of A's ? */
                    if (rlep==1 || rle[i-rlep+1]==rlep) {       /* 'head' matches */
                        unsigned char *a = indata + i+1;        /* match  */
                        unsigned char *b = indata + p+rlep-1+1; /* curpos */
                        int topindex = inlen-(p+rlep-1);

                        j = 1;
                        while (j < topindex && *a++==*b++)
                            j++;

                        if (j + rlep-1 >= lzlen[p]) {
                            int tmppos = p-i+rlep-1;

                            rescan +=
                                LenLz(lzlen[p], lzpos[p]) -
                                LenLz(lzlen[p], tmppos);
                            lzpos[p] = tmppos;
                            break;
                        }
                    }
                    if (!backSkip[i])
                        break; /* No previous occurrances (near enough) */
                    i -= (int)backSkip[i];
                }
            }

            for (i=0; i<lzlen[p]; i++)
                length[p+i] = outPointer;
            OutputLz(&escape, lzlen[p], lzpos[p], p);
            p += lzlen[p];
            break;

        case RLE: /* rle */
            for (i=0; i<rle[p]; i++)
                length[p+i] = outPointer;
            OutputRle(&escape, indata+p, rle[p]);
            p += rle[p];
            break;

        default: /* Error Flynn :-) */
            p++;
            Warning(NULL, "Internal error: mode %d\n", mode[p]);
            break;
        }
    }
    OutputEof(&escape);

    i = inlen;
    for (p=0; p<inlen; p++) {
        int pos = (inlen - outPointer) + (int)length[p] - p;
        i = min(i, pos);
    }
    if (i<0)
        reservedBytes = -i + 2;
    else
        reservedBytes = 0;

    headerSize = 16 + rleUsed;
    outlen = outPointer + headerSize;   /* unpack code */

    if (flags & F_VERBOSE) fprintf(VERBOSE_OUT, "In: %d, out: %d, ratio: %5.2f%% (%4.2f[%4.2f] b/B)"
                                   ", gained: %5.2f%%\n",
                                   inlen, outlen, (double)outlen*100.0/(double)inlen + 0.005,
                                   8.0*(double)outlen/(double)inlen + 0.005,
                                   8.0*(double)(outlen-headerSize+rleUsed+4)/(double)inlen + 0.005,
                                   100.0 - (double)outlen*100.0/(double)inlen + 0.005);

    if (flags & F_VERBOSE) {
        fprintf(VERBOSE_OUT, "Gained RLE: %d (S+L:%d+%d), LZ: %d, Esc: %d"
                ", Decompressor: %d\n",
                gainedRle/8, gainedSRle/8, gainedLRle/8,
                gainedLz/8, -gainedEscaped/8, -headerSize);

        fprintf(VERBOSE_OUT, "Times  RLE: %d (%d+%d), LZ: %d, Esc: %d (normal: %d)"
                ", %d escape bit%s\n",
                timesRle, timesSRle, timesLRle,
                timesLz, timesEscaped, timesNormal,
                escBits, (escBits==1)?"":"s" );
    }
    if ((flags & F_STATS)) {
        const char *ll[] = {"2", "3-4", "5-8", "9-16", "17-32", "33-64",
                            "65-128", "129-256"};
        fprintf(VERBOSE_OUT, "(Gained by RLE Code: %d, LZPOS LO Bits %d"
                ", maxLen: %d, tag bit/prim. %4.2f)\n",
                gainedRlecode/8 - rleUsed,
                extraLZPosBits + 8,
                (2<<maxGamma),
                (double)((timesRle+timesLz)*escBits +
                         timesEscaped*(escBits + 3))/
                (double)(timesRle+timesLz+timesNormal) + 0.0049);

        fprintf(VERBOSE_OUT, "   LZPOS HI+2 LZLEN S-RLE RLEcode\n");
        fprintf(VERBOSE_OUT, "   ------------------------------\n");
        for (i=0; i<=maxGamma; i++) {
            fprintf(VERBOSE_OUT, "%-7s %5d %5d", ll[i],
                    lenStat[i][0], lenStat[i][1]);
            if (i<maxGamma)
                fprintf(VERBOSE_OUT, " %5d", lenStat[i][2]);
            else
                fprintf(VERBOSE_OUT, "     -");

            if (i<6)
                fprintf(VERBOSE_OUT, "   %5d%s\n", lenStat[i][3], (i==5)?"*":"");
            else
                fprintf(VERBOSE_OUT, "       -\n");
        }
        fprintf(VERBOSE_OUT, "LZ77 rescan gained %d bytes\n", rescan/8);
    }

errorexit:
    if (rle)       free(rle);
    if (elr)       free(elr);
    if (lzlen)     free(lzlen);
    if (lzpos)     free(lzpos);
    if (length)    free(length);
    if (mode)      free(mode);
    if (newesc)    free(newesc);
    if (lastPair)  free(lastPair);
    if (backSkip)  free(backSkip);
    if (hashValue) free(hashValue);
    return err_occured;
}


//=============================================================================
// as usual: the main, but a long one ...
//=============================================================================
int TTPack(int flags, int in_len, unsigned char *in_data, EXP_FILE *out_file) {
    int   startAddr   = 0x258;
    int   lzlen       = -1;
    int   startEscape = 0;
    int   n;

    unsigned long timeused = clock();


    TTPackInit();


    lrange    = LRANGE;
    maxlzlen  = MAXLZLEN;
    maxrlelen = MAXRLELEN;

    InitValueLen();

    if (lzlen == -1) lzlen = DEFAULT_LZLEN;

    inlen  = in_len;
    indata = in_data;


    if (startAddr + inlen -1 > 0xffff) {
        Error(NULL, "File is too large to handle (>64936 Bytes)");
        if (indata) free(indata);
        return 1;
    }

    n = PackLz77(lzlen, flags, &startEscape);

    if (!n) {
        // outBuffer ... static global array (65536 Bytes)

        SavePack(outBuffer, outPointer, out_file, startEscape, rleValues, extraLZPosBits);

        timeused = clock()-timeused;
        if (!timeused) timeused++;
        if (flags & F_VERBOSE) fprintf(VERBOSE_OUT,
                                       "Compressed %d bytes in %4.2f seconds (%4.2f kB/sec)\n",
                                       inlen,
                                       (double)timeused/CLOCKS_PER_SEC,
                                       (double)CLOCKS_PER_SEC*inlen/timeused/1024.0);
        return 0;
    }

    if (indata) free(indata);
    return n;
}
