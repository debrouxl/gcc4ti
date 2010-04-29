/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       ttpack.c
* initial date:    14/08/2000
* authors:         albert@cs.tut.fi
*                  thomas.nussbaumer@gmx.net
* description:     packing program
*
* -----------------------------------------------------------------------------
*
* Packing program using the PuCrunch algorithm.
* Based on code from Pasi 'Albert' Ojala, albert@cs.tut.fi
* Heavily reduced to fit to the needs by thomas.nussbaumer@gmx.net
* Pucrunch 1997-2005 by Pasi 'Albert' Ojala, a1bert@iki.fi
* See http://www.cs.tut.fi/~albert/Dev/pucrunch/ for details on the used algorithm
* Pucrunch is under GNU LGPL:
*  See http://creativecommons.org/licenses/LGPL/2.1/ or
*      http://www.gnu.org/copyleft/lesser.html
*
*
*  The decompression code is distributed under the
*  WXWindows Library Licence:
*  See http://www.wxwidgets.org/licence3.txt
*
*  In short: binary version of the decompression code can
*  accompany the compressed data or be used in decompression
*  programs.
******************************************************************************/

#ifndef __TTPACK__
#define __TTPACK__

// if EMBEDDED_USE is defined, than we use this sourcefile from within another
// sourcefile

#ifndef EMBEDDED_USE

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>
#include <time.h>

#include "tt.h"          // generic defines
#include "ttversion.h"   // TI-68k Developer Utilities version info
#include "revtools.h"    // used for id displaying
#include "packhead.h"    // compressed header definition

#ifdef FILE_REVISION
#undef FILE_REVISION
#endif
#define FILE_REVISION "1.9"

//=============================================================================
// outputs usage information of this tool
//=============================================================================
void PrintUsage() {
    PRINT_ID("TTPack");
    fprintf(USAGE_OUT, "Usage: ttpack [-<flags>] <infile> <outfile>\n"               \
                       "       -quiet    don't output standard messages (unsets v)\n"\
                       "       -hti      treat input as hex textinput\n"             \
                       "       -hto      generate hex textoutput\n"                  \
                       "       -fdelta   use delta-lz77 -- shortens some files\n"    \
                       "       e<val>    force escape bits\n"                        \
                       "       r<val>    restrict lz search range\n"                 \
                       "       n         no RLE/LZ length optimization\n"            \
                       "       s         full statistics\n"                          \
                       "       v         verbose (unsets -quiet)\n"                  \
                       "       p<val>    force extralzposbits\n"                     \
                       "       m<val>    max len 5..7 (2*2^5..2*2^7)\n");
}

#endif

#define FIXF_MACHMASK  0xff
#define FIXF_WRAP	   256
#define FIXF_DLZ	   512


#define F_VERBOSE    (1<<0)
#define F_STATS      (1<<1)
#define F_AUTO       (1<<2)
#define F_NOOPT      (1<<3)
#define F_AUTOEX     (1<<4)
#define F_TEXTINPUT  (1<<5)
#define F_TEXTOUTPUT (1<<6)
#define F_NORLE      (1<<9)
#define F_ERROR      (1<<15)

#ifndef min
#define min(a,b) ((a<b)?(a):(b))
#endif


#define LRANGE          (((2<<maxGamma)-3)*256) /* 0..125, 126 -> 1..127 */
#define MAXLZLEN        (2<<maxGamma)
#define MAXRLELEN       (((2<<maxGamma)-2)*256) /* 0..126 -> 1..127 */
#define DEFAULT_LZLEN   LRANGE



unsigned short *rle, *elr, *lzlen, *lzpos;
unsigned short *lzlen2, *lzpos2;
int *length, inlen;
unsigned char *indata, *mode, *newesc;
unsigned short *backSkip;


enum MODE {
    LITERAL = 0,
    LZ77    = 1,
    RLE     = 2,
    DLZ     = 3,
    MMARK   = 4
};

int lzopt          = 0;
int maxGamma       = 7;
int reservedBytes  = 2;
int escBits        = 2;
int escMask        = 0xc0;
int extraLZPosBits = 0;
int rleUsed        = 31;


unsigned char rleLen[256];
int lenValue[256];
int lrange, maxlzlen, maxrlelen;

int gainedEscaped = 0;
int gainedRle = 0, gainedSRle = 0, gainedLRle = 0;
int gainedLz = 0, gainedRlecode = 0;
int gainedDLz = 0, timesDLz = 0;

int timesEscaped = 0, timesNormal = 0;
int timesRle = 0, timesSRle = 0, timesLRle = 0;
int timesLz = 0;

int lenStat[8][4];

unsigned char rleValues[32] = {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                               0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
int rleHist[256];




#define OUT_SIZE 65536
unsigned char outBuffer[OUT_SIZE];
int outPointer = 0;
int bitMask    = 0x80;

int quiet      = 0;


void TTPackInit(void) {
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
    gainedDLz = 0, timesDLz = 0;

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
    lzlen2         = NULL;
    lzpos2         = NULL;
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
int SavePack(int flags,int type, unsigned char *data, int size, char *target,
             int start, int escape, unsigned char *rleValues,
             int endAddr, int extraLZPosBits,int memStart, int memEnd)
{
    FILE *fp = NULL;

    int  i;

    if (!data)   return 10;
    if (!target) fp = stdout;

    if ((type & FIXF_MACHMASK) == 0) {
        /* Save without decompressor */

        if (fp || (fp = fopen(target, "wb"))) {
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

            if (flags & F_TEXTOUTPUT) {
                unsigned int loop;
                unsigned int written = 0;
                for (i=0;i<(int)sizeof(PackedHeader);i++,written++) {
                    fprintf(fp,"0x%02x,",*(((unsigned char*)&cth)+i));
                    if ((!(written % DEFAULT_ITEMS_PER_LINE)) && written) fputc('\n',fp);
                }
                for (i=0;i<cth.rleentries;i++,written++) {
                    fprintf(fp,"0x%02x,",re.value[i]);
                    if (!(written % DEFAULT_ITEMS_PER_LINE)) fputc('\n',fp);
                }
                for (loop=0;loop < (unsigned int)size;loop++,written++) {
                    if (loop < (unsigned int)size - 1)  fprintf(fp,"0x%02x,",data[loop]);
                    else                  fprintf(fp,"0x%02x",data[loop]);
                    if (!(written % DEFAULT_ITEMS_PER_LINE)) fputc('\n',fp);
                }
            }
            else {
                fwrite(&cth, 1, sizeof(PackedHeader), fp); // write header
                fwrite(&re,  1, cth.rleentries, fp);       // write rle values
                fwrite(data, size, 1, fp);                 // write compressed data
                if(fp != stdout) fclose(fp);
            }
            return 0;
        }
        fprintf(stderr, "ERROR: Could not open %s for writing\n", target);
        return 10;
    }

    fprintf(stderr, "FATAL: invalid type!!\n");
    return 10;
}



//=============================================================================
//
//=============================================================================
void FlushBits(void) {
    if (bitMask != 0x80) outPointer++;
}


//=============================================================================
//
//=============================================================================
void PutBit(int bit) {
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
void PutValue(int value) {
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
void InitValueLen() {
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
void PutNBits(int byte, int bits) {
    while (bits--)
        PutBit((byte & (1<<bits)));
}




//=============================================================================
//
//=============================================================================
int OutputNormal(int *esc, unsigned char *data, int newesc) {
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
void OutputEof(int *esc) {
    /* EOF marker */
    PutNBits((*esc>>(8-escBits)), escBits);     /* escBits>=0 */
    PutValue(3);        /* >2 */
    PutValue((2<<maxGamma)-1);  /* Maximum value */
    FlushBits();
}


//=============================================================================
//
//=============================================================================
void PutRleByte(int data) {
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
void InitRleLen() {
    int i;

    for (i=0; i<256; i++) rleLen[i] = LenValue(32 + 0) + 3;
    for (i=1; i<32; i++) rleLen[rleValues[i]] = LenValue(i);
}

#define LenRleByte(d) (rleLen[d])


//=============================================================================
//
//=============================================================================
int LenRle(int len, int data) {
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
int OutputRle(int *esc, unsigned char *data, int rlelen) {
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
int LenLz(int lzlen, int lzpos) {
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
int OutputLz(int *esc, int lzlen, int lzpos, char *data, int curpos) {
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
            if (lzpos > 256) fprintf(stderr,"ERROR at %d: lzpos too long (%d) for lzlen==2\n",curpos, lzpos);
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
    fprintf(stderr, "ERROR: lzlen too short/long (%d)\n", lzlen);
    return lzlen;
}



/* Non-recursive version */
/* NOTE! IMPORTANT! the "length" array length must be inlen+1 */

//=============================================================================
//
//=============================================================================
int OptimizeLength(int optimize) {
    int i;

    length[inlen] = 0;          /* one off the end, our 'target' */
    for (i=inlen-1; i>=0; i--) {
        int r1 = 8 + length[i+1], r2, r3;

        if (!lzlen[i] && !rle[i] && (!lzlen2 || !lzlen2[i])) {
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
        if (lzlen2 && lzlen2[i] > 3) {
            r3 = escBits + 2*maxGamma + 16 + LenValue(lzlen2[i]-1) + length[i + lzlen2[i]];
            //r3 = LenDLz(lzlen2[i], lzpos2[i]) + length[i + lzlen2[i]];
            if (r3 < length[i]) {
                length[i] = r3;
                mode[i]   = DLZ;
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
int OptimizeEscape(int *startEscape, int *nonNormal) {
    int  i, j, states = (1<<escBits);
    long minp = 0, minv = 0, other = 0;
    long a[256]; /* needs int/long */
    long b[256]; /* Remembers the # of escaped for each */
    int  esc8 = 8-escBits;

    for (i=0; i<256; i++) b[i] = a[i] = -1;

    if (states>256) {
        fprintf(stderr, "Escape optimize: only 256 states (%d)!\n",states);
        return 0;
    }

    /* Mark those bytes that are actually outputted */
    for (i=0; i<inlen; ) {
        switch (mode[i]) {
            case DLZ:
                other++;
                i += lzlen2[i];
                break;

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
void InitRle(int flags) {
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
void OptimizeRle(int flags) {
    int p, mr, mv, i;

    if ((flags & F_NORLE)) {
        rleUsed = 0;
        return;
    }
    if (flags & F_STATS) fprintf(stderr, "RLE Byte Code Re-Tune, RLE Ranks:\n");

    for (p=0; p<256; p++) rleHist[p] = 0;

    for (p=0; p<inlen; ) {
        switch (mode[p]) {
            case DLZ:
                p += lzlen2[p];
                break;
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
                fprintf(stderr, " %2d.0x%02x %-3d ", i, mr, mv);
                if (!((i - 1) % 6)) fprintf(stderr, "\n");
            }
            rleHist[mr] = -1;
        }
        else {
            break;
        }
    }
    rleUsed = i-1;

    if (flags & F_STATS)
        if (((i - 1) % 6)!=1) fprintf(stderr, "\n");
    InitRleLen();
}


//=============================================================================
//
//=============================================================================
int PackLz77(int lzsz, int flags, int *startEscape,int endAddr, int memEnd, int type)
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
        fprintf(stderr, "LZ range must be from 0 to %d (was %d). Set to %d.\n",
                lrange, lzsz, lrange);
        lzsz = lrange;
    }
    if (lzsz > 65535) {
        fprintf(stderr,
                "LZ range must be from 0 to 65535 (was %d). Set to 65535.\n",
                lzsz);
        lzsz = 65535;
    }
    if (!lzsz) fprintf(stderr, "Warning: zero LZ range. Only RLE packing used.\n");

    InitRleLen();
    length = (int *)calloc(sizeof(int), inlen + 1);
    mode   = (unsigned char  *)calloc(sizeof(unsigned char),  inlen);
    rle    = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    elr    = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    lzlen  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    lzpos  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    if ((type & FIXF_DLZ)) {
        lzlen2  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
        lzpos2  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    }
    else {
        lzlen2 = lzpos2 = NULL;
    }
    newesc    = (unsigned char  *)calloc(sizeof(unsigned char),  inlen);
    backSkip  = (unsigned short *)calloc(sizeof(unsigned short), inlen);
    hashValue = (unsigned char  *)malloc(inlen);
    lastPair  = (unsigned short *)calloc(sizeof(unsigned short), 256*256);


    /* error checking */
    if (!length || !mode || !rle || !elr || !lzlen || !lzpos || !newesc ||
        !lastPair || !backSkip
        || ((type & FIXF_DLZ) && (!lzlen2 || !lzpos2))
        || !hashValue)
    {
        fprintf(stderr, "ERROR: Memory allocation failed!\n");
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
                fprintf(stderr, "\r%d ", p);
                fflush(stderr);     /* for SAS/C */
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
                    fprintf(stderr,"Error @ %d, lzlen %d, pos %d - exceeds inlen\n",p, maxval, maxpos);
                    maxval = inlen - p;
                }
                if (maxpos<=256 || maxval > 2) {
                    if (maxpos < 0) fprintf(stderr, "Error @ %d, lzlen %d, pos %d\n",p, maxval, maxpos);
                    lzlen[p] = (maxval<maxlzlen)?maxval:maxlzlen;
                    lzpos[p] = maxpos;
                }
            }
        }

        /* check LZ77 code again, ROT1..255 */
        if ((type & FIXF_DLZ) && p+rle[p]+1<inlen) {
        int rot;

        for (rot = 1; rot < 255; rot++) {
            int bot = p - /*lzsz*/256, maxval, maxpos, rlep = rle[p];
            unsigned char valueCompare = (indata[p+2] + rot) & 0xff;

            if (rlep <= 0) rlep = 1;
            if (bot < 0)   bot = 0;
            bot += (rlep-1);

            i = (int)lastPair[ (((indata[p] + rot) & 0xff)<<8) |
                                ((indata[p+1] + rot) & 0xff) ] -1;
            if (i>=0 && i>=bot) {
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

                i = (int)lastPair[(((indata[p+(rlep-1)] + rot) & 0xff)<<8) |
                                   ((indata[p+rlep] + rot) & 0xff)] -1;
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
                        if (indata[i+maxval-rlep+1] == valueCompare) {
                            unsigned char *a = indata + i+2;    /* match  */
                            unsigned char *b = indata + p+rlep-1+2;/* curpos */
                            int topindex = inlen-(p+rlep-1);

                            /* the 2 first bytes ARE the same.. */
                            j = 2;
                            while (j < topindex && *a++==((*b++ + rot) & 0xff))
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

                                    valueCompare = (indata[p+maxval] + rot) & 0xff;
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

                if (p+maxval > inlen) {
                    fprintf(stderr,"Error @ %d, lzlen %d, pos %d - exceeds inlen\n",p, maxval, maxpos);
                    maxval = inlen - p;
                }
                if (maxval > 3 && maxpos <= 256 &&
                    (maxval > lzlen2[p] ||
                     (maxval == lzlen2[p] && maxpos < lzpos2[p]))) {
                    if (maxpos < 0)
                        fprintf(stderr, "Error @ %d, lzlen %d, pos %d\n",p, maxval, maxpos);
                    lzlen2[p] = (maxval<maxlzlen)?maxval:maxlzlen;
                    lzpos2[p] = maxpos;
                }
            }
        }
        if (lzlen2[p] <= lzlen[p] || lzlen2[p] <= rle[p]) {
            lzlen2[p] = lzpos2[p] = 0;
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
    if ((flags & F_NORLE)) {
        for (p=1; p<inlen; p++) {
            if (rle[p-1]-1 > lzlen[p]) {
                lzlen[p] = (rle[p]<maxlzlen)?rle[p]:maxlzlen;
                lzpos[p] = 1;
            }
        }
        for (p=0; p<inlen; p++) {
            rle[p] = 0;
        }
    }

    if (flags & F_VERBOSE) {
        fprintf(stderr, "\rChecked: %d \n", p);
        fflush(stderr);
    }


    /* Initialize the RLE selections */
    InitRle(flags);

    /* Check the normal bytes / all ratio */
    if ((flags & F_AUTO)) {
        int mb, mv;

        if (flags & F_VERBOSE) {
            fprintf(stderr, "Selecting the number of escape bits.. ");
            fflush(stderr);
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
                fprintf(stderr, " %d:%d", escBits, c);
                fflush(stderr); /* for SAS/C */
            }
            if (c < mv) {
                mb = escBits;
                mv = c;
            } else {
                /* minimum found */
                break;
            }
            if (escBits==4 && (flags & F_STATS)) fprintf(stderr, "\n");
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
                fprintf(stderr, " %d:%d", escBits, 3*escaped);
                fflush(stderr); /* for SAS/C */
            }
            if (3*escaped < mv) {
                mb = 0;
                /* mv = 3*escaped; */
            }
        }
        if ((flags & F_STATS)) fprintf(stderr, "\n");

        if (flags & F_VERBOSE) fprintf(stderr, "Selected %d-bit escapes\n", mb);
        escBits = mb;
        escMask = (0xff00>>escBits) & 0xff;
    }


    if (!(flags & F_NOOPT)) {
        if (flags & F_VERBOSE) {
            fprintf(stderr, "Optimizing LZ77 and RLE lengths...");
            fflush(stderr);
        }
    }

    /* Find the optimum path (optimize) */
    OptimizeLength((flags & F_NOOPT)?0:1);
    if (flags & F_STATS) {
        if (!(flags & F_NOOPT)) fprintf(stderr, " gained %d units.\n", lzopt/8);
    }
    else {
        if (flags & F_VERBOSE) fprintf(stderr, "\n");
    }

    if (1 || (flags & F_AUTOEX)) {
        long lzstat[5] = {0,0,0,0,0}, i, cur = 0, old = extraLZPosBits;

        if (flags & F_VERBOSE) {
            fprintf(stderr, "Selecting LZPOS LO length.. ");
            fflush(stderr);
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
            case DLZ:
                p += lzlen2[p];
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
            if (flags & F_STATS) fprintf(stderr, " %ld:%ld", i + 8, lzstat[i]);

            if (lzstat[i] < lzstat[cur]) cur = i;
        }
        extraLZPosBits = (flags & F_AUTOEX)?cur:old;

        if (flags & F_STATS) fprintf(stderr, "\n");

        if (flags & F_VERBOSE) {
            fprintf(stderr, "Selected %d-bit LZPOS LO part\n",extraLZPosBits + 8);
            if (cur != old) fprintf(stderr,"Note: Using option -p%ld you may get better results.\n",cur);
        }
        /* Find the optimum path (optimize) */
        if (extraLZPosBits != old) OptimizeLength((flags & F_NOOPT)?0:1);
    }
    if (1) {
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
            case DLZ:
                p += lzlen2[p];
                break;
            default:
                p++;
                break;
            }
        }
        /* TODO: better formula.. */
        if (maxGamma < 7 && stat[0] + stat[1] + stat[3] > 10) {
            if (flags & F_VERBOSE) fprintf(stderr,"Note: Using option -m%d you may get better results.\n",maxGamma+1);
        }
        if (maxGamma > 5 && stat[0] + stat[1] + stat[3] < 4) {
            if (flags & F_VERBOSE) fprintf(stderr,"Note: Using option -m%d you may get better results.\n",maxGamma-1);
        }
    }

    /* Optimize the escape selections */
    OptimizeEscape(&escape, NULL);
    if (startEscape) *startEscape = escape;
    OptimizeRle(flags); /* Retune the RLE selections */

    if (flags & F_VERBOSE) {
        int oldEscape = escape;
        if (flags & F_VERBOSE) printf("normal RLE  LZLEN LZPOS(absolute)\n\n");

        for (p=0; p<inlen; ) {
            switch (mode[p]) {
            case LZ77:
                mode[p - lzpos[p]] |= MMARK; /* Was referred to by lz77 */
                p += lzlen[p];
                break;
            case RLE:
                p += rle[p];
                break;
            case DLZ:
                mode[p - lzpos2[p]] |= MMARK; /* Was referred to by lz77 */
                p += lzlen2[p];
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
            case MMARK | DLZ:
            case DLZ:
                if (j==p) {
                    if (flags & F_VERBOSE) printf(">");
                    j += lzlen2[p];
                } else
                    printf(" ");
                if (lzpos2) {
                    if (flags & F_VERBOSE) printf(" %04x*%03d*+%02x", lzpos2[p], lzlen2[p],(indata[p] - indata[p-lzpos2[p]]) & 0xff);
                }
                if (flags & F_VERBOSE) printf(" 001   %03d   %03d  %04x(%04x)  %02x %s\n",
                                              rle[p],lzlen[p],lzpos[p],p-lzpos[p],indata[p],
                                              (mode[p] & MMARK)?"#":" ");
                break;
            case MMARK | LITERAL:
            case LITERAL:
                if (flags & F_VERBOSE) {
                    if (j==p) printf(">");
                    else      printf(" ");

                    if (lzpos2) {
                        printf(" %04x %03d +%02x", lzpos2[p], lzlen2[p],
                               (indata[p] - indata[p-lzpos2[p]]) & 0xff);
                    }
                }
                if (j==p) {
                    if (flags & F_VERBOSE) {
                        printf("*001*  %03d   %03d  %04x(%04x)  %02x %s %02x",
                               rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                               (mode[p] & MMARK)?"#":" ", newesc[p]);
                    }
                    if ((indata[p] & escMask) == escape) {
                        escape = newesc[p];
                        if (flags & F_VERBOSE) printf("«");
                    }
                    if (flags & F_VERBOSE) printf("\n");
                    j += 1;
                } else {
                    if (flags & F_VERBOSE) printf("*001*  %03d   %03d  %04x(%04x)  %02x %s %02x\n",
                                                  rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                                  (mode[p] & MMARK)?"#":" ", newesc[p]);
                }
                break;
            case MMARK | LZ77:
            case LZ77:
                if (j==p) {
                    if (flags & F_VERBOSE) printf(">");
                    j += lzlen[p];
                } else
                    if (flags & F_VERBOSE) printf(" ");
                if (lzpos2) {
                    if (flags & F_VERBOSE) printf(" %04x %03d +%02x", lzpos2[p], lzlen2[p],
                                                  (indata[p] - indata[p-lzpos2[p]]) & 0xff);
                }
                if (flags & F_VERBOSE) printf(" 001   %03d  *%03d* %04x(%04x)  %02x %s\n",
                                              rle[p], lzlen[p], lzpos[p], p-lzpos[p], indata[p],
                                              (mode[p] & MMARK)?"#":" ");
                break;
            case MMARK | RLE:
            case RLE:
                if (j==p) {
                    if (flags & F_VERBOSE) printf(">");
                    j += rle[p];
                } else
                    if (flags & F_VERBOSE) printf(" ");
                if (lzpos2) {
                    if (flags & F_VERBOSE) printf(" %04x %03d +%02x", lzpos2[p], lzlen2[p],
                                                  (indata[p] - indata[p-lzpos2[p]]) & 0xff);
                }
                if (flags & F_VERBOSE) printf(" 001  *%03d*  %03d  %04x(%04x)  %02x %s\n",
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

        case DLZ:
            for (i=0; i<lzlen2[p]; i++) length[p+i] = outPointer;

            PutNBits((escape>>(8-escBits)), escBits);
            PutValue(lzlen2[p]-1);
            PutValue((2<<maxGamma)-1);
            PutNBits((indata[p] - indata[p-lzpos2[p]]) & 0xff, 8);
            PutNBits(((lzpos2[p]-1) & 0xff) ^ 0xff, 8);
            gainedDLz += 8*lzlen2[p] -(escBits + LenValue(lzlen2[p]-1) + 2*maxGamma + 16);
            timesDLz++;
            p += lzlen2[p];
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
            OutputLz(&escape, lzlen[p], lzpos[p], (char *)(indata+p-lzpos[p]), p);
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
            fprintf(stderr, "Internal error: mode %d\n", mode[p]);
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

    // TOMTOM !!!

    if ((type & FIXF_MACHMASK) == 0) {
        headerSize = 16 + rleUsed;
    } else
    {
        if (endAddr + reservedBytes + 3 > memEnd) {
            type |= FIXF_WRAP;
        } else {
            type &= ~FIXF_WRAP;
        }
        headerSize = 47 + rleUsed - 31;
    }
    outlen = outPointer + headerSize;   /* unpack code */

    if (flags & F_VERBOSE) fprintf(stderr, "In: %d, out: %d, ratio: %5.2f%% (%4.2f[%4.2f] b/B)"
                                   ", gained: %5.2f%%\n",
                                   inlen, outlen, (double)outlen*100.0/(double)inlen + 0.005,
                                   8.0*(double)outlen/(double)inlen + 0.005,
                                   8.0*(double)(outlen-headerSize+rleUsed+4)/(double)inlen + 0.005,
                                   100.0 - (double)outlen*100.0/(double)inlen + 0.005);

    if ((type & FIXF_DLZ)) {
        if (flags & F_VERBOSE) {
            fprintf(stderr, "Gained RLE: %d (S+L:%d+%d), DLZ: %d, LZ: %d, Esc: %d"
                    ", Decompressor: %d\n",
                    gainedRle/8, gainedSRle/8, gainedLRle/8, gainedDLz/8,
                    gainedLz/8, -gainedEscaped/8, -headerSize);

            fprintf(stderr, "Times  RLE: %d (%d+%d), DLZ: %d, LZ: %d, Esc: %d (normal: %d)"
                    ", %d escape bit%s\n",
                    timesRle, timesSRle, timesLRle, timesDLz,
                    timesLz, timesEscaped, timesNormal,
                    escBits, (escBits==1)?"":"s" );
        }
    }
    else {
        if (flags & F_VERBOSE) {
            fprintf(stderr, "Gained RLE: %d (S+L:%d+%d), LZ: %d, Esc: %d"
                    ", Decompressor: %d\n",
                    gainedRle/8, gainedSRle/8, gainedLRle/8,
                    gainedLz/8, -gainedEscaped/8, -headerSize);

            fprintf(stderr, "Times  RLE: %d (%d+%d), LZ: %d, Esc: %d (normal: %d)"
                    ", %d escape bit%s\n",
                    timesRle, timesSRle, timesLRle,
                    timesLz, timesEscaped, timesNormal,
                    escBits, (escBits==1)?"":"s" );
        }
    }
    if ((flags & F_STATS)) {
        char *ll[] = {"2", "3-4", "5-8", "9-16", "17-32", "33-64",
                            "65-128", "129-256"};
        fprintf(stderr, "(Gained by RLE Code: %d, LZPOS LO Bits %d"
                ", maxLen: %d, tag bit/prim. %4.2f)\n",
                gainedRlecode/8 - rleUsed,
                extraLZPosBits + 8,
                (2<<maxGamma),
                (double)((timesRle+timesLz)*escBits +
                         timesEscaped*(escBits + 3))/
                (double)(timesRle+timesLz+timesNormal) + 0.0049);

        fprintf(stderr, "   LZPOS HI+2 LZLEN S-RLE RLEcode\n");
        fprintf(stderr, "   ------------------------------\n");
        for (i=0; i<=maxGamma; i++) {
            fprintf(stderr, "%-7s %5d %5d", ll[i],
                    lenStat[i][0], lenStat[i][1]);
            if (i<maxGamma)
                fprintf(stderr, " %5d", lenStat[i][2]);
            else
                fprintf(stderr, "     -");

            if (i<6)
                fprintf(stderr, "   %5d%s\n", lenStat[i][3], (i==5)?"*":"");
            else
                fprintf(stderr, "       -\n");
        }
        fprintf(stderr, "LZ77 rescan gained %d bytes\n", rescan/8);
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

#define NO_HEX_CHARACTER       255

//=============================================================================
// converts hexdigit to number
//=============================================================================
unsigned char hex2int(unsigned char c) {
    c = tolower(c);

    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= '0' && c <= '9') return c - '0';
    return NO_HEX_CHARACTER;
}


//=============================================================================
// converts hex text into binary
//=============================================================================
int ConvertText2Bin(unsigned char* ib,int origlen) {
    int           pos;
    int           cnt = 0;
    int           searchforendofline = 0;
    int           len_after_convert  = 0;
    unsigned char val = 0;
    unsigned char actual;

    len_after_convert = 0;
    for (pos = 0; pos < origlen;pos++) {
        if (searchforendofline) {
            if (ib[pos] == '\n') searchforendofline = 0;
            continue;
        }
        if (ib[pos] == '/') {
            cnt = 0;
            if (pos < origlen-1 && ib[pos+1] == '/') searchforendofline = 1;
            continue;
        }

        actual = hex2int(ib[pos]);
        if (actual == NO_HEX_CHARACTER) {
            cnt = 0;
            continue;
        }
        if (cnt == 0) {
            val = actual*16;
            cnt++;
        }
        else {
            val += actual;
            cnt=0;
            ib[len_after_convert++] = val;
        }
    }
    return(len_after_convert);
}


//=============================================================================
// as usual: the main, but a long one ...
//=============================================================================
#ifndef EMBEDDED_USE
int main(int argc, char *argv[]) {
#else
int TTPack(int argc,char *argv[]) {
#endif
    int   startAddr   = 0x258;
    int   flags       = F_AUTO | F_AUTOEX;
    int   lzlen       = -1;
    int   buflen;
    int   newlen;
    int   startEscape;
    int   n;
    char *fileIn  = NULL;
    char *fileOut = NULL;
    FILE *infp;

    unsigned long timeused = clock();

    int   memStart    = 0x801;
    int   memEnd      = 0x10000;
    int   type        = 0;

    quiet = 0;


    TTPackInit();


    lrange    = LRANGE;
    maxlzlen  = MAXLZLEN;
    maxrlelen = MAXRLELEN;

    InitValueLen();

    for (n=1; n<argc; n++) {
        if (!strcmp(argv[n], "-fnorle"))      flags |= F_NORLE;
        else if (!strcmp(argv[n], "-fdelta")) type  |= FIXF_DLZ;
        else if (!strcmp(argv[n], "-hti"))    flags |= F_TEXTINPUT;
        else if (!strcmp(argv[n], "-hto"))    flags |= F_TEXTOUTPUT;
        else if (!strcmp(argv[n], "-quiet"))  flags &= ~F_VERBOSE, quiet = 1;

        else if (argv[n][0]=='-' || argv[n][0]=='/') {
            int i = 1;
            char *val, *tmp, c;
            long tmpval;

            while (argv[n][i]) {
                switch (argv[n][i]) {

                case 'n':       /* noopt, no rle/lzlen optimization */
                    flags |= F_NOOPT;
                    break;

                case 's':
                    flags |= F_STATS;
                    break;

                case 'v':
                    flags |= F_VERBOSE;
                    quiet = 0;
                    break;

                case 'h':
                case '?':
                    flags |= F_ERROR;
                    break;

                case 'r':
                case 'm':
                case 'e':
                case 'p':
                    c = argv[n][i]; /* Remember the option */
                    if (argv[n][i+1]) {
                        val = argv[n]+i+1;
                    } else if (n+1 < argc) {
                        val = argv[n+1];
                        n++;
                    } else {
                        flags |= F_ERROR;
                        break;
                    }

                    i = strlen(argv[n])-1;
                    if (*val=='$') tmpval = strtol(val+1, &tmp, 16);
                    else           tmpval = strtol(val, &tmp, 0);

                    if (*tmp) {
                        fprintf(stderr,"ERROR: invalid number: \"%s\"\n", val);
                        flags |= F_ERROR;
                        break;
                    }

                    switch (c) {
                        case 'r':
                            lzlen = tmpval;
                            break;
                        case 'm':
                            maxGamma = tmpval;
                            if (maxGamma < 5 || maxGamma > 7) {
                                fprintf(stderr, "ERROR: Max length must be 5..7!\n");
                                flags |= F_ERROR;
                                maxGamma = 7;
                            }
                            lrange = LRANGE;
                            maxlzlen = MAXLZLEN;
                            maxrlelen = MAXRLELEN;
                            InitValueLen();
                            break;
                        case 'e':
                            escBits = tmpval;
                            if (escBits < 0 || escBits > 8) {
                                fprintf(stderr, "ERROR: Escape bits must be 0..8!\n");
                                flags |= F_ERROR;
                            }
                            else flags &= ~F_AUTO;
                            escMask = (0xff00>>escBits) & 0xff;
                            break;
                        case 'p':
                            extraLZPosBits = tmpval;
                            if (extraLZPosBits < 0 || extraLZPosBits > 4) {
                                fprintf(stderr,"ERROR: Extra LZ-pos bits must be 0..4!\n");
                                flags |= F_ERROR;
                            }
                            else flags &= ~F_AUTOEX;
                            break;
                    }
                    break;

                default:
                    fprintf(stderr, "ERROR: Unknown option \"%c\"\n",argv[n][i]);
                    flags |= F_ERROR;
                }
                i++;
            }
        } else {
            if (!fileIn) {
                fileIn = argv[n];
            }
            else if (!fileOut) {
                fileOut = argv[n];
            }
            else {
                fprintf(stderr, "ERROR: Only two filenames wanted!\n");
                flags |= F_ERROR;
            }
        }
    }

    // input and output file
    if ((flags & F_ERROR) || !fileIn || !fileOut) {
#ifndef EMBEDDED_USE
        PrintUsage();
#endif
        return 1;
    }

#ifndef EMBEDDED_USE
    if (!quiet) PRINT_ID("TTPack");
#endif

    if (lzlen == -1) lzlen = DEFAULT_LZLEN;

    if (fileIn) {
        if (!(infp = fopen(fileIn, "rb"))) {
            fprintf(stderr, "ERROR: Could not open %s for reading!\n", fileIn);
            return 1;
        }
    }
    else {
        fprintf(stderr, "assuming stdin as text input.\nCtrl-C to abort, Ctrl-Z for EOF.\n");
        infp = stdin;
    }

    /* Read in the data */
    inlen  = 0;
    buflen = 0;
    indata = NULL;
    while (1) {
        if (buflen < inlen + lrange) {
            unsigned char *tmp = realloc(indata, buflen + lrange);
            if (!tmp) {
                free(indata);
                fprintf(stderr, "ERROR: realloc failed!\n");
                return 1;
            }
            indata = tmp;
            buflen += lrange;
        }
        newlen = fread(indata + inlen, 1, lrange, infp);
        if (newlen <= 0) break;
        inlen += newlen;
    }

    //-----------------------------------------------------------
    // convert the input buffer from hex text to binary
    // if the user asks for it
    //-----------------------------------------------------------
    if (flags & F_TEXTINPUT) inlen = ConvertText2Bin(indata,inlen);

    if (infp != stdin) fclose(infp);


    if (startAddr + inlen -1 > 0xffff) {
        fprintf(stderr,"ERROR: File is too large to handle (>64936 Bytes)");
        if (indata) free(indata);
        return 1;
    }

    if (flags & F_VERBOSE) {
        fprintf(stderr, "Load address 0x%04x=%d, Last byte 0x%04x=%d\n",
                         startAddr, startAddr, startAddr+inlen-1, startAddr+inlen-1);
        fprintf(stderr, "New load address 0x%04x=%d\n", memStart, memStart);
    }

    n = PackLz77(lzlen, flags, &startEscape, startAddr + inlen, memEnd, type);

    if (!n) {
        int endAddr = startAddr + inlen; /* end for uncompressed data */

        if (endAddr - ((outPointer + 255) & ~255) < memStart + 3) {
            /* would overwrite the decompressor, move a bit upwards */
            if (flags & F_VERBOSE) fprintf(stderr,"$%x < $%x, decompressor overwrite possible, moving upwards\n",
                                          endAddr - ((outPointer + 255) & ~255), memStart + 3);
            endAddr = memStart + 3 + ((outPointer + 255) & ~255);
        }

        /* 3 bytes reserved for EOF */
        /* bytes reserved for temporary data expansion (escaped chars) */
        endAddr += 3 + reservedBytes;

        if (!timesDLz) type &= ~FIXF_DLZ;

        // type      ... may vary
        // outBuffer ... static global array (65536 Bytes)

        SavePack(flags,type, outBuffer, outPointer, fileOut,
                 startAddr, startEscape, rleValues,
                 endAddr, extraLZPosBits,
                 memStart, memEnd);

        timeused = clock()-timeused;
        if (!timeused) timeused++;
        if (flags & F_VERBOSE) fprintf(stderr,
                                       "Compressed %d bytes in %4.2f seconds (%4.2f kB/sec)\n",
                                       inlen,
                                       (double)timeused/CLOCKS_PER_SEC,
                                       (double)CLOCKS_PER_SEC*inlen/timeused/1024.0);
        return 0;
    }

    if (indata) free(indata);
    return n;
}

#endif

//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.9  2009/01/25           Lionel Debroux
// Changes by Romain Liévin and/or me for 64-bit compatibility.
// Adapt to new version display (revtools.h).
//
// Revision 1.8  2002/03/14 10:47:41  tnussb
// (1) new flag "-quiet" added (suppress standard messages)
// (2) some error messages rewritten
//
// Revision 1.7  2002/03/04 14:32:42  tnussb
// now tool can be used as embedded version from within other tools
// by defining EMBEDDED_VERSION before including the sourcefile
//
// Revision 1.6  2002/02/07 09:49:37  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.5  2000/11/28 00:07:32  Thomas Nussbaumer
// using now USAGE_OUT stream for usage info
//
// Revision 1.4  2000/08/23 20:10:56  Thomas Nussbaumer
// header corrected
//
// Revision 1.3  2000/08/23 20:10:17  Thomas Nussbaumer
// adapted to automatic version display (revtools.h)
//
// Revision 1.2  2000/08/20 15:30:11  Thomas Nussbaumer
// minor correction of header
//
// Revision 1.1  2000/08/16 23:05:59  Thomas Nussbaumer
// initial version
//
//
