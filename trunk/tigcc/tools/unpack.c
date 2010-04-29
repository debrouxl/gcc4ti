/******************************************************************************
*
* project name:    TI-68k Developer Utilities
* file name:       unpack.c
* initial date:    14/08/2000
* authors:         albert@cs.tut.fi
*                  thomas.nussbaumer@gmx.net
* description:     unpacking routine
*
* NOTE: the behavior ("fast/long code/unsecure" or "slow/short code/safe") can
*       be set by defining DO_FAST_UNPACKING
*
* -----------------------------------------------------------------------------
*
* Unpacking program using the PuCrunch algorithm.
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
*
******************************************************************************
*
* NOTE: This is a slow, but generic implementation which can be used
*       on the PC as well as for the calculator software.
*
******************************************************************************/

#include "ttunpack.h"    // errorcodes definition
#include "packhead.h"    // compressed header definition


#ifdef DO_FAST_UNPACKING
#define _ttcallmode_ inline
#else
//#define ALL_CHECKS      // additionally runtime checks (enable this for tests)
#define _ttcallmode_
#endif

//-----------------------------------------------------------------------------
// All globals and functions are prefixed with _tt_ by the intention to
// prevent possible name conflicts in the global namespace
//-----------------------------------------------------------------------------
unsigned char* _tt_InBuffer  = 0; // input buffer
unsigned int   _tt_InMask    = 0; // input buffer byte mask


//---------------------------------------------------------------------------
// _tt_InMask is initialized with 0x80 and gets shifted right each time about
// 1 bit. CORRECT_IN_MASK will handle the shift right and if the mask becomes
// zero it will set it again to 0x80 + increments the inputbuffer
//
// THIS SHOULD BE OPTIMIZABLE WITH A ROTATE WITH CARRY
//---------------------------------------------------------------------------
#define CORRECT_IN_MASK    if (!(_tt_InMask >>= 1)) _tt_InMask = 0x80,_tt_InBuffer++;


//---------------------------------------------------------------------------
// returns >0 if next bit is set, otherwise 0
//---------------------------------------------------------------------------
#define NEXT_BIT_SET       (*_tt_InBuffer & _tt_InMask)



//=============================================================================
// gets a number of bits from the input buffer
//=============================================================================
_ttcallmode_ unsigned int _tt_GetBits(unsigned int bits) {
    unsigned int val = 0;

    while (bits--) {
        val <<= 1;
        if (NEXT_BIT_SET) val |= 1;
        CORRECT_IN_MASK;
    }
    return val;
}


//=============================================================================
// get next 8 bits
//=============================================================================
_ttcallmode_ unsigned int _tt_Get8Bit() {
    unsigned int v;
    // very fast if we are already on a byte boundary
    if (_tt_InMask == 0x80) return *_tt_InBuffer++;

    v = *(_tt_InBuffer++) << 8;
    v |= *_tt_InBuffer;

    switch(_tt_InMask) {
       case 0x40: v &= 0x7fff; return v >>= 7;
       case 0x20: v &= 0x3fff; return v >>= 6;
       case 0x10: v &= 0x1fff; return v >>= 5;
       case 0x08: v &= 0x0fff; return v >>= 4;
       case 0x04: v &= 0x07ff; return v >>= 3;
       case 0x02: v &= 0x03ff; return v >>= 2;
       case 0x01: v &= 0x01ff; return v >>= 1;
    }

    return 0;
}


//=============================================================================
// get next value from the input buffer
//=============================================================================
unsigned int _tt_GetValue(unsigned int maxgamma) {
    unsigned int i = 0;

    while (i<maxgamma) {
        if (!(NEXT_BIT_SET)) {
            CORRECT_IN_MASK;
            break;
        }
        CORRECT_IN_MASK;
        i++;
    }

    return (1<<i) | _tt_GetBits(i);
}


//=============================================================================
// the decompression routine
//
// using it is very simple: feed in a filled source array and a buffer which is
// large enough to hold the decompression result
//
// returns 0 if okay
//
//=============================================================================
int _tt_Decompress(unsigned char *src, unsigned char *dest)  {
    long           startEsc;
    unsigned char* byteCodeVec;
    PackedHeader*  cth = (PackedHeader*)src;
    unsigned int   maxgamma1;
    unsigned int   maxgamma2;
    unsigned int   maxgamma8;
    unsigned int   escbits8;
    unsigned int   escbits;
    unsigned int   extralzposbits;
    unsigned int   maxgamma;
    unsigned char* outbuffer;
#ifdef ALL_CHECKS
    unsigned char* pend_in;
    unsigned char* pend_out;
#endif


    //---------------------------------------------------------------------
    // check if the magic markers exists. if they are not present we cannot
    // decompress this type of file
    //---------------------------------------------------------------------
    if (cth->magic1 != MAGIC_CHAR1 || cth->magic2 != MAGIC_CHAR2) return ERRPCK_NOMAGIC;

    startEsc       = cth->esc1;
    escbits        = cth->esc2;
    maxgamma       = cth->gamma1 - 1;
    extralzposbits = cth->extralz;

    maxgamma1 = 1 << maxgamma;
    maxgamma2 = 2 << maxgamma;
    maxgamma8 = 8 - maxgamma;
    escbits8  = 8 - escbits;


    if (escbits > 8)                                               return ERRPCK_ESCBITS;
    if (cth->gamma2 != maxgamma1 || maxgamma < 5 || maxgamma > 7)  return ERRPCK_MAXGAMMA;
    if (extralzposbits > 4)                                        return ERRPCK_EXTRALZP;


    byteCodeVec = &src[15];    // ??? shouldn't it start at 16 -- strange ???

    //--------------------------
    // initialize buffer globals
    //--------------------------
    outbuffer    = dest;
    _tt_InBuffer = src + sizeof(PackedHeader) + cth->rleentries;   // points at start of data
    _tt_InMask   = 0x80;


#ifdef ALL_CHECKS
    pend_in  = _tt_InBuffer + (((unsigned int)cth->compsize_lo) | ((unsigned int)(cth->compsize_hi << 8)));
    pend_out = dest + (((unsigned int)cth->origsize_lo) | ((unsigned int)(cth->origsize_hi << 8)));
#endif

    while (1) {
        int sel = startEsc;

#ifdef ALL_CHECKS
        if (outbuffer > pend_out)     return ERRPCK_OUTBUFOVERRUN;
        if (_tt_InBuffer  > pend_in)  return ERRPCK_NOESCFOUND;
#endif

        if (escbits) sel = _tt_GetBits(escbits);

        if (sel == startEsc) {
            unsigned int lzPos, lzLen = _tt_GetValue(maxgamma), i;
            unsigned int add = 0;

            if (lzLen != 1) {
                unsigned int lzPosHi = _tt_GetValue(maxgamma)-1, lzPosLo;

                if (lzPosHi == maxgamma2-2) {
                    if (lzLen > 3) {
                        add   = _tt_Get8Bit();
                        lzPos = _tt_Get8Bit() ^ 0xff;
                    }
                    else {
                        break;  // finish !!!
                    }
                }
                else {
                    if (extralzposbits) lzPosHi = (lzPosHi<<extralzposbits) | _tt_GetBits(extralzposbits);

                    lzPosLo = _tt_Get8Bit() ^ 0xff;
                    lzPos   = (lzPosHi<<8) | lzPosLo;
                }
            }
            else {
                if (NEXT_BIT_SET) {
                    unsigned int rleLen, byteCode, byte;
                    CORRECT_IN_MASK;

                    if (!NEXT_BIT_SET) {
                        unsigned int newEsc;
                        CORRECT_IN_MASK;

                        newEsc = _tt_GetBits(escbits);

                        *outbuffer++ = (startEsc<<escbits8) | _tt_GetBits(escbits8);
                        startEsc = newEsc;
#ifdef ALL_CHECKS
                        if (outbuffer > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
                        continue;
                    }
                    //else {
                    CORRECT_IN_MASK;
                    //}
                    rleLen = _tt_GetValue(maxgamma);
                    if (rleLen >= maxgamma1) {
                        rleLen = ((rleLen-maxgamma1)<<maxgamma8) | _tt_GetBits(maxgamma8);
                        rleLen |= ((_tt_GetValue(maxgamma)-1)<<8);
                    }
                    byteCode = _tt_GetValue(maxgamma);
                    if (byteCode < 32) byte = (unsigned int)(byteCodeVec[byteCode]);
                    else               byte = ((byteCode-32)<<3) | _tt_GetBits(3);

                    for (i=0; i<=rleLen; i++) *outbuffer++ = byte;
                    continue;
                }
                //else {
                CORRECT_IN_MASK;
                //}
                lzPos = _tt_Get8Bit() ^ 0xff;
            }

#ifdef ALL_CHECKS
            if (outbuffer + lzLen + 1 > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
            for (i=0; i<=lzLen; i++) {
                *outbuffer = *(outbuffer - lzPos - 1) + add;
                outbuffer++;
            }
        }
        else {
            *outbuffer++ = (sel<<escbits8) | _tt_GetBits(escbits8);
#ifdef ALL_CHECKS
            if (outbuffer > pend_out) return ERRPCK_OUTBUFOVERRUN;
#endif
        }
    }

    return ERRPCK_OKAY;
}



//#############################################################################
//###################### NO MORE FAKES BEYOND THIS LINE #######################
//#############################################################################
//
//=============================================================================
// Revision History
//=============================================================================
//
// Revision 1.2  2002/02/07 09:49:38  tnussb
// all local includes changed, because header files are now located in pctools folder
//
// Revision 1.1  2001/02/05 20:45:05  Thomas Nussbaumer
// moved to pctools folder
//
//
//
