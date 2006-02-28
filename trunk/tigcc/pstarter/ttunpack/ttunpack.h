/******************************************************************************
*
* project name:    TIGCC Tools Suite
* file name:       ttunpack.h
* initial date:    01/02/2001
* authors:         thomas.nussbaumer@gmx.net
*                  samuel@nyall.net
*
* description:     defines for exepack functionalities including unpack routine
*                  (complete rewrite of TIGCC Tools Suite header file)
*
*   This is a 100% assembly version of the ttunpack routine, which is
*   based on code by Pasi 'Albert' Ojala, albert@cs.tut.fi, then
*   reduced by Thomas Nussbaumer to fit his needs.  For a full details
*   on the algorithm see:
*
*	  http://www.cs.tut.fi/~albert/Dev/pucrunch/index.html
*
*
* Liscense:
*
*   The liscense is quite similar to that of the origional version in c.
*
*   The code may be freely used in any non commercial application.  If
*   you do use it please give credit to tict (http://tict.ticalc.org) and
*   myself (Samuel Stearley).  I state this as a request but it is actually
*   not.
*
*   The author(s) (Samuel/Tict/Any other contributor) make no
*   representations or warranties about the suitability of the software,
*   either express or implied. The author(s) shall not be liable for
*   any damages suffered as a result of using or distributing this
*   software.
*
*   If you distribute this file on a website or as part of
*   the source of a project do not remove the liscense from this file.
*
*
* $Id: ttunpack.h,v 1.1 2006-02-28 23:36:58 kevinkofler Exp $
*
******************************************************************************/
#ifndef __TTUNPACK_H__
#define __TTUNPACK_H__

typedef struct {
    unsigned char  osize_lo;   // original size lowbyte
    unsigned char  osize_hi;   // original size highbyte
    unsigned char  magic1;     // must be equal to TTUNPACK_MAGIC1
    unsigned char  magic2;     // must be equal to TTUNPACK_MAGIC2
    unsigned char  csize_lo;   // compressed size lowbyte
    unsigned char  csize_hi;   // compressed size lowbyte
    unsigned char  esc1;       // escape >> (8-escBits)
    unsigned char  notused3;
    unsigned char  notused4;
    unsigned char  esc2;       // escBits
    unsigned char  gamma1;     // maxGamma + 1
    unsigned char  gamma2;     // (1<<maxGamma)
    unsigned char  extralz;    // extraLZPosBits
    unsigned char  notused1;
    unsigned char  notused2;
    unsigned char  rleentries; // rleUsed
} TTUNPACK_HEADER;

#define TTUNPACK_MAGIC1 0x54
#define TTUNPACK_MAGIC2 0x50

#define ttunpack_size(_p_)  ((unsigned short)(((TTUNPACK_HEADER*)(_p_))->osize_lo | (((TTUNPACK_HEADER*)(_p_))->osize_hi << 8)))
#define ttunpack_valid(_p_) (((TTUNPACK_HEADER*)(_p_))->magic1 == TTUNPACK_MAGIC1 && ((TTUNPACK_HEADER*)(_p_))->magic2 == TTUNPACK_MAGIC2)

#define TTUNPACK_OKAY             0
#define TTUNPACK_NOESCFOUND     248
#define TTUNPACK_ESCBITS        249
#define TTUNPACK_MAXGAMMA       250
#define TTUNPACK_EXTRALZP       251
#define TTUNPACK_NOMAGIC        252
#define TTUNPACK_OUTBUFOVERRUN  253
#define TTUNPACK_LZPOSUNDERRUN  254

#define ttunpack_decompress ((unsigned short(*)(unsigned char*,unsigned char*))(unsigned short[])\
{\
0x48e7,0x1f3e,0x1a2b,0x0006,0x7e80,0x7008,0x548b,0x0c5b,0x5450,0x661c,\
0x5a8b,0x181b,0xb800,0x6214,0x0c5b,0x0880,0x660e,0xd02b,0x0003,0x43f3,\
0x08fc,0x0c13,0x0005,0x6512,0x4cdf,0x7cf8,0x4e75,0x6136,0xc145,0x7208,\
0x9204,0x613e,0x10c0,0x612a,0xb005,0x66f2,0x613c,0x3600,0x5340,0x6746,\
0x6134,0x5200,0x67da,0x5500,0x1213,0x6122,0x7207,0x6114,0x4600,0x4480,\
0x10f0,0x08ff,0x51cb,0xfffa,0x60d4,0x1204,0x7000,0x600a,0xe31f,0x6402,\
0x1c19,0xdc06,0xd140,0x5301,0x6af2,0x44c0,0x4e75,0x7406,0x61ea,0x54ca,\
0xfffc,0x7206,0x9242,0x7001,0x60e8,0x61dc,0x64c2,0x61d8,0x6498,0x61e6,\
0x1600,0x6a0c,0x61ce,0x3f00,0x61dc,0x5300,0x1e80,0x361f,0x61d4,0x5502,\
0x6b06,0x1033,0x0003,0x6004,0x7202,0x61b4,0x10c0,0x7000,0x60a2})

#endif

//=============================================================================
// Revision History
//=============================================================================
//
// $Log: not supported by cvs2svn $
//
// Revision 2.0  2004/08/30 12:00:00  Samuel Stearley
// New extraction routine in 100% asm.
//
// Revision 1.4  2002/03/28 21:17:28  tnussb
// project name in header changed to TIGCC Tools Suite
//
// Revision 1.3  2002/03/13 15:09:58  tnussb
// new exepack decompression function generated from old/unpack.c added
// (its a little bit smaller now). The new function should work as the old one,
// but if there are problems I will leave the old hexcode array here, too
//
// Revision 1.2  2001/02/05 20:33:01  Thomas Nussbaumer
// (1) magic of ttunpack header splitted again into 2 bytes to prevent address errors
// (2) new (stable?) pc-relative version of unpacking routine
//
//


 