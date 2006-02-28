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

#define GREG_UNPACK_HEXARRAY

#define ttunpack_decompress ((unsigned short(*)(unsigned char*,unsigned char*))(unsigned short[])\
{\
0x48e7,0x1f3e,0x2c6f,0x002c,0x2a6f,0x0030,0x47ee,0x000f,0x7a00,0x1a2e,\
0x0006,0x7e08,0x7004,0x0c2e,0x0054,0x0002,0x667c,0x0c2e,0x0050,0x0003,\
0x6674,0x7006,0x0c2e,0x0008,0x000a,0x666a,0x0c2e,0x0080,0x000b,0x6662,\
0x7007,0x7200,0x122e,0x0009,0x2801,0x5141,0x6254,0x7005,0x7200,0x122e,\
0x000c,0x2241,0x5b41,0x6e46,0x7400,0x09c2,0x5342,0xe83a,0xe83d,0x7000,\
0x1013,0x4df6,0x0010,0x49fa,0x003c,0x4a04,0x6604,0x49fa,0x0052,0x4fef,\
0xfefc,0x244f,0x421a,0x7000,0x7207,0x7600,0x01c3,0x14c0,0x5343,0x66fa,\
0x5240,0x51c9,0xfff2,0x244f,0x1c1e,0x4ed4,0x7000,0x4fef,0x0104,0x4400,\
0x4cdf,0x7cf8,0x4e75,0x1ac0,0x1f06,0x301f,0x1c1e,0x1006,0xee68,0x1202,\
0xc200,0xb205,0x66ec,0x558e,0x1c1e,0x9e44,0x6204,0x1c1e,0x5047,0x6100,\
0x0110,0x3600,0x5340,0x6778,0x6100,0x0106,0x5200,0x662c,0x5943,0x65bc,\
0x5843,0x1206,0x4841,0x1f1e,0x321f,0x121e,0x1c01,0xeea9,0x70ff,0x1001,\
0x3f01,0x121f,0x41f5,0x0000,0x1018,0xd001,0x1ac0,0x51cb,0xfff8,0x4ed4,\
0x5500,0x3209,0x671c,0xe368,0x7200,0x0fc1,0x5341,0xc206,0x9e49,0x620a,\
0x1c1e,0x1f01,0x321f,0x1206,0x5047,0xee69,0x8041,0x7200,0x1f00,0x321f,\
0x1f06,0x301f,0x1c1e,0x1006,0xee68,0x1200,0x4601,0x4681,0x41f5,0x1800,\
0x1ad8,0x51cb,0xfffc,0x4ed4,0x5307,0x0f06,0x676a,0x4a07,0x6604,0x7e08,\
0x1c1e,0x5307,0x0f06,0x6700,0x00c6,0x4a07,0x6604,0x7e08,0x1c1e,0x6170,\
0x1600,0x6a1c,0xd603,0x5307,0x0f06,0x56c0,0x9600,0x4a07,0x6604,0x7e08,\
0x1c1e,0x6158,0x5300,0x3f03,0x1e80,0x361f,0x614e,0x0c00,0x0020,0x6406,\
0x1033,0x0000,0x601a,0xe708,0x1206,0x5747,0x620a,0x1c1e,0x1f01,0x321f,\
0x1206,0x5047,0xee69,0x0201,0x0007,0x8001,0x1ac0,0x51cb,0xfffc,0x4ed4,\
0x4a07,0x6604,0x7e08,0x1c1e,0x1f06,0x321f,0x1c1e,0x1206,0xee69,0x76ff,\
0x1601,0x41f5,0x3800,0x1ad8,0x1ad8,0x4ed4,0x7000,0x3207,0x0fc0,0x5340,\
0xcc00,0xbd00,0x6606,0x1016,0x4600,0x5041,0x9232,0x0000,0x0c01,0x0008,\
0x6504,0x7208,0x5247,0x9e41,0x6204,0x5047,0x1c1e,0x7000,0x5301,0x6716,\
0x0fc0,0x5340,0xc006,0x9e41,0x620a,0x1c1e,0x1f00,0x301f,0x1006,0x5047,\
0xee68,0x03c0,0x4e75,0x4a07,0x6604,0x7e08,0x1c1e,0x1f06,0x301f,0x101e,\
0x1c00,0xee68,0x1205,0x1a02,0xca00,0x4602,0xc002,0x4602,0x8001,0x1ac0,\
0x4ed4})

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


 