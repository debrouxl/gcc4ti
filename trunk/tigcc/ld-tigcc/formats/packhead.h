/* packhead.h: Header definition of compressed data

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

#ifndef PACKHEAD_H
#define PACKHEAD_H


#define MAGIC_CHAR1     'T'
#define MAGIC_CHAR2     'P'

#define MAX_RLE_ENTRIES 31

// size = 16 bytes 
typedef struct {
    unsigned char origsize_lo; // original size lowbyte
    unsigned char origsize_hi; // original size highbyte
    unsigned char magic1;      // must be equal to MAGIC_CHAR1
    unsigned char magic2;      // must be equal to MAGIC_CHAR2
    unsigned char compsize_lo; // compressed size lowbyte
    unsigned char compsize_hi; // compressed size lowbyte
    unsigned char esc1;        // escape >> (8-escBits)
    unsigned char notused3;
    unsigned char notused4;
    unsigned char esc2;        // escBits
    unsigned char gamma1;      // maxGamma + 1
    unsigned char gamma2;      // (1<<maxGamma)
    unsigned char extralz;     // extraLZPosBits
    unsigned char notused1;
    unsigned char notused2;
    unsigned char rleentries;  // rleUsed
} PackedHeader;


#define GetUnPackedSize(p)  (unsigned int)((p)->origsize_lo | ((p)->origsize_hi << 8))
#define IsPacked(p)         ((p)->magic1 == MAGIC_CHAR1 && (p)->magic2 == MAGIC_CHAR2)


typedef struct { 
    unsigned char value[MAX_RLE_ENTRIES];
} RLEEntries;


#endif
