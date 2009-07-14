/*
   dasm-tigcc - Disassembler for TI calculators

   Copyright (C) 2000-2002 Thomas Nussbaumer
   Copyright (C) 2005 Lionel Debroux
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#include <stdint.h>
extern int calc_type;
extern unsigned char *ram;
extern unsigned char *rom;
extern unsigned char unused[0x10000];
extern uint32_t rom_base;
extern uint32_t rom_size;
extern uint32_t ram_size;
extern uint32_t entry_point;
void PrintAddress (int32_t reladdr, uint32_t absaddr, int flags, void *stream,
                   void (*fprintf_f) (void *, const char *, ...),
                   void (*fputs_f) (const char *, void *));
void Offset2Name(int addr);
