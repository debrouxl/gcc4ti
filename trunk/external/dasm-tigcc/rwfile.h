/* Hey EMACS -*- linux-c -*- */
/* $Id: misc.h 368 2004-03-22 18:42:08Z roms $ */

/*  libtifiles - file format library, a part of the TiLP project
 *  Copyright (C) 1999-2005  Romain Liévin
 *  Copyright (C) 2007 Kevin Kofler
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef __TIFILES_MISC__
#define __TIFILES_MISC__

#define gfopen fopen

int fread_n_bytes(FILE * f, int n, uint8_t *s);
int fwrite_n_bytes(FILE * f, int n, const uint8_t *s);

int fread_n_chars(FILE * f, int n, char *s);
int fwrite_n_chars(FILE * f, int n, const char *s);
int fwrite_n_chars2(FILE * f, int n, const char *s);

int fread_8_chars(FILE * f, char *s);
int fwrite_8_chars(FILE * f, const char *s);

int fskip(FILE * f, int n);

int fread_byte(FILE * f, uint8_t * data);
int fread_word(FILE * f, uint16_t * data);
int fread_long(FILE * f, uint32_t * data);

int fwrite_byte(FILE * f, uint8_t data);
int fwrite_word(FILE * f, uint16_t data);
int fwrite_long(FILE * f, uint32_t data);

#endif
