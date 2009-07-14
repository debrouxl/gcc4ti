/* Hey EMACS -*- linux-c -*- */
/* $Id: rwfile.c 3227 2007-02-26 19:40:27Z roms $ */

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

/*
  This unit contains some miscellaneous but useful functions.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>

#include "tifiles.h"
#include "integers.h"

#define tifiles_error(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))

/********************/
/* Read/Write bytes */
/********************/

/*
   Read a block of 'n' bytes from a file
   - s [out]: a buffer for storing the data
   - f [in]: a file descriptor
   - [out]: -1 if error, 0 otherwise.
*/
int fread_n_bytes(FILE * f, int n, uint8_t *s)
{
  int i;

  if (s == NULL) 
    for (i = 0; i < n; i++)
      fgetc(f);
  else 
	if(fread(s, 1, n, f) < (size_t)n)
		return -1;

  return 0;
}

/*
  Write a string of 'n' chars max (NULL padded) to a file
  - s [in]: a string
  - f [in]: a file descriptor
  - [out]: -1 if error, 0 otherwise.
*/
int fwrite_n_bytes(FILE * f, int n, const uint8_t *s)
{
  if(fwrite(s, 1, n, f) < (size_t)n)
	  return -1;

  return 0;
}

/**********************/
/* Read/Write strings */
/**********************/

/*
   Read a string of 'n' chars max from a file
   - s [out]: a buffer for storing the string
   - f [in]: a file descriptor
   - [out]: -1 if error, 0 otherwise.
*/
int fread_n_chars(FILE * f, int n, char *s)
{
	int i;
	
	if(fread_n_bytes(f, n, (uint8_t *)s) < 0) 
		return -1;

	if(s != NULL)
	{	
		// set NULL terminator
		s[n] = '\0';
		// and set unused bytes to 0
		for(i = strlen(s); i < n; i++)
			s[i] = '\0';
	}

	return 0;
}

/*
  Write a string of 'n' chars max (NULL padded) to a file
  - s [in]: a string
  - f [in]: a file descriptor
  - [out]: -1 if error, 0 otherwise.
*/
int fwrite_n_chars(FILE * f, int n, const char *s)
{
  int i;
  int l = n;

  l = strlen(s);
  if (l > n) 
  {
    tifiles_error("string passed in 'write_string8' is too long (>n chars).\n");
    abort();
  }

  for (i = 0; i < l; i++)
    if(fputc(s[i], f) == EOF)
		return -1;
  for (i = l; i < n; i++) 
    if(fputc(0x00, f) == EOF)
		return -1;

  return 0;
}

/*
  Write a string of 'n' chars max (SPC padded) to a file
  - s [in]: a string
  - f [in]: a file descriptor
  - [out]: -1 if error, 0 otherwise.
*/
int fwrite_n_chars2(FILE * f, int n, const char *s)
{
  int i;
  int l = n;

  l = strlen(s);
  if (l > n) 
  {
    tifiles_error("string passed in 'write_string8' is too long (>n chars).\n");
    abort();
  }

  for (i = 0; i < l; i++)
    if(fputc(s[i], f) == EOF)
		return -1;
  for (i = l; i < n; i++) 
    if(fputc(0x20, f) == EOF)
		return -1;

  return 0;
}


int fread_8_chars(FILE * f, char *s)
{
  return fread_n_chars(f, 8, s);
}

int fwrite_8_chars(FILE * f, const char *s)
{
  return fwrite_n_chars(f, 8, s);
}

int fskip(FILE * f, int n)
{
  return fseek(f, n, SEEK_CUR);
}

/***************************/
/* Read byte/word/longword */
/***************************/

int fread_byte(FILE * f, uint8_t * data)
{
  if (data != NULL)
	  return (fread((void *) data, sizeof(uint8_t), 1, f) < 1) ? -1 : 0;
  else
    return fskip(f, 1);

  return 0;
}

int fread_word(FILE * f, uint16_t * data)
{
  int ret = 0;

  if (data != NULL)
  {
	  ret = (fread((void *) data, sizeof(uint16_t), 1, f) < 1) ? -1 : 0;
	*data = ReadHI2(*(HI2*)data);
  }
  else
    ret = fskip(f, 2);

  return ret;
}

int fread_long(FILE * f, uint32_t * data)
{
  int ret = 0;

  if (data != NULL)
  {
	  ret = (fread((void *) data, sizeof(uint32_t), 1, f) < 1) ? -1 : 0;
	*data = ReadHI4(*(HI4*)data);
  }
  else
    ret = fskip(f, 4);

  return ret;
}

/****************************/
/* Write byte/word/longword */
/****************************/

int fwrite_byte(FILE * f, uint8_t data)
{
	return (fwrite(&data, sizeof(uint8_t), 1, f) < 1) ? -1 : 0;
}

int fwrite_word(FILE * f, uint16_t data)
{
	WriteHI2(*(HI2*)&data, data);
	return (fwrite(&data, sizeof(uint16_t), 1, f) < 1) ? -1 : 0;
}

int fwrite_long(FILE * f, uint32_t data)
{
	WriteHI4(*(HI4*)&data, data);
  return (fwrite(&data, sizeof(uint32_t), 1, f) < 1) ? -1 : 0;
}
