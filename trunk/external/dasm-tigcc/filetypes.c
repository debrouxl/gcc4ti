/* Hey EMACS -*- linux-c -*- */
/* $Id: typesxx.c 912 2005-03-30 20:49:06Z roms $ */

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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define g_ascii_strcasecmp strcasecmp

#include "tifiles.h"
#include "error.h"
#include "rwfile.h"
#include "sysdep.h"

/****************/
/* Global types */
/****************/

#define NCALCS FILES_NCALCS

static const char GROUP_FILE_EXT[NCALCS + 1][4] = 
{
	"XxX", 
	"73g", "82g", "83g", "8Xg", "8Xg", "85g", "86g", 
	"89g", "89g", "92g", "9Xg", "V2g", "8Xg", "89g",
};

static const char BACKUP_FILE_EXT[NCALCS + 1][4] = 
{
	"XxX", 
	"73b", "82b", "83b", "8Xb", "8Xb", "85b", "86b", 
	"89g", "89g", "92b", "9Xg", "V2g", "8Xg", "89g",
};

static const char FLASH_APP_FILE_EXT[NCALCS + 1][4] = 
{
	"XxX", 
	"73k", "???", "???", "8Xk", "8Xk", "???", "???",
	"89k", "89k", "???", "9Xk", "V2k", "8Xk", "89k",
};

static const char FLASH_OS_FILE_EXT[NCALCS + 1][4] = 
{
	"XxX", 
	"73u", "???", "???", "8Xu", "8Xu", "???", "???",
	"89u", "89u", "???", "9Xu", "V2u", "8Xu", "89u",
};

static const char CERTIF_FILE_EXT[NCALCS + 1][4] = 
{
	"XxX", 
	"73q", "???", "???", "8Xq", "8Xq", "???", "???",
	"89q", "89q", "???", "9Xq", "V2q", "8Xq", "89q",
};

/*******************/
/* File extensions */
/*******************/

/**
 * tifiles_fext_get:
 * @filename: a filename as string.
 *
 * Returns file extension part.
 *
 * Return value: a file extension without dot as string (like "89g").
 **/
TIEXPORT2 const char *TICALL tifiles_fext_get(const char *filename)
{
  char *d = NULL;

  d = strrchr(filename, '.');
  if (d == NULL)
    return "";

  return (++d);
}

/**********************/
/* Signature checking */
/**********************/

static int tifiles_file_has_ti_header(const char *filename)
{
	FILE *f;
	char buf[9];
	char *p;

	f = gfopen(filename, "rb");
	if (f == NULL)
		return 0;
	
	fread_8_chars(f, buf);
	for(p = buf; *p != '\0'; p++)
		*p = toupper(*p);

	if (!strcmp(buf, "**TI73**") || !strcmp(buf, "**TI82**") ||
      !strcmp(buf, "**TI83**") || !strcmp(buf, "**TI83F*") ||
      !strcmp(buf, "**TI85**") || !strcmp(buf, "**TI86**") ||
      !strcmp(buf, "**TI89**") || !strcmp(buf, "**TI92**") ||
      !strcmp(buf, "**TI92P*") || !strcmp(buf, "**V200**") ||
      !strcmp(buf, "**TIFL**")) {
		fclose(f);
		return !0;
	}

	fclose(f);
	return 0;
}

#define TIB_SIGNATURE	"Advanced Mathematics Software"

static int tifiles_file_has_tib_header(const char *filename)
{
	FILE *f;
	char str[128];
	const char *e = tifiles_fext_get(filename);

	if (!strcmp(e, ""))
	  return 0;

	if(g_ascii_strcasecmp(e, "tib"))
		return 0;

	f = gfopen(filename, "rb");
	if(f == NULL)
		return 0;

	fread_n_chars(f, 22, str);
	fread_n_chars(f, strlen(TIB_SIGNATURE), str);
	str[strlen(TIB_SIGNATURE)] = '\0';
	if(!strcmp(str, TIB_SIGNATURE)) 
	{
		fclose(f);
		return !0;
	}

	return 0;
}

/**************/
/* File types */
/**************/

#ifndef __WIN32__
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

static int is_regfile(const char *filename ATTRIBUTE_UNUSED)
{
#ifndef __WIN32__
	struct stat buf;

	if (stat(filename, &buf) < 0)
		return 0;

	if (S_ISREG(buf.st_mode))
		return !0;
	else
		return 0;
#else
	return !0;
#endif
}

/**
 * tifiles_file_is_ti:
 * @filename: a filename as string.
 *
 * Check whether file is a TI file by checking the signature.
 *
 * Return value: a boolean value.
 **/
TIEXPORT2 int TICALL tifiles_file_is_ti(const char *filename)
{
	// bug: check that file is not a FIFO
	if (!is_regfile(filename))
		return 0;

	if(tifiles_file_has_ti_header(filename))
		return !0;

	if(tifiles_file_has_tib_header(filename))
		return !0;

	return 0;
}


/**
 * tifiles_file_is_os:
 * @filename: a filename as string.
 *
 * Check whether file is a FLASH OS file (tib or XXu)
 *
 * Return value: a boolean value.
 **/
TIEXPORT2 int TICALL tifiles_file_is_os(const char *filename)
{
  int i;
  const char *e = tifiles_fext_get(filename);

  if (!strcmp(e, ""))
    return 0;

  if (!tifiles_file_is_ti(filename))
    return 0;

  if(tifiles_file_is_tib(filename))
	  return !0;

  for (i = 1; i < NCALCS + 1; i++) 
  {
    if (!g_ascii_strcasecmp(e, FLASH_OS_FILE_EXT[i]))
      return !0;
  }

  return 0;
}

/**
 * tifiles_file_is_app:
 * @filename: a filename as string.
 *
 * Check whether file is a FLASH app file
 *
 * Return value: a boolean value.
 **/
TIEXPORT2 int TICALL tifiles_file_is_app(const char *filename)
{
  int i;
  const char *e = tifiles_fext_get(filename);

  if (!strcmp(e, ""))
    return 0;

  if (!tifiles_file_is_ti(filename))
    return 0;

  for (i = 1; i < NCALCS + 1; i++) 
  {
    if (!g_ascii_strcasecmp(e, FLASH_APP_FILE_EXT[i]))
      return !0;
  }

  return 0;
}

/**
 * tifiles_file_is_flash:
 * @filename: a filename as string.
 *
 * Check whether file is a FLASH file (os or app).
 *
 * Return value: a boolean value.
 **/
TIEXPORT2 int TICALL tifiles_file_is_flash(const char *filename)
{
  return tifiles_file_is_os(filename) || tifiles_file_is_app(filename);
}

/**
 * tifiles_file_is_tib:
 * @filename: a filename as string.
 *
 * Check whether file is a TIB formatted file.
 *
 * Return value: a boolean value.
 **/
TIEXPORT2 int TICALL tifiles_file_is_tib(const char *filename)
{
	return tifiles_file_has_tib_header(filename);
}

/********/
/* Misc */
/********/

/* Note: a better way should be to open the file and read the signature */
/**
 * tifiles_file_get_model:
 * @filename: a filename as string.
 *
 * Returns the calculator model targetted for this file.
 *
 * Return value: a model taken in #CalcModel.
 **/
TIEXPORT2 CalcModel TICALL tifiles_file_get_model(const char *filename)
{
  const char *ext = tifiles_fext_get(filename);
  int type = CALC_NONE;
  char str[3];

  if (!strcmp(ext, ""))
    return CALC_NONE;

  strncpy(str, ext, 2);
  str[2] = '\0';

  if (!g_ascii_strcasecmp(str, "73"))
    type = CALC_TI73;
  else if (!g_ascii_strcasecmp(str, "82"))
    type = CALC_TI82;
  else if (!g_ascii_strcasecmp(str, "83"))
    type = CALC_TI83;
  else if (!g_ascii_strcasecmp(str, "8x"))
    type = CALC_TI83P;
  else if (!g_ascii_strcasecmp(str, "85"))
    type = CALC_TI85;
  else if (!g_ascii_strcasecmp(str, "86"))
    type = CALC_TI86;
  else if (!g_ascii_strcasecmp(str, "89"))
    type = CALC_TI89;
  else if (!g_ascii_strcasecmp(str, "92"))
    type = CALC_TI92;
  else if (!g_ascii_strcasecmp(str, "9X"))
    type = CALC_TI92P;
  else if (!g_ascii_strcasecmp(str, "V2"))
    type = CALC_V200;
  //else if (!g_ascii_strcasecmp(str, "tib"))
    //type = CALC_TI89;	// consider .tib as TI89
  else
    type = CALC_NONE;

  return type;
}
