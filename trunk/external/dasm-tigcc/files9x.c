/* Hey EMACS -*- linux-c -*- */
/* $Id: files9x.c 3227 2007-02-26 19:40:27Z roms $ */

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
	TI File Format handling routines
	Calcs: 89/89tm/92/92+/V200
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include "tifiles.h"
#include "error.h"
#include "macros.h"
#include "files9x.h"
#include "rwfile.h"

#define tifiles_info(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))
#define TI89_AMS     0x23
#define TI89_APPL    0x24
#define TI89_CERTIF  0x25
#define TI89_LICENSE 0x3E

#ifndef DISABLE_TI9X

/********/
/* Misc */
/********/

/***********/
/* Reading */
/***********/

static int check_device_type(uint8_t id)
{
	const uint8_t types[] = { 0, DEVICE_TYPE_89, DEVICE_TYPE_92P };
	int i;

	for(i = 1; i <= (int)(sizeof(types)/sizeof(uint8_t)); i++)
		if(types[i] == id)
			return i;

	return 0;
}

static int check_data_type(uint8_t id)
{
	const uint8_t types[] = { 0, TI89_AMS, TI89_APPL, TI89_CERTIF, TI89_LICENSE  };
	int i;

	for(i = 1; i <= (int)(sizeof(types)/sizeof(uint8_t)); i++)
		if(types[i] == id)
			return i;

	return 0;
}

/**
 * ti9x_file_read_flash:
 * @filename: name of flash file to open.
 * @content: where to store the file content.
 *
 * Load the flash file into a #FlashContent structure.
 *
 * Structure content must be freed with #tifiles_content_delete_flash when
 * no longer used. If error occurs, the structure content is released for you.
 *
 * Return value: an error code, 0 otherwise.
 **/
int ti9x_file_read_flash(const char *filename, Ti9xFlash *head)
{
	FILE *f;
	Ti9xFlash *content = head;
	int tib = 0;
	char signature[9];

	if (!tifiles_file_is_flash(filename) && !tifiles_file_is_tib(filename))
		return ERR_INVALID_FILE;

	// detect file type (old or new format)
	tib = tifiles_file_is_tib(filename);

	f = gfopen(filename, "rb");
	if (f == NULL) 
	{
	    tifiles_info("Unable to open this file: %s\n", filename);
		return ERR_FILE_OPEN;
	}  

	if (tib) 
	{	// tib is an old format but mainly used by developers
		memset(content, 0, sizeof(Ti9xFlash));
		if(fseek(f, 0, SEEK_END)) goto tfrf;
		content->data_length = (uint32_t) ftell(f);
		if(fseek(f, 0, SEEK_SET)) goto tfrf;

		strcpy(content->name, "basecode");
		content->data_type = 0x23;	// FLASH os

		content->data_part = (uint8_t *) calloc(content->data_length, 1);
		if (content->data_part == NULL) 
		{
			fclose(f);
			return ERR_MALLOC;
		}

		if(fread(content->data_part, 1, content->data_length, f) < content->data_length) goto tfrf;
		switch(content->data_part[8])
		{
		case 1: content->device_type = DEVICE_TYPE_92P; break;	// TI92+
		case 3: content->device_type = DEVICE_TYPE_89; break;	// TI89
		// value added by the TI community according to HWID parameter
		// doesn't have any 'legal' existence.
		case 8: content->device_type = DEVICE_TYPE_92P; break;	// V200PLT
		case 9: content->device_type = DEVICE_TYPE_89; break;	// Titanium
		}

		content->next = NULL;
	} 
	else 
	{
		for (content = head;; content = content->next) 
		{
		    if(fread_8_chars(f, signature) < 0) goto tfrf;
		    content->model = tifiles_file_get_model(filename);
		    if(fread_byte(f, &(content->revision_major)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->revision_minor)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->flags)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->object_type)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->revision_day)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->revision_month)) < 0) goto tfrf;
		    if(fread_word(f, &(content->revision_year)) < 0) goto tfrf;
		    if(fskip(f, 1) < 0) goto tfrf;
		    if(fread_8_chars(f, content->name) < 0) goto tfrf;
		    if(fskip(f, 23) < 0) goto tfrf;
		    if(fread_byte(f, &(content->device_type)) < 0) goto tfrf;
		    if(fread_byte(f, &(content->data_type)) < 0) goto tfrf;
		    if(fskip(f, 23) < 0) goto tfrf;
			if(fread_byte(f, &(content->hw_id)) < 0) goto tfrf;
		    if(fread_long(f, &(content->data_length)) < 0) goto tfrf;

			if(content->data_type != TI89_LICENSE && !check_device_type(content->device_type))
				return ERR_INVALID_FILE;
			if(!check_data_type(content->data_type))
				return ERR_INVALID_FILE;

			content->data_part = (uint8_t *) calloc(content->data_length, 1);
			if (content->data_part == NULL) 
			{
				fclose(f);
				tifiles_content_delete_flash(content);
				return ERR_MALLOC;
			}

			if(fread(content->data_part, 1, content->data_length, f) < content->data_length) goto tfrf;
			content->next = NULL;

			// check for end of file
			if(fread_8_chars(f, signature) < 0)
				break;
			if(strcmp(signature, "**TIFL**") || feof(f))
				break;
			if(fseek(f, -8, SEEK_CUR)) goto tfrf;

			content->next = (Ti9xFlash *) calloc(1, sizeof(Ti9xFlash));
			if (content->next == NULL) 
			{
				fclose(f);
				tifiles_content_delete_flash(content);
				return ERR_MALLOC;
			}
		}
	}

	fclose(f);
	return 0;

tfrf:	// release on exit
    fclose(f);
	tifiles_content_delete_flash(content);
	return ERR_FILE_IO;
}

#endif
