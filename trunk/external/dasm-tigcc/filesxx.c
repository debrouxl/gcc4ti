/* Hey EMACS -*- linux-c -*- */
/* $Id: filesxx.c 3227 2007-02-26 19:40:27Z roms $ */

/*  libtifiles - file format library, a part of the TiLP project
 *  Copyright (C) 1999-2006  Romain Liévin
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
  This unit contains a TI file independant API
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "tifiles.h"
#include "error.h"
#include "files9x.h"

#define tifiles_calc_is_ti9x(model) 1

/**
 * tifiles_content_create_flash:
 * @model: a calculator model (compulsory).
 *
 * Allocates a #FlashContent structure.
 *
 * Return value: the allocated block.
 **/
TIEXPORT2 FlashContent* TICALL tifiles_content_create_flash(CalcModel model)
{
	FlashContent* content = calloc(1, sizeof(FlashContent));

	content->model = model;
	if(tifiles_calc_is_ti9x(content->model))
	{
	  time_t tt;
	  struct tm *lt;

	  time(&tt);
	  lt = localtime(&tt);
	  content->revision_major = 1;
	  content->revision_minor = 0;
	  content->flags = 0;
	  content->object_type = 0;
	  content->revision_day = lt->tm_mday;
	  content->revision_month = lt->tm_mon;
	  content->revision_year = lt->tm_year + 1900;
	}

	return content;
}

/**
 * tifiles_content_delete_flash:
 *
 * Free the whole content of a #FlashContent structure.
 *
 * Return value: none.
 **/
TIEXPORT2 int TICALL tifiles_content_delete_flash(FlashContent *content)
{
	int i;
	assert(content != NULL);

	{
		FlashContent *ptr;

		free(content->data_part);

		ptr = content->next;
		while (ptr != NULL) 
		{
			FlashContent *next = ptr->next;

			free(ptr->data_part);
			free(ptr);

			for(i = 0; i < content->num_pages; i++)
			{
				free(content->pages[i]->data);
				free(content->pages[i]);
			}
			free(content->pages);

			ptr = next;
		}

		free(content);
	}

  return 0;
}

/**
 * tifiles_file_read_flash:
 * @filename: name of FLASH file to open.
 * @content: where to store the file content.
 *
 * Load the FLASH file into a FlashContent structure.
 *
 * Structure content must be freed with #tifiles_content_delete_flash when
 * no longer used.
 *
 * Return value: an error code, 0 otherwise.
 **/
TIEXPORT2 int tifiles_file_read_flash(const char *filename, FlashContent *content)
{
	if (tifiles_calc_is_ti9x(tifiles_file_get_model(filename)) || tifiles_file_is_tib(filename))
		return ti9x_file_read_flash(filename, content);
	else
    return ERR_BAD_CALC;

	return 0;
}
