/* Hey EMACS -*- linux-c -*- */
/* $Id: tifiles.h 3205 2007-02-24 11:09:24Z roms $ */

/*  libTIFILES - file format library, a part of the TiLP project
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

#ifndef __TIFILES_H__
#define __TIFILES_H__

#include <stdint.h>
#define TIEXPORT2 /**/
#define TICALL /**/

#define FILES_NCALCS	14	// # of supported calcs

/**
 * CalcModel:
 *
 * An enumeration which contains the following calculator types:
 **/
#if !defined(__TICONV_H__)
typedef enum 
{
	CALC_NONE = 0,
	CALC_TI73, CALC_TI82, CALC_TI83, CALC_TI83P, CALC_TI84P, CALC_TI85, CALC_TI86,
	CALC_TI89, CALC_TI89T, CALC_TI92, CALC_TI92P, CALC_V200,
	CALC_TI84P_USB, CALC_TI89T_USB,
} CalcModel;
#endif

/**
 * DeviceType:
 *
 * An enumeration which contains soem device IDs for FLASH apps:
 **/
typedef enum
{
	DEVICE_TYPE_83P = 0x73,
	DEVICE_TYPE_73	= 0x74,
	DEVICE_TYPE_89  = 0x98,
	DEVICE_TYPE_92P = 0x88,
} DeviceType;

/**
 * FlashPage:
 * @offset: FLASH offset (see TI link guide).
 * @page: FLASH page (see TI link guide).
 * @flag: see link guide.
 * @size: length of pure data (up to 16384 bytes)
 * @data: pure FLASH data.
 *
 * A generic structure used to store the content of a TI8x memory page for FLASH.
 **/
typedef struct 
{
  uint16_t	addr;
  uint16_t	page;
  uint8_t	flag;
  uint16_t	size;
  uint8_t*	data;

} FlashPage;

/**
 * FlashContent:
 * @model: a calculator model.
 * @revision_major:
 * @revision_minor:
 * @flags:
 * @object_type:
 * @revision_day:
 * @revision_month:
 * @revision_year:
 * @name: name of FLASH app or OS
 * @device_type: a device ID
 * @data_type: a type ID
 * @hw_id: hardware ID (used on TI9x only, 0 otherwise)
 * @data_length: length of pure data
 * @data_part: pure FLASH data (TI9x only) or license or certificate
 * @num_pages: number of FLASH pages (TI8x only)
 * @pages: NULL-terminated array of FLASH pages (TI8x only)
 * @next: pointer to next structure (linked list of contents)
 *
 * A generic structure used to store the content of a FLASH file (os or app).
 **/
typedef struct _FlashContent FlashContent;
struct _FlashContent
{
  CalcModel		model;

  //FlashHeader	header;
  uint8_t		revision_major;
  uint8_t		revision_minor;
  uint8_t		flags;
  uint8_t		object_type;
  uint8_t		revision_day;
  uint8_t		revision_month;
  uint16_t		revision_year;
  char			name[9];
  uint8_t		device_type;
  uint8_t		data_type;
  uint8_t		hw_id;
  uint32_t		data_length;

  uint8_t*		data_part;	// TI9x only
  int			num_pages;	// TI8x only
  FlashPage**	pages;		// TI8x only

  FlashContent*	next;		// TI9x only
};

/* Functions */

// namespace scheme: library_class_function like tifiles_fext_get

#ifdef __cplusplus
extern "C" {
#endif

  /*********************/
  /* General functions */
  /*********************/

  // filetypes.c
  TIEXPORT2 int TICALL tifiles_file_is_ti (const char *filename);
  TIEXPORT2 int TICALL tifiles_file_is_os(const char *filename);
  TIEXPORT2 int TICALL tifiles_file_is_tib (const char *filename);
  TIEXPORT2 int TICALL tifiles_file_is_flash (const char *filename);
  TIEXPORT2 CalcModel TICALL tifiles_file_get_model (const char *filename);

  // filesXX.c
  TIEXPORT2 FlashContent* TICALL tifiles_content_create_flash(CalcModel model);
  TIEXPORT2 int           TICALL tifiles_content_delete_flash(FlashContent *content);
  TIEXPORT2 int TICALL tifiles_file_read_flash(const char *filename, FlashContent *content);

#ifdef __cplusplus
}
#endif

#endif
