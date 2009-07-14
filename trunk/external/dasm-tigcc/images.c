/* Hey EMACS -*- linux-c -*- */
/* $Id: images.c 2417 2007-03-29 01:42:01Z roms $ */

/*  TiEmu - Tiemu Is an EMUlator
 *
 *  Copyright (c) 2000-2001, Thomas Corvazier, Romain Lievin
 *  Copyright (c) 2001-2003, Romain Lievin
 *  Copyright (c) 2003, Julien Blache
 *  Copyright (c) 2004, Romain Liévin
 *  Copyright (c) 2005-2007, Romain Liévin, Kevin Kofler
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
 *  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301, USA.
 */

/*
	This module handles loading of images or upgrades.
	Images can be:
	- ROM dump
	- FLASH upgrade as a ROM dump
	
  	Note:0x12000 is the beginning of the system privileged part.
*/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "tifiles.h"
#include "images.h"
#include "macros.h"
#include "hwpm.h"
#include "ti68k_def.h"
#include "ti68k_err.h"
#include "mem_size.h"

#define _(x) (x)
#define tiemu_info(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))
#define tiemu_warning(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))
#define tiemu_error(x...) (fprintf(stderr, x),fprintf(stderr, "\n"))

#define is_num(c)   isdigit(c)
#define is_alnum(c) isalnum(c)

#define SPP	0x12000		// system privileged part
#define BO  0x88        // offset from SPP to boot

static int get_rom_version(char *ptr, int size, char *version);

/*
	Utility functions
*/
int ti68k_is_a_rom_file(const char *filename)
{
	char *ext;

	ext = strrchr(filename, '.');
	if(ext == NULL)
		return 0;
	else if(!strcasecmp(ext, ".rom"))
		return !0;

	return 0;
}

int ti68k_is_a_tib_file(const char *filename)
{
	return tifiles_file_is_os(filename);
}

int ti68k_is_a_img_file(const char *filename)
{
	char *ext;

	ext = strrchr(filename, '.');
	if(ext == NULL)
		return 0;
	else if(!strcasecmp(ext, ".img"))
		return !0;

	return 0;
}

int ti68k_is_a_sav_file(const char *filename)
{
	char *ext;

	ext = strrchr(filename, '.');
	if(ext == NULL)
		return 0;
	else if(!strcasecmp(ext, ".sav"))
		return !0;

	return 0;
}

/*
	Get some information on the ROM dump:
	- size
	- ROM base address
	- FLASH/EPROM
	- os version
	- calc type
	Note: if the data field is NULL, memory is allocated. 
	Otherwise, data is overwritten.
	Thanks to Kevin for HW2 detection code.
*/
int ti68k_get_rom_infos(const char *filename, IMG_INFO *rom, int preload)
{
  	FILE *file;
    HW_PARM_BLOCK hwblock;

	// Open file
  	file = fopen(filename, "rb");
  	if(file == NULL)
    {
      tiemu_info(_("Unable to open this file: <%s>"), filename);
      return ERR_CANT_OPEN;
    }

	// Clear infos
	memset(rom, 0, sizeof(IMG_INFO));

  	// Retrieve ROM size
  	fseek(file, 0, SEEK_END);
  	rom->size = ftell(file);
  	fseek(file, 0, SEEK_SET);

  	if(rom->size < 256) 
    	return ERR_INVALID_ROM_SIZE;
	if (rom->size == 8*MB)
	{
	  // TiLP used to dump 8 MB images for HW4, try to load them anyway.
	  tiemu_info(_("/* Warning: truncating 8 MB image to 4 MB: <%s> */"), filename);
	  rom->size = 4*MB;
	}
  	if (rom->size > 4*MB)
    	return ERR_INVALID_ROM_SIZE;
  
	if(rom->data == NULL)
  		rom->data = malloc(rom->size + 4);
	if(rom->data == NULL)
		return ERR_MALLOC;
	memset(rom->data, 0xff, rom->size);
	if (fread(rom->data, 1, rom->size, file) < (size_t)rom->size)
	{
	  tiemu_info(_("Failed to read from file: <%s>"), filename);
	  fclose(file);
	  return ERR_CANT_OPEN;
	}
	if (fclose(file))
	{
	  tiemu_info(_("Failed to close file: <%s>"), filename);
	  return ERR_CANT_OPEN;
	}

    rom->has_boot = 1;
    rom->rom_base = rom->data[0x05] & 0xf0;
  	rom->flash = (rom->data[0x65] & 0x0f) ? 0 : FLASH_ROM;

    get_rom_version(rom->data, rom->size, rom->version);

    if(!rom->flash)
    {
        rom->calc_type = TI92;
        rom->hw_type = HW1;
    }
    else
    {
        // Get hw param block to determine calc type & hw type
        if(ti68k_get_hw_param_block((uint8_t*)rom->data, rom->rom_base, 
				    &hwblock) == -1)
	    return ERR_INVALID_ROM;

        switch(hwblock.hardwareID)
        {
        case HWID_TI92P: rom->calc_type = TI92p; break;
        case HWID_TI89: rom->calc_type = TI89;  break;
        case HWID_V200: rom->calc_type = V200;  break;
        case HWID_TI89T: rom->calc_type = TI89t; break;
        default: break;
        }

        if(rom->flash)
        {
            if(hwblock.len < 24)
                rom->hw_type = HW1;
            else
                rom->hw_type = (char)hwblock.gateArray;
        }
    }

	if(!preload)
		free(rom->data);

	return 0;
}


/*
  Get some information on the FLASH upgrade:
  - size
  - ROM base address
  - os version
  - calc type
*/
int ti68k_get_tib_infos(const char *filename, IMG_INFO *tib, int preload)
{
	FlashContent *content;
	FlashContent *ptr;
	int nheaders = 0;
	int i;

	// Check valid file
	if(!tifiles_file_is_ti(filename))
		return ERR_NOT_TI_FILE;
		
	if(!tifiles_file_is_os(filename))
		return ERR_INVALID_UPGRADE;

	// Clear infos
	memset(tib, 0, sizeof(IMG_INFO));

	// Load file
	content = tifiles_content_create_flash(CALC_TI89);
	if(tifiles_file_read_flash(filename, content) != 0)
        return ERR_INVALID_UPGRADE;
	
	// count headers
  	for (ptr = content; ptr != NULL; ptr = ptr->next)
    	nheaders++;
  	
  	// keep the last one (data)
  	for (i = 0, ptr = content; i < nheaders - 1; i++)
    	ptr = ptr->next;
    	
  	// Load TIB into memory and relocate at SPP
	if(tib->data == NULL)
  		tib->data = malloc(SPP + ptr->data_length + 4);
	if(tib->data == NULL)
		return ERR_MALLOC;

    memset(tib->data + SPP, 0xff, ptr->data_length);
  	memcpy(tib->data + SPP, ptr->data_part, ptr->data_length);
  	
  	// Update current rom infos
    tib->rom_base = tib->data[BO+5 + SPP] & 0xf0;

	// libtifiles can't distinguish TI89/TI89t and 92+/V200. We need to look.
	switch(ptr->device_type & 0xff)
	{
		case DEVICE_TYPE_89:    // can be a Titanium, too
            switch(tib->rom_base & 0xff)
            {
            case 0x20: tib->calc_type = TI89;  break;
            case 0x80: tib->calc_type = TI89t; break;
            default: return ERR_INVALID_UPGRADE;
            }
		break;
		case DEVICE_TYPE_92P:
            switch(tib->rom_base & 0xff)
            {
            case 0x20: tib->calc_type = V200;  break;
            case 0x40: tib->calc_type = TI92p; break;
            default: return ERR_INVALID_UPGRADE;
            }
		break;
		default:
			tiemu_info("TIB problem: %02x!\n", 0xff & ptr->device_type);
			return ERR_INVALID_UPGRADE;
		break;
	}
    
  	tib->flash = FLASH_ROM;
  	tib->has_boot = 0;
  	tib->size = ptr->data_length + SPP;

  	get_rom_version(tib->data, tib->size, tib->version);
  	
  	tifiles_content_delete_flash(content);
	if(!preload)
		free(tib->data);

  	return 0;
}

/*
	Try to get some information on the ROM dump:
	- size
	- ROM base address
	- FLASH/EPROM
	- soft version
	- calc type
*/
int ti68k_get_img_infos(const char *filename, IMG_INFO *ri)
{
	FILE *f;

	// Check file
	if(!ti68k_is_a_img_file(filename))
	{
		tiemu_warning("Images must have '.img' extension (%s).\n",
			filename);
		return ERR_CANT_OPEN;
	}
	
	// Open dest file
  	f = fopen(filename, "rb");
  	if(f == NULL)
    {
      	tiemu_warning("Unable to open this file: <%s>\n", filename);
      	return ERR_CANT_OPEN;
    }
    
    // Read header
    if (fread(ri, sizeof(IMG_INFO), 1, f) < 1)
    {
      tiemu_warning("Failed to read from file: <%s>\n", filename);
      fclose(f);
      return ERR_CANT_OPEN;
    }

    if(strcmp(ri->signature, IMG_SIGN) || ri->size > 4*MB)
    {
      tiemu_warning("Bad image: <%s>\n", filename);
      return ERR_INVALID_UPGRADE;
    }

    // Close file
    if (fclose(f))
    {
      tiemu_warning("Failed to close file: <%s>\n", filename);
      return ERR_CANT_OPEN;
    }
    
    return 0;
}

const int ti_rom_sizes[] = { ROM_SIZE_TI92_I, ROM_SIZE_TI89, ROM_SIZE_TI92P, ROM_SIZE_V200, ROM_SIZE_TI89T };

static int log_b2(int i)
{
    int j, v;

    for(j = 0, v = i; v != 0; v >>= 1, j++);

    return j-1;
}

int ti68k_get_rom_size(int calc_type)
{
    if(calc_type > CALC_MAX)
    {
        tiemu_error(_("Bad argument!"));
        exit(0);
    }

    return ti_rom_sizes[log_b2(calc_type)];
}

/*
	Convert an upgrade into an image.
  	The image has neither boot block nor certificate.
*/
int ti68k_convert_tib_to_image(IMG_INFO *src, unsigned char *dst)
{
	int i, j;
	int num_blocks, last_block;
	int real_size;
	HW_PARM_BLOCK hwpb;

	// Fill header
	real_size = src->size - SPP;
	src->size = ti68k_get_rom_size(src->calc_type);

	// Write boot block
	memcpy(src->data, &src->data[SPP + BO], 256);
	memcpy(dst,src,256);
	dst+=256;

    // Write hardware param block
	// fill structure
	hwpb.len = 24;
	switch(src->calc_type)
	{
		case TI89:
			hwpb.hardwareID = HWID_TI89;
			hwpb.hardwareRevision = src->hw_type - 1;
			break;
		case TI92p:
			hwpb.hardwareID = HWID_TI92P;
			hwpb.hardwareRevision = src->hw_type - 1;
			break;
		case V200:
			hwpb.hardwareID = HWID_V200;
			hwpb.hardwareRevision = 2;
			break;
		case TI89t:
			hwpb.hardwareID = HWID_TI89T;
			hwpb.hardwareRevision = 2;
			break;
	}
	hwpb.bootMajor = hwpb.bootRevision = hwpb.bootBuild = 1;
	hwpb.gateArray = src->hw_type;
	ti68k_put_hw_param_block((uint8_t *)src->data, src->rom_base, &hwpb);

	// write filler
	*(dst++)=0xfe; *(dst++)=0xed; *(dst++)=0xba; *(dst++)=0xbe;
	//fwrite(&hwpb, 1hwpb.len+2, f);

	// write address (pointer)
	*(dst++)=0x00;
	*(dst++)=src->rom_base;
	*(dst++)=0x01;
	*(dst++)=0x08;

	// write structure
	*(dst++)=MSB(hwpb.len);
	*(dst++)=LSB(hwpb.len);
	*(dst++)=MSB(MSW(hwpb.hardwareID));
	*(dst++)=LSB(MSW(hwpb.hardwareID));
	*(dst++)=MSB(LSW(hwpb.hardwareID));
	*(dst++)=LSB(LSW(hwpb.hardwareID));
	*(dst++)=MSB(MSW(hwpb.hardwareRevision));
	*(dst++)=LSB(MSW(hwpb.hardwareRevision));
	*(dst++)=MSB(LSW(hwpb.hardwareRevision));
	*(dst++)=LSB(LSW(hwpb.hardwareRevision));
	*(dst++)=MSB(MSW(hwpb.bootMajor));
	*(dst++)=LSB(MSW(hwpb.bootMajor));
	*(dst++)=MSB(LSW(hwpb.bootMajor));
	*(dst++)=LSB(LSW(hwpb.bootMajor));
	*(dst++)=MSB(MSW(hwpb.hardwareRevision));
	*(dst++)=LSB(MSW(hwpb.hardwareRevision));
	*(dst++)=MSB(LSW(hwpb.hardwareRevision));
	*(dst++)=LSB(LSW(hwpb.hardwareRevision));
	*(dst++)=MSB(MSW(hwpb.bootBuild));
	*(dst++)=LSB(MSW(hwpb.bootBuild));
	*(dst++)=MSB(LSW(hwpb.bootBuild));
	*(dst++)=LSB(LSW(hwpb.bootBuild));
	*(dst++)=MSB(MSW(hwpb.gateArray));
	*(dst++)=LSB(MSW(hwpb.gateArray));
	*(dst++)=MSB(LSW(hwpb.gateArray));
	*(dst++)=LSB(LSW(hwpb.gateArray));

	// Fill with 0xff up-to System Part
	for(i = 0x108 + hwpb.len+2; i < SPP; i++)
		*(dst++)=0xff;
 
	// Copy FLASH upgrade at 0x12000 (SPP)
	num_blocks = real_size / 65536;
	for(i = 0; i < num_blocks; i++ )
	{
		memcpy(dst,&src->data[65536 * i + SPP],65536);
		dst+=65536;
	}

	last_block = real_size % 65536;
	memcpy(dst,&src->data[65536 * i + SPP],last_block);
	dst+=last_block;
 
	for(j = SPP + real_size; j < src->size; j++)
		*(dst++)=0xff;

	return 0;
}


/*
  	This function loads an image.
*/
int ti68k_load_image(const char *filename, IMG_INFO *img)
{
  	FILE *f;  	
  	int err;

	// Clear infos
	memset(img, 0, sizeof(IMG_INFO));

	// Load infos
	err = ti68k_get_img_infos(filename, img);
  	if(err)
    {
      	tiemu_info(_("Unable to get information on image: %s"), filename);
      	return err;
    }
	
	// Open file
	f = fopen(filename, "rb");
	if(f == NULL)
	{
		tiemu_warning("Unable to open this file: <%s>\n", filename);
		return ERR_CANT_OPEN;
	}

	// Read pure data
	if (fseek(f, img->header_size, SEEK_SET))
	{
		tiemu_warning("Failed to read from file: <%s>\n", filename);
		fclose(f);
		return ERR_CANT_OPEN;
	}

	img->data = malloc(img->size + 4);
	if(img->data == NULL)
		return ERR_MALLOC;
	if (fread(img->data, 1, img->size, f) < (size_t)img->size)
	{
		tiemu_warning("Failed to read from file: <%s>\n", filename);
		fclose(f);
		return ERR_CANT_OPEN;
	}

#if 1
	{
		HW_PARM_BLOCK hwblock;

		ti68k_get_hw_param_block((uint8_t *)img->data, img->rom_base, 
					 &hwblock);
	}
#endif

	if (fclose(f))
	{
		tiemu_warning("Failed to close file: <%s>\n", filename);
		return ERR_CANT_OPEN;
	}

	return 0;
}

/*
  	Search the version string in the ROM
	Arguments:
  	- ptr: a ROM or update image
  	- size: the size of the buffer
  	- version: the returned string version
*/
static int get_rom_version(char *ptr, int size, char *version)
{
  	int i;

  	strcpy(version, "?.??");

  	for (i = SPP; i < size-16; i += 2)
    {
      if (is_num(ptr[i])&&(ptr[i+1]=='.') && is_num(ptr[i+2]) &&
	  (ptr[i+3]==0)&&is_alnum(ptr[i+4]) && is_alnum(ptr[i+5]) &&
	  (ptr[i+6]=='/')&&is_alnum(ptr[i+7]) && is_alnum(ptr[i+8]) &&
	  (ptr[i+9]=='/')&&is_alnum(ptr[i+10]) && is_alnum(ptr[i+11]))
	  	break;

      if (is_num(ptr[i]) && (ptr[i+1]=='.') && is_num(ptr[i+2]) && 
	  is_num(ptr[i+3]) && (ptr[i+4]==0) && is_alnum(ptr[i+5]) && 
	  is_alnum(ptr[i+6]) && (ptr[i+7]=='/') && is_alnum(ptr[i+8]) && 
	  is_alnum(ptr[i+9]) && (ptr[i+10]=='/') && is_alnum(ptr[i+11]) && 
	  is_alnum(ptr[i+12]))
		break;
	
	  if (is_num(ptr[i]) && (ptr[i+1]=='.') && is_num(ptr[i+2]) && 
	  (ptr[i+3]==0) && is_alnum(ptr[i+4]) && is_alnum(ptr[i+5]) && 
	  is_alnum(ptr[i+6]) && is_alnum(ptr[i+7]) && is_alnum(ptr[i+8]) && 
	  is_alnum(ptr[i+9]) && is_alnum(ptr[i+10]) && is_alnum(ptr[i+11]))
	  	break;

      if (is_num(ptr[i]) && (ptr[i+1]=='.') && is_num(ptr[i+2]) && 
	  is_alnum(ptr[i+3]) && (ptr[i+4]==0) && is_alnum(ptr[i+5]) && 
	  is_alnum(ptr[i+6]) && is_alnum(ptr[i+7]) && is_alnum(ptr[i+8]) && 
	  is_alnum(ptr[i+9]) && is_alnum(ptr[i+10]) && is_alnum(ptr[i+11]))
	  	break;
    }
  
  	if (i < size-16) 
    {
      	int n;
      
      	for(n = i; n < i+16; n++) 
		{
	  		if (ptr[n]==0) 
	    	{
	      		strcpy(version, ptr+i);
	      		(version)[n-i]=0;

	      		return 0;
	    	}
		}
    }
    
  	return 0;
}

