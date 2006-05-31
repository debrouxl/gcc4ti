/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2006 Kevin Kofler
   Copyright (C) 2006 Joey Adams

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

#include <cstddef>
#include <kconfig.h>
#include <kaboutdata.h>

extern const char *tigcc_base;
extern const char *quill_drv;
extern bool have_fargo;
extern bool have_flashos;
extern char tempdir[];
extern void write_temp_file(const char *filename, const char *data, const size_t len);
extern void delete_temp_file(const char *filename);
extern void force_qt_assistant_page(int n);
extern KConfig *pconfig;
extern KAboutData *pabout;
extern const char *parg;
