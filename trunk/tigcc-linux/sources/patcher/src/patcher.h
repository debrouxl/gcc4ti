/*
   tigcc_patch - Parse assembly file and apply patches.

   Copyright (C) 2002 Romain Liévin
   Copyright (C) 2002-2003 Kevin Kofler

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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#ifndef PATCHER_H
#define PATCHER_H

/* internal functions */
static void usage (int status);
static int decode_switches (int argc, char **argv);
static const char *file_extension (const char *filename);
static void output_line(unsigned char *buffer, FILE *outfile, int *pfline_ROM_CALLs, int *preg_relative);
static char *change_extension(char *file, const char *newext);

/* The name the program was run with, stripped of any leading path. */
char *program_name;

#endif
