/*  TIGCC - A front-end for the compiler, assembler, linker and some other
 *  stuffs.
 *  Copyright (C) 2001 John David Ratliff
 *  Copyright (C) 2001-2002 Romain Liévin
 *  Copyright (C) 2002-2006 Kevin Kofler
 *  Modified by Nils Gesbert, 2003
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

#ifndef _TIGCC_H
#define _TIGCC_H

#define VERSION "1.3.2"
#define TIGCC_VERSION "0.96 Beta 8 r1"

/* global enumerations */
enum Boolean 	{FALSE,TRUE};
enum FileTypes	{BADFILE,CFILE,ASMFILE,SFILE,OFILE,AFILE,QLLFILE};

/* global definitions */
#define REDIRECT		"tigcc.log"
#define DEVNULL                 "/dev/null"

#define TMP_PCK                 "tempprog.pck"

/* constants */
// #define MAX_ARRAY_FILES	128
/* (NG) Should be OK : */
#define MAX_ARRAY_FILES argc

/* global arrays */
char **gcc_argv = NULL;
char **ld_argv  = NULL;

typedef struct {
  unsigned short count;
  char *files[0];
} array_of_files;

array_of_files *src_files    = NULL;
array_of_files *obj_files    = NULL;
array_of_files *asm_files    = NULL;
array_of_files *a68k_files   = NULL;
array_of_files *ar_files     = NULL;

const char *tigcc_args[] =
{
  "--version", "-V", "-h", "--help", "-q", "--quiet", "-v", "--verbose", "-v0",
  "-E", "-S", "-c", "-pack", "-bsr", "-outputbin", "--outputbin", "-standalone",
  "--standalone", "-o", "--output", "-r", "--redirect", "-g", "--debug", "-Wa,",
  "-WA,", "-np", "-ar", "-quill", "-keep", "--keep", "-save-temps",
  "--save-temps", "--native", "--fargo", "--flash-os", "--remove-unused",
  "--optimize-relocs", "--optimize-code", "--optimize-nops",
  "--optimize-returns", "--optimize-branches", "--optimize-moves",
  "--optimize-tests", "--optimize-calcs", "--cut-ranges", "--reorder-sections",
  "--merge-constants", "--all-relocs", "--omit-bss-init", "-n", "--varname",
  "-d", "--data-var", "--data-var-copy=never", "--data-var-copy=always",
  "--data-var-copy=archived", "--dump", "--dump0", "--dump1", "--dump2",
  "--dump3", "--dump4", "--dump5", "--dump6", "--dump7", "--dump8", "--dump9",
  NULL
};

/* global vars */
char *outfile = NULL;
char packfile[9];
char *tigcc_base = NULL;

char *as_args   = NULL;
char *a68k_args = NULL;

/* global control variables */
short int verbose = FALSE, quiet = FALSE, redirect = FALSE, delete = TRUE;
short int debug = FALSE, nostdlib = FALSE, outputbin = FALSE, do_pack = FALSE;
short int do_compile = TRUE, do_assemble = TRUE, do_link = TRUE, staticlib = FALSE;
short int keepobj = FALSE, savetemps = FALSE, fargo = FALSE, flashos = FALSE;
short int allrelocs = FALSE, nomergesections = FALSE, optreturns = FALSE;
short int printcommands = FALSE;

short int gcc_argc = 1;
short int ld_argc = 1;

/* internal/developer use only */
short int patch = TRUE;

#endif
