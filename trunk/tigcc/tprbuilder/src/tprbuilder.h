/*  TPRbuilder - A make program for TIGCC project files (tpr).
 *
 *  Copyright (C) 2002 Romain Liévin
 *  Copyright (C) 2002-2006 Kevin Kofler
 *  Copyright (C) 2005 Lionel Debroux
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributelabeld in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

#ifndef TPRBUILDER_H
#define TPRBUILDER_H

/* global enumerations */
enum Boolean_ 	{FALSE,TRUE};
typedef enum Boolean_ Boolean;

#define EXIT_FAILURE 1

/* global enumerations */
enum SectionType_ { SECTION_NONE, SECTION_SETTINGS, SECTION_LIBOPTS,
                    SECTION_FILEEDIT, SECTION_FILES };
typedef enum SectionType_ SectionType;

/* definitions */
typedef struct settings Settings;
struct settings
{
    Boolean archive;         // we want to build an archive
    Boolean quill;           // we want to build a Quill project
    Boolean pack;            // we want to pack the executable
    Boolean debug_info;      // pass -g option to tigcc
    Boolean std_lib;         // link against tigcc.a
    Boolean use_data_var;
    Boolean copy_data_var;
    Boolean copy_data_var_arc;
    Boolean optimize_nops;
    Boolean optimize_returns;
    Boolean optimize_branches;
    Boolean optimize_moves;
    Boolean optimize_tests;
    Boolean optimize_calcs;
    Boolean remove_unused;
    Boolean outputbin;
    Boolean fargo;
    Boolean flash_os;
    Boolean cut_ranges;
    Boolean reorder_sections;
    Boolean merge_constants;
    Boolean initialize_bss;

    char *pack_name;
    char *prj_name;
    char *cc_switches;
    char *cmdline_cc_switches;
    char *as_switches;
    char *a68k_switches;
    char *post_build;
    char *data_var;
};

typedef enum reloctypes RelocType;
enum reloctypes {
  RT_NONE, // AMS (relocs) / Direct (ROM_CALLs) / None (BSS), not for DataVar
  RT_PRECOMP, // ROM_CALLs only: Precomputed (Optimized)
  RT_KERNEL, // Kernel
  RT_COMPRESSED, // Compressed
  RT_MLINK, // MLink
  RT_FLINE // ROM_CALLs only: F-Line
};

typedef struct libopts LibOpts;
struct libopts
{
    Boolean use_ti89;
    Boolean use_ti92p;
    Boolean use_v200;
    Boolean opt_calc_consts;
    Boolean use_kernel;
    Boolean use_preos;
    Boolean use_minams;
    Boolean unofficial_os;
    Boolean use_fline_jumps;
    Boolean use_4b_fline_jumps;
    Boolean use_internal_fline_emu;
    Boolean use_return_value;
    Boolean enable_error_return;
    Boolean save_screen;
    Boolean opt_rom_calls;

    int minams;

    RelocType reloc_format;
    RelocType rom_call_format;
    RelocType bss_ref_format;
    RelocType data_ref_format;
};

/* Local declarations */
static const char *file_extension(const char *filename);
static int decode_switches (int argc, char **argv);
__attribute__((noreturn)) static void usage (int status);
static void safe_exit(void);
__attribute__((noreturn)) static void outofmem(void);
static char *dynstrcat(char *s, const char *t);

int delete(char *filename);
int execute(char *cmdline);

int dyn_file_array_size(char **array, int *size);
int dyn_file_array_add(char *file, char ***array, int *file_count);
void dyn_file_array_free(char **files);

char* encapsulate_long_filename(const char *file);
char* tail_long_filename(char *file);
char *change_extension(char *file, const char *newext);

char* strip(char *str);
void  stop (int line);
#define find_param(s_,t_) find_param_ex((s_),(t_),sizeof(t_)-1)
char *find_param_ex(char *s, const char *t, size_t l);
char *find_numbered_param(char *s, const char *t, int *i);

int process_settings(char *outfile, char **pargs);
int parse_file(const char *filename);
int process_files(char **pfiles);

void build_files(char *outfile);
void clean_files(char *outfile);

#endif
