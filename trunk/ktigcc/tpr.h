/*
   ktigcc - TIGCC IDE for KDE
   
   tpr handling routines adapted from tprbuilder
   Copyright (C) 2002 Romain Liévin
   Copyright (C) 2002-2006 Kevin Kofler
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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*Structure definitions from tprbuilder source; slightly modified
    to work under C++
*/

typedef unsigned char Boolean;

/* global enumerations */
enum SectionType_ { SECTION_NONE, SECTION_SETTINGS, SECTION_LIBOPTS,
                    SECTION_FILEEDIT, SECTION_FILES };
typedef enum SectionType_ SectionType;

/* definitions */
typedef struct tprsettings tprSettings;
struct tprsettings
{
    tprsettings() : archive(0),pack(0),debug_info(0),std_lib(1),
                    use_data_var(0),copy_data_var(1),copy_data_var_arc(1),
                    optimize_nops(1),optimize_returns(1),optimize_branches(1),
                    optimize_moves(1),optimize_tests(1),optimize_calcs(1),
                    remove_unused(1),outputbin(0),fargo(0),flash_os(0),
                    cut_ranges(1),reorder_sections(1),merge_constants(1),
                    initialize_bss(1),
                    cc_switches("-Os -Wall -W -Wwrite-strings -ffunction-sections -fdata-sections"),
                    as_switches(""),
                    a68k_switches("-g -t")
                  {}
    Boolean archive;         // we want to build an archive
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

    QString pack_name;
    QString cc_switches;
    QString as_switches;
    QString a68k_switches;
    QString cmd_line;
    QString post_build;
    QString data_var;
};

enum tprreloctypes {
  RT_NONE, // None (BSS, data var)
  RT_DIRECT, // Direct (ROM_CALLs)
  RT_AMS, // AMS (relocs)
  RT_PRECOMP, // ROM_CALLs only: Precomputed (Optimized)
  RT_KERNEL, // Kernel
  RT_COMPRESSED, // Compressed
  RT_MLINK, // MLink
  RT_FLINE // ROM_CALLs only: F-Line
};
typedef enum tprreloctypes tprRelocType;

typedef struct tprlibopts tprLibOpts;
struct tprlibopts
{
    tprlibopts() : use_ti89(1),use_ti92p(1),use_v200(1),opt_calc_consts(0),
                   use_kernel(0),use_preos(0),use_minams(1),
                   unofficial_os(0),use_fline_jumps(0),use_4b_fline_jumps(0),
                   use_internal_fline_emu(0),use_return_value(0),
                   enable_error_return(0),save_screen(1),opt_rom_calls(0),
                   minams(100),reloc_format(RT_AMS),rom_call_format(RT_DIRECT),
                   bss_ref_format(RT_KERNEL),data_ref_format(RT_KERNEL)
                 {}
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

    tprRelocType reloc_format;
    tprRelocType rom_call_format;
    tprRelocType bss_ref_format;
    tprRelocType data_ref_format;
};

typedef struct
{
  QStringList path;
  QStringList folder;
} TPRFileList;

typedef struct
{
  QString prj_name;
  QString open_file;
  tprSettings settings;
  tprLibOpts libopts;
  TPRFileList h_files;
  TPRFileList c_files;
  TPRFileList s_files;
  TPRFileList asm_files;
  TPRFileList o_files;
  TPRFileList a_files;
  TPRFileList txt_files;
  TPRFileList oth_files;
  TPRFileList quill_files;
} TPRDataStruct;

enum {PATH_ERROR,PATH_FILE,PATH_FOLDER}; //return types for getPathType

const char *smartAscii(const QString &s);

void newSettings(tprSettings *settings,tprLibOpts *libopts);

int loadTPR(const QString &fileName,TPRDataStruct *dest);
QString loadFileText(const char *fileName);

int saveTPR(const QString &fileName,TPRDataStruct *src);
int saveFileText(const char *fileName,const QString &fileText);

void kurlNewFileName(KURL &dir,const QString &newFileName);
int checkFileName(const QString &fileName,const QStringList &fileNameList);

int copyFile(const char *src, const char *dest);

int getPathType(const QString &thePath);
