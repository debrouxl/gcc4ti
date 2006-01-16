
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
    tprsettings() : archive(0),pack(0),debug_info(0),std_lib(0),
                    use_data_var(0),copy_data_var(0),copy_data_var_arc(0),
                    optimize_nops(1),optimize_returns(1),optimize_branches(1),
                    optimize_moves(1),optimize_tests(1),optimize_calcs(1),
                    remove_unused(1),outputbin(0),fargo(0),flash_os(0),
                    cut_ranges(1),reorder_sections(1),merge_constants(1),
                    initialize_bss(1)
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
  RT_NONE, // AMS (relocs) / Direct (ROM_CALLs) / None (BSS), not for DataVar
  RT_PRECOMP, // ROM_CALLs only: Precomputed (Optimized)
  RT_KERNEL, // Kernel
  RT_COMPRESSED, // Compressed
  RT_FLINE // ROM_CALLs only: F-Line
};
typedef enum tprreloctypes tprRelocType;

typedef struct tprlibopts tprLibOpts;
struct tprlibopts
{
    tprlibopts() : use_ti89(0),use_ti92p(0),use_v200(0),opt_calc_consts(0),
                   use_kernel(0),use_preos(0),use_minams(0),
                   unofficial_os(0),use_fline_jumps(0),use_4b_fline_jumps(0),
                   use_internal_fline_emu(0),use_return_value(0),
                   enable_error_return(0),save_screen(0),opt_rom_calls(0),
                   minams(0),reloc_format(RT_NONE),rom_call_format(RT_NONE),
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

int loadTPR(QString &fileName,TPRDataStruct *dest);

QString loadFileText(const char *fileName);
