
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
    Boolean archive;         // we want to build an archive
    Boolean quill;           // we want to build a Quill project
    Boolean pack;            // we want to pack the executable
    Boolean bsr_patch;       // use BSR patches
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
    QString prj_name;
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
  tprSettings settings;
  tprLibOpts libopts;
  QStringList h_files;
  QStringList c_files;
  QStringList s_files;
  QStringList asm_files;
  QStringList o_files;
  QStringList a_files;
  QStringList txt_files;
  QStringList oth_files;
} TPRDataStruct;

#define loadTPR(fileName) (loadTPRIndirect(fileName,&TPRData))
extern TPRDataStruct TPRData;
short loadTPRIndirect(QString &fileName,TPRDataStruct *dest);

QString loadFileText(const char *fileName);
