/*
   tprbuilder - tprbuilder is a MAKE program for TIGCC projects (.tpr files)

   Copyright (C) 2002 Romain Liévin
   Copyright (C) 2002-2006 Kevin Kofler
   Copyright (C) 2005 Lionel Debroux

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "tprbuilder.h"

#define EXIT_FAILURE 1

/* The name the program was run with, stripped of any leading path. */
char *program_name;

/* global arrays */
char **c_files    = NULL;
char **s_files    = NULL;
char **h_files    = NULL;
char **o_files    = NULL;
char **a_files    = NULL;
char **asm_files  = NULL;

/* global vars */
char *tigcc_base = NULL;
int c_file_count=0, s_file_count=0, m_file_count=0, h_file_count=0,
    asm_file_count=0, a_file_count=0, o_file_count=0;
Settings settings = {0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0};
LibOpts libopts = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,100,RT_NONE,RT_NONE,RT_KERNEL,RT_KERNEL};

int clean = 0;
int verbose = 0;
int quiet = 0;

/*
   An implementation of system() for Win32 which doesn't use the crippled
   command.com to execute commands.
   Note that this implementation hardcodes the name of the program to launch. A
   complete implementation would have to read it from the command line.
*/
#ifdef __WIN32__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline int win32_system(const char *cmdline)
{
  char execname[MAX_PATH], *p;
  long exitcode;
  STARTUPINFO startupinfo={.cb=sizeof(startupinfo)};
  PROCESS_INFORMATION processinfo;
  _searchenv("tigcc.exe","PATH",execname);
  if (!*execname) return 1;
  while ((p=strchr(execname,'/'))) *p='\\';
  if (!CreateProcess(execname,(char *)cmdline,NULL,NULL,FALSE,0,NULL,NULL,
                     &startupinfo,&processinfo)) return 1;
  if (WaitForSingleObject(processinfo.hProcess,INFINITE)==WAIT_FAILED) return 1;
  if (!GetExitCodeProcess(processinfo.hProcess,&exitcode)) return 1;
  CloseHandle(processinfo.hProcess);
  CloseHandle(processinfo.hThread);
  return exitcode;
}

#undef system
#define system win32_system
#endif

/*
   Our main function
*/
int main(int argc, char **argv)
{
    int i;

    /* GNU stuffs */
    program_name = argv[0];

    i = decode_switches (argc, argv);

    /* Do some checkings */
    if (argc < 2) {
        fprintf(stderr, "tprbuilder: no input files\n");
        exit(0);
    }

    /* Register exit callback */
    if (atexit(safe_exit) != 0) {
        fprintf(stderr, "Fatal error: unable to register safe exit callback\n");
        exit(-1);
    }

    /* Parse for files */
    if (i < argc)
    {
        size_t l=strlen(argv[i])-3;
        char outfile[l];

        if(!strcmp(file_extension(argv[i]),".tpr")) {
            parse_file(argv[i]);
        }
        else {
            usage(EXIT_FAILURE);
        }

        strncpy(outfile,argv[i],l-1);
        outfile[l-1]=0;

        /* Build files */
        if(!clean) {
            build_files(outfile);
        }

        /* Clean-up (if enabled) */
        if(clean) {
            clean_files(outfile);
        }
    } else usage(0);

    return 0;
}

/*
   Determine the size of a file array
*/
int dyn_file_array_size(char **array, int *size)
{
    char **p;

    *size = 0;
    if(array != NULL) {
        for(p=array; *p != NULL; p++, (*size)++);
    }

    return *size;
}

/*
   Add a filename to a filename array while maintaing a file count
*/
int dyn_file_array_add(char *file, char ***array, int *file_count)
{
    int len = strlen(file) + 1;
    int nfiles = 0;
    char **files = *array;

    // Determine size of array
    dyn_file_array_size(files, &nfiles);

    // Resize array
    files = (char **) realloc(files, (nfiles + 2) * sizeof(char *));
    if (files == NULL) outofmem();

    // Allocate and copy string
    if ((files[nfiles] = (char *)calloc(len, sizeof(char))) == NULL) outofmem();

    // Maintain array consistency
    strcpy(files[nfiles], file);
    files[++nfiles] = NULL;
    *file_count = nfiles;
    *array = files;

    return TRUE;
}

/*
   Free a file array and the associated strings
*/
void dyn_file_array_free(char **files)
{
    int i, count;

    if(files == NULL) return;
    dyn_file_array_size(files, &count);

    for(i=0; i<count; i++) {
        free(files[i]);
    }

    free(files);
}

/*
   Encapsulate long filenames with "".
   The returned string must be freed when no longer used
*/
char* encapsulate_long_filename(const char *file)
{
    char *s;

    if ((s = calloc(strlen(file)+1+2, sizeof(char))) == NULL) outofmem();

    if(!strchr(file, ' '))
        strcpy(s, file);
    else {
        strcpy(s, "\"");
        strcat(s, file);
        strcat(s, "\"");
    }

#ifndef __WIN32__
   {
       char *p;
       while ((p=strchr(s,'\\')))
           *p='/';
   }
#endif

    return s;
}

/*
  Remove beginning & ending '"' from files.
  The string is modified in place !
*/
char* tail_long_filename(char *file)
{
    char *p = file;

    if( (file[0] != '\"') && (file[strlen(file)] != '\"') )
        return file;

    p++;
    memmove(file, p, strlen(p)+1); /* remove first quote */
    p[strlen(p)-1] = '\0';         /* remove last quote */

    return file;
}

/* Return the filename extension or NULL */
static const char *file_extension(const char *filename)
{
  int i;
  const char *p;

  for(i=strlen(filename); i > 0; i--)
    {
      if(filename[i] == '.') break;
    }
  p=filename+i;

  return p;
}

/*
   Change filename extension and keep long filenames.
   If the filename has no extension, then the extension is append
   Dynamic allocation to do ...
*/
char *change_extension(char *file, const char *newext)
{
    char *start = (char *)strrchr(file, '.');
    if(start == NULL) {
        start = file; //return file;
    } else {
        sprintf(start, "%s", newext);
    }

    if(file[0] == '\"') strcat(start, "\"");

    return file;
}

/*
   Set all the option flags according to the switches specified.
   Return the index of the first non-option argument.
*/
static int decode_switches (int argc, char **argv)
{
  int c;
  char * cmdline_cc_switches=malloc(5);
  if (!cmdline_cc_switches) outofmem();

  cmdline_cc_switches[0] = 0;

  for (c=1;c<argc;c++) {
    if (!strcmp(argv[c],"-V")||!strcmp(argv[c],"--version")) {
       printf ("tprbuilder 1.0.17\n");
       exit(0);
    } else if (!strcmp(argv[c],"-h")||!strcmp(argv[c],"--help")) {
       usage(0);
    } else if (!strcmp(argv[c],"-c")||!strcmp(argv[c],"--clean")) {
       clean = 1;
    } else if (!strcmp(argv[c],"-q")||!strcmp(argv[c],"--quiet")) {
       quiet = 1;
    } else if (!strcmp(argv[c],"-v")||!strcmp(argv[c],"--verbose")) {
       verbose = 1;
    } else if (!strncmp(argv[c],"-D",2)) {
       cmdline_cc_switches = dynstrcat(cmdline_cc_switches,argv[c]);
       cmdline_cc_switches = dynstrcat(cmdline_cc_switches," ");
    } else break;
  }

  if (c>=argc) usage(EXIT_FAILURE);

  settings.cmdline_cc_switches = cmdline_cc_switches;

  return c;
}

/*
  Display usage options
*/
__attribute__((noreturn)) static void usage (int status)
{
    printf ("%s - \
tprbuilder is a MAKE program for TIGCC projects (.tpr files)\n", program_name);
    printf ("Usage: %s [OPTION]... [FILE]...\n", program_name);
    printf ("\
Options:\n\
  -h, --help                 display this help and exit\n\
  -V, --version              output version information and exit\n\
  -v, --verbose              enable verbose mode (make tigcc verbose)\n\
  -q, --quiet                enable quiet mode (display nothing)\n\
  -D[DEFINE]                 pass -D[DEFINE] to compiler\n\
  --clean                    do a cleanup of files\n\
");
    exit (status);
}

/*
   Destroy allocated resources (exit callback)
 */
void safe_exit(void)
{
    free(settings.pack_name);
    free(settings.prj_name);
    free(settings.cc_switches);
    if (settings.cmdline_cc_switches) free(settings.cmdline_cc_switches);
    free(settings.as_switches);
    free(settings.a68k_switches);
    free(settings.post_build);
    free(settings.data_var);

    dyn_file_array_free(c_files);
    dyn_file_array_free(s_files);
    dyn_file_array_free(h_files);
    dyn_file_array_free(asm_files);
    dyn_file_array_free(a_files);
}

/*
   Remove/unlink a file
*/
int delete(char *filename)
{
    if (!quiet) {
        fprintf(stderr, "tprbuilder: removing %s\n", filename);
    }

    tail_long_filename(filename);
    remove(filename);

    return 0;
}

/*
   Execute a program by calling the shell
*/
int execute(char *cmdline)
{
    if (!quiet) {
        fprintf(stderr, "tprbuilder: %s\n", cmdline);
    }

    if(system(cmdline) != 0) {
        exit(0);
    }

    return 0;
}

/*
  Execute tigcc
*/
void execute_tigcc(char *filename, char *args)
{
    char *cmdline; const char *quill_flag = "";

    /* We need to do this here because Win32 tigcc.exe only understands -quill
       at the beginning of the command line. */
    if(settings.quill == TRUE) quill_flag = "-quill ";

    cmdline = calloc(strlen(filename) + strlen(args) + 25, sizeof(char));
    if (!cmdline) outofmem();
    sprintf(cmdline, "tigcc %s%s %s", quill_flag, filename, args);
    if(verbose)
      strcat(cmdline, " --verbose");
    execute(cmdline);
    free(cmdline);
}


/*
   Strip CR and/or LF terminators
*/
char* strip(char *str)
{
    int len = strlen(str);

    if(len > 0)
        if( (str[len-1] == '\r') || (str[len-1] == '\n') )
            str[len-1] = '\0';

    if(len > 1)
        if( (str[len-2] == '\r') || (str[len-2] == '\n') )
            str[len-2] = '\0';

    return str;
}

/*
   Print an error msg
*/
void stop (int line)
{
    fprintf(stderr, "Configuration file error at line %i.\n", line);
}

/*
   Read a line from file and do a clean-up
*/
int read_line(FILE *f, char *buffer, int *l)
{
    strcpy(buffer, "");
    if(feof(f)) return EOF;
    fgets(buffer, 256, f);
    strip(buffer);
    (*l)++;
    while( (buffer[0] == '\r') || (buffer[0] == '\n') || (buffer[0] == '\0') )
    {
        if(feof(f)) return EOF;
        fgets(buffer, 256, f);
        strip(buffer);
        (*l)++;
    }

    return 0;
}


/*
   Find a token in a string and return the subsequent string
*/
char *find_param_ex(char *s, const char *t, size_t l)
{
  if (!strncmp(s,t,l))
    return s+l;
  else
    return NULL;
}


/*
   Same as above but this one can extract a number such as scanf
*/
char *find_numbered_param(char *s, const char*t, int *i)
{
    char *p;
    int endpos = 0;
    int ret = 0;
    char arglist[256];

    strcpy(arglist, t);
    strcat(arglist, "%n");

    ret = sscanf(s, arglist, i, &endpos);
    if(ret < 1 || !endpos) return NULL;

    p = s + endpos;
    return p;
}


/*
   Parse a TPR file, fill the Setting structure and the filename arrays
*/
int parse_file(const char *filename)
{
    FILE *f;
    char buffer[256];
    int l = 0;
    SectionType stype = SECTION_NONE;

    f = fopen(filename, "rt");
    if(f == NULL) {
        fprintf(stderr, "Unable to open this file: <%s>\n", filename);
        exit(-1);
    }

    while(!feof(f))
    {
        char *p;

        // Get a line from file
        if(read_line(f, buffer, &l) == EOF) break;

        // Search for sections
        if( !strcmp(buffer, "[Settings]") )
        {
            stype = SECTION_SETTINGS;
            continue;
        }

        if( !strcmp(buffer, "[Library Options]") )
        {
            stype = SECTION_LIBOPTS;
            continue;
        }

        if( !strcmp(buffer, "[File Editing]") )
        {
            stype = SECTION_FILEEDIT;
            continue;
        }

        if( !strcmp(buffer, "[Included Files]") )
        {
            stype = SECTION_FILES;
            continue;
        }

        // Keywords in the [Settings] section
        if(stype == SECTION_SETTINGS) {

#define boolean_param(token,setting) \
            if ( (p=find_param(buffer, token)) ) \
            { \
                if(!strcmp(p, "0")) settings.setting = FALSE; \
                else if(!strcmp(p, "1")) settings.setting = TRUE; \
                else stop(l); \
                continue; \
            } else

#define string_param(token,setting) \
            if ( (p=find_param(buffer, token)) ) \
            { \
                if (*p) settings.setting = strdup(p); \
                continue; \
            } else

#define ignore_param(token) \
            if( (p=find_param(buffer, token)) ) \
            { \
                continue; \
            } else

            boolean_param("Archive=",archive)
            boolean_param("Pack=",pack)
            string_param("Packed Variable=",pack_name)
            string_param("Project Name=",prj_name)
            string_param("GCC Switches=",cc_switches)
            string_param("Assembler Switches=",a68k_switches)
            ignore_param("Linker Switches=") // Obsolete. Ignore.
            ignore_param("GNU Linker Switches=") // Obsolete. Ignore.
            string_param("GNU Assembler Switches=",as_switches)
            ignore_param("BSR Patch=") // Obsolete. Ignore.
            boolean_param("Debug Info=",debug_info)
            boolean_param("Standard Library=",std_lib)
            ignore_param("Command Line=") // Used only for transferring. Ignore.
            string_param("Post-Build Process=",post_build)
            boolean_param("Use Data Variable=",use_data_var)
            string_param("Data Variable=",data_var)
            boolean_param("Copy Data Variable=",copy_data_var)
            boolean_param("Copy Data Variable if Archived=",copy_data_var_arc)
            boolean_param("Optimize NOPs=",optimize_nops)
            boolean_param("Optimize Returns=",optimize_returns)
            boolean_param("Optimize Branches=",optimize_branches)
            boolean_param("Optimize Moves=",optimize_moves)
            boolean_param("Optimize Tests=",optimize_tests)
            boolean_param("Optimize Calculations=",optimize_calcs)
            boolean_param("Remove Unused Sections=",remove_unused)
            boolean_param("Binary Output=",outputbin)
            boolean_param("Fargo=",fargo)
            boolean_param("Flash OS=",flash_os)
            boolean_param("Cut Unused Ranges=",cut_ranges)
            boolean_param("Reorder Sections=",reorder_sections)
            boolean_param("Merge Constants=",merge_constants)
            boolean_param("Initialize BSS=",initialize_bss)
            stop(l);

#undef boolean_param
#undef string_param
#undef ignore_param
        }

        // Keywords in the [Library Options] section
        if(stype == SECTION_LIBOPTS) {
#define boolean_param(token,setting) \
            if ( (p=find_param(buffer, token)) ) \
            { \
                if(!strcmp(p, "0")) libopts.setting = FALSE; \
                else if(!strcmp(p, "1")) libopts.setting = TRUE; \
                else stop(l); \
                continue; \
            } else

#define reloc_param(token,setting) \
            if ( (p=find_param(buffer, token)) ) \
            { \
                if (!strcmp(p,"None") || !strcmp(p,"AMS") \
                    || !strcmp(p,"Direct")) \
                    libopts.setting = RT_NONE; \
                else if (!strcmp(p, "Precomputed")) \
                    libopts.setting = RT_PRECOMP; \
                else if (!strcmp(p, "Kernel")) \
                    libopts.setting = RT_KERNEL; \
                else if (!strcmp(p, "Compressed")) \
                    libopts.setting = RT_COMPRESSED; \
                else if (!strcmp(p, "MLink")) \
                    libopts.setting = RT_MLINK; \
                else if (!strcmp(p, "F-Line")) \
                    libopts.setting = RT_FLINE; \
                else if (strcmp(p, "Unknown")) \
                    stop(l); \
                continue; \
            } else

            boolean_param("Use TI-89=",use_ti89)
            boolean_param("Use TI-92 Plus=",use_ti92p)
            boolean_param("Use V200=",use_v200)
            boolean_param("Optimize Calc Consts=",opt_calc_consts)
            boolean_param("Use Kernel=",use_kernel)
            boolean_param("Use PreOS=",use_preos)
            boolean_param("Minimum AMS Version Defined=",use_minams)
            if ( (p=find_param(buffer, "Minimum AMS Version=")) )
            {
                int major, minor;
                if ((strlen(p)==4) && (sscanf(p,"%1d.%2d",&major,&minor)==2))
                    libopts.minams = major * 100 + minor;
                else
                    stop(l);
                continue;
            } else
            boolean_param("Unofficial OS Support=",unofficial_os)
            reloc_param("Reloc Format=",reloc_format)
            reloc_param("ROM Call Format=",rom_call_format)
            reloc_param("BSS Ref Format=",bss_ref_format)
            reloc_param("Data Ref Format=",data_ref_format)
            boolean_param("Use F-Line Jumps=",use_fline_jumps)
            boolean_param("Use 4-Byte F-Line Jumps=",use_4b_fline_jumps)
            boolean_param("Use Internal F-Line Emulator=",use_internal_fline_emu)
            boolean_param("Use Return Value=",use_return_value)
            boolean_param("Enable Error Return=",enable_error_return)
            boolean_param("Save Screen=",save_screen)
            boolean_param("Optimize ROM Calls=",opt_rom_calls)
            stop(l);

#undef boolean_param
#undef reloc_param
        }

        // Ignore [File Editing] section, it is used only for editing.

        // Keywords in the [Included Files] section
        if(stype == SECTION_FILES)
        {
            int v;

            if( (p=find_numbered_param(buffer, "C File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &c_files, &c_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "C File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "GNU Assembler File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &s_files, &s_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "GNU Assembler File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Header File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &h_files, &h_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "Header File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Assembler File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &asm_files, &asm_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "Assembler File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Object File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &o_files, &o_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "Object File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Archive File %i=", &v)) )
            {
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &a_files, &a_file_count);
                free(s);

                continue;
            }
            else if( (p=find_numbered_param(buffer, "Archive File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Text File %i=", &v)) )
            { // text file: do nothing
                continue;
            }
            else if( (p=find_numbered_param(buffer, "Text File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Quill File %i=", &v)) )
            { // Quill file: treat like C file and add -quill flag
                char *s = encapsulate_long_filename(p);
                dyn_file_array_add(s, &c_files, &c_file_count);
                free(s);

                settings.quill = TRUE; // -quill flag needed

                continue;
            }
            else if( (p=find_numbered_param(buffer, "Quill File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else if( (p=find_numbered_param(buffer, "Other File %i=", &v)) )
            { // other file: do nothing
                continue;
            }
            else if( (p=find_numbered_param(buffer, "Other File %i Folder=", &v)) )
            { // ignore folder specification for now
                continue;
            }

            else stop(l);
        }

    }
    fclose(f);

    return 0;
}

/*
   Build command line arguments (Settings and Library Options sections)
*/
int process_settings(char *outfile, char **pargs)
{
#define args (*pargs)
    args=malloc(5);
    if (!args) outofmem();
    strcpy(args, "-o \"");
    args = dynstrcat(args, outfile);
    args = dynstrcat(args, "\"");

    // Process settings
    if (settings.archive) {
        args = dynstrcat(args, " -ar");
    }

    if (settings.pack) {
        args = dynstrcat(args, " -pack ");
        args = dynstrcat(args, settings.pack_name);
    }

    if (settings.debug_info) {
        args = dynstrcat(args, " -g");
    }

    if (!settings.std_lib) {
        args = dynstrcat(args, " -standalone");
    }

    if (settings.prj_name) {
        args = dynstrcat(args, " -n ");
        args = dynstrcat(args, settings.prj_name);
    }

    if (settings.cc_switches) {
        args = dynstrcat(args, " ");
        args = dynstrcat(args, settings.cc_switches);
    }

    if (settings.cmdline_cc_switches) {
        args = dynstrcat(args, " ");
        args = dynstrcat(args, settings.cmdline_cc_switches);
    }

    if (settings.as_switches) {
        size_t i;
        args = dynstrcat(args, " -Wa,");
        for (i=0;i<strlen(settings.as_switches);i++) if (settings.as_switches[i]==' ') settings.as_switches[i]=',';
        args = dynstrcat(args, settings.as_switches);
    }

    if (settings.a68k_switches) {
        size_t i;
        args = dynstrcat(args, " -WA,");
        for (i=0;i<strlen(settings.a68k_switches);i++) {
            char *p=settings.a68k_switches+i;
            if (*p==',') *p=';'; else if (*p==' ') *p=',';
        }
        args = dynstrcat(args, settings.a68k_switches);
    }

    if (!settings.archive) {
        if (settings.use_data_var && settings.data_var) {
            args = dynstrcat(args, " -d ");
            args = dynstrcat(args, settings.data_var);
            if (!settings.copy_data_var) {
                args = dynstrcat(args, " --data-var-copy=never");
            } else if (!settings.copy_data_var_arc) {
                args = dynstrcat(args, " --data-var-copy=always");
            }
        }

        if (settings.optimize_nops) {
            args = dynstrcat(args, " --optimize-nops");
        }
        if (settings.optimize_returns) {
            args = dynstrcat(args, " --optimize-returns");
        }
        if (settings.optimize_branches) {
            args = dynstrcat(args, " --optimize-branches");
        }
        if (settings.optimize_moves) {
            args = dynstrcat(args, " --optimize-moves");
        }
        if (settings.optimize_tests) {
            args = dynstrcat(args, " --optimize-tests");
        }
        if (settings.optimize_calcs) {
            args = dynstrcat(args, " --optimize-calcs");
        }

        if (settings.remove_unused) {
            args = dynstrcat(args, " --remove-unused");
        }

        if (settings.cut_ranges) {
            args = dynstrcat(args, " --cut-ranges");
        }

        if (settings.reorder_sections) {
            args = dynstrcat(args, " --reorder-sections");
        }

        if (settings.merge_constants) {
            args = dynstrcat(args, " --merge-constants");
        }

        if (settings.outputbin) {
            args = dynstrcat(args, " --outputbin");
        }

        if (!settings.initialize_bss) {
            args = dynstrcat(args, " --omit-bss-init");
        }
    }

    if (settings.fargo) {
        args = dynstrcat(args, " --fargo");
    }

    if (settings.flash_os) {
        args = dynstrcat(args, " --flash-os");
    }

    // Process library options
    if (libopts.use_ti89) {
        args = dynstrcat(args, " -DUSE_TI89");
    }
    if (libopts.use_ti92p) {
        args = dynstrcat(args, " -DUSE_TI92PLUS");
    }
    if (libopts.use_v200) {
        args = dynstrcat(args, " -DUSE_V200");
    }

    if (libopts.opt_calc_consts) {
        args = dynstrcat(args, " -DOPTIMIZE_CALC_CONSTS");
    }

    if (libopts.use_kernel || libopts.use_preos) {
        args = dynstrcat(args, " -DUSE_KERNEL");
    }
    if (libopts.use_preos) {
        args = dynstrcat(args, " -DUSE_PREOS_COMPRESSED_TABLES");
    }

    if (libopts.use_minams) {
        char buffer[14];
        sprintf(buffer, " -DMIN_AMS=%d", libopts.minams);
        args = dynstrcat(args, buffer);
    }

    if (libopts.unofficial_os) {
        args = dynstrcat(args, " -DUNOFFICIAL_OS_SUPPORT");
    }

    if (libopts.use_preos) {
        if (libopts.bss_ref_format == RT_NONE)
            args = dynstrcat(args, " -DMERGE_BSS");
    } else {
        switch (libopts.reloc_format) {
          case RT_KERNEL:
            args = dynstrcat(args, " -DKERNEL_FORMAT_RELOCS");
            break;
          case RT_COMPRESSED:
            args = dynstrcat(args, " -DCOMPRESSED_FORMAT_RELOCS");
            break;
          case RT_MLINK:
            args = dynstrcat(args, " -DMLINK_FORMAT_RELOCS");
            break;
          case RT_FLINE:
            args = dynstrcat(args, " -DUSE_FLINE_JUMPS");
            break;
          default:
            break;
        }
        switch (libopts.rom_call_format) {
          case RT_KERNEL:
            args = dynstrcat(args, " -DKERNEL_FORMAT_ROM_CALLS");
            break;
          case RT_COMPRESSED:
            args = dynstrcat(args, " -DCOMPRESSED_FORMAT_ROM_CALLS");
            break;
          case RT_MLINK:
            args = dynstrcat(args, " -DMLINK_FORMAT_ROM_CALLS");
            break;
          case RT_PRECOMP:
            args = dynstrcat(args, " -DOPTIMIZE_ROM_CALLS");
            break;
          case RT_FLINE:
            args = dynstrcat(args, " -DUSE_FLINE_ROM_CALLS -fno-function-cse");
            break;
          default:
            break;
        }
        if (libopts.opt_rom_calls) {
            args = dynstrcat(args, " -DOPTIMIZE_ROM_CALLS");
        }
        switch (libopts.bss_ref_format) {
          case RT_NONE:
            args = dynstrcat(args, " -DMERGE_BSS");
            break;
          case RT_KERNEL:
            args = dynstrcat(args, " -DKERNEL_FORMAT_BSS");
            break;
          case RT_COMPRESSED:
            args = dynstrcat(args, " -DCOMPRESSED_FORMAT_BSS");
            break;
          case RT_MLINK:
            args = dynstrcat(args, " -DMLINK_FORMAT_BSS");
            break;
          default:
            break;
        }
    }

    switch (libopts.data_ref_format) {
      case RT_KERNEL:
        args = dynstrcat(args, " -DKERNEL_FORMAT_DATA_VAR");
        break;
      case RT_COMPRESSED:
        args = dynstrcat(args, " -DCOMPRESSED_FORMAT_DATA_VAR");
        break;
      case RT_MLINK:
        args = dynstrcat(args, " -DMLINK_FORMAT_DATA_VAR");
        break;
      default:
        break;
    }

    if (libopts.use_fline_jumps) {
        args = dynstrcat(args, " -DUSE_FLINE_JUMPS");
        if (libopts.use_4b_fline_jumps) {
            args = dynstrcat(args, " -DUSE_4BYTE_FLINE_JUMPS");
        }
    }

    if (libopts.use_internal_fline_emu) {
        args = dynstrcat(args, " -DUSE_INTERNAL_FLINE_EMULATOR");
    }

    if (libopts.use_return_value) {
        args = dynstrcat(args, " -DRETURN_VALUE");
    }

    if (libopts.enable_error_return) {
        args = dynstrcat(args, " -DENABLE_ERROR_RETURN");
    }

    if (libopts.save_screen) {
        args = dynstrcat(args, " -DSAVE_SCREEN");
    }

    return 0;
#undef args
}

/*
   Build command line files (Included Files section)
*/
int process_files(char **pfiles)
{
#define files (*pfiles)
    int i, count;

    files=malloc(1);
    if (!files) outofmem();
    *files=0;

    /* Process .c files */
    dyn_file_array_size(c_files, &count);
    for(i=0; i<count; i++) {
        files = dynstrcat(files, c_files[i]);
        files = dynstrcat(files, " ");
    }

    /* Process .s files */
    dyn_file_array_size(s_files, &count);
    for(i=0; i<count; i++) {
        files = dynstrcat(files, s_files[i]);
        files = dynstrcat(files, " ");
    }

    /* Process .asm files */
    dyn_file_array_size(asm_files, &count);
    for(i=0; i<count; i++) {
        files = dynstrcat(files, asm_files[i]);
        files = dynstrcat(files, " ");
    }

    /* Process .o files */
    dyn_file_array_size(o_files, &count);
    for(i=0; i<count; i++) {
        files = dynstrcat(files, o_files[i]);
        files = dynstrcat(files, " ");
    }

    /* Process .a files */
    dyn_file_array_size(a_files, &count);
    for(i=0; i<count; i++) {
        files = dynstrcat(files, a_files[i]);
        files = dynstrcat(files, " ");
    }
    return 0;
#undef files
}

/*
  Call tigcc for making files
*/
void build_files(char *outfile)
{
    char *options = NULL;
    char *files = NULL;

    process_settings(outfile, &options);
    process_files(&files);

    execute_tigcc(files, options);

    free(options);
    free(files);
}


/*
  Remove files generated by tigcc
*/
void clean_files(char *outfile)
{
    char executable[strlen(outfile)+14];
    int i, count;

    /* Process .c files */
    dyn_file_array_size(c_files, &count);
    for(i=0; i<count; i++) {
        char tmpfile[strlen(c_files[i])+3];

        strcpy(tmpfile, c_files[i]);
        change_extension(tmpfile, ".s");
        delete(tmpfile);

        strcpy(tmpfile, c_files[i]);
        change_extension(tmpfile, ".o");
        delete(tmpfile);
    }

    /* Process .s files */
    dyn_file_array_size(s_files, &count);
    for(i=0; i<count; i++) {
        char tmpfile[strlen(s_files[i])+3];

        strcpy(tmpfile, s_files[i]);
        change_extension(tmpfile, ".o");
        delete(tmpfile);
    }

    /* Process .asm files */
    dyn_file_array_size(asm_files, &count);
    for(i=0; i<count; i++) {
        char tmpfile[strlen(asm_files[i])+3];

        strcpy(tmpfile, asm_files[i]);
        change_extension(tmpfile, ".o");
        delete(tmpfile);
    }

    /* Remove executables */
    if (settings.archive) {
        strcpy(executable, outfile);
        strcat(executable, ".a");
        delete(executable);
    } else if (settings.fargo) {
        strcpy(executable, outfile);
        strcat(executable, settings.outputbin?".p92":".92p");
        delete(executable);
    } else if (settings.flash_os) {
        if (settings.outputbin) {
            strcpy(executable, outfile);
            strcat(executable, "-89.tib");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, "-89ti.tib");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, "-9x.tib");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, "-v2.tib");
            delete(executable);
        } else {
            strcpy(executable, outfile);
            strcat(executable, ".89u");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".9xu");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".v2u");
            delete(executable);
        }
    } else {
        if (settings.outputbin) {
            strcpy(executable, outfile);
            strcat(executable, ".z89");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".z9x");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".zv2");
            delete(executable);
        } else {
            strcpy(executable, outfile);
            strcat(executable, ".89z");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".9xz");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".v2z");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".89y");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".9xy");
            delete(executable);
            strcpy(executable, outfile);
            strcat(executable, ".v2y");
            delete(executable);

            if (settings.pack) {
                strcpy(executable, outfile);
                strcat(executable, "-titanium.89z");
                delete(executable);
                strcpy(executable, outfile);
                strcat(executable, "-Titanium.89z");
                delete(executable);
            }
        }
    }
}

__attribute__((noreturn)) static void outofmem(void)
{
  fprintf(stderr,"Fatal error: not enough free memory\n");
  exit(-1);
}

static char *dynstrcat(char *s, const char *t)
{
  char *p;
  size_t l = strlen(s) + strlen(t) + 1;
  p = realloc(s,l);
  if (p) {
    strcat(p,t);
    return p;
  } else {
    free(s);
    outofmem();
  }
}

