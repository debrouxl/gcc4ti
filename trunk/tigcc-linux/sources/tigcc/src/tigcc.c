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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "tigcc.h"

/* The name the program was run with, stripped of any leading path. */
char *program_name;

void print_version(void)
{
  fprintf(stderr, "tigcc version %s built for TIGCC/*nix version %s\n", VERSION, TIGCC_VERSION);
  exit(0);
}

/* Display helping informations */
static void
usage (int status)
{
  printf ("%s - front-end for TIGCC\n", program_name);
  printf ("Usage: %s [OPTION]... [FILE]...\n", program_name);
  printf ("Options:\n"
          "  -h, --help            display this help and exit\n"
          "  -V, --version         output version information and exit\n"
          "  -v, --verbose         set verbosity on: show what the program is doing\n"
          "  -q, --quiet           display nothing\n"
          "  -E,                   pre-process but don't compile or assemble\n"
          "  -S,                   compile but don't assemble or link\n"
          "  -c,                   compile/assemble, but don't link\n"
          "  -g, --debug           turns on debugging information (DWARF 2 format)\n"
          "  -bsr                  ignored for backwards compatibility\n"
          "  -o [outfile]          output to file outfile\n"
          "  -pack [varname]       compress the program using ExePack technology\n"
          "  -outputbin            produce a pure binary file (as with ttstrip)\n"
          "  -standalone           does not link against the tigcc.a archive\n"
          "  -r, --redirect        redirect stdout to the tigcc.log file\n"
          "  -Wa/WA,options        pass options to as/a68k\n"
          "  -ar                   create static library (function archive)\n"
          "  -keep                 keep object files\n"
          "  --all-relocs          assemble files in all-relocs mode (for range cutting)\n"
          "Linker options are automatically passed on to ld-tigcc. See ld-tigcc --help for\n"
          "details.\n"
          "\n");
  exit (status);
}

#define print_help() usage(0)

/* Process cmdline args */
short int process_arg(short int arg, char *argv[], int argc)
{
  char *cur_arg = argv[arg];

  if (!strcmp("-h", cur_arg) || !strcmp("--help", cur_arg)) {
    print_help();
  } else if (!strcmp("-V", cur_arg) || !strcmp("--version", cur_arg)) {
    print_version();
  } else if (!strcmp("-v", cur_arg) || !strcmp("--verbose", cur_arg)) {
    ld_argv[ld_argc++] = cur_arg;
    gcc_argv[gcc_argc++] = cur_arg;
    verbose = TRUE;
    printcommands = TRUE;
    quiet = FALSE;
  } else if (!strcmp("-v0", cur_arg)) {
    printcommands = TRUE;
  } else if (!strcmp("-q", cur_arg) || !strcmp("--quiet", cur_arg)) {
    quiet = TRUE;
    verbose = FALSE;
  } else if(!strcmp("-E", cur_arg)) {
    delete = FALSE;
    patch = FALSE;
    do_compile = FALSE;
    do_link = FALSE;
    do_assemble = FALSE;
  } else if (!strcmp("-S", cur_arg)) {
    delete = FALSE;
    do_link = FALSE;
    do_assemble = FALSE;
  } else if (!strcmp("-c", cur_arg)) {
    do_link = FALSE;
  } else if (!strcmp("-g", cur_arg)) {
    debug = TRUE;
  } else if (!strcmp("--debug", cur_arg)) {
    debug = TRUE;
  } else if (!strcmp("-pack", cur_arg)) {
    char *next_arg;
    if(arg >= argc-1) {
      fprintf(stderr, "Error: you didn't specify a pack file\n");
      exit(-1);
    } else {
      next_arg = argv[++arg];
      if(next_arg[0] == '-') {
        fprintf(stderr, "Error: invalid pack filename\n");
        exit(-1);
      }
      packfile[0] = 0;
      strncpy(packfile, next_arg, 8);
      do_pack = TRUE;
    }
  } else if(!strcmp("-bsr", cur_arg)) {
/* Empty. Ignored for backwards compatibility. */
  } else if (!strcmp("-outputbin", cur_arg) || !strcmp("--outputbin", cur_arg)) {
    outputbin = TRUE;
  } else if (!strcmp("-standalone", cur_arg) || !strcmp("--standalone", cur_arg)) {
    nostdlib = TRUE;
  } else if (!strcmp("-ar", cur_arg)) {
    staticlib = TRUE;
    nostdlib = TRUE;
    allrelocs = TRUE;
    optreturns = TRUE;
  } else if (!strcmp("-quill", cur_arg)) {
    char *quill_drv = malloc (strlen(tigcc_base) + 25);
    if (!quill_drv) {
      fprintf(stderr, "Fatal error: not enough free memory\n");
      exit(-1);
    }
    sprintf(quill_drv, "%s/bin/quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/bin/Quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/c/quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/c/Quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/quill/quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/quill/Quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/Quill/quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/include/Quill/Quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/lib/quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;
    sprintf(quill_drv, "%s/lib/Quill.drv", tigcc_base);
    if(access(quill_drv, F_OK) != -1) goto quill_drv_found;

    fprintf(stderr, "Quill installation problem: quill.drv file is missing.\n");
    exit(-1);

  quill_drv_found:
    gcc_argv[gcc_argc++] = "-include";
    gcc_argv[gcc_argc++] = quill_drv;
    gcc_argv[gcc_argc++] = "-x";
    gcc_argv[gcc_argc++] = "c";
  } else if (!strcmp("-o", cur_arg) || !strcmp("--output", cur_arg)) {
    if (++arg > argc) {
      fprintf(stderr, "Error: you didn't specify an output file\n");
    } else {
      outfile = argv[arg];
    }
  } else if (!strcmp("-r", cur_arg) || !strcmp("--redirect", cur_arg)) {
    freopen (REDIRECT, "wt", stdout);
    dup2 (STDOUT_FILENO, STDERR_FILENO);
  } else if (!strncmp("-Wa,", cur_arg, 4)) {
    if (as_args) {
      as_args = realloc(as_args, strlen(as_args)+strlen(cur_arg)-2);
      if (!as_args) {
        fputs("Fatal error: not enough free memory\n", stderr);
        exit(-1);
      }
      strcat(as_args, ",");
      strcat(as_args, cur_arg+4);
    } else {
      as_args = strdup(cur_arg);
      if (!as_args) {
        fputs("Fatal error: not enough free memory\n", stderr);
        exit(-1);
      }
    }
  } else if (!strncmp("-WA,", cur_arg, 4)) {
    if (a68k_args) {
      a68k_args = realloc(a68k_args, strlen(a68k_args)+strlen(cur_arg)-2);
      if (!a68k_args) {
        fputs("Fatal error: not enough free memory\n", stderr);
        exit(-1);
      }
      strcat(a68k_args, ",");
      strcat(a68k_args, cur_arg+4);
    } else {
      a68k_args = strdup(cur_arg);
      if (!a68k_args) {
        fputs("Fatal error: not enough free memory\n", stderr);
        exit(-1);
      }
    }
  } else if (!strcmp("-np", cur_arg)) {
    printf("Developer option: no patching enabled !\n");
    patch = FALSE;
  } else if (!strcmp("-keep", cur_arg) || !strcmp("--keep", cur_arg)) {
    keepobj = TRUE;
  } else if (!strcmp("-save-temps", cur_arg) || !strcmp("--save-temps", cur_arg)) {
    savetemps = keepobj = TRUE; delete = FALSE;
    gcc_argv[gcc_argc++] = cur_arg;
  } else if (!strcmp("--fargo", cur_arg)) {
    fargo = TRUE;
    ld_argv[ld_argc++] = cur_arg;
  } else if (!strcmp("--flash-os", cur_arg)) {
    flashos = TRUE;
    ld_argv[ld_argc++] = cur_arg;
  } else if (!strcmp("--cut-ranges", cur_arg)) {
    allrelocs = TRUE;
    ld_argv[ld_argc++] = cur_arg;
  } else if (!strcmp("--all-relocs", cur_arg)) {
    allrelocs = TRUE;
  } else if (!strcmp("--optimize-code", cur_arg) || !strcmp("--optimize-returns", cur_arg)) {
    optreturns = TRUE;
    ld_argv[ld_argc++] = cur_arg;
  } else if (!strcmp("--native", cur_arg) || !strcmp("--remove-unused", cur_arg)
             || !strcmp("--optimize-relocs", cur_arg) || !strcmp("--optimize-nops", cur_arg)
             || !strcmp("--optimize-branches", cur_arg) || !strcmp("--optimize-moves", cur_arg)
             || !strcmp("--optimize-tests", cur_arg) || !strcmp("--optimize-calcs", cur_arg)
             || !strcmp("--reorder-sections", cur_arg) || !strcmp("--merge-constants", cur_arg)
             || !strcmp("--omit-bss-init", cur_arg) || !strcmp("--data-var-copy=never", cur_arg)
             || !strcmp("--data-var-copy=always", cur_arg)
             || !strcmp("--data-var-copy=archived", cur_arg) || !strcmp("--dump", cur_arg)
             || !strcmp("--dump0", cur_arg) || !strcmp("--dump1", cur_arg) || !strcmp("--dump2", cur_arg)
             || !strcmp("--dump3", cur_arg) || !strcmp("--dump4", cur_arg) || !strcmp("--dump5", cur_arg)
             || !strcmp("--dump6", cur_arg) || !strcmp("--dump7", cur_arg) || !strcmp("--dump8", cur_arg)
             || !strcmp("--dump9", cur_arg)) {
    ld_argv[ld_argc++] = cur_arg;
  } else if (!strcmp("-n", cur_arg) || !strcmp("--varname", cur_arg)) {
    if (++arg > argc) {
      fprintf(stderr, "Error: you didn't specify a variable name\n");
    } else {
      ld_argv[ld_argc++] = cur_arg;
      ld_argv[ld_argc++] = argv[arg];
    }
  } else if (!strcmp("-d", cur_arg) || !strcmp("--data-var", cur_arg)) {
    if (++arg > argc) {
      fprintf(stderr, "Error: you didn't specify a data variable name\n");
    } else {
      ld_argv[ld_argc++] = cur_arg;
      ld_argv[ld_argc++] = argv[arg];
      nomergesections = TRUE;
    }
  }

  return arg;
}

short int is_tigcc_arg(char *arg)
{
  short int loop;

  for (loop = 0; tigcc_args[loop] != NULL; loop++) {
    if (strncmp(arg, tigcc_args[loop], 4) == 0) {
      return TRUE;
    }
  }

  return FALSE;
}

/* Return file type */
short int is_source_file(char *file)
{
  char *ext = strrchr(file, '.');
  short int filetype = BADFILE;

  if (ext == NULL) {
    return filetype;
  }
    
  if (strcasecmp(ext, ".c") == 0) {
    filetype = CFILE;
  } else if (strcasecmp(ext, ".asm") == 0) {
    filetype = ASMFILE;
  } else if (strcasecmp(ext, ".s") == 0) {
    filetype = SFILE;
  } else if (strcasecmp(ext, ".o") == 0) {
    filetype = OFILE;
  } else if (strcasecmp(ext, ".a") == 0) {
    filetype = AFILE;
  } else if (strcasecmp(ext, ".qll") == 0) {
    filetype = QLLFILE;
  }

  return filetype;
}

#define add_to_file_array(file,array) do {array->files[array->count++] = file;} while (0)

void free_file_array(array_of_files *array) 
{

  /* (NG) Well, it doesn't work even if *all* the strings in the array are malloc-ated.
     I don't understand why... :(
     Anyway, it won't work if a68k() is called as it is now (see there). */

#if 0
  short int loop;
    
  for (loop = 0; loop < array->count; loop++) {
    free(array->files[loop]);
  }
#endif
    
  free(array);
}

/* Change filename extension and keep long filenames */
char *change_extension(char *file, char *newext)
{
  char *start = (char *)strrchr(file, '.');
  if(start == NULL) {
    start = file + strlen(file);
  }

  sprintf(start, "%s", newext);

  /* (NG) So to be safe file must be strlen(file) + strlen (newext) + 1 long... */

  return file;
}

void parse_args(int *argc, char *argv[])
{
  static int firstsourcefile = 0;
  short int loop, filetype;
  array_of_files *array = NULL;

  for (loop = 1; loop < (*argc); loop++) {

    if ((!strcmp(argv[loop],"-include")||!strcmp(argv[loop],"-x")||!strcmp(argv[loop],"--param")||!strcmp(argv[loop],"-isystem")) && loop+1<*argc) {
      gcc_argv[gcc_argc++] = argv[loop++];
      gcc_argv[gcc_argc++] = argv[loop];
    } else if (is_tigcc_arg(argv[loop])) {
      loop = process_arg(loop, argv, *argc);
    } else if (argv[loop][0] == '-') {
      // if it's not one of our args, pass it to gcc
      gcc_argv[gcc_argc++] = argv[loop];
    } else {
      filetype = is_source_file(argv[loop]);

      switch (filetype) {
      case CFILE:
        array = src_files;
      handle_outfile:
        if (!firstsourcefile) firstsourcefile = loop;
        break;
      case ASMFILE:
        array = a68k_files;
        goto handle_outfile;
      case SFILE:
        array = asm_files;
        goto handle_outfile;
      case OFILE:
        array = obj_files;
        goto handle_outfile;
      case AFILE:
        array = ar_files;
        break;
      case QLLFILE: /* Quill files are just C files which need a special
                       -include switch to include the quill.drv header */
        array = src_files;
        goto handle_outfile;
      default: fprintf(stderr, "Invalid option %s\n", argv[loop]); break;
      }

      if (array != NULL) {
#if 0 /* Necessary if we want free_file_array not to crash, but it's a waste of memory... */
        char *s = strdup(argv[loop]);
        if (!s) {
          fputs ("Fatal error: not enough free memory.\n", stderr);
          exit (1);
        }
        add_to_file_array(s, array);
#else
        add_to_file_array(argv[loop], array);
#endif
      }
    }
  }

  // If no output filename, first arg used as default name.
  // However, don't do this with -S or -c. Instead, let compile() or
  // assemble() guess an appropriate file name for each individual file.
  if (firstsourcefile && do_link && !outfile) {
    outfile = malloc(strlen(argv[firstsourcefile])+3);
    strcpy(outfile,argv[firstsourcefile]);
    change_extension(outfile,".o");
  }
}

static inline void escape_arg(char *escaped_arg, const char *unescaped_arg)
{
  char *p;
  strcpy(escaped_arg,unescaped_arg);
  for (p=escaped_arg; *p; p++) {
    switch(*p) {
     /* The following characters are shell characters and need to be escaped! */
      case ' ':
      case '\\':
      case '\"':
      case '\'':
      case '$':
      case '!':
      case '^':
      case '&':
      case '*':
      case '(':
      case ')':
      case '~':
      case '[':
      case ']':
      case '|':
      case '{':
      case '}':
      case ';':
      case '<':
      case '>':
      case '?':
        memmove(p+1,p,strlen(p)+1);
        *(p++)='\\';
      default:
        break;
    }
  }
}

/* Execute a program */
void execute(const char *program, char **argv)
{
  pid_t pid;
  int status;

  if (printcommands) {
    char **ptr;
    fprintf(stderr, "tigcc: %s", program);
    for (ptr = argv + 1; *ptr; ptr++)
    {
      char escapedarg[((strlen(*ptr))<<1)+1];
      escape_arg(escapedarg,*ptr);
      fprintf (stderr, " %s", escapedarg);
    }
    putc ('\n', stderr);
  }

  fflush (stdout);
  pid = fork();
  if (pid == 0) {
    execv (program, argv);
    // execv only returns in case of an error
    fprintf (stderr, "Error executing %s\n", program);
    exit (1);
  }
  else if (pid < 0 || (waitpid (pid, &status, 0) != pid) || !WIFEXITED(status)) {
    fprintf (stderr, "Error executing %s\n", program);
    exit (1);
  }
  /* Fail silently if another program already showed an error message. */
  if (WEXITSTATUS(status)) exit(1);
}

/* Execute GNU cc */
void compile(char *file) 
{
  unsigned short local_argc = gcc_argc;
  char gcc_name[9 + strlen (tigcc_base)];
  char bindir[6 + strlen (tigcc_base)];
  char includedir[13 + strlen (tigcc_base)];

  sprintf (gcc_name, "%s/bin/gcc", tigcc_base);
  sprintf (bindir, "-B%s/bin/", tigcc_base);
  sprintf (includedir, "-I%s/include/c", tigcc_base);
  gcc_argv[0] = gcc_name;
  gcc_argv[local_argc++] = bindir;
  gcc_argv[local_argc++] = includedir;
  gcc_argv[local_argc++] = file;

  if (!do_compile) { // preprocess only
    gcc_argv[local_argc++] = "-E";
    if (outfile) {
      gcc_argv[local_argc++] = "-o";
      gcc_argv[local_argc++] = outfile;
    }

    gcc_argv[local_argc] = NULL;
    execute(gcc_name, gcc_argv);
  }
  else {
    char *tmpfile;
    if (!do_assemble && outfile) tmpfile = outfile;
    else {
      tmpfile = malloc (strlen (file) + 3);

      if (!tmpfile) {
        fprintf(stderr, "Fatal error: not enough free memory\n");
        exit(-1);
      }

      strcpy(tmpfile, file);
      change_extension(tmpfile, ".s");
    }

    if (debug) {
      gcc_argv[local_argc++] = "-gdwarf-2";
      gcc_argv[local_argc++] = "-g3";
      gcc_argv[local_argc++] = "-fasynchronous-unwind-tables";
    }
    if (nomergesections) {
      gcc_argv[local_argc++] = "-mno-merge-sections";
    }
    gcc_argv[local_argc++] = "-S";
    if (fargo) gcc_argv[local_argc++] = "-DFARGO";
    if (flashos) gcc_argv[local_argc++] = "-DFLASH_OS";
    gcc_argv[local_argc++] = "-o";
    gcc_argv[local_argc++] = tmpfile;

    add_to_file_array(tmpfile, asm_files);

    gcc_argv[local_argc] = NULL;
    execute(gcc_name, gcc_argv);

    // patch assembly file
    if(patch) {
      char patcher_name[strlen(tigcc_base) + 13];
      char *argv[] = {patcher_name, tmpfile, "-o", tmpfile, NULL};
      sprintf (patcher_name, "%s/bin/patcher", tigcc_base);

      execute (patcher_name, argv);
    }
  }
}

/* Execute GNU as */
void assemble(char *file)
{
  char as_name[strlen(tigcc_base) + 8];
  char includedir[strlen(tigcc_base) + 13];
  char *tmpfile;
  char *argv[7 + (as_args ? strlen (as_args) / 2 : 0) + allrelocs + optreturns + debug];
  char asargtokens[as_args?(strlen(as_args)+1):0];
  int i = 6;
  argv[0] = as_name;
  argv[1] = "-mc68000";
  argv[2] = includedir;
  argv[3] = file;
  argv[4] = "-o";

  if (!do_link && outfile) tmpfile = outfile;
  else {
    tmpfile = malloc (strlen (file) + 3);
    if (!tmpfile) {
      fprintf(stderr, "Fatal error: not enough free memory\n");
      exit(-1);
    }
    strcpy(tmpfile, file);
    change_extension(tmpfile, ".o");
  }

  argv[5] = tmpfile;
  sprintf (as_name, "%s/bin/as", tigcc_base);
  sprintf (includedir, "-I%s/include/s", tigcc_base);

  // add the file to the list
  add_to_file_array(tmpfile, obj_files);

  if (allrelocs) argv[i++] = "--all-relocs";
  if (optreturns) argv[i++] = "--keep-locals";
  if (debug) argv[i++] = "--gdwarf2";
  if (as_args) {
    strcpy (asargtokens, as_args);
    strtok (asargtokens, ",");
    while ((argv[i++] = strtok (NULL, ",")));
  }
  else argv[i] = NULL;

  execute(as_name, argv);
}

/* Execute ld-tigcc */
void ld(void)
{
  short int loop;

  unsigned short local_argc = ld_argc;
  char ld_name[strlen(tigcc_base) + 14];

  char tmpfile[strlen(outfile) + 1];

  sprintf (ld_name, "%s/bin/ld-tigcc", tigcc_base);
  ld_argv[0] = ld_name;

  for (loop = 0; loop < obj_files->count; loop++)
    ld_argv[local_argc++] = obj_files->files[loop];

  for (loop = 0; loop < ar_files->count; loop++)
    ld_argv[local_argc++] = ar_files->files[loop];

  ld_argv[local_argc++] = "-o";
  ld_argv[local_argc++] = tmpfile;

  strcpy(tmpfile, outfile);
  change_extension(tmpfile, "");

  if (do_pack)
    ld_argv[local_argc++] = "--outputbin-main-only";
  else if (outputbin)
    ld_argv[local_argc++] = "--outputbin";

  ld_argv[local_argc] = NULL;

  execute(ld_name, ld_argv);
}

/* Execute A68K */
void a68k(char *file)
{
  char *tmpfile;
  if (!do_link && outfile) {
    tmpfile = malloc (strlen (outfile) + 3);
    if (!tmpfile) {
      fprintf(stderr, "Fatal error: not enough free memory\n");
      exit(-1);
    }
    sprintf(tmpfile, "-o%s", outfile);
  }
  else {
    tmpfile = malloc (strlen (file) + 5);
    if (!tmpfile) {
      fprintf(stderr, "Fatal error: not enough free memory\n");
      exit(-1);
    }
    sprintf(tmpfile, "-o%s", file);
    change_extension(tmpfile, ".o");
  }
  tmpfile += 2; /* Warning: this prevents free_file_array from working */

  /* First, assemble file with A68k which produces AmigaOS files */

  {
    char a68k_name[strlen(tigcc_base) + 10];
    char includedir[strlen(tigcc_base) + 16];
    char *argv[7 + (a68k_args ? strlen (a68k_args) / 2 : 0) + allrelocs + optreturns + quiet];
    char a68kargtokens[a68k_args?(strlen(a68k_args)+1):0];
    int i = 6;
    argv[0] = a68k_name;
    argv[1] = includedir;
    argv[2] = "-g";
    argv[3] = "-t";
    argv[4] = file;
    argv[5] = tmpfile;
    
    sprintf (a68k_name, "%s/bin/a68k", tigcc_base);
    sprintf (includedir, "-i%s/include/asm/", tigcc_base);

    // add the file to the list
    add_to_file_array(tmpfile, obj_files);

    if (allrelocs) argv[i++] = "-a";
    if (optreturns) argv[i++] = "-d";
    if (quiet) argv[i++] = "-q";
    if (a68k_args) {
      strcpy (a68kargtokens, a68k_args);
      strtok (a68kargtokens, ",");
      while ((argv[i++] = strtok (NULL, ",")));
    }
    else argv[i] = NULL;

    execute(a68k_name, argv);
  }
}

/* Parse pstarter file and change internal varname */
static short int parse_pstarter(const char *input, const char *output,
                                const char *packvar)
{
  FILE *f;
  char array[65536];
  char *token = "tempprog";
  unsigned int i, n;
  unsigned int j, k;

  // open file in reading
  f = fopen(input, "rb");
  if(f == NULL) {
    fprintf(stderr, "Unable to open pstarter.o\n");
    exit(-1);
  }

  n = fread(array, sizeof(char), 65536*sizeof(char), f);
  fclose(f);

  // change varname
  for(i=0; i<n-strlen(token); i++) {
    for(j=0, k=0; j<strlen(token); j++) {
      if(array[i+j] == token[j]) {
        k++;
      }
    }
    if(k==strlen(token)) {
      //printf("offset = %i = 0x%04x\n", i, i);
      break;
    }
  }
  sprintf(array+i, "%s", packvar);

  // open file in writing
  f = fopen(output, "wb");
  if(f == NULL) {
    fprintf(stderr, "Unable to open pstarter.o\n");
    exit(-1);
  }

  fwrite(array, sizeof(char), n*sizeof(char), f);
  fclose(f);

  return 0;
}

/* Execute ar-tigcc */
void ar(void)
{
  int i, loop;
  char buffer[strlen(outfile) + 3];
  char ar_name[14 + strlen(tigcc_base)];
  char *argv[5 + obj_files->count + ar_files->count];
  argv[0] = ar_name;
  argv[1] = "-o";
  argv[2] = buffer;
  argv[3] = "--no-names";

  strcpy(buffer, outfile);
  change_extension(buffer, ".a");

  unlink (buffer);

  sprintf(ar_name, "%s/bin/ar-tigcc", tigcc_base);

  i = 4;

  for (loop = 0; loop < obj_files->count; loop++)
    argv[i++] = obj_files->files[loop];

  for (loop = 0; loop < ar_files->count; loop++)
    argv[i++] = ar_files->files[loop];

  argv[i] = NULL;

  execute(ar_name, argv);
}

/* Compress TI executable */
void pack(void)
{
  char pstarter_file[25 + strlen(tigcc_base)];
  char tmpfile[strlen(outfile) + 14];
  int ti89_targeted=0;

  // check for decompressor program
  sprintf(pstarter_file, "%s/lib/pstarter.o", tigcc_base);
  if(access(pstarter_file, F_OK) == -1) {
    fprintf(stderr, "TIGCC installation problem: pstarter.o file is missing.\n");
    return;
  }

  strcpy(tmpfile, outfile);
  change_extension(tmpfile, ".z89");

  if(access(tmpfile, F_OK) != -1) {
    ti89_targeted=1;
    // compress on calc variable
    {
      char ttpack_name[strlen (tigcc_base) + 12];
      char *argv[5];
      int i=1;
      *argv = ttpack_name;
      if (verbose)
        argv[i++] = "-v";
      else if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = tmpfile;
      argv[i++] = TMP_PCK;
      argv[i] = NULL;

      sprintf (ttpack_name, "%s/bin/ttpack", tigcc_base);
      execute(ttpack_name, argv);
    }
    // encapsulate in var (.89y)
    {
      char ttbin2oth_name[strlen(tigcc_base) + 15];
      char *argv[7];
      int i=1;
      *argv = ttbin2oth_name;
      if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = "-89";
      argv[i++] = "ppg";
      argv[i++] = TMP_PCK;
      argv[i++] = packfile;
      argv[i] = NULL;

      sprintf(ttbin2oth_name, "%s/bin/ttbin2oth", tigcc_base);
      execute(ttbin2oth_name, argv);
    }

    unlink(tmpfile);
    unlink(TMP_PCK);
  }

  change_extension(tmpfile, ".zv2");

  if(access(tmpfile, F_OK) != -1) {
    // compress on calc variable
    {
      char ttpack_name[strlen (tigcc_base) + 12];
      char *argv[5];
      int i=1;
      *argv = ttpack_name;
      if (verbose)
        argv[i++] = "-v";
      else if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = tmpfile;
      argv[i++] = TMP_PCK;
      argv[i] = NULL;

      sprintf (ttpack_name, "%s/bin/ttpack", tigcc_base);
      execute(ttpack_name, argv);
    }
    // encapsulate in var (.v2y)
    {
      char ttbin2oth_name[strlen(tigcc_base) + 15];
      char filename9x[strlen(packfile)+5];
      char filenamev2[strlen(packfile)+5];
      char *argv[7];
      int i=1;
      *argv = ttbin2oth_name;
      if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = "-92";
      argv[i++] = "ppg";
      argv[i++] = TMP_PCK;
      argv[i++] = packfile;
      argv[i] = NULL;

      sprintf(ttbin2oth_name, "%s/bin/ttbin2oth", tigcc_base);
      execute(ttbin2oth_name, argv);
      strcpy(filename9x,packfile);
      strcat(filename9x,".9xy");
      strcpy(filenamev2,packfile);
      strcat(filenamev2,".v2y");
      rename(filename9x,filenamev2);
    }

    unlink(tmpfile);
    unlink(TMP_PCK);
  }

  change_extension(tmpfile, ".z9x");

  if(access(tmpfile, F_OK) != -1) {
    // compress on calc variable
    {
      char ttpack_name[strlen (tigcc_base) + 12];
      char *argv[5];
      int i=1;
      *argv = ttpack_name;
      if (verbose)
        argv[i++] = "-v";
      else if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = tmpfile;
      argv[i++] = TMP_PCK;
      argv[i] = NULL;

      sprintf (ttpack_name, "%s/bin/ttpack", tigcc_base);
      execute(ttpack_name, argv);
    }
    // encapsulate in var (.9xy)
    {
      char ttbin2oth_name[strlen(tigcc_base) + 15];
      char *argv[7];
      int i=1;
      *argv = ttbin2oth_name;
      if (quiet)
        argv[i++] = "-quiet";
      argv[i++] = "-92";
      argv[i++] = "ppg";
      argv[i++] = TMP_PCK;
      argv[i++] = packfile;
      argv[i] = NULL;

      sprintf(ttbin2oth_name, "%s/bin/ttbin2oth", tigcc_base);
      execute(ttbin2oth_name, argv);
    }

    unlink(tmpfile);
    unlink(TMP_PCK);
  }

  // parse pstarter.o and change varname
  parse_pstarter(pstarter_file, "pstarter.o", packfile);

  // create decompressor (.??z)
  {
    char ld_tigcc_name[strlen(tigcc_base) + 14];
    char *argv[] = {ld_tigcc_name, "pstarter.o", "-o", tmpfile, NULL};

    sprintf (ld_tigcc_name, "%s/bin/ld-tigcc", tigcc_base);
    change_extension(tmpfile, "");
    execute(ld_tigcc_name, argv);
  }
  unlink("pstarter.o");

  if (!ti89_targeted) return; // no TI-89 -> no Titanium

#if 1
  // zap any leftover Titanium launchers
  strcat(tmpfile, "-titanium.89z");
  unlink(tmpfile);
  strcpy(tmpfile, outfile);
  strcat(tmpfile, "-Titanium.89z");
  unlink(tmpfile);
#else
  // check for Titanium decompressor program
  sprintf(pstarter_file, "%s/lib/pstarter-titanium.o", tigcc_base);
  if(access(pstarter_file, F_OK) == -1) {
    fprintf(stderr, "TIGCC installation problem: pstarter-titanium.o file is missing.\n");
    return;
  }

  // parse pstarter.o and change varname
  parse_pstarter(pstarter_file, "pstarter.o", packfile);

  // create decompressor (*-titanium.89z)
  {
    char ld_tigcc_name[strlen(tigcc_base) + 14];
    char *argv[] = {ld_tigcc_name, "pstarter.o", "-o", tmpfile, NULL};

    sprintf (ld_tigcc_name, "%s/bin/ld-tigcc", tigcc_base);
    strcat(tmpfile, "-titanium");
    execute(ld_tigcc_name, argv);
    // zap non-TI-89 Titanium launchers
    change_extension(tmpfile, ".9xz");
    unlink(tmpfile);
    change_extension(tmpfile, ".v2z");
    unlink(tmpfile);
  }
  unlink("pstarter.o");
#endif
}

/* Destroy allocated resources */
void safe_exit(void)
{
  free(gcc_argv);
  free(ld_argv);
  free_file_array(src_files);
  free_file_array(obj_files);
  free_file_array(asm_files);
  free_file_array(a68k_files);
  free_file_array(ar_files);

  free(as_args); free(a68k_args);
}

int main(int argc, char *argv[]) 
{
  short int loop;

  program_name = argv[0];
    
  if (argc < 2) {
    fprintf(stderr, "tigcc: no input files\n");
    exit(0);
  }
  
  if ((tigcc_base = getenv("TIGCC")) == NULL) {
    fprintf(stderr, "Fatal error: TIGCC is not defined in the environment. TIGCC must be defined before tigcc can run.\nFor (ba)sh, try: export TIGCC=/path/to/tigcc\nFor (t)csh, try: setenv TIGCC /path/to/tigcc\n");
    exit(-1);
  }

  if (atexit(safe_exit) != 0) {
    fprintf(stderr, "Fatal error: unable to register safe exit callback\n");
    exit(-1);
  }

  /* worst case (every arg is a gcc arg): argc + -Bprefix/bin/ + -Idir + -gdwarf-2 + -g3 +
                                          + -fasynchronous-unwind-tables + -mno-merge-sections + -S
                                          + -DFARGO/-DFLASH_OS + -include + quill.drv + -x + c + -o + filename */
  gcc_argv = malloc((argc + 15) * sizeof (char*));

  /* worst case: each arg is either a file to link or a linker switch; add
                 tigcc.a/fargo.a/flashos.a, -o outfile, --outputbin, NULL */
  ld_argv = malloc((argc + 5) * sizeof (char*));

  src_files = malloc(sizeof (array_of_files) + MAX_ARRAY_FILES * sizeof (char*));
  obj_files = malloc(sizeof (array_of_files) + MAX_ARRAY_FILES * sizeof (char*));
  asm_files = malloc(sizeof (array_of_files) + MAX_ARRAY_FILES * sizeof (char*));
  a68k_files = malloc(sizeof (array_of_files) + MAX_ARRAY_FILES * sizeof (char*));
  ar_files = malloc(sizeof (array_of_files) + MAX_ARRAY_FILES * sizeof (char*));

  if ((gcc_argv == NULL) || (ld_argv == NULL) || (src_files == NULL) || (obj_files == NULL) ||
      (asm_files == NULL) || (a68k_files == NULL) || (ar_files == NULL)) {
    fprintf(stderr, "Fatal error: not enough free memory\n");
    exit(-1);
  }

  src_files -> count = 0;
  obj_files -> count = 0;
  asm_files -> count = 0;
  a68k_files -> count = 0;
  ar_files -> count = 0;

  // parse the arguments
  parse_args(&argc, argv);

  // check there is at least one file to process
  if(!src_files->count && !asm_files->count && !a68k_files->count && !obj_files->count)
    {
      fprintf(stderr, "tigcc: no input files\n");
      exit(0);
    }

  // set the archive file
  if(!nostdlib) {
    char *buffer = malloc(strlen(tigcc_base)+13);
    sprintf(buffer, flashos?"%s/lib/flashos.a":(fargo?"%s/lib/fargo.a":"%s/lib/tigcc.a"), tigcc_base);
    add_to_file_array(buffer, ar_files);
  }

  if(1) {
    for (loop = 0; loop < src_files->count; loop++) {
      compile(src_files->files[loop]);
    }
  }

  if (do_assemble) {
    for (loop = 0; loop < asm_files->count; loop++) {
      assemble(asm_files->files[loop]);
    }

    for (loop = 0; loop < a68k_files->count; loop++) {
      a68k(a68k_files->files[loop]);
    }
  }

  if (staticlib) ar(); else {

    if (do_link && obj_files->count > 0) {
      ld();
    }

  }

  if(delete) {
    for (loop = 0; loop < src_files->count; loop++) {
      char tmpfile[strlen(src_files->files[loop]) + 3];
      strcpy(tmpfile, src_files->files[loop]);
      change_extension(tmpfile, ".s");
      unlink(tmpfile);
    }
  }

  if(do_link && !keepobj) {
    for (loop = 0; loop < src_files->count; loop++) {
      char tmpfile[strlen(src_files->files[loop]) + 3];
      strcpy(tmpfile, src_files->files[loop]);
      change_extension(tmpfile, ".o");
      unlink(tmpfile);
    }
    for (loop = 0; loop < asm_files->count; loop++) {
      char tmpfile[strlen(asm_files->files[loop]) + 3];
      strcpy(tmpfile, asm_files->files[loop]);
      change_extension(tmpfile, ".o");
      unlink(tmpfile);
    }
    for (loop = 0; loop < a68k_files->count; loop++) {
      char tmpfile[strlen(a68k_files->files[loop]) + 3];
      strcpy(tmpfile, a68k_files->files[loop]);
      change_extension(tmpfile, ".o");
      unlink(tmpfile);
    }
  }

  if (!staticlib && do_pack && do_link) {
    pack();
  }
  return 0;
}
