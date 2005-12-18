/*
   patcher - Parse assembly file and apply patches.

   Copyright (C) 2002 Romain Liévin
   Copyright (C) 2002-2005 Kevin Kofler

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
#include <ctype.h>
#include <sys/types.h>
#include <unistd.h>
#include <dirent.h>

#include "patcher.h"

#define EXIT_FAILURE 1


char *output_file = NULL;

int
main (int argc, char **argv)
{
  int i, fline_ROM_CALLs=0, reg_relative=0;
  FILE *infile, *outfile;
  char *temp_file;
  unsigned char buffer[32772]; // 4 extra bytes for the patches

  /* GNU stuff */
  program_name = argv[0];
  i = decode_switches (argc, argv);

  if ((i>=argc) || strcmp(file_extension(argv[i]),".s")) usage(EXIT_FAILURE);
  if (!output_file) output_file=argv[i];

  temp_file = malloc(strlen(argv[i])+3);
  if (!temp_file) {fprintf(stderr, "Fatal error: malloc failed\n");exit(EXIT_FAILURE);}
  strcpy(temp_file,argv[i]);change_extension(temp_file,".tmp");

  unlink(temp_file);
  outfile = fopen(temp_file, "wt");
  if (!outfile)
    {
      fprintf(stderr, "Unable to open the temporary file <%s>\n", argv[i]);
      exit(-1);
    }
  infile = fopen(argv[i], "rt");
  if (!infile)
    {
      fclose(outfile);
      unlink(temp_file);
      fprintf(stderr, "Unable to open the file <%s>\n", argv[i]);
      exit(-1);
    }

  // Copy infile line by line, applying patches if needed:
  while(1) {
    fgets(buffer, 32768, infile);

    if (feof(infile)) break;

    if (buffer[strlen(buffer)-1]=='\n') buffer[strlen(buffer)-1]=0;
    output_line(buffer, outfile, &fline_ROM_CALLs, &reg_relative);
  }
  
  fclose(outfile);
  fclose(infile);

  if (!strcmp(output_file,argv[i])) unlink(output_file);
  rename(temp_file,output_file);

  free(temp_file);

  exit (0);
}

/* Set all the option flags according to the switches specified.
   Return the index of the first non-option argument.  */

static int
decode_switches (int argc, char **argv)
{
  int c;

  for (c=1;c<argc;c++) {
    if (!strcmp(argv[c],"-V")||!strcmp(argv[c],"--version")) {
       printf ("patcher 1.11\n");
       exit(0);
    } else if (!strcmp(argv[c],"-h")||!strcmp(argv[c],"--help")) {
       usage(0);
    } else if (!strcmp(argv[c],"-o")) {
       if (c+1<argc) output_file=argv[c+1]; else usage(EXIT_FAILURE);
    } else break;
  }

  if (c>=argc) usage(EXIT_FAILURE);
  
  return c;
}


static void
usage (int status)
{
  printf ("%s - parse assembly files and apply TIGCC patches\n",
          program_name);
  printf ("Usage: %s [OPTION]... [FILE]...\n", program_name);
  printf ("\
Options:\n\
  -h, --help                 display this help and exit\n\
  -V, --version              output version information and exit\n\
  -o output_file             select output file (default: same as input)\n\
");
  exit (status);
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

static void output_line(unsigned char *buffer, FILE *outfile, int *pfline_ROM_CALLs, int *preg_relative)
{
// This is a straight C translation of Sebastian's ParseSFile Delphi source code.
#define fline_ROM_CALLs (*pfline_ROM_CALLs)
#define reg_relative (*preg_relative)
  if (*buffer) {
    char *p1,*p2;

    // detect -f-reg-relative
    if (!strcmp(buffer,"\t.set __relation,__ld_entry_point_plus_0x8000"))
      reg_relative=1;

    // detect F-LINE ROM_CALLs
    if (strstr(buffer,"_F_LINE"))
      fline_ROM_CALLs=1;

    // don't patch .ascii or .asciz commands
    if (strncmp(buffer,"\t.ascii",7) && strncmp(buffer,"\t.asciz",7))
    {
      // handle -f-reg-relative
      if (reg_relative && (p1=strstr(buffer,"-__relation"))) {
        char *p4=buffer;
        while (p1) {
          if ((p2=strstr(p4,"_CALL_")) && (p2<p1) && (p1-p2<10)) {
            char *p3=strchr(p1+12,')');
            if (p3) memmove(p1,p3+1,strlen(p3)); // zap "-__relation(%an)"
            p4=p1;
          } else if ((p2=strstr(p4,"_ER_CODE_")) && (p2<p1) && (p1-p2<14)) {
            char *p3=strchr(p1+12,')');
            if (p3) memmove(p1,p3+1,strlen(p3)); // zap "-__relation(%an)"
            p4=p1;
          }
          p1=strstr(p1+1,"-__relation");
        }
      }

      // handle A-LINE ER_THROW
      if (!strncmp(buffer,"\tjra _ER_CODE_",14) || !strncmp(buffer,"\tjmp _ER_CODE_",14)) {
        memmove(buffer+2,buffer+1,strlen(buffer)); // replace "jxx _ER_CODE_" with
        strncpy(buffer+1,".word _A_LINE+",14);     // ".word _A_LINE+"
      } else {
        // handle ROM_CALLs and RAM_CALLs (always use jsr so -l works)
        if (!strncmp(buffer,"\tjbsr",5)&&strstr(buffer,"_CALL_"))
          memmove(buffer+2,buffer+3,strlen(buffer)-1); // zap the 'b'
        else if (!strncmp(buffer,"\tjra",4)&&strstr(buffer,"_CALL_")) {
          buffer[2]='m'; // jra -> jmp
          buffer[3]='p';
        }
        // handle __ld_calc_const
        else if (!strncmp(buffer,"\tmove.l #__ld_calc_const_",25)) {
          buffer[6]='w'; // move.l -> move.w
        }

        // handle F-LINE ROM_CALLs
        if (fline_ROM_CALLs && !strncmp(buffer,"\tjsr _ROM_CALL_",15) && strlen(buffer)<=18) {
          memmove(buffer+3,buffer+1,strlen(buffer)); // replace "jsr _ROM_CALL_"
          strncpy(buffer+1,".word _F_LINE+0x",16);     // with ".word _F_LINE+0x"
        } else {
          // add :l to ROM_CALLs
          char *p4=strstr(buffer,"_ROM_CALL_");
          while (p4) {
            char *p5=p4;
            p4+=10;
            while (isalnum(*p4)||*p4=='_'||*p4=='+'||*p4=='-'||*p4=='*'||*p4=='/') p4++;
            while (*p4==':'||isalpha(*p4)) memmove(p4,p4+1,strlen(p4)); // zap :w if it's there
            if (strlen(buffer)>32769) break; // avoid buffer overflow
            memmove(p4+2,p4,strlen(p4)+1);
            *(p4++)=':'; // add ":l"
            *(p4++)='l';
            if (!strncmp(p4,"(%pc)",5)||!strncmp(p4,"(%Pc)",5)||!strncmp(p4,"(%pC)",5)||!strncmp(p4,"(%PC)",5)) {
              memmove(p4,p4+5,strlen(p4+5)+1);
            } else if ((!strncmp(p4,",%pc)",5)||!strncmp(p4,",%Pc)",5)||!strncmp(p4,",%pC)",5)
                        ||!strncmp(p4,",%PC)",5))
                       &&(p5>buffer)&&(p5[-1]=='(')) {
              memmove(p4,p4+5,strlen(p4+5)+1);
              memmove(p5-1,p5,strlen(p5)+1);
            }
            p4=strstr(p4,"_ROM_CALL_");
          }
          p4=strstr(buffer,"__ld_calc_const_");
          while (p4) {
            char *p5=p4;
            p4+=16;
            while ((*p4>='0'&&*p4<='9')||(*p4>='a'&&*p4<='z')||(*p4>='A'&&*p4<='Z')||(*p4=='_')||(*p4==':')) p4++;
            if (!strncmp(p4,"(%pc)",5)||!strncmp(p4,"(%Pc)",5)||!strncmp(p4,"(%pC)",5)||!strncmp(p4,"(%PC)",5)) {
              memmove(p4,p4+5,strlen(p4+5)+1);
            } else if ((!strncmp(p4,",%pc)",5)||!strncmp(p4,",%Pc)",5)||!strncmp(p4,",%pC)",5)
                        ||!strncmp(p4,",%PC)",5))
                       &&(p5>buffer)&&(p5[-1]=='(')) {
              memmove(p4,p4+5,strlen(p4+5)+1);
              memmove(p5-1,p5,strlen(p5)+1);
            }
            p4=strstr(p4,"__ld_calc_const_");
          }
        }
      }
    }

    fputs(buffer, outfile);
    fputc('\n',outfile);
  }
#undef fline_ROM_CALLs
#undef reg_relative
}

static char *change_extension(char *file, const char *newext)
{
  char *start = (char *)strrchr(file, '.');
  if(start == NULL) {
    start = file; //return file;
  } else {
    sprintf(start, "%s", newext);
  }

  return file;
}
