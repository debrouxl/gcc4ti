/*  A68k compatibility wrapper for GNU as.
 *  Copyright (C) 2005 Kevin Kofler
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
#ifdef __WIN32__
#include <windows.h>
#undef IGNORE
#endif

#define fatal(s) ({fprintf(stderr,(s)); return 1;})

#ifdef __WIN32__
static int run_cmdline(const char *cmdline)
{
  long exitcode;
  STARTUPINFO startupinfo={.cb=sizeof(startupinfo)};
  PROCESS_INFORMATION processinfo;
  if (!CreateProcess(NULL,(char *)cmdline,NULL,NULL,FALSE,0,NULL,NULL,
                     &startupinfo,&processinfo)) return 1;
  if (WaitForSingleObject(processinfo.hProcess,INFINITE)==WAIT_FAILED) return 1;
  if (!GetExitCodeProcess(processinfo.hProcess,&exitcode)) return 1;
  CloseHandle(processinfo.hProcess);
  CloseHandle(processinfo.hThread);
  return exitcode;
}
#else
#define run_cmdline system
#endif

__attribute__((noreturn)) static void outofmem(void)
{
  fprintf(stderr,"Fatal error: not enough free memory\n");
  exit(1);
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

#ifndef __WIN32__
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
#endif

static char *dynargcat(char *s, const char *t)
{
  char *p;
#ifdef __WIN32__
  size_t l = strlen(s)+strlen(t)+3;
#else
  size_t l = strlen(s)+2*strlen(t)+1;
#endif
  p = realloc(s,l);
  if (p) {
#ifdef __WIN32__
    strcat(p,"\"");
    strcat(p,t);
    strcat(p,"\"");
#else
    escape_arg(p+strlen(p),t);
    char *q = realloc(p,strlen(p)+1);
    if (q) p=q;
#endif
    return p;    
  } else {
    free(s);
    outofmem();
  }
}

int main(int argc, char *argv[]) 
{
  /* Find GNU as */
#ifdef __WIN32__
  HMODULE module_h = GetModuleHandle("a68k.exe");
  if (!module_h) fatal("Fatal error: Can't obtain module handle.");
  char path_to_as[MAX_PATH];
  if (!GetModuleFileName(module_h,path_to_as,MAX_PATH)) fatal("Fatal error: GetModuleFileName failed.");
  char *p=strrchr(path_to_as,'\\');
  if (!p) fatal("Fatal error: No backslash in module file name.");
  strcpy(p+1,"as.exe");
#else
  const char *tigcc_base=getenv("TIGCC");
  if (!tigcc_base) fatal("Fatal error: $TIGCC not defined in the environment.\n");
  char path_to_as[strlen(tigcc_base)+8];
  sprintf(path_to_as,"%s/bin/as",tigcc_base);
#endif

  /* Allocate argument string */
  char *argstr=calloc(1,1);
  argstr=dynargcat(argstr,path_to_as);
  argstr=dynstrcat(argstr," --a68k");

  /* Translate arguments */
  int i;
  for (i=1; i<argc; i++) {
#define IGNORE(a) if (!strncmp(argv[i],(a),2)) continue;
#define FAIL(a) if (!strncmp(argv[i],a,2)) {free(argstr);fatal("switch " a " not supported");}
#define CONVERT(a,b) if (!strcmp(argv[i],(a))) argstr=dynstrcat(argstr,(b)); else
#define PARSE(a) else if (!strncmp(argv[i],(a),2)) {char *arg=argv[i]+2;

    IGNORE("-e")
    IGNORE("-f")
    IGNORE("-g")
    IGNORE("-n")
    IGNORE("-p")
    IGNORE("-q")
    IGNORE("-r")
    IGNORE("-s")
    IGNORE("-t")
    IGNORE("-w")
    IGNORE("-z")
    FAIL("-h");
    FAIL("-m");
    argstr=dynstrcat(argstr," ");
    CONVERT("-a","--all-relocs")
    CONVERT("-k","-Z")
    CONVERT("-u","--unaligned")
    CONVERT("-y","--statistics")
    if (!strncmp(argv[i],"-d",2)) argstr=dynstrcat(argstr,"-L");
    PARSE("-i")
      const char *incpath=strtok(arg,",;");
      argstr=dynstrcat(argstr,"-I ");
      argstr=dynargcat(argstr,incpath);
      while ((incpath=strtok(NULL,",;"))) {
        argstr=dynstrcat(argstr," -I ");
        argstr=dynargcat(argstr,incpath);
      }
    }
    PARSE("-o")
      argstr=dynstrcat(argstr,"-o ");
      argstr=dynargcat(argstr,arg);
    }
    PARSE("-l")
      argstr=dynstrcat(argstr,"-ahl=");
      argstr=dynargcat(argstr,arg);
    }
    PARSE("-x")
      argstr=dynstrcat(argstr,"-a=");
      argstr=dynargcat(argstr,arg);
    }
    PARSE("-v")
      char *p;
      argstr=dynstrcat(argstr,"--defsym ");
      if ((p=strchr(arg,','))) *p='=';
      if ((p=strchr(arg,';'))) *p='=';
      argstr=dynargcat(argstr,arg);
    } else {
      argstr=dynargcat(argstr,argv[i]);
    }
  }
 
  /* Run GNU as */
  int exitcode=run_cmdline(argstr);
  free(argstr);

  return exitcode;
} 
