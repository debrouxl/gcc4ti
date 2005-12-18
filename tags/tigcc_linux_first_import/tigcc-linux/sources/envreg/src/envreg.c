/*
   envreg - Register TIGCC environment variables into bashrc

   Copyright (C) 2004 Kevin Kofler

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
#include <unistd.h>

int main(void) {
  char *bashrc;
  char *tigcc;
  char *oldtigcc=NULL;
  FILE *f=NULL;
  char **lines=NULL;
  int numlines=0;
  int i;
  char *p;

  // Find out what bashrc file to use
  if (geteuid()) {
    const char *homedir;
    puts("envreg: Running as normal user");
    homedir=getenv("HOME");
    if (!homedir) {
      puts("envreg: error: $HOME not set");
      return 1;
    }
    bashrc=malloc(strlen(homedir)+9);
    if (!bashrc) goto outofmem;
    strcpy(bashrc,homedir);
    if (bashrc[strlen(bashrc)-1]!='/') strcat(bashrc,"/");
    strcat(bashrc,".bashrc");
  } else {
    puts("envreg: Running as root");
    bashrc=malloc(12);
    if (!bashrc) goto outofmem;
    strcpy(bashrc,"/etc/bashrc");
  }
  printf("envreg: Using bashrc file: `%s'\n",bashrc);

  // Read the current file contents
  f=fopen(bashrc,"rb");
  // If we can't open the file, assume it doesn't exist and treat it as empty
  if (f) {
    // Get the file size
    fseek(f,0,SEEK_END);
    i=ftell(f);
    fseek(f,0,SEEK_SET);

    // Allocate a buffer for the entire file and read it into memory
    lines=malloc(sizeof(unsigned char *));
    if (!lines) goto outofmem;
    *lines=malloc(i+1);
    if (!*lines) goto outofmem;
    fread(*lines,1,i,f);
    (*lines)[i]=0;

    // Close the file
    fclose(f);
    f=NULL;

    // Now split the file into lines
    while((p=strchr(lines[numlines],'\n'))) {
      lines=realloc(lines,((++numlines)+1)*sizeof(unsigned char *));
      if (!lines) goto outofmem;
      lines[numlines]=malloc(strlen(p+1)+1);
      if (!lines[numlines]) goto outofmem;
      strcpy(lines[numlines],p+1);
      *p=0;
      if (p[-1]=='\r') p[-1]=0;
      lines[numlines-1]=realloc(lines[numlines-1],strlen(lines[numlines-1])+1);
      if (!lines[numlines-1]) goto outofmem; // Some stupid systems copy even when cutting.
    }
    // If the last line is empty, drop it, else count it
    if (*(lines[numlines])) numlines++; else {
      free(lines[numlines]);
      lines=realloc(lines,numlines*sizeof(unsigned char *));
      if (!lines) goto outofmem; // Some stupid systems copy even when cutting.
    }
  } else {
    printf("envreg: `%s' doesn't exist, creating a new one\n",bashrc);
  }

  // Look for existing $TIGCC setting
  for (i=0;i<numlines;i++) {
    if (!strncmp(lines[i],"export TIGCC=",13)) {
      // Get old $TIGCC setting
      oldtigcc=malloc(strlen(lines[i])-12);
      strcpy(oldtigcc,lines[i]+13+(lines[i][13]=='\"'));
      if (oldtigcc[strlen(oldtigcc)-1]=='\"') oldtigcc[strlen(oldtigcc)-1]=0;
      printf("envreg: $TIGCC previously set to `%s'\n",oldtigcc);
      break;
    }
  }

  if (i==numlines) {
    // Add new $TIGCC setting
    i=numlines;
    lines=realloc(lines,++numlines*sizeof(unsigned char *));
    if (!lines) goto outofmem;
    lines[i]=NULL;
  }

  // Add the setting
  tigcc=getenv("TIGCC");
  if (!tigcc) {
    puts("envreg: error: $TIGCC not set");
    return 1;
  }
  printf("envreg: setting $TIGCC to `%s'\n",tigcc);
  lines[i]=realloc(lines[i],strlen(tigcc)+16);
  if (!lines[i]) goto outofmem;
  strcpy(lines[i],"export TIGCC=\"");
  strcat(lines[i],tigcc);
  strcat(lines[i],"\"");

  if (oldtigcc) {
    // Append "/bin" to oldtigcc
    oldtigcc=realloc(oldtigcc,strlen(oldtigcc)+5);
    if (!oldtigcc) goto outofmem;
    if (oldtigcc[strlen(oldtigcc)-1]!='/') strcat(oldtigcc,"/");
    strcat(oldtigcc,"bin");
  }

  // Look for existing $PATH settings
  for (i=0;i<numlines;i++) {
    if (!strncmp(lines[i],"export PATH=",12)) {
      // Check if it contains literal $TIGCC/bin
      if (strstr(lines[i],"$TIGCC/bin")) {
        puts("envreg: $PATH already contains $TIGCC/bin");
        goto writeout;
      }
      // Check if it contains old $TIGCC/bin
      while (oldtigcc && (p=strstr(lines[i],oldtigcc))
             && (p[-1]=='='||(p[-1]=='\"'&&p[-2]=='=')||p[-1]==':')) {
        int l;
        printf("envreg: $PATH contains `%s', removing\n",oldtigcc);
        l=strlen(oldtigcc);
        while (p[l] && p[l]!='\"' && p[l]!=':') l++; // look for end
        if ((!p[l]||(p[l]=='\"'&&!p[l+1]))
            && (p[-1]=='='||(p[-1]=='\"'&&p[-2]=='=')))
          *(lines[i])=0; // single entry, so zap entire line
        if ((!p[l]||(p[l]=='\"'&&!p[l+1]))) p--; // last entry, must zap : in
                                               // in front, not afterwards
        l++; // zap :
        memmove(p,p+l,strlen(p+l)+1);
      }
    }
  }

  // Add new $PATH setting
  puts("envreg: appending $TIGCC/bin to $PATH");
  i=numlines;
  lines=realloc(lines,++numlines*sizeof(unsigned char *));
  if (!lines) goto outofmem;
  lines[i]=malloc(31);
  strcpy(lines[i],"export PATH=\"$PATH:$TIGCC/bin\"");

writeout:
  // Write out the new file
  f=fopen(bashrc,"wt");
  // If we can't open the file, now, we have a permissions problem
  if (!f) {
    printf("envreg: error: can't write to `%s'\n",bashrc);
    return 1;
  }

  // Write out each line
  for (i=0;i<numlines;i++) {
    fprintf(f,"%s\n",lines[i]);
  }

  // Close the file
  fclose(f);
  f=NULL;

  return 0;

outofmem:
  if (f) fclose(f);
  puts("envreg: error: Out of memory");
  return -1;
}
