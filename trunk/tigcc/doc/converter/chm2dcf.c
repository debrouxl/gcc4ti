/*
   CHM2DCF - converter for HHC and HHK files used by the Microsoft CHM compiler
             to a QT Assistant DCF file

   Copyright (C) 2003-2004 Kevin Kofler

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

#define fatal(s) ({fprintf(stderr,"Error: %s\n",s);exit(1);})

int main(void)
{
  FILE *hhc,*hhk,*dcf;
  char buffer[32768];
  char **handledrefs=0;
  int handledrefc=0;
  int i;
  const char *close="";
  hhc=fopen("Contents.hhc","rt");
  if (!hhc) fatal("Failed to open Contents.hhc.");
  hhk=fopen("Index.hhk","rt");
  if (!hhk) {fclose(hhc);fatal("Failed to open Index.hhk.");}
  dcf=fopen("qt-assistant.dcf","wt");
  if (!dcf) {fclose(hhc);fclose(hhk);fatal("Failed to open qt-assistant.dcf.");}
  fputs("<!DOCTYPE DCF>\n<DCF ref=\"index.html\" category=\"development\" "
        "title=\"TIGCC Documentation\">\n",dcf);
  for (i=0;i<12;i++) fgets(buffer,32768,hhc); // skip 11 junk lines, read 12th
                                              // line
  while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
    buffer[strlen(buffer)-1]=0; // drop line ending characters
  while (strcmp(buffer,"</BODY>")) {
    if (!strcmp(buffer,"<LI> <OBJECT TYPE=\"TEXT/SITEMAP\">")) {
      char title[32768],ref[32768];
      fgets(buffer,32768,hhc); // get title
      while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
        buffer[strlen(buffer)-1]=0; // drop line ending characters
      if (strncmp(buffer,"<PARAM NAME=\"Name\" VALUE=\"",26)) goto junk;
      strcpy(title,buffer+26);
      if (strlen(title)<2||title[strlen(title)-1]!='>'
          ||title[strlen(title)-2]!='\"') {
        printf("Invalid title: %s\n",title);
        goto junk;
      }
      title[strlen(title)-2]=0;
      fgets(buffer,32768,hhc); // get ref
      while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
        buffer[strlen(buffer)-1]=0; // drop line ending characters
      if (!strcmp(buffer,"</OBJECT>")) {
        fprintf(dcf,"%s<section ref=\"index.html\" title=\"%s\"",close,title);
        close="/>\n";
      } else {
        char keywordref[32768];
        int lineno;
        if (strncmp(buffer,"<PARAM NAME=\"Local\" VALUE=\"",27)) goto junk;
        strcpy(ref,buffer+27);
        if (strlen(ref)<2||ref[strlen(ref)-1]!='>'
            ||ref[strlen(ref)-2]!='\"') {
          printf("Invalid ref: %s\n",ref);
          goto junk;
        }
        ref[strlen(ref)-2]=0;
        fgets(buffer,32768,hhc); // drop junk line
        fprintf(dcf,"%s<section ref=\"%s\" title=\"%s\"",close,ref,title);
        close="/>\n";
        // skip keyword handling if we already had this URL
        for (i=0;i<handledrefc;i++)
          if (!strcmp(ref,handledrefs[i])) goto skipkwd;
        // record this URL
        handledrefs=realloc(handledrefs,(++handledrefc)*sizeof(char *));
        handledrefs[handledrefc-1]=strdup(ref);
        sprintf(keywordref,"<PARAM NAME=\"Local\" VALUE=\"%s",ref);
        lineno=0;
        fseek(hhk,0,SEEK_SET);
        do {
          char kwdref[32768],kwd[32768],fullkwd[32768];
          int ullevel=0;
          while(1) { // search for any keywords for this section
            int tempullevel;
            lineno++;
            fgets(buffer,32768,hhk);
            if (feof(hhk)) {lineno=0;break;}
            if (!strncmp(buffer,"<UL>",4)) {ullevel++;continue;}
            if (!strncmp(buffer,"</UL>",5)) {ullevel--;continue;}
            while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
              buffer[strlen(buffer)-1]=0; // drop line ending characters
            if (!strncmp(buffer,keywordref,strlen(keywordref))) { // keyword found
              int goback=0;
              strcpy(kwdref,buffer+27);
              if (strlen(kwdref)<2||kwdref[strlen(kwdref)-1]!='>'
                  ||kwdref[strlen(kwdref)-2]!='\"') {
                printf("Invalid keyword ref: %s\n",kwdref);
                kwdjunk:
                fclose(hhc);
                fclose(hhk);
                fclose(dcf);
                printf("Invalid line: %s\n",buffer);
                fatal("Junk line in Index.hhk.");
              }
              kwdref[strlen(kwdref)-2]=0;
              *fullkwd=0;
              tempullevel=ullevel;
              up2lines:
              fseek(hhk,0,SEEK_SET); // search 2 lines higher for the title
              goback+=2;
              for (i=0;i<lineno-goback;i++) fgets(buffer,32768,hhk);
              while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
                buffer[strlen(buffer)-1]=0; // drop line ending characters
              if (!strncmp(buffer,"<PARAM NAME=\"Local\" VALUE=\"",27)) goto up2lines;
              if (strncmp(buffer,"<PARAM NAME=\"Name\" VALUE=\"",26)) goto kwdjunk;
              strcpy(kwd,buffer+26);
              if (strlen(kwd)<2||kwd[strlen(kwd)-1]!='>'
                  ||kwd[strlen(kwd)-2]!='\"') {
                printf("Invalid keyword: %s\n",kwd);
                goto kwdjunk;
              }
              kwd[strlen(kwd)-2]=0;
              if (*fullkwd) {
                memmove(fullkwd+strlen(kwd)+2,fullkwd,strlen(fullkwd)+1);
                strncpy(fullkwd,kwd,strlen(kwd));
                fullkwd[strlen(kwd)]=',';fullkwd[strlen(kwd)+1]=' ';
              } else strcpy(fullkwd,kwd);
              if (tempullevel>=2) {
                // This is actually just a disambiguator, so we need to go up for the actual
                // keyword.
                fseek(hhk,0,SEEK_SET); // search 2 lines higher for <UL>
                goback+=2;
                for (i=0;i<lineno-goback;i++) fgets(buffer,32768,hhk);
                if (!strncmp(buffer,"<UL>",4)) {goback+=2; tempullevel--; goto up2lines;}
                while (1) { // We know there is an <UL> somewhere, so keep looking.
                  fseek(hhk,0,SEEK_SET); // search 5 lines higher for <UL>
                  goback+=5;
                  for (i=0;i<lineno-goback;i++) fgets(buffer,32768,hhk);
                  if (!strncmp(buffer,"<UL>",4)) {goback+=2; tempullevel--; goto up2lines;}
                }
              }
              if (!strcmp(close,"/>\n")) fputs(">\n",dcf);
              fprintf(dcf,"<keyword ref=\"%s\">%s</keyword>\n",kwdref,fullkwd);
              close="</section>\n";
              for (i=0;i<goback;i++) fgets(buffer,32768,hhk); // go back to the original line
            }
          }
        } while (lineno);
        skipkwd:;
      }
    } else if (!strcmp(buffer,"<UL>")) {
      if (!strcmp(close,"</section>\n")) close=""; else close=">\n";
    } else if (!strcmp(buffer,"</UL>")) {
      fprintf(dcf,"%s",close);
      close="</section>\n";
    } else {
      junk:
      fclose(hhc);
      fclose(hhk);
      fclose(dcf);
      printf("Invalid line: %s\n",buffer);
      fatal("Junk line in Contents.hhc.");
    }
    fgets(buffer,32768,hhc); // read next line
    while (buffer[strlen(buffer)-1]=='\r'||buffer[strlen(buffer)-1]=='\n')
      buffer[strlen(buffer)-1]=0; // drop line ending characters
  }
  fputs("</DCF>\n",dcf);
  fclose(hhc);
  fclose(hhk);
  fclose(dcf);
  return 0;
}
