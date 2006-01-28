/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2006 Kevin Kofler

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

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <kapplication.h>
#include <kcmdlineargs.h>
#include <kaboutdata.h>
#include <qtextcodec.h>
#include <kconfig.h>
#include "mainform.h"
using namespace std;
void qCleanupImages_ktigcc();
void qInitImages_ktigcc();

const char *tigcc_base;
char tempdir[]="/tmp/ktigccXXXXXX";
char *quill_drv;
KConfig *pconfig;
KAboutData *pabout;

int main( int argc, char *argv[] )
{
  // We're doing plenty of C string <-> QString conversions, let's make sure
  // they're not lossy!
  QTextCodec::setCodecForCStrings(QTextCodec::codecForName("UTF-8"));

  KAboutData about("KTIGCC","TIGCC IDE for KDE","1.00",
  "TIGCC C and ASM SDK", KAboutData::License_GPL,
  "Copyright (C) 2004-2006 Kevin Kofler and Joey Adams. All rights reserved.\n"
  "TIGCC Copyright (C) 1999-2005 The TIGCC Team.",
  "Original linker by Xavier and Niklas\n"
  "Compiler modifications by Jean, Sebastian and Kevin\n"
  "Linker by Sebastian and Kevin\n"
  "Documentation by Zeljko\n"
  "A68k modifications by Kevin\n"
  "Windows IDE by Sebastian\n"
  "KTIGCC IDE by Kevin and Joey\n"
  "Documentation conversion to CHM by Philipp\n"
  "Documentation conversion to Qt Assistant by Kevin",
  "http://tigcc.ticalc.org/linux/", "Bugs@tigcc.ticalc.org");
  pabout=&about;
  KCmdLineArgs::init(argc,argv,&about);
  KApplication app;
  // Readd the images KDE kindly removes...
  qCleanupImages_ktigcc();
  qInitImages_ktigcc();
  KConfig config("ktigccrc");
  pconfig=&config;
  
  if ((tigcc_base = getenv("TIGCC")) == NULL) {
    fprintf(stderr, "Fatal error: TIGCC is not defined in the environment. "
                    "TIGCC must be defined before tigcc can run.\nFor (ba)sh, "
                    "try: export TIGCC=/path/to/tigcc\nFor (t)csh, try: setenv "
                    "TIGCC /path/to/tigcc\n");
    exit(-1);
  }
  quill_drv = (char *) malloc (strlen(tigcc_base) + 25);
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
  free(quill_drv);
  quill_drv=NULL;
  quill_drv_found:

  /* Make sure the Kate color schema we need exists */
  char *home=getenv("HOME");
  char fname[strlen(home)+32];
  sprintf(fname,"%s/.kde/share/config/kateschemarc",home);
  FILE *f=fopen(fname,"r+b");
  if (!f) f=fopen(fname,"w+b");
  if (!f) exit(1);
  fseek(f,0,SEEK_END);
  size_t flen=ftell(f);
  fseek(f,0,SEEK_SET);
  char buffer[flen+1];
  memset(buffer,0,flen+1);
  if (fread(buffer,1,flen,f)<flen) exit(1);
  if (!strstr(buffer,"[ktigcc - Grayed Out]\n")) {
    fseek(f,0,SEEK_END);
    fputs("\n\n[ktigcc - Grayed Out]\n"
          "Color Background=230,230,230\n"
          "Color Highlighted Bracket=255,255,153\n"
          "Color Highlighted Line=230,230,230\n"
          "Color Icon Bar=234,233,232\n"
          "Color Line Number=0,0,0\n"
          "Color MarkType1=0,0,255\n"
          "Color MarkType2=255,0,0\n"
          "Color MarkType3=255,255,0\n"
          "Color MarkType4=255,0,255\n"
          "Color MarkType5=160,160,164\n"
          "Color MarkType6=0,255,0\n"
          "Color MarkType7=255,0,0\n"
          "Color Selection=76,89,166\n"
          "Color Tab Marker=0,0,0\n"
          "Color Word Wrap Marker=120,120,120\n"
          "Font=Monospace,10,-1,5,50,0,0,0,1,0\n",f);
  }
  fclose(f);
  
  if (!mkdtemp(tempdir)) exit(1);
  
  MainForm mainForm;
  app.setMainWidget( &mainForm );
  mainForm.show();
  
  int exitcode=app.exec();

  rmdir(tempdir);
  
  return exitcode;
}

void write_temp_file(const char *filename, const char *data, const size_t len)
{
  char buffer[strlen(tempdir)+strlen(filename)+2];
  sprintf(buffer,"%s/%s",tempdir,filename);
  FILE *f=fopen(buffer,"wb");
  if (!f) {error: fputs("Fatal error: Can't write temp file!\n",stderr); exit(1);}
  if (len) {
    if (fwrite(data,1,len,f)<len) goto error;
  } else {
    if (fputs(data,f)==EOF) goto error;
  }
  fclose(f);
}

void delete_temp_file(const char *filename)
{
  char buffer[strlen(tempdir)+strlen(filename)+2];
  sprintf(buffer,"%s/%s",tempdir,filename);
  if (unlink(buffer)) {fputs("Fatal error: Can't delete temp file!\n",stderr); exit(1);}
}

void force_qt_assistant_page(int n)
{
  char *home=getenv("HOME");
  char fname[strlen(home)+20];
  sprintf(fname,"%s/.qt/qt_assistantrc",home);
  FILE *f=fopen(fname,"r+b");
  if (!f) f=fopen(fname,"w+b");
  if (!f) exit(1);
  fseek(f,0,SEEK_END);
  size_t flen=ftell(f);
  fseek(f,0,SEEK_SET);
  char buffer[flen+1];
  memset(buffer,0,flen+1);
  if (fread(buffer,1,flen,f)<flen) exit(1);
  char *p=strstr(buffer,"SideBarPage=");
  if (p) {
    fseek(f,p-buffer+12,SEEK_SET);
    fputc('0'+n,f);
  } else {
    fseek(f,0,SEEK_END);
    fprintf(f,"\n\n[3.3]\nSideBarPage=%d\n",n);
  }
  fclose(f);
}
