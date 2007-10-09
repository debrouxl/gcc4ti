/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2007 Kevin Kofler
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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <QString>
#include <QImage>
#include <QDir>
#include <QSettings>
#include <kapplication.h>
#include <kcmdlineargs.h>
#include <kaboutdata.h>
#include <QTextCodec>
#include <QIcon>
#include <QSize> 
#include <QPixmap>
#include <kglobal.h>
#include <kicontheme.h>
#include <kiconloader.h>
#include <kconfig.h>
#include <glib.h>
#include "mainform.h"
#include "tpr.h"
using namespace std;

// convert to KLocalizedString without translating (we don't support NLS)
#define KLS(x) ki18nc("untranslatable",(x))

const char *tigcc_base;
char tempdir[]="/tmp/ktigccXXXXXX";
char *quill_drv;
bool have_fargo;
bool have_flashos;
KSharedConfigPtr pconfig;
KAboutData *pabout;
const char *parg;

static void log_eater(const gchar *log_domain __attribute__((unused)),
                      GLogLevelFlags log_level __attribute__((unused)),
                      const gchar *message __attribute__((unused)),
                      gpointer user_data __attribute__((unused)))
{
  // Do nothing.
}

int main(int argc, char *argv[])
{
  // Create the libticonv text codec.
  new TiconvTextCodec();
  // Match the locale for the default C string <-> QString conversions.
  // Hopefully it is a .UTF-8 locale, if it isn't, don't complain about
  // characters lost converting!
  QTextCodec::setCodecForCStrings(QTextCodec::codecForLocale());

  KAboutData about("ktigcc", QByteArray(), // catalogName
  KLS("TIGCC IDE for KDE"),"1.80",
  KLS("TIGCC C and ASM SDK"), KAboutData::License_GPL,
  KLS("Copyright (C) 2004-2007 Kevin Kofler and Joey Adams. All rights reserved.\n"
  "TIGCC Copyright (C) 1999-2007 The TIGCC Team."),
  KLS("Original linker by Xavier and Niklas\n"
  "Compiler modifications by Jean, Sebastian and Kevin\n"
  "Linker by Sebastian and Kevin\n"
  "Documentation by Zeljko\n"
  "A68k modifications by Kevin\n"
  "Windows IDE by Sebastian\n"
  "KTIGCC IDE by Kevin and Joey\n"
  "Documentation conversion to CHM by Philipp\n"
  "Documentation conversion to Qt Assistant by Kevin"),
  "http://tigcc.ticalc.org/linux/", "Bugs@tigcc.ticalc.org");
  pabout=&about;
  KCmdLineArgs::init(argc,argv,&about);
  KCmdLineOptions options;
  options.add("+[file]", KLS("Project or file to open at startup"));
  KCmdLineArgs::addCmdLineOptions(options);
  KCmdLineArgs::addStdCmdLineOptions();
  KApplication app;
  about.setProgramLogo(QPixmap(":/images/icon.png"));
  pconfig=KGlobal::config();
  
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

  char runtime_archive[strlen(tigcc_base) + 15];
  sprintf(runtime_archive, "%s/lib/fargo.a", tigcc_base);
  have_fargo=(access(runtime_archive, F_OK)!=-1);
  sprintf(runtime_archive, "%s/lib/flashos.a", tigcc_base);
  have_flashos=(access(runtime_archive, F_OK)!=-1);
  
  if (!mkdtemp(tempdir)) exit(1);
  
  KCmdLineArgs *args=KCmdLineArgs::parsedArgs();
  
  if (args->count())
    parg=args->arg(0);
  else
    parg=NULL;

  // Keep the tilibs from spamming the console with INFO messages.
  g_log_set_handler("ticables",(GLogLevelFlags)(G_LOG_LEVEL_INFO|G_LOG_LEVEL_DEBUG),
                    log_eater,NULL);
  g_log_set_handler("tifiles",(GLogLevelFlags)(G_LOG_LEVEL_INFO|G_LOG_LEVEL_DEBUG),
                    log_eater,NULL);
  g_log_set_handler("ticalcs",(GLogLevelFlags)(G_LOG_LEVEL_INFO|G_LOG_LEVEL_DEBUG),
                    log_eater,NULL);

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

static bool clear_dir(const QString &dirname)
{
  QDir qdir(dirname);
  QStringList subdirs=qdir.entryList(QDir::Dirs|QDir::Hidden);
  for (QStringList::Iterator it=subdirs.begin();it!=subdirs.end();++it) {
    if ((*it).compare(".") && (*it).compare("..")) {
      QString subdir=dirname+"/"+*it;
      if (!clear_dir(subdir)) return FALSE;
      if (!qdir.rmdir(subdir)) return FALSE;
    }
  }
  QStringList files=qdir.entryList(QDir::Files|QDir::Hidden);
  for (QStringList::Iterator it=files.begin();it!=files.end();++it) {
    QString file=dirname+"/"+*it;
    if (!qdir.remove(file)) return FALSE;
  }
  return TRUE;
}

void clear_temp_dir(void)
{
  if (!clear_dir(tempdir)) {fputs("Fatal error: Can't delete temp file!\n",stderr); exit(1);};
}

void force_qt_assistant_page(int n)
{
  QSettings assistantSettings("Trolltech","Assistant");
  const char *qtVersion=qVersion();
  int qtMajor, qtMinor;
  sscanf(qtVersion, "%d.%d", &qtMajor, &qtMinor);
  assistantSettings.beginGroup(QString("%1.%2").arg(qtMajor).arg(qtMinor));
  assistantSettings.setValue("SideBarPage",n);
}

const char *lookup_doc_keyword(const char *keyword)
{
  static char filename[256];
  memset(filename,0,256);
  char fname[strlen(tigcc_base)+27];
  sprintf(fname,"%s/doc/html/qt-assistant.adp",tigcc_base);
  FILE *f=fopen(fname,"r+b");
  if (!f) return "";
  char buffer[32768], keywbuf[32768];
  while (!feof(f)) {
    if (!fgets(buffer,32768,f)) {fclose(f); return "";}
    memset(keywbuf,0,32768);
    if (sscanf(buffer,"<keyword ref=\"%255[^\"]\">%32767[^<]</keyword>",filename,keywbuf)<2)
      continue;
    if (!strcmp(keywbuf,keyword)) {fclose(f); return filename;}
  }
  fclose(f);
  return "";
}
