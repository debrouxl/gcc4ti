/*
   ktigcc - TIGCC IDE for KDE

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

#include <stdlib.h>
#include <unistd.h>
#include <kapplication.h>
#include <kcmdlineargs.h>
#include "mainform.h"
void qCleanupImages_ktigcc();
void qInitImages_ktigcc();

const char *tigcc_base;
char *quill_drv;

int main( int argc, char *argv[] )
{
  KCmdLineArgs::init(argc,argv,"KTIGCC","TIGCC IDE for KDE","1.00");
  KApplication app;
  // Readd the images KDE kindly removes...
  qCleanupImages_ktigcc();
  qInitImages_ktigcc();
  
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
  
  MainForm mainForm;
  app.setMainWidget( &mainForm );
  mainForm.show();
  
  return app.exec();
}
