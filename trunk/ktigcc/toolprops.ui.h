/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/

/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006 Kevin Kofler

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

#include <ktextedit.h>
#include <kurl.h>
#include <kurlrequester.h>
#include <kfiledialog.h>
#include <kprocess.h>
#include "ktigcc.h"

void ToolProperties::init()
{
  if (toolIndex>=0) {
    Tool &tool=tempTools[toolIndex];
    toolTitle->setText(tool.title);
    commandLine->setText(tool.commandLine);
    if (!tool.workingDirectory.isEmpty())
      workingDirectory->setKURL(KUrl::fromPathOrUrl(tool.workingDirectory));
    runInTerminal->setChecked(tool.runInTerminal);
  }
}

void ToolProperties::accept()
{
  Tool tool(toolTitle->text(),commandLine->text(),
            workingDirectory->url().isEmpty()?QString():
            KUrl(workingDirectory->url()).path(),runInTerminal->isChecked());
  if (toolIndex>=0)
    tempTools[toolIndex]=tool;
  else
    tempTools.append(tool);
  QDialog::accept();
}

void ToolProperties::validate()
{
  okButton->setEnabled(!toolTitle->text().isEmpty()
                       && !commandLine->text().isEmpty()
                       && (workingDirectory->url().isEmpty()
                           || (KUrl(workingDirectory->url()).isValid()
                               && KUrl(workingDirectory->url()).isLocalFile())));
}

void ToolProperties::browseButton_clicked()
{
  QString ret=KFileDialog::getOpenFileName("/usr/bin",
    "application/x-executable application/x-executable-script",this,
    "Choose executable");
  if (!ret.isEmpty())
    commandLine->setText(KProcess::quote(ret));
}
