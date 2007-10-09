/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006-2007 Kevin Kofler

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

#include "toolprops.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <ktextedit.h>
#include <kurl.h>
#include <kurlrequester.h>
#include <kfiledialog.h>
#include <kshell.h>
#include <kpushbutton.h> 
#include "ktigcc.h"

ToolProperties::ToolProperties(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  okButton->setGuiItem(KStandardGuiItem::Ok);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
  workingDirectory->setMode(KFile::LocalOnly|KFile::ExistingOnly|KFile::Directory);

  if (toolIndex>=0) {
    Tool &tool=tempTools[toolIndex];
    toolTitle->setText(tool.title);
    commandLine->setText(tool.commandLine);
    if (!tool.workingDirectory.isEmpty())
      workingDirectory->setUrl(KUrl(tool.workingDirectory));
    runInTerminal->setChecked(tool.runInTerminal);
  }
}

ToolProperties::~ToolProperties()
{
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
  QString ret=KFileDialog::getOpenFileName(KUrl("/usr/bin"),
    "application/x-executable application/x-executable-script",this,
    "Choose executable");
  if (!ret.isEmpty())
    commandLine->setText(KShell::quoteArg(ret));
}

void ToolProperties::languageChange()
{
  retranslateUi(this);
}

