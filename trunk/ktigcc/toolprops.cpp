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

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>
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

/*
 *  Constructs a ToolProperties as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
ToolProperties::ToolProperties(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ToolProperties::~ToolProperties()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ToolProperties::languageChange()
{
    retranslateUi(this);
}

