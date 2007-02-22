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

#include "toolsdlg.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>
#include <k3listview.h>
#include "ktigcc.h"
#include "toolprops.h"

void ToolsDialog::init()
{
  tempTools=tools;
  listView->clear();
  listView->setSorting(-1);
  Tools::iterator it;
  for(it=tempTools.begin(); it!=tempTools.end(); ++it)
    new K3ListViewItem(listView,listView->lastItem(),(*it).title,
                      (*it).commandLine,(*it).workingDirectory,
                      (*it).runInTerminal?"Yes":"No");
}

void ToolsDialog::addButton_clicked()
{
  toolIndex=-1;
  ToolProperties toolProperties;
  toolProperties.exec();
  if (toolProperties.result()==QDialog::Accepted) {
    Tool &newTool=tempTools.last();
    new K3ListViewItem(listView,listView->lastItem(),newTool.title,
                      newTool.commandLine,newTool.workingDirectory,
                      newTool.runInTerminal?"Yes":"No");
  } else listView_selectionChanged(); // set the real toolIndex again
}

void ToolsDialog::editButton_clicked()
{
  ToolProperties toolProperties;
  toolProperties.exec();
  if (toolProperties.result()==QDialog::Accepted) {
    Tool &newTool=tempTools[toolIndex];
    Q3ListViewItem *toolItem=listView->selectedItem();
    toolItem->setText(0,newTool.title);
    toolItem->setText(1,newTool.commandLine);
    toolItem->setText(2,newTool.workingDirectory);
    toolItem->setText(3,newTool.runInTerminal?"Yes":"No");
  }
}

void ToolsDialog::removeButton_clicked()
{
  tempTools.erase(&tempTools[toolIndex]);
  delete listView->selectedItem();
}

void ToolsDialog::listView_selectionChanged()
{
  toolIndex=listView->itemIndex(listView->selectedItem());
  editButton->setEnabled(toolIndex>=0);
  removeButton->setEnabled(toolIndex>=0);
}

void ToolsDialog::accept()
{
  tools=tempTools;
  QDialog::accept();
}

/*
 *  Constructs a ToolsDialog as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
ToolsDialog::ToolsDialog(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ToolsDialog::~ToolsDialog()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ToolsDialog::languageChange()
{
    retranslateUi(this);
}

