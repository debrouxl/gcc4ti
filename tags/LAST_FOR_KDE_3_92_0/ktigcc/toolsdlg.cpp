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

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <k3listview.h>
#include <kpushbutton.h> 
#include "ktigcc.h"
#include "toolprops.h"

ToolsDialog::ToolsDialog(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  okButton->setGuiItem(KStandardGuiItem::Ok);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
  tempTools=tools;
  listView->clear();
  listView->setSorting(-1);
  listView->header()->setClickEnabled(false);
  listView->header()->setResizeEnabled(false);
  Tools::iterator it;
  for(it=tempTools.begin(); it!=tempTools.end(); ++it)
    new K3ListViewItem(listView,listView->lastItem(),(*it).title,
                      (*it).commandLine,(*it).workingDirectory,
                      (*it).runInTerminal?"Yes":"No");
}

ToolsDialog::~ToolsDialog()
{
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

void ToolsDialog::languageChange()
{
  retranslateUi(this);
}

