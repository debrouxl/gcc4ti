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

#include "selectcolors.h"

#include <kcolordialog.h>
#include <kpushbutton.h> 
#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <QColor>
#include "colorlistitem.h"

SelectColors::SelectColors(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  okButton->setGuiItem(KStandardGuiItem::Ok);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
}

SelectColors::~SelectColors()
{
}

void SelectColors::colorList_selectionChanged()
{
  bool haveSelectedItem=!!colorList->selectedItem();
  removeButton->setEnabled(haveSelectedItem);
  editButton->setEnabled(haveSelectedItem);
}

void SelectColors::addButton_clicked()
{
  QColor color;
  if (KColorDialog::getColor(color,this)==KColorDialog::Accepted)
    new ColorListItem(colorList,color);
}

void SelectColors::removeButton_clicked()
{
  delete colorList->selectedItem();
}

void SelectColors::editButton_clicked()
{
  ColorListItem *item=static_cast<ColorListItem *>(colorList->selectedItem());
  if (item) {
    QColor color=item->color();
    if (KColorDialog::getColor(color,this)==KColorDialog::Accepted)
      item->setColor(color);
  }
}

void SelectColors::languageChange()
{
  retranslateUi(this);
}

