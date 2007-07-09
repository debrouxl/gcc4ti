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

#include "functions.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <kpushbutton.h> 

FunctionDialog::FunctionDialog(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
}

FunctionDialog::~FunctionDialog()
{
}

// Make this slot public.
void FunctionDialog::accept()
{
  QDialog::accept();
}

void FunctionDialog::languageChange()
{
  retranslateUi(this);
}

