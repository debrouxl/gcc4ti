/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2007 Kevin Kofler

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

#pragma once

#include "ui_toolsdlg.h"

class ToolsDialog : public QDialog, public Ui::ToolsDialog
{
    Q_OBJECT

public:
    ToolsDialog(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ToolsDialog();

public slots:
    virtual void init();
    virtual void addButton_clicked();
    virtual void editButton_clicked();
    virtual void removeButton_clicked();
    virtual void listView_selectionChanged();
    virtual void accept();

protected slots:
    virtual void languageChange();

};
