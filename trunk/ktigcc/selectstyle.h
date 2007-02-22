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

#include "ui_selectstyle.h"

class SelectStyle : public QDialog, public Ui::SelectStyle
{
    Q_OBJECT

public:
    SelectStyle(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~SelectStyle();

public slots:
    virtual void customStyle_toggled( bool on );

protected slots:
    virtual void languageChange();

};
