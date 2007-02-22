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

#include "ui_errorlist.h"

class ErrorList : public QWidget, public Ui::ErrorList
{
    Q_OBJECT

public:
    ErrorList(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = 0);
    ~ErrorList();

protected:
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void focusInEvent(QFocusEvent *);
    virtual void focusOutEvent(QFocusEvent *);

protected slots:
    virtual void languageChange();

};
