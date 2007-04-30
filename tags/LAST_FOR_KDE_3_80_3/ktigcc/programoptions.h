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

#include "ui_programoptions.h"

class ProgramOptions : public QDialog, public Ui::ProgramOptions
{
    Q_OBJECT

public:
    ProgramOptions(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ProgramOptions();

    void ImportSettings();
    void ExportSettings();

public slots:
    virtual void CMinimumAMSVersion_toggled( bool on );
    virtual void CalcCheckbox_toggled( bool on_unused );
    virtual void KernelRadiobutton_toggled( bool on_unused );
    virtual void RelocSettings_toggled( bool on_unused );

protected:
    virtual void mousePressEvent( QMouseEvent * e );

protected slots:
    virtual void languageChange();

};
