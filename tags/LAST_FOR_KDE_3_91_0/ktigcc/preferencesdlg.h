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

#include "ui_preferencesdlg.h"

class Preferences : public QDialog, public Ui::Preferences
{
    Q_OBJECT

public:
    Preferences(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~Preferences();

public slots:
    virtual void linkTarget_toggled(bool on);
    virtual void bgColorChange_clicked();
    virtual void editorFontChange_clicked();
    virtual void syntaxLanguage_activated(int index);
    virtual void syntaxEnabled_toggled(bool on);
    virtual void resetButton_clicked();
    virtual void numberColorButton_clicked();
    virtual void numberStyleButton_clicked();
    virtual void symbolColorButton_clicked();
    virtual void symbolStyleButton_clicked();
    virtual void parenthesisColorsButton_clicked();
    virtual void parenthesisStyleButton_clicked();
    virtual void syntaxListView_selectionChanged();
    virtual void syntaxListView_itemRenamed(Q3ListViewItem *item, const QString &str, int col);
    virtual void syntaxListViewShortcut_activated();
    virtual void newStyleButton_clicked();
    virtual void newListButton_clicked();
    virtual void editButton_clicked();
    virtual void editDialog_colorButton_clicked();
    virtual void editDialog_styleButton_clicked();
    virtual void clearSelectionButton_clicked();
    virtual void applyButton_clicked();
    virtual void templateListBox_selectionChanged();
    virtual void templateListBox_currentChanged(Q3ListBoxItem *item);
    virtual void templateIdentifier_textChanged(const QString &text);
    virtual void regenCompletionInfoButton_clicked();

protected slots:
    virtual void languageChange();

};
