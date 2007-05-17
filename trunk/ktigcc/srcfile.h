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

#pragma once

#include "tpr.h"
#include "srcfilewin.h"
#include "parsing.h"

#include <QString>
#include <Q3PopupMenu>
#include <QLabel>
class MainForm;
namespace KTextEditor {
  class View;
}
class KReplaceWithSelectionS;
class QLabel;
class Q3PopupMenu;
class QClipboard;
class QShortcut;
class KFindDialog;
class KDirWatch;
class FunctionDialog;

struct SourceFile : public SourceFileWindow {
  SourceFile(MainForm *mainfrm, const QString &fn,
             const QString &hlm, const bool *hle, void *cat,
             bool isc, bool isasm, bool istxt) :
    SourceFileWindow(), mainForm(mainfrm), fileName(fn),
    hlMode(hlm), hlEnabled(hle), category(cat),
    isCFile(isc), isASMFile(isasm), isTextFile(istxt)
  {
    initBase(); // We can do this only after initializing the variables here.
    show();
  }

  MainForm *mainForm;
  QString fileName;
  QString hlMode;
  const bool *hlEnabled;
  void *category;
  bool isCFile;
  bool isASMFile;
  bool isTextFile;
  LineStartList lineStartList;

  KReplaceWithSelectionS *kreplace;
  KTextEditor::View *kateView;
  QLabel *rowStatusLabel;
  QLabel *colStatusLabel;
  QLabel *charsStatusLabel;
  QLabel *rightStatusLabel;
  Q3PopupMenu *te_popup;
  QShortcut *shortcuts[11];
  KFindDialog *kfinddialog;
  int findCurrentLine;
  KDirWatch *dirWatch;
  Q3PopupMenu *findFunctionsPopup;
  SourceFileFunctions sourceFileFunctions;
  FunctionDialog *functionDialog;
};
