/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006 Kevin Kofler

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

#include "srcfilewin.h"

#include <qstring.h>
namespace Kate {
  class View;
}
class KReplaceWithSelectionS;
class QLabel;
class QPopupMenu;
class QClipboard;
class QAccel;
class KFindDialog;

struct SourceFile : public SourceFileWindow {
  SourceFile(const QString &fn, const QString &hlm, bool isc, bool isasm) :
    SourceFileWindow(), fileName(fn), hlMode(hlm), isCFile(isc),
    isASMFile(isasm) {}

  QString fileName;
  QString hlMode;
  bool isCFile;
  bool isASMFile;

  KReplaceWithSelectionS *kreplace;
  Kate::View *kateView;
  QLabel *rowStatusLabel;
  QLabel *colStatusLabel;
  QLabel *charsStatusLabel;
  QLabel *rightStatusLabel;
  QPopupMenu *te_popup;
  QAccel *accel;
  KFindDialog *kfinddialog;
  unsigned findCurrentLine;
};
