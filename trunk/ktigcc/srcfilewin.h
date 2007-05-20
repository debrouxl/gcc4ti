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

#include "ui_srcfilewin.h"
#include "tpr.h"
#include "parsing.h"

#include <QString>
class MainForm;
namespace KTextEditor {
  class Document;
  class View;
  class Cursor;
}
class KReplaceWithSelectionS;
class QLabel;
class Q3PopupMenu;
class QClipboard;
class QShortcut;
class KFindDialog;
class KDirWatch;
class FunctionDialog;

class SourceFileWindow : public QMainWindow, public Ui::SourceFileWindow
{
  Q_OBJECT

public:
  SourceFileWindow(MainForm *mainfrm, const QString &fn,
                   const QString &hlm, const bool *hle, void *cat,
                   bool isc, bool isasm, bool istxt);
  ~SourceFileWindow();

  void *createView(const QString &fileName, const QString &hlModeName, unsigned tabWidth);
  virtual int savePrompt(void);
  virtual void applyPreferences();

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

public slots:
  virtual void te_popup_aboutToShow();
  virtual void te_popup_activated(int index);
  virtual void shortcut_0_activated();
  virtual void shortcut_1_activated();
  virtual void shortcut_2_activated();
  virtual void shortcut_3_activated();
  virtual void shortcut_4_activated();
  virtual void shortcut_5_activated();
  virtual void shortcut_6_activated();
  virtual void shortcut_7_activated();
  virtual void shortcut_8_activated();
  virtual void shortcut_9_activated();
  virtual void shortcut_10_activated();
  virtual void fileSave();
  virtual void fileSaveAs();
  virtual void fileAddToProject();
  virtual void fileCompile();
  virtual void filePrint();
  virtual void filePrintQuickly();
  virtual void editUndo();
  virtual void editRedo();
  virtual void editClear();
  virtual void editCut();
  virtual void editCopy();
  virtual void editPaste();
  virtual void editSelectAll();
  virtual void editIncreaseIndent();
  virtual void editDecreaseIndent();
  virtual void findFind();
  virtual void findFind_next();
  virtual void findFind_highlight(const QString &text, int matchingindex, int matchedlength);
  virtual void findFind_stop();
  virtual void findReplace();
  virtual void findReplace_next();
  virtual void findReplace_highlight(const QString &text, int matchingindex, int matchedlength);
  virtual void findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength);
  virtual void findReplace_stop();
  virtual void findFunctions();
  virtual void findFunctions_functionListBox_highlighted(int index);
  virtual void findFunctions_functionListBox_selected(int index);
  virtual void findFunctions_prototypeButton_clicked();
  virtual void findFunctions_implementationButton_clicked();
  virtual void findFunctionsPopup_aboutToShow();
  virtual void findFunctionsPopup_aboutToHide();
  virtual void findFunctionsPopup_aboutToHide_async();
  virtual void findFunctionsPopup_activated(int id);
  virtual void findOpenFileAtCursor();
  virtual void findFindSymbolDeclaration();
  virtual void resizeEvent(QResizeEvent *event);
  virtual void statusBar_messageChanged(const QString &message);
  virtual void current_view_cursorPositionChanged(KTextEditor::View *view, const KTextEditor::Cursor &newPosition);
  virtual void current_view_textChanged(KTextEditor::Document *document);
  virtual void current_view_undoChanged();
  virtual void current_view_selectionChanged(KTextEditor::View *view);
  virtual void current_view_textInserted(KTextEditor::View *view, const KTextEditor::Cursor &position, const QString &text);
  virtual void clipboard_dataChanged();
  virtual void KDirWatch_dirty(const QString &fileName);
  virtual void completionPopup_closed();

protected:
  virtual void closeEvent(QCloseEvent *e);

protected slots:
  virtual void languageChange();

private:
  void removeTrailingSpacesFromView(void *view);
  void findReplace_next(bool firstTime);
  void updateSizes();
  void updateRightStatusLabel();
  void current_view_newLineHook();
  void shortcutActivated(int index);
};
