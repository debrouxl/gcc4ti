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

#include <qobject.h>
#include <qpopupmenu.h>
#include <qmap.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qvaluelist.h>
#include <kate/view.h>

struct CompletionInfo {
  CompletionInfo() : dirty(false), searched(false) {}
  bool dirty;
  bool searched;
  QStringList includedSystem;
  QStringList included;
  QMap<QString,unsigned> lineNumbers;
  QValueList<KTextEditor::CompletionEntry> entries;
};

// Maps file name to a CompletionInfo.
extern QMap<QString,CompletionInfo> systemHeaderCompletion, projectCompletion;

class MainForm;
bool findSymbolInFile(const QString &symbol,
                      const QString &fileText,
                      const QString &fileName,
                      MainForm *mainForm,
                      QString &symbolFile,
                      unsigned &symbolLine,
                      bool &systemHeader);
bool completionEntriesForFile(const QString &fileText,
                              const QString &fileName,
                              MainForm *mainForm,
                              QValueList<KTextEditor::CompletionEntry> &result);

class QWidget;
bool parseHelpSources(QWidget *parent, const QString &directory,
                      QMap<QString,CompletionInfo> &sysHdrCompletion);
bool parseSystemHeaders(QWidget *parent, const QString &directory,
                        QMap<QString,CompletionInfo> &sysHdrCompletion);

void loadSystemHeaderCompletion(void);
void saveSystemHeaderCompletion(void);

class TemplatePopup : public QPopupMenu {
  Q_OBJECT

  public:
    TemplatePopup(Kate::View *parent);
    virtual ~TemplatePopup() {}
  private slots:
    void QPopupMenu_activated(int id);
  private:
    Kate::View *view;
};

class CompletionPopup : public QObject {
  Q_OBJECT

  public:
    CompletionPopup(Kate::View *parent, const QString &fileName,
                    MainForm *mainForm, QObject *receiver);
    virtual ~CompletionPopup() {}

  signals:
    void closed();
};
