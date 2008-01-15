/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006-2007 Kevin Kofler
   Copyright (C) 2007 Konrad Meyer

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

#include <QObject>
#include <Q3PopupMenu>
#include <QMap>
#include <QString>
#include <QStringList>
#include <Q3ValueList>
#include <QList>
#include <QEvent>
#include <ktexteditor/view.h>
#include <ktexteditor/codecompletionmodel.h>

struct CompletionEntry
{
  QString type;
  QString text;
  QString prefix;
  QString postfix;
  QString comment;

  QString userdata;

  bool operator==( const CompletionEntry &c ) const {
    return c.type == type
           && c.text == text
           && c.postfix == postfix
           && c.prefix == prefix
           && c.comment == comment
           && c.userdata == userdata;
  }
};

struct CompletionInfo {
  CompletionInfo() : dirty(false), searched(false) {}
  bool dirty;
  bool searched;
  QStringList includedSystem;
  QStringList included;
  QMap<QString,unsigned> lineNumbers;
  QList<CompletionEntry> entries;
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
                              QList<CompletionEntry> &result);

class QWidget;
bool parseHelpSources(QWidget *parent, const QString &directory,
                      QMap<QString,CompletionInfo> &sysHdrCompletion);
bool parseSystemHeaders(QWidget *parent, const QString &directory,
                        QMap<QString,CompletionInfo> &sysHdrCompletion);

void loadSystemHeaderCompletion(void);
void saveSystemHeaderCompletion(void);

class TemplatePopup : public Q3PopupMenu {
  Q_OBJECT

  public:
    TemplatePopup(KTextEditor::View *parent);
    virtual ~TemplatePopup() {}
  private slots:
    void QPopupMenu_activated(int id);
  private:
    KTextEditor::View *view;
};

class CompletionModel : public KTextEditor::CodeCompletionModel
{
  Q_OBJECT
  public:
    CompletionModel(QObject *parent, const QList<CompletionEntry> &entries);
    ~CompletionModel();

    virtual int rowCount(const QModelIndex &parent) const;
    virtual QModelIndex index(int row, int column,
                              const QModelIndex &parent=QModelIndex()) const;
    virtual QVariant data(const QModelIndex &index, int role) const;

  private:
    const QList<CompletionEntry> m_entries;
};

class QEvent;
class QWidget;
class CompletionPopup : public QObject {
  Q_OBJECT

  public:
    CompletionPopup(KTextEditor::View *parent, const QString &fileName,
                    MainForm *mainForm, QObject *receiver);
    virtual ~CompletionPopup() {}
  private slots:
    void slotDone();
  signals:
    void closed();
  private:
    bool done;
    QWidget *completionPopup;
    CompletionModel *completionModel;
};

class ArgHintPopup : public QObject {
  Q_OBJECT

  public:
    ArgHintPopup(KTextEditor::View *parent, const QString &fileName,
                 MainForm *mainForm);
    virtual ~ArgHintPopup() {}
  private slots:
    void slotDone();
  protected:
    bool eventFilter(QObject *o, QEvent *e);
  private:
    bool done;
    QWidget *argHintPopup;
};
