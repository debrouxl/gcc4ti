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

#include <qstring.h>
#include <qvaluelist.h>
#include <qpair.h>
#include <qpoint.h>
#include <qregexp.h>
#include <qfileinfo.h>
#include <qdir.h>
#include <kmessagebox.h>
#include <kate/view.h>
#include <kate/document.h>
#include <ktexteditor/editinterfaceext.h>
#include "completion.h"
#include "parsing.h"
#include "preferences.h"
#include "mainform.h"
#include "tpr.h"

// Maps file name to a CompletionInfo.
QMap<QString,CompletionInfo> systemHeaderCompletion, projectCompletion;

static void resetSearchedFlags(void)
{
  for (QMap<QString,CompletionInfo>::Iterator it=projectCompletion.begin();
       it!=projectCompletion.end(); ++it)
    (*it).searched=false;
  for (QMap<QString,CompletionInfo>::Iterator it=systemHeaderCompletion.begin();
       it!=systemHeaderCompletion.end(); ++it)
    (*it).searched=false;
}

static void findSymbolInSystemHeaders(const QString &symbol,
                                      const QStringList &systemHeaders,
                                      QString &symbolFile,
                                      unsigned &symbolLine,
                                      bool &systemHeader)
{
  for (QStringList::ConstIterator it=systemHeaders.begin();
       it!=systemHeaders.end(); ++it) {
    const QString &headerName=*it;
    // Avoid infinite recursion.
    if (systemHeaderCompletion.contains(headerName)
        && !systemHeaderCompletion[headerName].searched) {
      CompletionInfo &completionInfo=systemHeaderCompletion[headerName];
      completionInfo.searched=true;
      if (completionInfo.lineNumbers.contains(symbol)) {
        symbolFile=headerName;
        symbolLine=completionInfo.lineNumbers[symbol];
        systemHeader=true;
        return;
      } else {
        findSymbolInSystemHeaders(symbol,completionInfo.includedSystem,
                                  symbolFile,symbolLine,systemHeader);
        if (!symbolFile.isNull()) return;
      }
    }
  }
}

static bool findSymbolInFileRecursive(const QString &symbol,
                                      const QString &fileText,
                                      const QString &fileName,
                                      MainForm *mainForm,
                                      QString &symbolFile,
                                      unsigned &symbolLine,
                                      bool &systemHeader)
{
  symbolFile=QString::null;
  systemHeader=false;
  if (!projectCompletion.contains(fileName) || projectCompletion[fileName].dirty) {
    QFileInfo fileInfo(fileName);
    QString pathInProject=fileInfo.isRelative()?fileInfo.dirPath():".";
    CompletionInfo completionInfo=parseFileCompletion(fileText,pathInProject);
    if (completionInfo.dirty) return false;
    projectCompletion.insert(fileName,completionInfo);
  }
  CompletionInfo &completionInfo=projectCompletion[fileName];
  // Avoid infinite recursion.
  if (completionInfo.searched) return true;
  completionInfo.searched=true;
  if (completionInfo.lineNumbers.contains(symbol)) {
    symbolFile=fileName;
    symbolLine=completionInfo.lineNumbers[symbol];
    return true;
  }
  for (QStringList::ConstIterator it=completionInfo.included.begin();
       it!=completionInfo.included.end(); ++it) {
    const QString &headerName=*it;
    QString headerText=mainForm->textForHeader(headerName);
    if (!headerText.isNull()) {
      if (!findSymbolInFile(symbol,headerText,headerName,mainForm,symbolFile,
                            symbolLine,systemHeader))
        return false;
      if (!symbolFile.isNull()) return true;
    }
  }
  findSymbolInSystemHeaders(symbol,completionInfo.includedSystem,symbolFile,
                            symbolLine,systemHeader);
  return true;
}

bool findSymbolInFile(const QString &symbol,
                      const QString &fileText,
                      const QString &fileName,
                      MainForm *mainForm,
                      QString &symbolFile,
                      unsigned &symbolLine,
                      bool &systemHeader)
{
  resetSearchedFlags();
  return findSymbolInFileRecursive(symbol,fileText,fileName,mainForm,symbolFile,
                                   symbolLine,systemHeader);
}

static void mergeCompletionEntries(QValueList<KTextEditor::CompletionEntry> &dest,
                                   const QValueList<KTextEditor::CompletionEntry> &src)
{
  for (QValueList<KTextEditor::CompletionEntry>::ConstIterator it=src.begin();
       it!=src.end(); ++it)
    dest.append(*it);
}

static void completionEntriesForSystemHeaders(const QStringList &systemHeaders,
                                              QValueList<KTextEditor::CompletionEntry> &result)
{
  for (QStringList::ConstIterator it=systemHeaders.begin();
       it!=systemHeaders.end(); ++it) {
    const QString &headerName=*it;
    // Avoid infinite recursion.
    if (systemHeaderCompletion.contains(headerName)
        && !systemHeaderCompletion[headerName].searched) {
      CompletionInfo &completionInfo=systemHeaderCompletion[headerName];
      completionInfo.searched=true;
      mergeCompletionEntries(result,completionInfo.entries);
      completionEntriesForSystemHeaders(completionInfo.includedSystem,result);
    }
  }
}

static bool completionEntriesForFileRecursive(const QString &fileText,
                                              const QString &fileName,
                                              MainForm *mainForm,
                                              QValueList<KTextEditor::CompletionEntry> &result)
{
  if (!projectCompletion.contains(fileName) || projectCompletion[fileName].dirty) {
    QFileInfo fileInfo(fileName);
    QString pathInProject=fileInfo.isRelative()?fileInfo.dirPath():".";
    CompletionInfo completionInfo=parseFileCompletion(fileText,pathInProject);
    if (completionInfo.dirty) return false;
    projectCompletion.insert(fileName,completionInfo);
  }
  CompletionInfo &completionInfo=projectCompletion[fileName];
  // Avoid infinite recursion.
  if (completionInfo.searched) return true;
  completionInfo.searched=true;
  mergeCompletionEntries(result,completionInfo.entries);
  completionEntriesForSystemHeaders(completionInfo.includedSystem,result);
  for (QStringList::ConstIterator it=completionInfo.included.begin();
       it!=completionInfo.included.end(); ++it) {
    const QString &headerName=*it;
    QString headerText=mainForm->textForHeader(headerName);
    if (!headerText.isNull())
      if (!completionEntriesForFile(headerText,headerName,mainForm,result))
        return false;
  }
  return true;
}

bool completionEntriesForFile(const QString &fileText,
                              const QString &fileName,
                              MainForm *mainForm,
                              QValueList<KTextEditor::CompletionEntry> &result)
{
  resetSearchedFlags();
  return completionEntriesForFileRecursive(fileText,fileName,mainForm,result);
}

bool parseHelpSources(QWidget *parent, const QString &directory,
                      QMap<QString,CompletionInfo> &sysHdrCompletion)
{
  return true; // TODO
}

bool parseSystemHeaders(QWidget *parent, const QString &directory,
                        QMap<QString,CompletionInfo> &sysHdrCompletion)
{
  QDir qdir(directory);
  QStringList headers=qdir.entryList("*.h",QDir::Files);
  for (QStringList::ConstIterator it=headers.begin(); it!=headers.end(); ++it) {
    const QString &header=*it;
    QString fileText=loadFileText(QFileInfo(qdir,header).filePath());
    if (fileText.isNull()) {
      KMessageBox::error(parent,QString("Can't open \'%1\'.").arg(header));
      return false;
    }
    sysHdrCompletion[header]=parseFileCompletion(fileText,QString::null);
    if (sysHdrCompletion[header].dirty) return false;
  }
  return true;
}

TemplatePopup::TemplatePopup(Kate::View *parent)
  : QPopupMenu(parent), view(parent)
{
  connect(this,SIGNAL(activated(int)),this,SLOT(QPopupMenu_activated(int)));
  unsigned i=0;
  for (QValueList<QPair<QString,QString> >::ConstIterator it=preferences.templates.begin();
       it!=preferences.templates.end(); ++it, i++)
    insertItem((*it).first,i);
  QPoint pos=parent->cursorCoordinates();
  if (pos.x()<0 || pos.y()<0) {
    // Cursor outside of the view, so center on view instead.
    QSize parentSize=parent->size();
    QSize popupSize=sizeHint();
    pos.setX((parentSize.width()-popupSize.width())>>1);
    pos.setY((parentSize.height()-popupSize.height())>>1);
  }
  exec(parent->mapToGlobal(pos));
  deleteLater();
}

void TemplatePopup::QPopupMenu_activated(int id)
{
  QString code=preferences.templates[id].second;
  QString indent=view->currentTextLine();
  // Remove everything starting from the first non-whitespace character.
  indent=indent.remove(QRegExp("(?!\\s).*$"));
  indent.prepend('\n');
  code.replace('\n',indent);
  int cursorPos=code.find('|');
  if (cursorPos>=0) {
    QString left=code.left(cursorPos);
    QString right=code.mid(cursorPos+1);
    unsigned row, col;
    KTextEditor::EditInterfaceExt *editExt=KTextEditor::editInterfaceExt(view->getDoc());
    editExt->editBegin();
    view->insertText(left);
    view->cursorPositionReal(&row,&col);
    view->insertText(right);
    editExt->editEnd();
    view->setCursorPositionReal(row,col);
  } else view->insertText(code);
}
