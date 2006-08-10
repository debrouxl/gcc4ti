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
#include <kate/view.h>
#include <kate/document.h>
#include <ktexteditor/editinterfaceext.h>
#include "completion.h"
#include "parsing.h"
#include "preferences.h"
#include "mainform.h"

// Maps file name to a CompletionInfo.
QMap<QString,CompletionInfo> systemHeaderCompletion, projectCompletion;

bool findSymbolInFile(const QString &symbol,
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
  const CompletionInfo &completionInfo=projectCompletion[fileName];
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
  for (QStringList::ConstIterator it=completionInfo.includedSystem.begin();
       it!=completionInfo.includedSystem.end(); ++it) {
    const QString &headerName=*it;
    if (systemHeaderCompletion.contains(headerName)
        && systemHeaderCompletion[headerName].lineNumbers.contains(symbol)) {
      symbolFile=headerName;
      symbolLine=systemHeaderCompletion[headerName].lineNumbers[symbol];
      systemHeader=true;
      return true;
    }
  }
  return true;
}

static void mergeCompletionEntries(QValueList<KTextEditor::CompletionEntry> &dest,
                                   const QValueList<KTextEditor::CompletionEntry> &src)
{
  for (QValueList<KTextEditor::CompletionEntry>::ConstIterator it=src.begin();
       it!=src.end(); ++it)
    dest.append(*it);
}

bool completionEntriesForFile(const QString &fileText,
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
  const CompletionInfo &completionInfo=projectCompletion[fileName];
  mergeCompletionEntries(result,completionInfo.entries);
  for (QStringList::ConstIterator it=completionInfo.includedSystem.begin();
       it!=completionInfo.includedSystem.end(); ++it) {
    const QString &headerName=*it;
    if (systemHeaderCompletion.contains(headerName))
      mergeCompletionEntries(result,systemHeaderCompletion[headerName].entries);
  }
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
