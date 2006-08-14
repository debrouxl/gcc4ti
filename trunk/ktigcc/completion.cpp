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
#include <qapplication.h>
#include <qwidget.h>
#include <qwidgetlist.h>
#include <qevent.h>
#include <kmessagebox.h>
#include <kate/view.h>
#include <kate/document.h>
#include <ktexteditor/editinterfaceext.h>
#include <kconfig.h>
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

static QValueList<KTextEditor::CompletionEntry> sortCompletionEntries(
  const QValueList<KTextEditor::CompletionEntry> &entries)
{
  QMap<QString,QValueList<KTextEditor::CompletionEntry> > map;
  for (QValueList<KTextEditor::CompletionEntry>::ConstIterator it=entries.begin();
       it!=entries.end(); ++it) {
    const KTextEditor::CompletionEntry &entry=*it;
    QValueList<KTextEditor::CompletionEntry> &list=map[entry.text];
    if (list.find(entry)==list.end()) list.append(entry);
  }
  QValueList<KTextEditor::CompletionEntry> result;
  for (QMap<QString,QValueList<KTextEditor::CompletionEntry> >::ConstIterator
       it=map.begin(); it!=map.end(); ++it)
    mergeCompletionEntries(result,*it);
  return result;
}

static QStringList prototypesForIdentifier(const QString &identifier,
  const QValueList<KTextEditor::CompletionEntry> &entries)
{
  QStringList result;
  for (QValueList<KTextEditor::CompletionEntry>::ConstIterator it=entries.begin();
       it!=entries.end(); ++it) {
    const KTextEditor::CompletionEntry &entry=*it;
    if (entry.text==identifier) {
      QString prototype=entry.prefix+' '+entry.text+entry.postfix;
      if (result.find(prototype)==result.end()) result.append(prototype);
    }
  }
  return result;
}

bool parseHelpSources(QWidget *parent, const QString &directory,
                      QMap<QString,CompletionInfo> &sysHdrCompletion)
{
  QDir qdir(directory);
  QStringList headers=qdir.entryList("*.h",QDir::Dirs);
  for (QStringList::ConstIterator it=headers.begin(); it!=headers.end(); ++it) {
    const QString &header=*it;
    CompletionInfo &completionInfo=sysHdrCompletion[header];
    QValueList<KTextEditor::CompletionEntry> &entries=completionInfo.entries;
    QDir hdrQdir(QFileInfo(qdir,header).filePath());
    QStringList hsfs=hdrQdir.entryList("*.hsf *.ref",QDir::Files);
    for (QStringList::ConstIterator it=hsfs.begin(); it!=hsfs.end(); ++it) {
      const QString &hsf=*it;
      QString fileText=loadFileText(QFileInfo(hdrQdir,hsf).filePath());
      if (fileText.isNull()) {
        KMessageBox::error(parent,QString("Can't open \'%1/%2\'.").arg(header)
                                                                  .arg(hsf));
        return false;
      }
      if (hsf.endsWith(".ref")) {
        QString realHeader=fileText.stripWhiteSpace();
        QDir realHdrQdir(QFileInfo(qdir,realHeader).filePath());
        QString realHsf=hsf;
        realHsf.replace(realHsf.length()-3,3,"hsf");
        fileText=loadFileText(QFileInfo(realHdrQdir,realHsf).filePath());
        if (fileText.isNull()) {
          KMessageBox::error(parent,QString("Can't open \'%1/%2\'.").arg(realHeader)
                                                                    .arg(realHsf));
          return false;
        }
      }
      KTextEditor::CompletionEntry entry;
      QStringList lines=QStringList::split('\n',fileText,TRUE);
      for (QStringList::ConstIterator it=lines.begin(); it!=lines.end(); ++it) {
        const QString &line=*it;
        if (line.startsWith("Name=")) {
          entry.text=line.mid(5);
          break;
        }
      }
      bool isType=false;
      for (QStringList::ConstIterator it=lines.begin(); it!=lines.end(); ++it) {
        const QString &line=*it;
        if (line.startsWith("Type=")) {
          QString hsfType=line.mid(5);
          if (hsfType=="Type") isType=true;
          entry.prefix=isType?"type"
                       :(hsfType=="Function")?"func"
                       :(hsfType=="Constant")?"const"
                       :(hsfType=="Variable")?"var":hsfType;
          break;
        }
      }
      QRegExp comments("/\\*.*\\*/");
      comments.setMinimal(true);
      QString definition;
      for (QStringList::ConstIterator it=lines.begin(); it!=lines.end(); ++it) {
        const QString &line=*it;
        if (line.startsWith("Definition=")) {
          definition=line.mid(11);
          definition.remove(comments);
          int pos=definition.find(entry.text);
          QString left=(pos>=0)?definition.left(pos).stripWhiteSpace()
                               :QString::null;
          QString right;
          if (left.startsWith("typedef")) {
            entry.postfix=left.mid(8).simplifyWhiteSpace();
            left=QString::null;
          } else if (left=="unknown_retval") left="?";
          else if (left=="#define") left=QString::null;
          if (!left.isEmpty()) {
            left.prepend(' ');
            entry.prefix+=left;
          }
          entry.postfix+=definition.mid(pos+entry.text.length()).simplifyWhiteSpace();
          break;
        }
      }
      QStringList::ConstIterator desc=lines.find("[Description]");
      QString description;
      if (desc!=lines.end() && ++desc!=lines.end()) description=*desc;
      description.remove(QRegExp("<A [^>]*>",FALSE)).remove("</A>",FALSE);
      if (description.isEmpty()) description=QString::null;
      entry.comment=description;
      if (isType) {
        for (QStringList::ConstIterator it=lines.begin(); it!=lines.end(); ++it) {
          const QString &line=*it;
          if (line.startsWith("Subtype=") || (line[0]=='[' && line!="[Main]")) {
            if (line=="Subtype=Enumeration") {
              int pos1=definition.find('{');
              if (pos1>=0) {
                QString left=definition.left(pos1).stripWhiteSpace();
                int pos2=definition.find('}',++pos1);
                if (pos2>=0) {
                  QString itemList=definition.mid(pos1,pos2-pos1);
                  if (itemList=="...") {
                    for (QStringList::ConstIterator it=lines.begin(); it!=lines.end(); ++it) {
                      const QString &line=*it;
                      if (line.startsWith("Real Definition=")) {
                        QString realDefinition=line.mid(16);
                        realDefinition.remove(comments);
                        pos1=realDefinition.find('{');
                        if (pos1>=0) {
                          left=realDefinition.left(pos1).stripWhiteSpace();
                          pos2=realDefinition.find('}',++pos1);
                          if (pos2>=0) {
                            itemList=realDefinition.mid(pos1,pos2-pos1);
                            goto foundDefinition;
                          }
                        }
                        break;
                      }
                    }
                  } else {
                    foundDefinition:
                    QStringList enumItems=QStringList::split(',',itemList);
                    for (QStringList::ConstIterator it=enumItems.begin();
                         it!=enumItems.end(); ++it) {
                      const QString &enumItem=*it;
                      KTextEditor::CompletionEntry enumEntry;
                      int pos=enumItem.find('=');
                      if (pos>=0) {
                        enumEntry.text=enumItem.left(pos).stripWhiteSpace();
                        enumEntry.postfix=enumItem.mid(pos+1).stripWhiteSpace();
                      } else enumEntry.text=enumItem.stripWhiteSpace();
                      enumEntry.prefix=left;
                      enumEntry.comment=description;
                      entries.append(enumEntry);
                    }
                  }
                }
              }
            }
            break;
          }
        }
      }
      if (entry.text.stripWhiteSpace().isEmpty()) {
        // No function name, so use HSF name. Can happen for _ROM_CALL_*.
        if (!hsf.startsWith("_ROM_CALL_"))
          KMessageBox::sorry(parent,QString("No name found in %1/%2").arg(header)
                                                                     .arg(hsf),
                             "Warning");
        entry.text=hsf.left(hsf.length()-4);
      }
      entries.append(entry);
    }
  }
  return true;
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
    sysHdrCompletion[header]=parseFileCompletion(fileText,QString::null,
                                                 sysHdrCompletion[header]);
    if (sysHdrCompletion[header].dirty) return false;
  }
  return true;
}

void loadSystemHeaderCompletion(void)
{
  KConfig config("ktigcc/completion",true,false,"data");
  QStringList groupList=config.groupList();
  if (groupList.isEmpty()) {
    KMessageBox::queuedMessageBox(0,KMessageBox::Sorry,
      "<p>No completion data found for TIGCCLIB headers. KTIGCC will not be "
      "able to show completion entries for system headers. You have 2 options "
      "to fix this:</p>"
      "<p>1. Download ktigcc-completion-data from "
      "<a href=\"http://sourceforge.net/project/showfiles.php?group_id=31034"
      "&amp;package_id=200501\">http://sourceforge.net/project/showfiles.php?"
      "group_id=31034&amp;package_id=200501</a> (recommended).</p>"
      "<p>2. Regenerate the data yourself through File/Preferences/Coding "
      "(TIGCC source code required).</p>","No Completion Data",
      KMessageBox::Notify|KMessageBox::AllowLink);
  }
  systemHeaderCompletion.clear();
  for (QStringList::ConstIterator it=groupList.begin(); it!=groupList.end(); ++it) {
    const QString &key=*it;
    if (key.endsWith(" Lines")) continue;
    CompletionInfo completionInfo;
    config.setGroup(key);
    completionInfo.includedSystem=config.readListEntry("Included");
    unsigned numEntries=config.readUnsignedNumEntry("Num Entries");
    for (unsigned i=0; i<numEntries; i++) {
      KTextEditor::CompletionEntry entry;
      entry.type=config.readEntry(QString("Entry %1 Type").arg(i));
      entry.text=config.readEntry(QString("Entry %1 Text").arg(i));
      entry.prefix=config.readEntry(QString("Entry %1 Prefix").arg(i));
      entry.postfix=config.readEntry(QString("Entry %1 Postfix").arg(i));
      entry.comment=config.readEntry(QString("Entry %1 Comment").arg(i));
      entry.userdata=config.readEntry(QString("Entry %1 User Data").arg(i));
      completionInfo.entries.append(entry);
    }
    QMap<QString,QString> entryMap=config.entryMap(key+" Lines");
    for (QMap<QString,QString>::ConstIterator it=entryMap.begin();
         it!=entryMap.end(); ++it)
      completionInfo.lineNumbers.insert(it.key(),(*it).toUInt());
    systemHeaderCompletion.insert(key,completionInfo);
  }
}

void saveSystemHeaderCompletion(void)
{
  KConfig config("ktigcc/completion",false,false,"data");
  for (QMap<QString,CompletionInfo>::ConstIterator it=systemHeaderCompletion.begin();
       it!=systemHeaderCompletion.end(); ++it) {
    const QString &key=it.key();
    const CompletionInfo &completionInfo=*it;
    config.setGroup(key);
    config.writeEntry("Included",completionInfo.includedSystem);
    unsigned i=0;
    for (QValueList<KTextEditor::CompletionEntry>::ConstIterator it
         =completionInfo.entries.begin(); it!=completionInfo.entries.end();
         ++it, i++) {
      const KTextEditor::CompletionEntry &entry=*it;
      config.writeEntry(QString("Entry %1 Type").arg(i),entry.type);
      config.writeEntry(QString("Entry %1 Text").arg(i),entry.text);
      config.writeEntry(QString("Entry %1 Prefix").arg(i),entry.prefix);
      config.writeEntry(QString("Entry %1 Postfix").arg(i),entry.postfix);
      config.writeEntry(QString("Entry %1 Comment").arg(i),entry.comment);
      config.writeEntry(QString("Entry %1 User Data").arg(i),entry.userdata);
    }
    config.writeEntry("Num Entries",i);
    config.setGroup(key+" Lines");
    for (QMap<QString,unsigned>::ConstIterator it=completionInfo.lineNumbers.begin();
         it!=completionInfo.lineNumbers.end(); ++it)
      config.writeEntry(it.key(),*it);
  }
  config.sync();
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

CompletionPopup::CompletionPopup(Kate::View *parent, const QString &fileName,
                                 MainForm *mainForm, QObject *receiver)
  : QObject(parent), done(false), completionPopup(0)
{
  connect(this,SIGNAL(closed()),receiver,SLOT(completionPopup_closed()));
  QValueList<KTextEditor::CompletionEntry> entries;
  if (!completionEntriesForFile(parent->getDoc()->text(),fileName,mainForm,
                                entries)) {
    emit closed();
    deleteLater();
    return;
  }
  entries=sortCompletionEntries(entries);
  unsigned column=parent->cursorColumnReal();
  int offset=0;
  if (column) {
    QString textLine=parent->currentTextLine();
    if (column<=textLine.length()) {
      while (column && (textLine[--column].isLetterOrNumber()
                        || textLine[column]=='_' || textLine[column]=='$'))
        offset++;
    }
  }
  connect(parent,SIGNAL(completionAborted()),this,SLOT(slotDone()));
  connect(parent,SIGNAL(completionDone()),this,SLOT(slotDone()));
  parent->showCompletionBox(entries,offset);
  // Unfortunately, Kate doesn't always send the completionAborted or
  // completionDone event when it closes its popup. Work around that.
  QWidgetList *list=QApplication::topLevelWidgets();
  QWidgetListIt it(*list);
  while (QWidget *w=it.current()) {
    ++it;
    if (w->isVisible() && w->testWFlags(Qt::WType_Popup)) {
      completionPopup=w;
      break;      
    }
  }
  delete list;
  if (completionPopup)
    completionPopup->installEventFilter(this);
}

void CompletionPopup::slotDone()
{
  if (!done) {
    done=true;
    emit closed();
    deleteLater();
  }
}

bool CompletionPopup::eventFilter(QObject *o, QEvent *e)
{
  if (!done && o==completionPopup && e->type()==QEvent::Hide) {
    done=true;
    emit closed();
    deleteLater();
  }
  return false;
}

ArgHintPopup::ArgHintPopup(Kate::View *parent, const QString &fileName,
                           MainForm *mainForm)
  : QObject(parent), done(false), argHintPopup(0)
{
  QValueList<KTextEditor::CompletionEntry> entries;
  if (!completionEntriesForFile(parent->getDoc()->text(),fileName,mainForm,
                                entries)) {
    nothingFound:
    deleteLater();
    return;
  }
  unsigned column=parent->cursorColumnReal();
  if (!column || !--column) goto nothingFound;
  QString textLine=parent->currentTextLine();
  if (column>textLine.length() || textLine[column]!='(') goto nothingFound;
  unsigned startColumn=column, endColumn=column;
  while (column && (textLine[--column].isLetterOrNumber()
                    || textLine[column]=='_' || textLine[column]=='$'))
    startColumn--;
  QString identifier=textLine.mid(startColumn,endColumn-startColumn);
  QStringList prototypes=prototypesForIdentifier(identifier,entries);
  if (prototypes.isEmpty()) goto nothingFound;  
  connect(parent,SIGNAL(argHintHidden()),this,SLOT(slotDone()));
  parent->showArgHint(prototypes,"()",",");
  // Unfortunately, Kate doesn't always send the argHintHidden event when it
  // closes its popup. Work around that.
  QWidgetList *list=QApplication::topLevelWidgets();
  QWidgetListIt it(*list);
  while (QWidget *w=it.current()) {
    ++it;
    if (w->isVisible() && w->testWFlags(Qt::WType_Popup)) {
      argHintPopup=w;
      break;      
    }
  }
  delete list;
  if (argHintPopup)
    argHintPopup->installEventFilter(this);
}

void ArgHintPopup::slotDone()
{
  if (!done) {
    done=true;
    deleteLater();
  }
}

bool ArgHintPopup::eventFilter(QObject *o, QEvent *e)
{
  if (!done && o==argHintPopup && e->type()==QEvent::Hide) {
    done=true;
    deleteLater();
  }
  return false;
}
