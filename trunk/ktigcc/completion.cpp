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

#include <QString>
#include <QLinkedList>
#include <QPair>
#include <QPoint>
#include <QRegExp>
#include <QFileInfo>
#include <QDir>
#include <QApplication>
#include <QWidget>
#include <QEvent>
#include <Q3PopupMenu>
#include <kmessagebox.h>
#include <ktexteditor/view.h>
#include <ktexteditor/document.h>
#include <kconfig.h>
#include <cstring>
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
  foreach (QString headerName, systemHeaders) {
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
    QString pathInProject=fileInfo.isRelative()?fileInfo.path():".";
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
  foreach (QString headerName, completionInfo.included) {
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

static void mergeCompletionEntries(QLinkedList<CompletionEntry> &dest,
                                   const QLinkedList<CompletionEntry> &src)
{
  foreach (CompletionEntry entry, src) dest.append(entry);
}

static void completionEntriesForSystemHeaders(const QStringList &systemHeaders,
                                              QLinkedList<CompletionEntry> &result)
{
  foreach (QString headerName, systemHeaders) {
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
                                              QLinkedList<CompletionEntry> &result)
{
  if (!projectCompletion.contains(fileName) || projectCompletion[fileName].dirty) {
    QFileInfo fileInfo(fileName);
    QString pathInProject=fileInfo.isRelative()?fileInfo.path():".";
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
  foreach (QString headerName, completionInfo.included) {
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
                              QLinkedList<CompletionEntry> &result)
{
  resetSearchedFlags();
  return completionEntriesForFileRecursive(fileText,fileName,mainForm,result);
}

static QLinkedList<CompletionEntry> sortCompletionEntries(
  const QLinkedList<CompletionEntry> &entries)
{
  QMap<QString,QLinkedList<CompletionEntry> > map;
  foreach (CompletionEntry entry, entries) {
    QLinkedList<CompletionEntry> &list=map[entry.text];
    if (!list.contains(entry)) list.append(entry);
  }
  QLinkedList<CompletionEntry> result;
  for (QMap<QString,QLinkedList<CompletionEntry> >::ConstIterator
       it=map.begin(); it!=map.end(); ++it)
    mergeCompletionEntries(result,*it);
  return result;
}

static QStringList prototypesForIdentifier(const QString &identifier,
  const QLinkedList<CompletionEntry> &entries)
{
  QStringList result;
  QStringList reservedIdentifiers=QString("__alignof__\n"
                                          "__asm__\n"
                                          "__attribute__\n"
                                          "__complex__\n"
                                          "__const__\n"
                                          "__extension__\n"
                                          "__imag__\n"
                                          "__inline__\n"
                                          "__label__\n"
                                          "__real__\n"
                                          "__typeof__\n"
                                          "asm\n"
                                          "auto\n"
                                          "break\n"
                                          "case\n"
                                          "char\n"
                                          "const\n"
                                          "continue\n"
                                          "default\n"
                                          "do\n"
                                          "double\n"
                                          "else\n"
                                          "enum\n"
                                          "extern\n"
                                          "float\n"
                                          "for\n"
                                          "goto\n"
                                          "if\n"
                                          "inline\n"
                                          "int\n"
                                          "long\n"
                                          "register\n"
                                          "return\n"
                                          "short\n"
                                          "signed\n"
                                          "sizeof\n"
                                          "static\n"
                                          "struct\n"
                                          "switch\n"
                                          "typedef\n"
                                          "typeof\n"
                                          "union\n"
                                          "unsigned\n"
                                          "void\n"
                                          "volatile\n"
                                          "while\n").split('\n',QString::SkipEmptyParts);
  if (!reservedIdentifiers.contains(identifier)) {
    foreach (CompletionEntry entry, entries) {
      if (entry.text==identifier) {
        QString prototype=entry.prefix+' '+entry.text+entry.postfix;
        if (result.find(prototype)==result.end()) result.append(prototype);
      }
    }
    if (result.isEmpty()) {
      // Try approximate matching.
      unsigned identifierLength=identifier.length();
      if (identifierLength>=4) {
        QString identifierUpper=identifier.toUpper();
        QLinkedList<unsigned> distances;
        foreach (CompletionEntry entry, entries) {
          QString entryText=entry.text;
          unsigned entryTextLength=entryText.length();
          unsigned minLength=qMin(identifierLength,entryTextLength);
          unsigned i=0;
          for (; i<minLength && identifierUpper[i]==entryText[i].toUpper(); i++);
          unsigned distance=minLength-i;
          if (distance<=(minLength>>1)) {
            QString prototype=entryText+"? "+entry.prefix+' '+entry.postfix;
            if (result.find(prototype)==result.end()) {
              // Sort by similarity. Smaller distances first.
              QStringList::Iterator it1=result.begin();
              QLinkedList<unsigned>::Iterator it2=distances.begin();
              for (; it2!=distances.end() && *it2<=distance; ++it1,++it2);
              result.insert(it1,prototype);
              distances.insert(it2,distance);
            }
          }
        }
      }
    }
  }
  return result;
}

bool parseHelpSources(QWidget *parent, const QString &directory,
                      QMap<QString,CompletionInfo> &sysHdrCompletion)
{
  QDir qdir(directory);
  QStringList headers=qdir.entryList("*.h",QDir::Dirs);
  foreach (QString header, headers) {
    CompletionInfo &completionInfo=sysHdrCompletion[header];
    QLinkedList<CompletionEntry> &entries=completionInfo.entries;
    QDir hdrQdir(QFileInfo(qdir,header).filePath());
    QStringList hsfs=hdrQdir.entryList("*.hsf *.ref",QDir::Files);
	foreach (QString hsf, hsfs) {
      QString fileText=loadFileText(QFileInfo(hdrQdir,hsf).filePath());
      if (fileText.isNull()) {
        KMessageBox::error(parent,QString("Can't open \'%1/%2\'.").arg(header)
                                                                  .arg(hsf));
        return false;
      }
      if (hsf.endsWith(".ref")) {
        QString realHeader=fileText.trimmed();
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
      CompletionEntry entry;
      QStringList lines=fileText.split('\n');
      foreach (QString line, lines) {
        if (line.startsWith("Name=")) {
          entry.text=line.mid(5);
          break;
        }
      }
      bool isType=false;
      foreach (QString line, lines) {
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
      foreach (QString line, lines) {
        if (line.startsWith("Definition=")) {
          definition=line.mid(11);
          definition.remove(comments);
          int pos=definition.find(entry.text);
          QString left=(pos>=0)?definition.left(pos).trimmed()
                               :QString::null;
          QString right;
          if (left.startsWith("typedef")) {
            entry.postfix=left.mid(8).simplified();
            left=QString::null;
          } else if (left=="unknown_retval") left="?";
          else if (left=="#define") left=QString::null;
          if (!left.isEmpty()) {
            left.prepend(' ');
            entry.prefix+=left;
          }
          entry.postfix+=definition.mid(pos+entry.text.length()).simplified();
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
          if (line.startsWith("Subtype=")
              || (!line.isEmpty() && line[0]=='[' && line!="[Main]")) {
            if (line=="Subtype=Enumeration") {
              int pos1=definition.find('{');
              if (pos1>=0) {
                QString left=definition.left(pos1).trimmed();
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
                          left=realDefinition.left(pos1).trimmed();
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
                    QStringList enumItems=itemList.split(',',QString::SkipEmptyParts);
                    for (QStringList::ConstIterator it=enumItems.begin();
                         it!=enumItems.end(); ++it) {
                      const QString &enumItem=*it;
                      CompletionEntry enumEntry;
                      int pos=enumItem.find('=');
                      if (pos>=0) {
                        enumEntry.text=enumItem.left(pos).trimmed();
                        enumEntry.postfix=enumItem.mid(pos+1).trimmed();
                      } else enumEntry.text=enumItem.trimmed();
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
      if (entry.text.trimmed().isEmpty()) {
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
  foreach (QString header, headers) {
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
#ifdef HAVE_KSHAREDCONFIG_H
  KConfig config("data","ktigcc/completion",KConfig::NoGlobals);
#else
  KConfig config("ktigcc/completion",true,false,"data");
#endif
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
  foreach (QString key, groupList) {
    if (key.endsWith(" Lines")) continue;
    CompletionInfo completionInfo;
    config.setGroup(key);
    completionInfo.includedSystem=config.readListEntry("Included");
    unsigned numEntries=config.readUnsignedNumEntry("Num Entries");
    for (unsigned i=0; i<numEntries; i++) {
      CompletionEntry entry;
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
#ifdef HAVE_KSHAREDCONFIG_H
  KConfig config("data","ktigcc/completion",KConfig::NoGlobals);
#else
  KConfig config("ktigcc/completion",false,false,"data");
#endif
  for (QMap<QString,CompletionInfo>::ConstIterator it=systemHeaderCompletion.begin();
       it!=systemHeaderCompletion.end(); ++it) {
    const QString &key=it.key();
    const CompletionInfo &completionInfo=*it;
    config.setGroup(key);
    config.writeEntry("Included",completionInfo.includedSystem);
    unsigned i=0;
	foreach (CompletionEntry entry, completionInfo.entries) {
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

TemplatePopup::TemplatePopup(KTextEditor::View *parent)
  : Q3PopupMenu(parent), view(parent)
{
  connect(this,SIGNAL(activated(int)),this,SLOT(QPopupMenu_activated(int)));
  unsigned i=0;
  typedef const QPair<QString,QString> &StringPairConstRef;
  foreach (StringPairConstRef pair, preferences.templates)
    insertItem(pair.first, i++);
  QPoint pos=parent->cursorPositionCoordinates();
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
  KTextEditor::Document *doc=view->document();
  QString code=preferences.templates[id].second;
  QString indent=doc->line(view->cursorPosition().line());
  // Remove everything starting from the first non-whitespace character.
  indent=indent.remove(QRegExp("(?!\\s).*$"));
  indent.prepend('\n');
  code.replace('\n',indent);
  int cursorPos=code.find('|');
  if (cursorPos>=0) {
    QString left=code.left(cursorPos);
    QString right=code.mid(cursorPos+1);
    int row, col;
    doc->startEditing();
    view->insertText(left);
    view->cursorPosition().position(row,col);
    view->insertText(right);
    doc->endEditing();
    view->setCursorPosition(KTextEditor::Cursor(row,col));
  } else view->insertText(code);
}

CompletionPopup::CompletionPopup(KTextEditor::View *parent, const QString &fileName,
                                 MainForm *mainForm, QObject *receiver)
  : QObject(parent), done(false), completionPopup(0)
{
  connect(this,SIGNAL(closed()),receiver,SLOT(completionPopup_closed()));
  QLinkedList<CompletionEntry> entries;
  if (!completionEntriesForFile(parent->document()->text(),fileName,mainForm,
                                entries)) {
    emit closed();
    deleteLater();
    return;
  }
  entries=sortCompletionEntries(entries);
  KTextEditor::Cursor cursor=parent->cursorPosition();
  int column=cursor.column();
  int offset=0;
  if (column) {
    QString textLine=parent->document()->line(cursor.line());
    if (column<=textLine.length()) {
      while (column && (textLine[--column].isLetterOrNumber()
                        || textLine[column]=='_' || textLine[column]=='$'))
        offset++;
    }
  }
#if 0 // FIXME: Port completion.
  connect(parent,SIGNAL(completionAborted()),this,SLOT(slotDone()));
  connect(parent,SIGNAL(completionDone()),this,SLOT(slotDone()));
  parent->showCompletionBox(entries,offset);
  // Unfortunately, Kate doesn't always send the completionAborted or
  // completionDone event when it closes its popup. Work around that.
  QWidgetList *list=QApplication::topLevelWidgets();
  QWidgetListIt it(*list);
  while (QWidget *w=it.current()) {
    ++it;
    if (w->isVisible() && w->testWFlags(Qt::WType_Popup)
        && !std::strcmp(w->className(),"QVBox")) {
      completionPopup=w;
      break;
    }
  }
  delete list;
  if (completionPopup)
    completionPopup->installEventFilter(this);
#else
  slotDone();
#endif
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

ArgHintPopup::ArgHintPopup(KTextEditor::View *parent, const QString &fileName,
                           MainForm *mainForm)
  : QObject(parent), done(false), argHintPopup(0)
{
  QLinkedList<CompletionEntry> entries;
  if (!completionEntriesForFile(parent->document()->text(),fileName,mainForm,
                                entries)) {
    nothingFound:
    deleteLater();
    return;
  }
  KTextEditor::Cursor cursor=parent->cursorPosition();
  int column=cursor.column();
  if (!column || !--column) goto nothingFound;
  QString textLine=parent->document()->line(cursor.line());
  if (column>textLine.length() || textLine[column]!='(') goto nothingFound;
  while (column && textLine[column-1].isSpace()) column--;
  if (!column) goto nothingFound;
  unsigned startColumn=column, endColumn=column;
  while (column && (textLine[--column].isLetterOrNumber()
                    || textLine[column]=='_' || textLine[column]=='$'))
    startColumn--;
  if (startColumn==endColumn) goto nothingFound;
  QString identifier=textLine.mid(startColumn,endColumn-startColumn);
  QStringList prototypes=prototypesForIdentifier(identifier,entries);
  if (prototypes.isEmpty()) goto nothingFound;  
#if 0 // FIXME: Port completion.
  connect(parent,SIGNAL(argHintHidden()),this,SLOT(slotDone()));
  parent->showArgHint(prototypes,"()",",");
  // Unfortunately, Kate doesn't always send the argHintHidden event when it
  // closes its popup. Work around that.
  QWidgetList *list=QApplication::topLevelWidgets();
  QWidgetListIt it(*list);
  while (QWidget *w=it.current()) {
    ++it;
    if (w->isVisible() && w->testWFlags(Qt::WType_Popup)
        && !std::strcmp(w->className(),"KateArgHint")) {
      argHintPopup=w;
      break;
    }
  }
  delete list;
  if (argHintPopup)
    argHintPopup->installEventFilter(this);
#else
  slotDone();
#endif
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
