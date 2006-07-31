/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2006 Kevin Kofler
   Copyright (C) 2006 Joey Adams

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
#include <qstringlist.h>
#include <qcstring.h>
#include <qpair.h>
#include <qregexp.h>
#include <qapplication.h>
#include <qlabel.h>
#include <qstatusbar.h>
#include <qtimer.h>
#include <qdatetime.h>
#include <qdragobject.h>
#include <qassistantclient.h>
#include <qdir.h>
#include <qclipboard.h>
#include <qaccel.h>
#include <qeventloop.h>
#include <qdockwindow.h>
#include <qfileinfo.h>
#include <qdatetime.h>
#include <qtextcodec.h>
#include <qstylesheet.h>
#include <qtimer.h>
#include <qtoolbutton.h>
#include <qlistbox.h>
#include <kapplication.h>
#include <kparts/factory.h>
#include <klibloader.h>
#include <kate/document.h>
#include <kate/view.h>
#include <kconfig.h>
#include <ktexteditor/editinterfaceext.h>
#include <ktexteditor/configinterfaceextension.h>
#include <kaboutdata.h>
#include <khelpmenu.h>
#include <kfiledialog.h>
#include <kurl.h>
#include <kmessagebox.h>
#include <kdirwatch.h>
#include <kfinddialog.h>
#include <kfind.h>
#include <kreplacedialog.h>
#include <kreplace.h>
#include <kwin.h>
#include <kglobal.h>
#include <kicontheme.h>
#include <kiconloader.h>
#include <kprocio.h>
#include <kshell.h>
#include <ktextbrowser.h>
#include <krun.h>
#include <kpushbutton.h>
#include <kmacroexpander.h>
#include <dcopclient.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <glib.h>
#include <ticonv.h>
#include <ticables.h>
#include <tifiles.h>
#include <ticalcs.h>
#include "ktigcc.h"
#include "srcfile.h"
#include "tpr.h"
#include "preferences.h"
#include "projectoptions.h"
#include "errorlist.h"
#include "programoutput.h"
#include "tiemu.h"
#include "callbacks.h"
#include "parsing.h"
#include "functions.h"
#include "toolsdlg.h"
#include "newsdlg.h"

using std::puts;
using std::exit;

#define TIGCC_TPR_Filter "*.tpr|TIGCC Projects (*.tpr)\n"
#define TIGCC_H_Filter "*.h|Header Files (*.h)\n"
#define TIGCC_C_Filter "*.c|C Files (*.c)\n"
#define TIGCC_S_Filter "*.s|GNU Assembly Files (*.s)\n"
#define TIGCC_ASM_Filter "*.asm|A68k Assembly Files (*.asm)\n"
#define TIGCC_QLL_Filter "*.qll|Quill Files (*.qll)\n"
#define TIGCC_O_Filter "*.o|Object Files (*.o)\n"
#define TIGCC_A_Filter "*.a|Archive Files (*.a)\n"
#define TIGCC_TXT_Filter "*.txt|Text Files (*.txt)\n"
#define TIGCCAllFilter "*|All Files (*)"

enum {TIGCCOpenProjectFileFilter,TIGCCAddFilesFilter};

#define IS_CATEGORY(item) ((item) && ((item)==hFilesListItem \
                                      || (item)==cFilesListItem \
                                      || (item)==sFilesListItem \
                                      || (item)==asmFilesListItem \
                                      || (item)==qllFilesListItem \
                                      || (item)==oFilesListItem \
                                      || (item)==aFilesListItem \
                                      || (item)==txtFilesListItem \
                                      || (item)==othFilesListItem))
#define IS_EDITABLE_CATEGORY(item) ((item) && ((item)==hFilesListItem \
                                               || (item)==cFilesListItem \
                                               || (item)==sFilesListItem \
                                               || (item)==asmFilesListItem \
                                               || (item)==qllFilesListItem \
                                               || (item)==txtFilesListItem))
#define IS_FOLDER(item) ((item) && (item)->rtti()==0x716CC0)
#define IS_FILE(item) ((item) && (item)->rtti()==0x716CC1)
#define CATEGORY_OF(category,item) QListViewItem *category=(item); \
                                   while (category->parent()->rtti()==0x716CC0) \
                                     category=category->parent()
#define COUNTER_FOR_CATEGORY(category) ((category)==hFilesListItem?hFileCount: \
                                        (category)==cFilesListItem?cFileCount: \
                                        (category)==sFilesListItem?sFileCount: \
                                        (category)==asmFilesListItem?asmFileCount: \
                                        (category)==qllFilesListItem?qllFileCount: \
                                        (category)==oFilesListItem?oFileCount: \
                                        (category)==aFilesListItem?aFileCount: \
                                        (category)==txtFilesListItem?txtFileCount: \
                                        othFileCount)
#define CURRENT_VIEW (static_cast<Kate::View *>(widgetStack->visibleWidget()))

#define LOAD_ICON(name) (QIconSet(KGlobal::iconLoader()->loadIcon((name),KIcon::Small),KGlobal::iconLoader()->loadIcon((name),KIcon::MainToolbar)))
#define SYSICON(sysname,name) (preferences.useSystemIcons?KGlobal::iconLoader()->loadIcon((sysname),KIcon::Small,KIcon::SizeSmall):QPixmap::fromMimeSource((name)))

#define SET_TEXT_SAFE(doc,text) do { \
    bool oldModifiedSinceLastCompile=IS_FILE(currentListItem)? \
      static_cast<ListViewFile *>(currentListItem)->modifiedSinceLastCompile:FALSE; \
    (doc)->setText((text)); \
    if (IS_FILE(currentListItem)) \
      static_cast<ListViewFile *>(currentListItem)->modifiedSinceLastCompile=oldModifiedSinceLastCompile; \
  } while(0)

// For some reason, this flag is not in the public ConfigFlags enum.
#define CF_REMOVE_TRAILING_DYN 0x4000000

static QListViewItem *currentListItem;
static QListViewItem *replaceCurrentDocument;
static unsigned replaceCurrentLine;
static bool compiling;
class KReplaceWithSelection : public KReplace {
  public:
    KReplaceWithSelection(const QString &pattern, const QString &replacement,
                          long options, QWidget *parent=0) :
      KReplace(pattern,replacement,options,parent), m_haveSelection(FALSE) {}
    void setSelection(unsigned selStartLine, unsigned selStartCol,
                      unsigned selEndLine, unsigned selEndCol)
    {
      m_haveSelection=TRUE;
      m_selStartLine=selStartLine;
      m_selStartCol=selStartCol;
      m_selEndLine=selEndLine;
      m_selEndCol=selEndCol;
    }
    void invalidateSelection() {m_haveSelection=FALSE;}
    bool haveSelection() {
      // If another document was put under the cursor, invalidate selection.
      // The m_haveSelection&& is technically redundant, but necessary to avoid
      // possible undefined behavior if replaceCurrentDocument has been deleted.
      if (m_haveSelection&&currentListItem!=replaceCurrentDocument)
        m_haveSelection=FALSE;
      return m_haveSelection;
    }
    unsigned selStartLine() {return m_selStartLine;}
    unsigned selStartCol() {return m_selStartCol;}
    unsigned selEndLine() {return m_selEndLine;}
    unsigned selEndCol() {return m_selEndCol;}
    // Override to ask for restarting when replacing in a selection.
    bool shouldRestart(bool forceAsking=FALSE, bool showNumMatches=TRUE)
    {
      return KReplace::shouldRestart(forceAsking||m_haveSelection,showNumMatches);
    }
  protected:
    virtual bool validateMatch(const QString &text, int index, int matchedlength)
    {
      if (!KReplace::validateMatch(text,index,matchedlength)) return FALSE;
      // If another document was put under the cursor, invalidate selection.
      // The m_haveSelection&& is technically redundant, but necessary to avoid
      // possible undefined behavior if replaceCurrentDocument has been deleted.
      if (m_haveSelection&&currentListItem!=replaceCurrentDocument)
        m_haveSelection=FALSE;
      if (!m_haveSelection) return TRUE;
      if (replaceCurrentLine==m_selStartLine && replaceCurrentLine==m_selEndLine)
        return ((unsigned)index>=m_selStartCol)&&((unsigned)index+(unsigned)matchedlength<=m_selEndCol);
      else if (replaceCurrentLine==m_selStartLine)
        return ((unsigned)index>=m_selStartCol);
      else if (replaceCurrentLine==m_selEndLine)
        return ((unsigned)index+(unsigned)matchedlength<=m_selEndCol);
      else
        return (replaceCurrentLine>=m_selStartLine&&replaceCurrentLine<=m_selEndLine);
    }
  private:
    bool m_haveSelection;
    unsigned m_selStartLine, m_selStartCol, m_selEndLine, m_selEndCol;
};
static KReplaceWithSelection *kreplace;

// All the methods are inline because otherwise QT Designer will mistake them
// for slots of the main form.
class ListViewFolder : public KListViewItem {
  public:
  ListViewFolder(QListView *parent) : KListViewItem(parent)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(QListViewItem *parent) : KListViewItem(parent)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(QListView *parent, QListViewItem *after)
          : KListViewItem(parent, after)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
  }
  ListViewFolder(QListViewItem *parent, QListViewItem *after)
          : KListViewItem(parent, after)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  virtual int rtti(void) const {return 0x716CC0;}
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. KListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<KListView *>(listView())->rename(this,col);
  }
  protected:
};

class ListViewFile : public KListViewItem {
  public:
  ListViewFile(QListView *parent) : KListViewItem(parent),
                                    kateView(NULL), isNew(TRUE),
                                    modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent) : KListViewItem(parent),
                                        kateView(NULL), isNew(TRUE),
                                        modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListView *parent, QListViewItem *after)
          : KListViewItem(parent, after), kateView(NULL), isNew(TRUE),
            modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent, QListViewItem *after)
          : KListViewItem(parent, after), kateView(NULL),
            isNew(TRUE), modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  virtual ~ListViewFile()
  {
    MainForm::deleteErrorsForLVFile(this);
    if (kreplace && replaceCurrentDocument==this) {
      replaceCurrentDocument=static_cast<QListViewItem *>(NULL);
      kreplace->invalidateSelection();
    }
    if (fileName[0]=='/')
      KDirWatch::self()->removeFile(fileName);
    if (kateView) {
      Kate::Document *doc=kateView->getDoc();
      delete kateView;
      delete doc;
    }
  }
  virtual int rtti(void) const {return 0x716CC1;}
  Kate::View *kateView;
  QString textBuffer; // for lazy loading
  QString fileName; // full name of the file
  bool isNew;
  bool modifiedSinceLastCompile;
  LineStartList lineStartList;
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. KListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<KListView *>(listView())->rename(this,col);
  }
  protected:
};

class ListViewRoot : public KListViewItem {
  public:
  ListViewRoot(QListView *parent) : KListViewItem(parent)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(QListViewItem *parent) : KListViewItem(parent)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(QListView *parent, QListViewItem *after)
          : KListViewItem(parent, after)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(QListViewItem *parent, QListViewItem *after)
          : KListViewItem(parent, after)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. KListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<KListView *>(listView())->rename(this,col);
  }
  protected:
};

// These should be instance variables in clean C++, but QT Designer won't let me
// touch the class definition, so this is all I can do. And there is only one
// instance of MainForm anyway.
static QListViewItem *rootListItem;
static QListViewItem *hFilesListItem;
static QListViewItem *cFilesListItem;
static QListViewItem *sFilesListItem;
static QListViewItem *asmFilesListItem;
static QListViewItem *qllFilesListItem;
static QListViewItem *oFilesListItem;
static QListViewItem *aFilesListItem;
static QListViewItem *txtFilesListItem;
static QListViewItem *othFilesListItem;
static bool projectIsDirty, projectNeedsRelink;
static QLabel *leftStatusLabel;
static QLabel *rowStatusLabel;
static QLabel *colStatusLabel;
static QLabel *charsStatusLabel;
static QLabel *rightStatusLabel;
static KHelpMenu *khelpmenu;
static QPopupMenu *te_popup;
static bool headersModified;
static QDockWindow *errorListDock;
static ErrorList *errorList;
static unsigned errorCountTotal=0,errorCountErrors=0,errorCountWarnings=0;
static QString programOutput;
QAssistantClient *assistant;
static unsigned fileCount=0, hFileCount=0, cFileCount=0, sFileCount=0, asmFileCount=0, qllFileCount=0, oFileCount=0, aFileCount=0, txtFileCount=0, othFileCount=0;
tprSettings settings;
tprLibOpts libopts;
static QString projectFileName;
static QString lastDirectory;
QClipboard *clipboard;
static QAccel *accel, *fileTreeAccel, *errorListAccel;
static KFindDialog *kfinddialog;
static QListViewItem *findCurrentDocument;
static unsigned findCurrentLine;
QPtrList<SourceFile> sourceFiles;
static QPopupMenu *findFunctionsPopup;
bool have_usb;
Tools tools, tempTools;
int toolIndex;

class DnDListView : public KListView {
  private:
  public:
  DnDListView ( QWidget * parent = 0, const char * name = 0)
          : KListView(parent,name) {}
  // Make my hand-coded drag&drop code work (public part).
  // Maybe the built-in drag&drop support in KListView could be made to work as
  // expected, but for now just bypass it to use the existing code I wrote for
  // QListView.
  virtual void takeItem(QListViewItem *i) {QListView::takeItem(i);}
  virtual void setAcceptDrops(bool on) {QListView::setAcceptDrops(on);}
  protected:
  virtual QDragObject *dragObject() {
    QListViewItem *currItem=selectedItem();
    if (currItem==rootListItem || currItem->parent()==rootListItem)
      return NULL;
    QStoredDrag *storedDrag=new QStoredDrag("x-ktigcc-dnd", this);
    static QByteArray data(sizeof(QListViewItem*));
    data.duplicate(reinterpret_cast<char *>(&currItem),
                   sizeof(QListViewItem*));
    storedDrag->setEncodedData(data);
    return storedDrag;
  }
  virtual void dropEvent (QDropEvent *e) {
    if (!compiling && e->source()==this && e->provides("x-ktigcc-dnd")) {
      QListViewItem *currItem;
      currItem = *reinterpret_cast<QListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
      if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
        // dropping folder
        // can only drop on folder or category
        QListViewItem *item=itemAt(e->pos());
        if (IS_FOLDER(item)) {
          // need same category
          CATEGORY_OF(srcCategory,currItem);
          CATEGORY_OF(destCategory,item);
          if (srcCategory == destCategory) {
            // can't move folder into itself
            for (QListViewItem *destFolder=item; IS_FOLDER(destFolder); destFolder=destFolder->parent()) {
              if (destFolder==currItem) goto ignore;
            }
            // move folder
            e->accept();
            currItem->parent()->takeItem(currItem);
            item->insertItem(currItem);
            // put it at the right place
            if (currItem->nextSibling()) {
              QListViewItem *lastItem=currItem->nextSibling();
              while(lastItem->nextSibling())
                lastItem=lastItem->nextSibling();
              currItem->moveItem(lastItem);
            }
            projectIsDirty=TRUE;
            projectNeedsRelink=TRUE;
          } else {ignore: e->ignore();}
        } else e->ignore();
      } else if (IS_FILE(currItem)) {
        // dropping file
        QListViewItem *item=itemAt(e->pos());
        if (IS_FOLDER(item)) {
          // drop on folder
          // don't allow more than one Quill file per project
          CATEGORY_OF(srcCategory,currItem);
          CATEGORY_OF(destCategory,item);
          if (qllFilesListItem && srcCategory != qllFilesListItem
              && destCategory == qllFilesListItem && qllFileCount) {
            ignore2: e->ignore();
          } else {
            // moving from editable to non-editable category -
            // prompt for saving
            if (IS_EDITABLE_CATEGORY(srcCategory)
                && !IS_EDITABLE_CATEGORY(destCategory)) {
              if (static_cast<MainForm *>(parent()->parent()->parent())->fileSavePrompt(currItem))
                goto ignore2;
              if (static_cast<ListViewFile *>(currItem)->fileName[0]=='/')
                KDirWatch::self()->removeFile(static_cast<ListViewFile *>(currItem)->fileName);
            }
            // moving from non-editable to editable category
            if (!IS_EDITABLE_CATEGORY(srcCategory)
                && IS_EDITABLE_CATEGORY(destCategory)) {
              QString textBuffer=loadFileText(static_cast<ListViewFile *>(currItem)->fileName);
              if (textBuffer.isNull()) {
                KMessageBox::error(this,QString("Can't open \'%1\'").arg(static_cast<ListViewFile *>(currItem)->fileName));
                goto ignore2;
              }
              static_cast<ListViewFile *>(currItem)->kateView=reinterpret_cast<Kate::View *>(static_cast<MainForm *>(parent()->parent()->parent())->createView(static_cast<ListViewFile *>(currItem)->fileName,textBuffer,destCategory));
              MainForm::createErrorCursorsForSourceFile(currItem);
              // force reloading the text buffer
              if (currentListItem==currItem)
                currentListItem=NULL;
            }
            // move file
            e->accept();
            currItem->parent()->takeItem(currItem);
            COUNTER_FOR_CATEGORY(srcCategory)--;
            item->insertItem(currItem);
            COUNTER_FOR_CATEGORY(destCategory)++;
            // put it at the right place
            if (IS_FILE(currItem->nextSibling())) {
              QListViewItem *lastItem=currItem->nextSibling();
              while(IS_FILE(lastItem->nextSibling()))
                lastItem=lastItem->nextSibling();
              currItem->moveItem(lastItem);
            }
            projectIsDirty=TRUE;
            projectNeedsRelink=TRUE;
            setSelected(currItem,TRUE);
            ensureItemVisible(currItem);
            // update editor and counters
            static_cast<MainForm *>(parent()->parent()->parent())->fileTreeClicked(currItem);
            // moving from non-editable to editable category
            if (!IS_EDITABLE_CATEGORY(srcCategory)
                && IS_EDITABLE_CATEGORY(destCategory)) {
              if (static_cast<ListViewFile *>(currItem)->fileName[0]=='/')
                KDirWatch::self()->addFile(static_cast<ListViewFile *>(currItem)->fileName);
            }
            // moving from editable to non-editable category
            if (IS_EDITABLE_CATEGORY(srcCategory)
                && !IS_EDITABLE_CATEGORY(destCategory)) {
              if (static_cast<ListViewFile *>(currItem)->kateView) {
                Kate::Document *doc=static_cast<ListViewFile *>(currItem)->kateView->getDoc();
                delete static_cast<ListViewFile *>(currItem)->kateView;
                delete doc;
                static_cast<ListViewFile *>(currItem)->kateView=NULL;
              } else static_cast<ListViewFile *>(currItem)->textBuffer=QString::null;
            }
            // moving from editable to editable category
            if (IS_EDITABLE_CATEGORY(srcCategory)
                && IS_EDITABLE_CATEGORY(destCategory)
                && srcCategory!=destCategory
                && static_cast<ListViewFile *>(currItem)->kateView) {
              // update highlighting mode
              uint cnt=static_cast<ListViewFile *>(currItem)->kateView->getDoc()->hlModeCount(), i;
              QString fileText=static_cast<ListViewFile *>(currItem)->textBuffer;
              for (i=0; i<cnt; i++) {
                if (!static_cast<ListViewFile *>(currItem)->kateView->getDoc()->hlModeName(i).compare(
                    ((destCategory==sFilesListItem||(destCategory==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
                       "GNU Assembler 68k":
                     (destCategory==asmFilesListItem||(destCategory==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
                       "Motorola Assembler 68k":
                     (destCategory==cFilesListItem||destCategory==qllFilesListItem||destCategory==hFilesListItem)?
                       "C":
                     "None"))) break;
              }
              if (i==cnt) i=0;
              static_cast<ListViewFile *>(currItem)->kateView->getDoc()->setHlMode(i);
            }
            // update icon
            currItem->setPixmap(0,
              destCategory==cFilesListItem||destCategory==qllFilesListItem?SYSICON("source_c","filec.png"):
              destCategory==hFilesListItem?SYSICON("source_h","fileh.png"):
              destCategory==sFilesListItem||destCategory==asmFilesListItem?SYSICON("source_s","files.png"):
              destCategory==txtFilesListItem?SYSICON("txt","filet.png"):
              destCategory==oFilesListItem||destCategory==aFilesListItem?SYSICON("binary","fileo.png"):
              SYSICON("unknown","filex.png"));
          }
        } else if (IS_FILE(item)) {
          // drop on file
          // need same parent, but different items
          if (currItem->parent() == item->parent()
              && currItem != item) {
            // reorder files
            // figure out which one is the first
            for (QListViewItem *i=currItem->parent()->firstChild();i;
                 i=i->nextSibling()) {
              if (i==currItem) {
                // currItem is first, move currItem after item
                e->accept();
                currItem->moveItem(item);
                projectIsDirty=TRUE;
                projectNeedsRelink=TRUE;
                break;
              } else if (i==item) {
                // item is first, move currItem before item
                e->accept();
                currItem->moveItem(item);
                item->moveItem(currItem);
                projectIsDirty=TRUE;
                projectNeedsRelink=TRUE;
                break;
              }
            }
          } else e->ignore();
        } else e->ignore();
      } else e->ignore();
    } else e->ignore();
  }
  virtual void dragEnterEvent (QDragEnterEvent *e) {
    if (!compiling && e->source()==this&&(e->provides("x-ktigcc-dnd")))
	  e->accept();
  }
  virtual void dragMoveEvent (QDragMoveEvent *e) {
    if (!compiling && e->source()==this && e->provides("x-ktigcc-dnd")) {
      QListViewItem *currItem;
      currItem = *reinterpret_cast<QListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
      if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
        // dropping folder
        // can only drop on folder or category
        QListViewItem *item=itemAt(e->pos());
        if (IS_FOLDER(item)) {
          // need same category
          CATEGORY_OF(srcCategory,currItem);
          CATEGORY_OF(destCategory,item);
          if (srcCategory == destCategory) {
            // can't move folder into itself
            for (QListViewItem *destFolder=item; IS_FOLDER(destFolder); destFolder=destFolder->parent()) {
              if (destFolder==currItem) goto ignore;
            }
            e->accept();
          } else {ignore: e->ignore();}
        } else e->ignore();
      } else if (IS_FILE(currItem)) {
        // dropping file
        QListViewItem *item=itemAt(e->pos());
        if (IS_FOLDER(item)) {
          // drop on folder
          // don't allow more than one Quill file per project
          CATEGORY_OF(srcCategory,currItem);
          CATEGORY_OF(destCategory,item);
          if (qllFilesListItem && srcCategory != qllFilesListItem
              && destCategory == qllFilesListItem && qllFileCount)
            e->ignore();
          else
            e->accept();
        } else if (IS_FILE(item)) {
          // drop on file
          // need same parent, but different items
          if (currItem->parent() == item->parent()
              && currItem != item) e->accept(); else e->ignore();
        } else e->ignore();
      } else e->ignore();
    } else e->ignore();
  }
  // Make my hand-coded drag&drop code work (protected part).
  virtual void contentsDragMoveEvent(QDragMoveEvent *e) {
    QListView::contentsDragMoveEvent(e);
  }
  virtual void contentsMouseMoveEvent(QMouseEvent *e) {
    QListView::contentsMouseMoveEvent(e);
  }
  virtual void contentsDragLeaveEvent(QDragLeaveEvent *e) {
    QListView::contentsDragLeaveEvent(e);
  }
  virtual void contentsDropEvent(QDropEvent *e) {
    QListView::contentsDropEvent(e);
  }
  virtual void contentsDragEnterEvent(QDragEnterEvent *e) {
    QListView::contentsDragEnterEvent(e);
  }
  virtual void startDrag() {
    QListView::startDrag();
  }
};

enum ErrorTypes {etError, etWarning, etInfo};
class ErrorListItem : public KListViewItem {
  public:
  ErrorListItem(MainForm *pMainForm, ErrorTypes errType,
                const QString &errFile, const QString &errFunc,
                const QString &errMsg, unsigned errLine, unsigned errColumn)
    : KListViewItem(errorList->errorListView,
                    errorList->errorListView->lastItem()),
      lvFile(0), srcFile(0), cursor(0), errorLine(-1), errorColumn(0),
      mainForm(pMainForm), errorType(errType)
  {
    QString errMessage=errMsg.stripWhiteSpace();
    if (!errMessage.isEmpty()) errMessage[0]=errMessage[0].upper();
    switch(errType) {
      case etError:
        setPixmap(0,SYSICON("messagebox_critical","error.png"));
        break;
      case etWarning:
        setPixmap(0,SYSICON("messagebox_warning","warning.png"));
        break;
      default:
        setPixmap(0,SYSICON("messagebox_info","info.png"));
        break;
    }
    setText(0,errMessage);
    setText(1,errFile);
    setText(2,errFunc=="__exit"?"_exit":
              (errFunc=="__main"?"_main":errFunc));
    if (!errFile.isEmpty() && !errFile.endsWith(".a")) {
      // Look for the source file with the error.
      if (!findSourceFile(errFile) && errFile.endsWith(".o")) {
        QString errSrcFile=errFile;
        int lengthWoExt=errSrcFile.length()-2;
        errSrcFile.truncate(lengthWoExt);
        errSrcFile.append(".c");
        if (!findSourceFile(errSrcFile)) {
          errSrcFile.truncate(lengthWoExt);
          errSrcFile.append(".s");
          if (!findSourceFile(errSrcFile)) {
            errSrcFile.truncate(lengthWoExt);
            errSrcFile.append(".asm");
            findSourceFile(errSrcFile);
          }
        }
      }
      // Not found. Try to open it instead.
      // Don't do this if the name ends with ".tpr" because that would cause
      // openProject to close the current project and load the new one instead.
      if (!lvFile && !srcFile && !errFile.endsWith(".tpr",FALSE)) {
        if (errFile.contains('/')&&getPathType(errFile)==PATH_FILE)
          mainForm->openProject(errFile);
        else if (getPathType(QString("%1/include/c/%2").arg(tigcc_base).arg(errFile))==PATH_FILE)
          mainForm->openProject(QString("%1/include/c/%2").arg(tigcc_base).arg(errFile));
        else if (getPathType(QString("%1/include/asm/%2").arg(tigcc_base).arg(errFile))==PATH_FILE)
          mainForm->openProject(QString("%1/include/asm/%2").arg(tigcc_base).arg(errFile));
        else if (getPathType(QString("%1/include/s/%2").arg(tigcc_base).arg(errFile))==PATH_FILE)
          mainForm->openProject(QString("%1/include/s/%2").arg(tigcc_base).arg(errFile));
        findSourceFile(errFile);
      }
    }
    if (errLine!=(unsigned)-1 && (lvFile || srcFile)) {
      if (errColumn==(unsigned)-1) errColumn=0;
      const LineStartList &lineStartList=lvFile?lvFile->lineStartList
                                               :srcFile->lineStartList;
      if (lineStartList.isEmpty()) {
        errorLine=errLine;
        errorColumn=errColumn;
      } else if (errLine<lineStartList.count()) {
        QPair<unsigned,unsigned> pos=lineStartList[errLine];
        errorLine=pos.first;
        errorColumn=pos.second+errColumn;
      }
      createCursor();
    }
    if (preferences.jumpToError && errType==etError
        && errorList->errorListView->selectedItems().isEmpty()) {
      errorList->errorListView->setSelected(this,TRUE);
      jumpToLocation();
    }
    errorCountTotal++;
    if (errType==etError) errorCountErrors++;
    if (errType==etWarning) errorCountWarnings++;
    errorList->errorCount->setText(QString::number(errorCountErrors));
    errorList->warningCount->setText(QString::number(errorCountWarnings));
    mainForm->projectErrorsAndWarningsAction->setEnabled(TRUE);
    mainForm->projectErrorsAndWarnings(TRUE);
  }
  virtual ~ErrorListItem()
  {
    if (errorType==etError) errorCountErrors--;
    if (errorType==etWarning) errorCountWarnings--;
    errorList->errorCount->setText(QString::number(errorCountErrors));
    errorList->warningCount->setText(QString::number(errorCountWarnings));
    if (!--errorCountTotal) {
      mainForm->projectErrorsAndWarnings(FALSE);
      mainForm->projectErrorsAndWarningsAction->setEnabled(FALSE);
    }
  }
  virtual int rtti(void) const {return static_cast<int>(errorType);}
  void createCursor(void)
  {
    if (errorLine!=(unsigned)-1) {
      Kate::View *kateView=lvFile?lvFile->kateView:(srcFile?srcFile->kateView
                             :static_cast<Kate::View *>(NULL));
      if (kateView && errorLine<kateView->getDoc()->numLines()) {
        // Extract the main token for the error message.
        QString errMessage=text(0);
        int quotePos=errMessage.find('\'');
        if (quotePos>=0) {
          QString token=errMessage.mid(quotePos+1);
          int quotePos2=token.find('\'');
          if (quotePos2>=0) {
            token.truncate(quotePos2);
            if (!token.isEmpty()) {
              // Skip whitespace up to this token. TIGCC IDE does that too. Must
              // have something to do with how source splitting works.
              unsigned i=errorColumn;
              QString textLine=kateView->getDoc()->textLine(errorLine);
              unsigned lineLength=kateView->getDoc()->lineLength(errorLine);
              unsigned tokenLength=token.length();
              while ((i<lineLength) && textLine[i].isSpace()
                     && textLine.mid(i,tokenLength)!=token) i++;
              if (textLine.mid(i,tokenLength)==token) errorColumn=i;
            }
          }
        }
        cursor=kateView->getDoc()->createCursor();
        cursor->setPosition(errorLine,errorColumn);
      }
    }
  }
  void jumpToLocation(void)
  {
    // If the error corresponds to a list view file, select it. This will also
    // instantiate the Kate view and call createCursor for us.
    if (lvFile) mainForm->fileTreeClicked(lvFile);
    // If it corresponds to an external source file, activate the window.
    if (srcFile) KWin::activateWindow(srcFile->winId());
    // Now jump to the cursor's location if we have one.
    Kate::View *kateView=lvFile?lvFile->kateView:(srcFile?srcFile->kateView
                           :static_cast<Kate::View *>(NULL));
    if (cursor && kateView) {
      unsigned line,col;
      cursor->position(&line,&col);
      kateView->setCursorPositionReal(line,col);
    }
  }
  ListViewFile *lvFile;
  SourceFile *srcFile;
  KTextEditor::Cursor *cursor;
  private:
  unsigned errorLine;
  unsigned errorColumn;
  MainForm *mainForm;
  ErrorTypes errorType;
  bool findSourceFile(const QString &fileName)
  {
    bool inProject;
    void *sourceFile;
    bool found=mainForm->findSourceFile(inProject,sourceFile,fileName);
    if (found) {
      if (inProject)
        lvFile=reinterpret_cast<ListViewFile *>(sourceFile);
      else
        srcFile=reinterpret_cast<SourceFile *>(sourceFile);
    }
    return found;
  }
};

// This static helper function is needed because of limitations of both .ui.h
// files and C++: we can't have methods of other classes than MainForm here
// unless we write them into the class definition or Qt Designer gets confused,
// and C++ won't let 2 classes reference each other in methods declared within
// the class definition. And item is of type QListViewItem rather than
// ListViewFile because of another Qt Designer limitation: It isn't possible to
// use custom types in a .ui file method because it doesn't forward-declare them
// properly.
void MainForm::deleteErrorsForLVFile(QListViewItem *item)
{
  QListViewItemIterator lvit(errorList->errorListView);
  QListViewItem *errorItem;
  while ((errorItem=lvit.current())) {
    ++lvit;
    if (static_cast<ErrorListItem *>(errorItem)->lvFile
        ==static_cast<ListViewFile *>(item))
      delete errorItem;
  }
}

// Another static helper function, in this case because srcfilewin.ui.h doesn't
// have access to ErrorListItem for similar reasons. (I'd have to duplicate the
// class definition, one time with inline functions and one time without.)
void MainForm::deleteErrorsForSrcFile(void *srcFile)
{
  QListViewItemIterator lvit(errorList->errorListView);
  QListViewItem *errorItem;
  while ((errorItem=lvit.current())) {
    ++lvit;
    if (static_cast<ErrorListItem *>(errorItem)->srcFile
        ==reinterpret_cast<SourceFile *>(srcFile))
      delete errorItem;
  }
}

// And another.
void MainForm::createErrorCursorsForSourceFile(QListViewItem *item)
{
  QListViewItemIterator lvit(errorList->errorListView);
  QListViewItem *errorItem;
  for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
    if (static_cast<ErrorListItem *>(errorItem)->lvFile
        ==static_cast<ListViewFile *>(item))
      static_cast<ErrorListItem *>(errorItem)->createCursor();
  }
}

// And the last.
void MainForm::deleteOverwrittenErrorsIn(void *srcFile)
{
  SourceFile *sourceFile=reinterpret_cast<SourceFile *>(srcFile);
  QListViewItemIterator lvit(errorList->errorListView);
  ErrorListItem *errorItem;
  while ((errorItem=static_cast<ErrorListItem *>(lvit.current()))) {
    ++lvit;
    if (errorItem->srcFile==sourceFile && errorItem->cursor) {
      unsigned line,col;
      errorItem->cursor->position(&line,&col);
      if (sourceFile->kateView->cursorLine()==line
          && sourceFile->kateView->cursorColumnReal()==col)
        delete errorItem;
    }
  }
}

void MainForm::errorListView_clicked(QListViewItem *item)
{
  if (item) static_cast<ErrorListItem *>(item)->jumpToLocation();
}

void MainForm::errorListAccel_activated(int index)
{
  if (index==0 || index==1) {
    // Copy selected errors to the clipboard.
    QListViewItemIterator lvit(errorList->errorListView,
                               QListViewItemIterator::Selected);
    QListViewItem *errorItem;
    QString clipboardText;
    for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
      clipboardText.append(errorItem->text(0));
      clipboardText.append('\n');
    }
    clipboard->setText(clipboardText,QClipboard::Clipboard);
  }
}

bool MainForm::findSourceFile(bool &inProject, void *&srcFile, const QString &fileName)
{
  bool compareAbsPaths=fileName.contains('/');
  QListViewItemIterator lvit(fileTree);
  QListViewItem *item;
  for (item=lvit.current();item;item=(++lvit).current()) {
    if (IS_FILE(item)
        && (compareAbsPaths?fileName==static_cast<ListViewFile *>(item)->fileName
                           :fileName==QFileInfo(static_cast<ListViewFile *>(item)->fileName).fileName())) {
      inProject=TRUE;
      srcFile=static_cast<ListViewFile *>(item);
      return TRUE;
    }
  }
  QPtrListIterator<SourceFile> sfit(sourceFiles);
  SourceFile *sourceFile;
  for (sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
    if (compareAbsPaths?fileName==sourceFile->fileName
                       :fileName==QFileInfo(sourceFile->fileName).fileName()) {
      inProject=FALSE;
      srcFile=sourceFile;
      return TRUE;
    }
  }
  return FALSE;
}

void MainForm::init()
{
  setIcon(QPixmap::fromMimeSource("icon.png"));
  KWin::setIcons(winId(),*(icon()),QPixmap::fromMimeSource("ktigcc.png"));
  ticables_library_init();
  tifiles_library_init();
  ticalcs_library_init();
  have_usb=ticables_is_usb_enabled();
  compiling=FALSE;
  headersModified=FALSE;
  loadPreferences();
  fileNewFolderAction->setEnabled(FALSE);
  te_popup = new QPopupMenu(this);
  te_popup->insertItem("&Open file at cursor",0);
  te_popup->insertItem("&Find symbol declaration",1);
  te_popup->insertSeparator();
  te_popup->insertItem("&Undo",2);
  te_popup->insertItem("&Redo",3);
  te_popup->insertSeparator();
  te_popup->insertItem("&Clear",4);
  te_popup->insertItem("Cu&t",5);
  te_popup->insertItem("Cop&y",6);
  te_popup->insertItem("&Paste",7);
  te_popup->insertSeparator();
  te_popup->insertItem("&Select all",8);
  te_popup->insertSeparator();
  te_popup->insertItem("&Increase indent",9);
  te_popup->insertItem("&Decrease indent",10);
  connect(te_popup,SIGNAL(aboutToShow()),this,SLOT(te_popup_aboutToShow()));
  connect(te_popup,SIGNAL(activated(int)),this,SLOT(te_popup_activated(int)));
  QValueList<int> list;
  list.append(150);
  list.append(500);
  splitter->setSizes(list);
  leftStatusLabel=new QLabel("0 Files Total",this);
  leftStatusLabel->setMaximumWidth(splitter->sizes().first());
  statusBar()->addWidget(leftStatusLabel,1);
  rowStatusLabel=new QLabel("",this);
  rowStatusLabel->setAlignment(Qt::AlignRight);
  statusBar()->addWidget(rowStatusLabel,1);
  rowStatusLabel->hide();
  colStatusLabel=new QLabel("",this);
  colStatusLabel->setAlignment(Qt::AlignRight);
  statusBar()->addWidget(colStatusLabel,1);
  colStatusLabel->hide();
  charsStatusLabel=new QLabel("",this);
  statusBar()->addWidget(charsStatusLabel,1);
  charsStatusLabel->hide();
  rightStatusLabel=new QLabel("",this);
  rightStatusLabel->setMaximumWidth(splitter->sizes().last());
  statusBar()->addWidget(rightStatusLabel,1);
  statusBar()->setSizeGripEnabled(FALSE);
  connect(statusBar(),SIGNAL(messageChanged(const QString &)),this,SLOT(statusBar_messageChanged(const QString &)));
  fileTree->setSorting(-1);
  fileTree->setColumnWidthMode(0,QListView::Maximum);
  fileTree->header()->hide();
  rootListItem=new ListViewRoot(fileTree);
  rootListItem->setText(0,"Project1");
  rootListItem->setPixmap(0,SYSICON("exec","tpr.png"));
  rootListItem->setOpen(TRUE);
  QListViewItem *folderListItem=new ListViewFolder(rootListItem);
  hFilesListItem=folderListItem;
  folderListItem->setText(0,"Header Files");
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  cFilesListItem=folderListItem;
  folderListItem->setText(0,"C Files");
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  sFilesListItem=folderListItem;
  folderListItem->setText(0,"GNU Assembly Files");
  char a68k_path[strlen(tigcc_base)+10];
  sprintf(a68k_path, "%s/bin/a68k", tigcc_base);
  if(access(a68k_path, F_OK) != -1) {
    folderListItem=new ListViewFolder(rootListItem,folderListItem);
    asmFilesListItem=folderListItem;
    folderListItem->setText(0,"A68k Assembly Files");
  } else {
    qllFilesListItem=NULL;
    fileNewQuillSourceFileAction->setVisible(FALSE);
  }
  if (quill_drv) {
    folderListItem=new ListViewFolder(rootListItem,folderListItem);
    qllFilesListItem=folderListItem;
    folderListItem->setText(0,"Quill Files");
  } else {
    qllFilesListItem=NULL;
    fileNewQuillSourceFileAction->setVisible(FALSE);
  }
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  oFilesListItem=folderListItem;
  folderListItem->setText(0,"Object Files");
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  aFilesListItem=folderListItem;
  folderListItem->setText(0,"Archive Files");
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  txtFilesListItem=folderListItem;
  folderListItem->setText(0,"Text Files");
  folderListItem=new ListViewFolder(rootListItem,folderListItem);
  othFilesListItem=folderListItem;
  folderListItem->setText(0,"Other Files");
  khelpmenu=new KHelpMenu(this,pabout);
  assistant = new QAssistantClient("",this);
  QStringList args(QString("-profile"));
  args.append(QString("%1/doc/html/qt-assistant.adp").arg(tigcc_base));
  assistant->setArguments(args);
  lastDirectory=QString("%1/projects").arg(tigcc_base);
  projectFileName="";
  projectIsDirty=FALSE;
  projectNeedsRelink=FALSE;
  connect(KDirWatch::self(),SIGNAL(created(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  connect(KDirWatch::self(),SIGNAL(dirty(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  KDirWatch::self()->startScan();
  clipboard=QApplication::clipboard();
  connect(clipboard,SIGNAL(dataChanged()),this,SLOT(clipboard_dataChanged()));
  accel=new QAccel(this);
  accel->insertItem(ALT+Key_Backspace,0);
  accel->setItemEnabled(0,FALSE);
  accel->insertItem(SHIFT+ALT+Key_Backspace,1);
  accel->setItemEnabled(1,FALSE);
  accel->insertItem(SHIFT+Key_Delete,2);
  accel->setItemEnabled(2,FALSE);
  accel->insertItem(CTRL+Key_Insert,3);
  accel->setItemEnabled(3,FALSE);
  accel->insertItem(SHIFT+Key_Insert,4);
  accel->setItemEnabled(4,FALSE);
  accel->insertItem(Key_F1,5);
  accel->setItemEnabled(5,FALSE);
  accel->insertItem(Key_Enter,6);
  accel->setItemEnabled(6,FALSE);
  accel->insertItem(Key_Return,7);
  accel->setItemEnabled(7,FALSE);
  connect(accel,SIGNAL(activated(int)),this,SLOT(accel_activated(int)));
  fileTreeAccel=new QAccel(this);
  fileTreeAccel->insertItem(Key_Delete,0);
  connect(fileTreeAccel,SIGNAL(activated(int)),
          this,SLOT(fileTreeAccel_activated(int)));
  kfinddialog = static_cast<KFindDialog *>(NULL);
  kreplace = static_cast<KReplaceWithSelection *>(NULL);
  if (preferences.useSystemIcons) {
    setUsesBigPixmaps(TRUE);
    fileNewActionGroup->setIconSet(LOAD_ICON("filenew"));
    fileMenu->changeItem(fileMenu->idAt(0),LOAD_ICON("filenew"),"&New");
    fileOpenAction->setIconSet(LOAD_ICON("fileopen"));
    fileOpenActionGroup->setIconSet(LOAD_ICON("fileopen"));
    fileSaveAllAction->setIconSet(LOAD_ICON("filesave"));
    filePrintAction->setIconSet(LOAD_ICON("fileprint"));
    filePrintQuicklyAction->setIconSet(LOAD_ICON("fileprint"));
    editClearAction->setIconSet(LOAD_ICON("editdelete"));
    editCutAction->setIconSet(LOAD_ICON("editcut"));
    editCopyAction->setIconSet(LOAD_ICON("editcopy"));
    editPasteAction->setIconSet(LOAD_ICON("editpaste"));
    projectAddFilesAction->setIconSet(LOAD_ICON("edit_add"));
    projectCompileAction->setIconSet(LOAD_ICON("compfile"));
    projectMakeAction->setIconSet(LOAD_ICON("make_kdevelop"));
    projectBuildAction->setIconSet(LOAD_ICON("rebuild"));
    helpContentsAction->setIconSet(LOAD_ICON("help"));
    helpDocumentationAction->setIconSet(LOAD_ICON("help"));
    helpSearchAction->setIconSet(LOAD_ICON("filefind"));
    findFindAction->setIconSet(LOAD_ICON("filefind"));
    if (KGlobal::iconLoader()->iconPath("stock-find-and-replace",KIcon::Small,TRUE).isEmpty()) {
      QIconSet fileReplaceIconSet(QPixmap::fromMimeSource("filereplace.png"));
      int smallSize=IconSize(KIcon::Small);
      fileReplaceIconSet.setIconSize(QIconSet::Small,QSize(smallSize,smallSize));
      int largeSize=IconSize(KIcon::MainToolbar);
      fileReplaceIconSet.setIconSize(QIconSet::Large,QSize(largeSize,largeSize));
      findReplaceAction->setIconSet(fileReplaceIconSet);
    } else
      findReplaceAction->setIconSet(LOAD_ICON("stock-find-and-replace"));
    helpIndexAction->setIconSet(LOAD_ICON("contents"));
    editUndoAction->setIconSet(LOAD_ICON("undo"));
    editRedoAction->setIconSet(LOAD_ICON("redo"));
    findFunctionsAction->setIconSet(LOAD_ICON("view_tree"));
    editIncreaseIndentAction->setIconSet(LOAD_ICON("indent"));
    editDecreaseIndentAction->setIconSet(LOAD_ICON("unindent"));
    projectStopCompilationAction->setIconSet(LOAD_ICON("stop"));
    projectForceQuitAction->setIconSet(LOAD_ICON("button_cancel"));
    helpNewsAction->setIconSet(LOAD_ICON("kontact_news"));
    debugRunAction->setIconSet(LOAD_ICON("player_play"));
    debugPauseAction->setIconSet(LOAD_ICON("player_pause"));
    toolsConfigureAction->setIconSet(LOAD_ICON("configure"));
    debugResetAction->setIconSet(LOAD_ICON("player_stop"));
  }
  QToolButton *findFunctionsButton=static_cast<QToolButton *>(toolBar
    ->child("findFunctionsAction_action_button","QToolButton",FALSE));
  findFunctionsPopup=new QPopupMenu(findFunctionsButton);
  connect(findFunctionsPopup,SIGNAL(aboutToShow()),
          this,SLOT(findFunctionsPopup_aboutToShow()));
  connect(findFunctionsPopup,SIGNAL(aboutToHide()),
          this,SLOT(findFunctionsPopup_aboutToHide()));
  connect(findFunctionsPopup,SIGNAL(activated(int)),
          this,SLOT(findFunctionsPopup_activated(int)));
  findFunctionsButton->setPopupDelay(0);
  findFunctionsButton->setPopup(findFunctionsPopup);
  errorListDock=new QDockWindow(QDockWindow::InDock,this);
  errorListDock->setResizeEnabled(TRUE);
  errorListDock->setCloseMode(QDockWindow::Always);
  addToolBar(errorListDock,Qt::DockBottom);
  errorList=new ErrorList(errorListDock);
  errorListDock->setWidget(errorList);
  errorList->show();
  errorListDock->setCaption("Errors and Warnings");
  errorListDock->hide();
  setDockEnabled(errorListDock,Qt::DockTop,FALSE);
  setDockEnabled(errorListDock,Qt::DockLeft,FALSE);
  setDockEnabled(errorListDock,Qt::DockRight,FALSE);
  connect(errorListDock,SIGNAL(visibilityChanged(bool)),
          this,SLOT(projectErrorsAndWarnings(bool)));
  errorList->errorListView->setSorting(-1);
  connect(errorList->errorListView,SIGNAL(clicked(QListViewItem *)),
          this,SLOT(errorListView_clicked(QListViewItem *)));
  errorListAccel=new QAccel(errorList->errorListView);
  errorListAccel->insertItem(CTRL+Key_C,0);
  errorListAccel->insertItem(CTRL+Key_Insert,1);
  connect(errorListAccel,SIGNAL(activated(int)),
          this,SLOT(errorListAccel_activated(int)));
  if (preferences.linkTarget!=LT_TIEMU) {
    debugPauseAction->setEnabled(FALSE);
    debugResetAction->setEnabled(FALSE);
  }
  if (preferences.linkTarget==LT_NONE) {
    menuBar()->setItemVisible(5,FALSE); //debugMenu
    debugRunAction->setVisible(FALSE);
    debugPauseAction->setVisible(FALSE);
  }
  pconfig->setGroup("Recent files");
  if (parg) {
    QString fileName=QDir().absFilePath(parg);
    if (!openProject(fileName)) goto openRecent;
  } else {
    openRecent:;
    QString mostrecent=pconfig->readEntry("Current project");
    if (!mostrecent.isNull() && !mostrecent.isEmpty())
      openProject(mostrecent);
  }
  updateRecent();
  pconfig->setGroup("Tools");
  unsigned toolCount=pconfig->readUnsignedNumEntry("Count",0);
  tools.resize(toolCount);
  for (unsigned idx=0; idx<toolCount; idx++) {
    pconfig->setGroup(QString("Tool %1").arg(idx));
    Tool &tool=tools[idx];
    tool.title=pconfig->readEntry("Title");
    tool.commandLine=pconfig->readEntry("Command Line");
    tool.workingDirectory=pconfig->readEntry("Working Directory");
    tool.runInTerminal=pconfig->readBoolEntry("Terminal");
  }
  updateToolsMenu();
  connect(toolsMenu,SIGNAL(activated(int)),this,SLOT(toolsMenu_activated(int)));
  startTimer(100);
  if (preferences.downloadHeadlines) {
    NewsDialog newsDialog(this);
    if (newsDialog.loadNews())
      newsDialog.exec();
  }
}

void MainForm::destroy()
{
  delete errorListAccel;
  while (!sourceFiles.isEmpty()) {
    delete sourceFiles.getFirst();
  }
  if (kreplace) delete kreplace;
  if (kfinddialog) delete kfinddialog;
  delete fileTreeAccel;
  delete accel;
  delete te_popup;
  delete leftStatusLabel;
  delete rowStatusLabel;
  delete colStatusLabel;
  delete charsStatusLabel;
  delete rightStatusLabel;
  delete rootListItem;
  delete khelpmenu;
  delete assistant;
  delete errorList;
  delete errorListDock;
  ticalcs_library_exit();
  tifiles_library_exit();
  ticables_library_exit();
}

void MainForm::te_popup_aboutToShow()
{
  te_popup->setItemEnabled(0,findOpenFileAtCursorAction->isEnabled());
  te_popup->setItemEnabled(1,findFindSymbolDeclarationAction->isEnabled());
  te_popup->setItemEnabled(2,editUndoAction->isEnabled());
  te_popup->setItemEnabled(3,editRedoAction->isEnabled());
  te_popup->setItemEnabled(4,editClearAction->isEnabled());
  te_popup->setItemEnabled(5,editCutAction->isEnabled());
  te_popup->setItemEnabled(6,editCopyAction->isEnabled());
  te_popup->setItemEnabled(7,editPasteAction->isEnabled());
  te_popup->setItemEnabled(8,editSelectAllAction->isEnabled());
  te_popup->setItemEnabled(9,editIncreaseIndentAction->isEnabled());
  te_popup->setItemEnabled(10,editDecreaseIndentAction->isEnabled());
}

void MainForm::te_popup_activated(int index)
{
  switch (index) {
    case 0: findOpenFileAtCursor(); break;
    case 1: findFindSymbolDeclaration(); break;
    case 2: editUndo(); break;
    case 3: editRedo(); break;
    case 4: editClear(); break;
    case 5: editCut(); break;
    case 6: editCopy(); break;
    case 7: editPaste(); break;
    case 8: editSelectAll(); break;
    case 9: editIncreaseIndent(); break;
    case 10: editDecreaseIndent(); break;
    default: break;
  }
}

void MainForm::accel_activated(int index)
{
  if (CURRENT_VIEW && CURRENT_VIEW->hasFocus()) {
    switch (index) {
      case 0: editUndo(); break;
      case 1: editRedo(); break;
      case 2: editCut(); break;
      case 3: editCopy(); break;
      case 4: editPaste(); break;
      case 5: // F1 context help
      {
        QString wordUnderCursor=CURRENT_VIEW->currentWord();
        // always open at least the index
        force_qt_assistant_page(1);
        assistant->openAssistant();
        if (wordUnderCursor.isEmpty()) return;
        QString docFile=lookup_doc_keyword(wordUnderCursor);
        if (docFile.isEmpty()) return;
        // wait for Qt Assistant to actually open
        while (!assistant->isOpen())
          QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,1000);
        assistant->showPage(QString(tigcc_base)+QString("/doc/html/")+docFile);
        break;
      }
      case 6:
      case 7:
        CURRENT_VIEW->keyReturn();
        current_view_newLineHook();
        break;
      default: break;
    }
  } else if (index == 6 || index == 7) {
    QKeyEvent *keyEvent=new QKeyEvent(QEvent::KeyPress,Key_Return,'\n',0,"\n");
    QApplication::postEvent(focusWidget(),keyEvent);
  }
}

void MainForm::fileTreeAccel_activated(int index)
{
  QListViewItem *item=fileTree->currentItem();
  if (!index && item) removeItem(item);
}

void MainForm::clearProject()
{
  headersModified=FALSE;
  newSettings(&settings,&libopts);
  rootListItem->setText(0,"Project1");
  projectFileName="";
  fileTreeClicked(rootListItem);
  // Better do this now or the file destructors will have to iterate over the
  // error list each time, and I'd have to clear the list at the end anyway
  // because an error might not have a source file assigned.
  errorList->errorListView->clear();
  programOutput=QString::null;
  projectProgramOutputAction->setEnabled(FALSE);
  QListViewItem *f, *next;
  for (f=hFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  for (f=cFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  for (f=sFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  if (asmFilesListItem) {
    for (f=asmFilesListItem->firstChild();f;f=next) {
      next=f->nextSibling();
      delete f;
    }
  }
  if (qllFilesListItem) {
    for (f=qllFilesListItem->firstChild();f;f=next) {
      next=f->nextSibling();
      delete f;
    }
  }
  for (f=oFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  for (f=aFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  for (f=txtFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  for (f=othFilesListItem->firstChild();f;f=next) {
    next=f->nextSibling();
    delete f;
  }
  fileCount=cFileCount=hFileCount=sFileCount=asmFileCount=qllFileCount=oFileCount=aFileCount=txtFileCount=othFileCount=0;
  projectIsDirty=FALSE;
  projectNeedsRelink=FALSE;
  menuBar()->setItemVisible(5,preferences.linkTarget==LT_NONE); //debugMenu
  debugRunAction->setVisible(preferences.linkTarget==LT_NONE);
  debugPauseAction->setVisible(preferences.linkTarget==LT_NONE);
  updateLeftStatusLabel();
}

void MainForm::fileNewProject()
{
  if (compiling || savePrompt())
    return;
  clearProject();
  pconfig->setGroup("Recent files");
  pconfig->writeEntry("Current project","");
  pconfig->sync();
}

QString MainForm::findFilter(unsigned short job)
{
  QString ret;
  if (job==TIGCCOpenProjectFileFilter)
  {
    ret="*.tpr *.h *.c *.s ";
    if (asmFilesListItem)
      ret+="*.asm ";
    if (qllFilesListItem)
      ret+="*.qll ";
    ret+="*.txt";
    ret+="|All TIGCC Files ("+ret+")\n"
         TIGCC_TPR_Filter TIGCC_H_Filter TIGCC_C_Filter TIGCC_S_Filter;
    if (asmFilesListItem)
      ret+=TIGCC_ASM_Filter;
    if (qllFilesListItem)
      ret+=TIGCC_QLL_Filter;
    ret+=TIGCC_TXT_Filter TIGCCAllFilter;
  }
  else if (job==TIGCCAddFilesFilter)
  {
    ret="*.h *.c *.s ";
    if (asmFilesListItem)
      ret+="*.asm ";
    if (qllFilesListItem)
      ret+="*.qll ";
    ret+="*.o *.a *.txt";
    ret+="|All TIGCC Files ("+ret+")\n"
         TIGCC_H_Filter TIGCC_C_Filter TIGCC_S_Filter;
    if (asmFilesListItem)
      ret+=TIGCC_ASM_Filter;
    if (qllFilesListItem)
      ret+=TIGCC_QLL_Filter;
    ret+=TIGCC_O_Filter TIGCC_A_Filter TIGCC_TXT_Filter TIGCCAllFilter;
  }
  return ret;
}

QString MainForm::SGetFileName(int mode,const QString &fileFilter,const QString &caption,QWidget *parent)
{
  QString ret;
  if (static_cast<KFileDialog::OperationMode>(mode)==KFileDialog::Opening)
    ret=KFileDialog::getOpenFileName(lastDirectory,fileFilter,parent,caption);
  else
    ret=KFileDialog::getSaveFileName(lastDirectory,fileFilter,parent,caption);
  if (!ret.isNull())
  {
    KURL dir;
    dir.setPath(ret);
    dir.setFileName("");
    lastDirectory=dir.path();
  }
  return ret;
}

//no mode, since it you can't save multiple.
QStringList MainForm::SGetFileName_Multiple(const QString &fileFilter,const QString &caption,QWidget *parent)
{
  QStringList ret;
  ret=KFileDialog::getOpenFileNames(lastDirectory,fileFilter,parent,caption);
  if (!ret.empty())
  {
    KURL dir;
    dir.setPath(ret[0]);
    dir.setFileName("");
    lastDirectory=dir.path();
  }
  return ret;
}

void MainForm::updateRecent()
{
  pconfig->setGroup("Recent files");
  QString recent=pconfig->readEntry("Recent file 1");
  if (recent.isNull())
    fileRecent1Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent1Action->setVisible(TRUE);
    fileRecent1Action->setText(recentcut);
    fileRecent1Action->setStatusTip(recent);
  }
  recent=pconfig->readEntry("Recent file 2");
  if (recent.isNull())
    fileRecent2Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent2Action->setVisible(TRUE);
    fileRecent2Action->setText(recentcut);
    fileRecent2Action->setStatusTip(recent);
  }
  recent=pconfig->readEntry("Recent file 3");
  if (recent.isNull())
    fileRecent3Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent3Action->setVisible(TRUE);
    fileRecent3Action->setText(recentcut);
    fileRecent3Action->setStatusTip(recent);
  }
  recent=pconfig->readEntry("Recent file 4");
  if (recent.isNull())
    fileRecent4Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent4Action->setVisible(TRUE);
    fileRecent4Action->setText(recentcut);
    fileRecent4Action->setStatusTip(recent);
  }
}

void MainForm::addRecent(const QString &fileName)
{
  unsigned i,j;
  pconfig->setGroup("Recent files");
  // Find recent file to overwrite. If it isn't one of the first 3, by
  // elimination, it is the last, thus the test only goes up to <4, not <=4.
  for (i=1;i<4;i++) {
    QString recenti=pconfig->readEntry(QString("Recent file %1").arg(i));
    if (recenti.isNull() || !recenti.compare(fileName))
      break;
  }
  // Move entries up
  for (j=i;j>1;j--) {
    pconfig->writeEntry(QString("Recent file %1").arg(j),pconfig->readEntry(QString("Recent file %1").arg(j-1)));
  }
  // The first recent file is the current project.
  pconfig->writeEntry("Recent file 1",fileName);
  pconfig->writeEntry("Current project",fileName);
  pconfig->sync();
  updateRecent();
}

QListViewItem * MainForm::openFile(QListViewItem * category, QListViewItem * parent, const QString &fileCaption, const QString &fileName)
{
  QString fileText;
  switch (getPathType(fileName)) {
    case PATH_FILE: // OK
      break;
    case PATH_NOTFOUND:
      KMessageBox::error(this,QString("File \'%1\' not found").arg(fileName));
      return NULL;
    default:
    KMessageBox::error(this,QString("\'%1\' is not a regular file").arg(fileName));
    return NULL;
  }
  if (IS_EDITABLE_CATEGORY(category)) {
    fileText=loadFileText(fileName);
    if (fileText.isNull()) {
      KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
      return NULL;
    }
  }
  QListViewItem *item=NULL, *next=parent->firstChild();
  for (; IS_FILE(next); next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(parent,item)
                        :new ListViewFile(parent);
  newFile->isNew=FALSE;
  newFile->modifiedSinceLastCompile=FALSE;
  newFile->setText(0,fileCaption);
  newFile->setPixmap(0,
    category==cFilesListItem||category==qllFilesListItem?SYSICON("source_c","filec.png"):
    category==hFilesListItem?SYSICON("source_h","fileh.png"):
    category==sFilesListItem||category==asmFilesListItem?SYSICON("source_s","files.png"):
    category==txtFilesListItem?SYSICON("txt","filet.png"):
    category==oFilesListItem||category==aFilesListItem?SYSICON("binary","fileo.png"):
    SYSICON("unknown","filex.png"));
  newFile->fileName=fileName;
  if (IS_EDITABLE_CATEGORY(category)) {
    if (preferences.lazyLoading)
      newFile->textBuffer=fileText;
    else
      newFile->kateView=reinterpret_cast<Kate::View *>(createView(fileName,fileText,category));
    KDirWatch::self()->addFile(fileName);
  }
  fileCount++;
  COUNTER_FOR_CATEGORY(category)++;
  return newFile;
}

QListViewItem *MainForm::createFolder(QListViewItem *parent,const QString &name)
{
  QListViewItem *item=parent->firstChild();
  QListViewItem *startItem=item;
  QListViewItem *newItem;
  for (; item; item=item->nextSibling())
  {
    if (IS_FOLDER(item) && !item->text(0).compare(name))
      return item;
  }
  item=NULL;
  for (;startItem;startItem=item->nextSibling())
    item=startItem;
  newItem=item?new ListViewFolder(parent,item)
              :new ListViewFolder(parent);
  newItem->setText(0,name);
  newItem->setOpen(TRUE);
  return newItem;
}

void *MainForm::createView(const QString &fileName, const QString &fileText, QListViewItem *category)
{
  // Create Document object.
  KParts::Factory *factory = (KParts::Factory *)
    KLibLoader::self()->factory ("libkatepart");
  if (!factory) qFatal("Failed to load KatePart");
  Kate::Document *doc = (Kate::Document *)
      factory->createPart( 0, "", this, "", "Kate::Document" );
  // Set the file name for printing.
  doc->setModified(FALSE);
  if (doc->openStream("text/plain",fileName))
    doc->closeStream();
  // Create View object.
  Kate::View *newView = (Kate::View *) doc->createView( widgetStack, 0L );
  newView->hide();
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  // Set highlighting mode.
  uint cnt=newView->getDoc()->hlModeCount(), i;
  for (i=0; i<cnt; i++) {
    if (!newView->getDoc()->hlModeName(i).compare(
        ((category==sFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
           "GNU Assembler 68k":
         (category==asmFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
           "Motorola Assembler 68k":
         (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?
           "C":
         "None"))) break;
  }
  if (i==cnt) i=0;
  newView->getDoc()->setHlMode(i);
  // Set options.
  newView->setDynWordWrap(FALSE);
  if (preferences.removeTrailingSpaces)
    newView->getDoc()->setConfigFlags(newView->getDoc()->configFlags()|(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  else
    newView->getDoc()->setConfigFlags(newView->getDoc()->configFlags()&~(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  newView->setTabWidth(
    (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&(fileText[0]=='|'||fileText[0]==';'))))?preferences.tabWidthAsm:
    (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
    8
  );
  connect(newView,SIGNAL(cursorPositionChanged()),this,SLOT(current_view_cursorPositionChanged()));
  connect(newView->getDoc(),SIGNAL(textChanged()),this,SLOT(current_view_textChanged()));
  connect(newView->getDoc(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  connect(newView->getDoc(),SIGNAL(selectionChanged()),this,SLOT(current_view_selectionChanged()));
  connect(newView->getDoc(),SIGNAL(charactersInteractivelyInserted(int,int,const QString&)),this,SLOT(current_view_charactersInteractivelyInserted(int,int,const QString&)));
  newView->installPopup(te_popup);
  // Set text.
  SET_TEXT_SAFE(newView->getDoc(),fileText);
  newView->getDoc()->setModified(FALSE);
  newView->getDoc()->clearUndo();
  newView->getDoc()->clearRedo();
  newView->setCursorPositionReal(0,0);
  return newView;
}

void MainForm::adoptSourceFile(void *srcFile)
{
  if (compiling) return;
  SourceFile *sourceFile=reinterpret_cast<SourceFile *>(srcFile);
  QString fileName=sourceFile->fileName;
  Kate::View *newView=sourceFile->kateView;
  // Determine category and caption.
  QListViewItem *category=othFilesListItem;
  QString suffix,caption;
  int p;
  p=fileName.findRev('/');
  if (p<0) p=-1;
  caption=fileName.mid(p+1);
  p=caption.findRev('.');
  if (p>=0) {
    suffix=caption.mid(p+1);
    caption.truncate(p);
  }
  if (!checkFileName(fileName,extractAllFileNames())) {
    KMessageBox::error(this,QString("The file \'%1\' is already included in the project.").arg(caption));
    return;
  }
  if (!suffix.compare("h"))
    category=hFilesListItem;
  else if (!suffix.compare("c"))
    category=cFilesListItem;
  else if (!suffix.compare("s"))
    category=sFilesListItem;
  else if (!suffix.compare("asm"))
    category=asmFilesListItem;
  else if (!suffix.compare("qll"))
    category=qllFilesListItem;
  else
    category=txtFilesListItem;
  if (category && category==qllFilesListItem && qllFileCount) {
    KMessageBox::error(this,"There may be only one Quill source file in each project.","Quill Error");
    return;
  }
  if (!category)
    category=txtFilesListItem;
  // Create file tree entry.
  QListViewItem *item=NULL, *next=category->firstChild();
  for (; IS_FILE(next); next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(category,item)
                            :new ListViewFile(category);
  newFile->isNew=FALSE;
  newFile->setText(0,caption);
  newFile->setPixmap(0,
    category==cFilesListItem||category==qllFilesListItem?SYSICON("source_c","filec.png"):
    category==hFilesListItem?SYSICON("source_h","fileh.png"):
    category==sFilesListItem||category==asmFilesListItem?SYSICON("source_s","files.png"):
    SYSICON("txt","filet.png"));
  newFile->fileName=fileName;
  newFile->modifiedSinceLastCompile=newView->getDoc()->isModified();
  // Adopt View object.
  newFile->kateView=newView;
  newView->hide();
  newView->reparent(widgetStack,QPoint());
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  KDirWatch::self()->addFile(fileName);
  fileCount++;
  COUNTER_FOR_CATEGORY(category)++;
  // Set highlighting mode.
  QString fileText=newView->getDoc()->text();
  uint cnt=newView->getDoc()->hlModeCount(), i;
  for (i=0; i<cnt; i++) {
    if (!newView->getDoc()->hlModeName(i).compare(
        ((category==sFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
           "GNU Assembler 68k":
         (category==asmFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
           "Motorola Assembler 68k":
         (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?
           "C":
         "None"))) break;
  }
  if (i==cnt) i=0;
  newView->getDoc()->setHlMode(i);
  // Set options.
  newView->setTabWidth(
    (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&(fileText[0]=='|'||fileText[0]==';'))))?preferences.tabWidthAsm:
    (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
    8
  );
  connect(newView,SIGNAL(cursorPositionChanged()),this,SLOT(current_view_cursorPositionChanged()));
  connect(newView->getDoc(),SIGNAL(textChanged()),this,SLOT(current_view_textChanged()));
  connect(newView->getDoc(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  connect(newView->getDoc(),SIGNAL(selectionChanged()),this,SLOT(current_view_selectionChanged()));
  connect(newView->getDoc(),SIGNAL(charactersInteractivelyInserted(int,int,const QString&)),this,SLOT(current_view_charactersInteractivelyInserted(int,int,const QString&)));
  newView->installPopup(te_popup);
  // Mark project dirty.
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
  // Select file.
  fileTreeClicked(newFile);
  // Update errors to point to the in-project source file instead of the
  // external one.
  QListViewItemIterator lvit(errorList->errorListView);
  QListViewItem *errorItem;
  for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
    if (static_cast<ErrorListItem *>(errorItem)->srcFile==sourceFile) {
      static_cast<ErrorListItem *>(errorItem)->srcFile=static_cast<SourceFile *>(NULL);
      static_cast<ErrorListItem *>(errorItem)->lvFile=newFile;
    }
  }
  // Close separate source view.
  sourceFile->kateView=static_cast<Kate::View *>(NULL);
  sourceFile->deleteLater();
}

void MainForm::fileOpen_addList(QListViewItem *category,void *fileListV,void *dir, const QString &open_file)
{
  int i,e;
  int p,pslash;
  KURL tmp;
  TPRFileList *fileList=(TPRFileList*)fileListV;
  QString caption;
  QString treePath;
  QListViewItem *parent;
  e=fileList->path.count();
  if (e) category->setOpen(TRUE);
  for (i=0;i<e;i++)
  {
    tmp=*reinterpret_cast<const KURL *>(dir);
    kurlNewFileName(tmp,fileList->path[i]);
    caption=fileList->path[i];
    //fixed suffix truncation for file paths such as "/root/.dot/nodot" so it wouldn't truncate to "/root/"
    p=caption.findRev('.');
    pslash=caption.findRev('/');
    if (p>=0&&p>pslash) caption.truncate(p);
    if (pslash>=0) caption.remove(0,pslash+1);
    treePath=fileList->folder[i].stripWhiteSpace();
    //check for a backslash at the end and remove it if it's there.
    if (treePath[treePath.length()-1]=='\\')
      treePath.truncate(treePath.length()-1);
    parent=category;
    if (!treePath.isEmpty())
    {
        while ((p=treePath.find('\\'))>=0)
        {
          parent=createFolder(parent,treePath.left(p));
          treePath.remove(0,p+1);
        }
        parent=createFolder(parent,treePath);
    }
    
    ListViewFile *newFile=static_cast<ListViewFile *>(openFile(category,parent,caption,tmp.path()));
    if (!newFile) continue;
    if (!newFile->fileName.compare(open_file))
      fileTreeClicked(newFile);
  }
}

// Returns TRUE if the file is a project, FALSE if the file is a source file.
bool MainForm::openProject(const QString &fileName)
{
  TPRDataStruct TPRData;
  KURL dir;
  bool isProject=fileName.endsWith(".tpr",FALSE);
  dir.setPath(fileName);
  switch (getPathType(fileName)) {
    case PATH_FILE: // OK
      break;
    case PATH_NOTFOUND:
      KMessageBox::error(this,QString("File \'%1\' not found").arg(fileName));
      return isProject;
    default:
    KMessageBox::error(this,QString("\'%1\' is not a regular file").arg(fileName));
    return isProject;
  }
  if (isProject) {
    int ret=loadTPR(fileName, &TPRData);
    if (ret == -1) {
      KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
      return TRUE;
    }
    if (ret > 0) {
      KMessageBox::error(this,QString("Error at line %2 of \'%1\'").arg(fileName).arg(ret));
      return TRUE;
    }
    if (TPRData.asm_files.path.count() && !asmFilesListItem) {
      KMessageBox::error(this,"This project needs A68k, which is not installed.");
      return TRUE;
    }
    if (TPRData.quill_files.path.count() && !qllFilesListItem) {
      KMessageBox::error(this,"This project needs quill.drv, which is not installed.");
      return TRUE;
    }
    if (TPRData.settings.fargo && !have_fargo) {
      KMessageBox::error(this,"This project needs fargo.a, which is not installed.");
      return TRUE;
    }
    if (TPRData.settings.flash_os && !have_flashos) {
      KMessageBox::error(this,"This project needs flashos.a, which is not installed.");
      return TRUE;
    }
    clearProject();
    fileOpen_addList(hFilesListItem,&TPRData.h_files,&dir,TPRData.open_file);
    fileOpen_addList(cFilesListItem,&TPRData.c_files,&dir,TPRData.open_file);
    fileOpen_addList(qllFilesListItem,&TPRData.quill_files,&dir,TPRData.open_file);
    fileOpen_addList(sFilesListItem,&TPRData.s_files,&dir,TPRData.open_file);
    fileOpen_addList(asmFilesListItem,&TPRData.asm_files,&dir,TPRData.open_file);
    fileOpen_addList(oFilesListItem,&TPRData.o_files,&dir,TPRData.open_file);
    fileOpen_addList(aFilesListItem,&TPRData.a_files,&dir,TPRData.open_file);
    fileOpen_addList(txtFilesListItem,&TPRData.txt_files,&dir,TPRData.open_file);
    fileOpen_addList(othFilesListItem,&TPRData.oth_files,&dir,TPRData.open_file);
    rootListItem->setText(0,TPRData.prj_name);
    projectFileName=fileName;
    settings=TPRData.settings;
    libopts=TPRData.libopts;
    bool runnable=!settings.archive&&!settings.flash_os&&preferences.linkTarget!=LT_NONE;
    menuBar()->setItemVisible(5,runnable); //debugMenu
    debugRunAction->setVisible(runnable);
    debugPauseAction->setVisible(runnable);
    updateLeftStatusLabel();
    updateRightStatusLabel();
    addRecent(fileName);
    return TRUE;
  } else {
    QListViewItem *category=othFilesListItem;
    QString suffix,caption;
    int p;
    
    p=fileName.findRev('/');
    if (p<0) p=-1;
    caption=fileName.mid(p+1);
    p=caption.findRev('.');
    if (p>=0) {
      suffix=caption.mid(p+1);
      caption.truncate(p);
    }
    
    if (extractAllFileNames().contains(fileName)) {
      KMessageBox::error(this,QString("The file \'%1\' is already included in the project.").arg(caption));
      return FALSE;
    }
    QPtrListIterator<SourceFile> sfit(sourceFiles);
    SourceFile *sourceFile;
    for (sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
      if (!fileName.compare(sourceFile->fileName)) {
        KWin::activateWindow(sourceFile->winId());
        return FALSE;
      }
    }

    if (!suffix.compare("h"))
      category=hFilesListItem;
    else if (!suffix.compare("c"))
      category=cFilesListItem;
    else if (!suffix.compare("s"))
      category=sFilesListItem;
    else if (!suffix.compare("asm"))
      category=asmFilesListItem;
    else if (!suffix.compare("qll"))
      category=qllFilesListItem;
    else if (!suffix.compare("txt"))
      category=txtFilesListItem;
    else {
      KMessageBox::error(this,QString("\'%1\' is not a valid file for opening.").arg(fileName));
      return FALSE;
    }
    QString fileText=loadFileText(fileName);
    if (fileText.isNull()) {
      KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
      return FALSE;
    }
    int type=((category==sFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
                2:
              (category==asmFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
                3:
              (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?
                1:
              0);
    
    new SourceFile(this,fileName,fileText,
                   (type==2)?"GNU Assembler 68k":
                   (type==3)?"Motorola Assembler 68k":
                   (type==1)?"C":
                    "None",category,(type==1),(type>1),category==txtFilesListItem);
    return FALSE;
  }
}

void MainForm::fileOpen()
{
  if (compiling || savePrompt())
    return;
  QString fileName=SGetFileName(KFileDialog::Opening,findFilter(TIGCCOpenProjectFileFilter),"Open Project/File",this);
  KURL dir;
  dir.setPath(fileName);
  if (fileName.isEmpty())
    return;
  openProject(fileName);
}

void MainForm::fileRecent1()
{
  if (compiling || savePrompt())
    return;
  openProject(fileRecent1Action->statusTip());
}

void MainForm::fileRecent2()
{
  if (compiling || savePrompt())
    return;
  openProject(fileRecent2Action->statusTip());
}

void MainForm::fileRecent3()
{
  if (compiling || savePrompt())
    return;
  openProject(fileRecent3Action->statusTip());
}

void MainForm::fileRecent4()
{
  if (compiling || savePrompt())
    return;
  openProject(fileRecent4Action->statusTip());
}

int MainForm::fileSavePrompt(QListViewItem *fileItem)
{
  int result;
  ListViewFile *theFile=static_cast<ListViewFile *>(fileItem);
  if (!theFile->kateView) return 0;
  while (theFile->kateView->getDoc()->isModified()) { // "while" in case saving fails!
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(theFile->text(0)),QString::null,KStdGuiItem::save(),KStdGuiItem::discard());
    if (result==KMessageBox::Yes)
        fileSave_save(fileItem);
    else if (result==KMessageBox::No)
      theFile->kateView->getDoc()->setModified(FALSE);
    else
      return 1;
  }
  return 0;
}

//returns 1 if the current project data should not be cleared out, 0 if it can be cleared out.
int MainForm::savePrompt(void)
{
  int result;
  
  while (projectIsDirty) {
    result=KMessageBox::questionYesNoCancel(this,"The current project has been modified.  Do you want to save the changes?",QString::null,KStdGuiItem::save(),KStdGuiItem::discard());
    if (result==KMessageBox::Yes)
      fileSave();
    else if (result==KMessageBox::No)
      projectIsDirty=FALSE;
    else
      return 1;
  }
  
  QListViewItem *item=rootListItem->firstChild(),*next;
  while (item)
  {
    if (IS_FOLDER(item))
    {
      next=item->firstChild();
      if (next)
      {
        item=next;
        continue;
      }
    }
    if (IS_FILE(item))
    {
      if (fileSavePrompt(item))
        return 1;
    }
    next=item->nextSibling();
    while (!next)
    {
      next=item->parent();
      if (next==rootListItem||!next)
      {
        return 0;
      }
      item=next;
      next=item->nextSibling();
    }
    item=next;
  }
  
  return 0;
}

void MainForm::removeTrailingSpacesFromView(void *view)
{
  if (!preferences.removeTrailingSpaces) return;
  Kate::View *kateView=reinterpret_cast<Kate::View *>(view);
  Kate::Document *doc=kateView->getDoc();
  KTextEditor::EditInterfaceExt *editExt=KTextEditor::editInterfaceExt(doc);
  editExt->editBegin();
  unsigned numLines=doc->numLines();
  for (unsigned i=0; i<numLines; i++) {
    QString line=doc->textLine(i);
    int whitespace=line.find(QRegExp("\\s+$"));
    if (whitespace>=0) doc->removeText(i,whitespace,i,line.length());
  }
  editExt->editEnd();
}

void MainForm::fileSave_save(QListViewItem *theItem)
{
  if (!IS_FILE(theItem))
    return;
  CATEGORY_OF(category,theItem);
  if (!IS_EDITABLE_CATEGORY(category))
    return;
  ListViewFile *theFile=static_cast<ListViewFile *>(theItem);
  if (theFile->fileName[0]!='/') {
    fileSave_saveAs(theFile);
  }
  else {
    KDirWatch::self()->removeFile(theFile->fileName);
    if (saveFileText(theFile->fileName,theFile->kateView?theFile->kateView->getDoc()->text():theFile->textBuffer)) {
      KMessageBox::error(this,QString("Can't save to \'%1\'").arg(theFile->text(0)));
      KDirWatch::self()->addFile(theFile->fileName);
    }
    else {
      KDirWatch::self()->addFile(theFile->fileName);
      theFile->isNew=FALSE;
      if (theFile->kateView) {
        removeTrailingSpacesFromView(theFile->kateView);
        theFile->kateView->getDoc()->setModified(FALSE);
      }
      projectIsDirty=TRUE;
      projectNeedsRelink=TRUE;
    }
  }
}

void MainForm::fileSave_saveAs(QListViewItem *theItem)
{
  if (!IS_FILE(theItem))
    return;
  CATEGORY_OF(category,theItem);
  QString saveFileName=SGetFileName(KFileDialog::Saving,
  category==hFilesListItem?TIGCC_H_Filter TIGCCAllFilter:
  category==cFilesListItem?TIGCC_C_Filter TIGCCAllFilter:
  category==sFilesListItem?TIGCC_S_Filter TIGCCAllFilter:
  category==asmFilesListItem?TIGCC_ASM_Filter TIGCCAllFilter:
  category==qllFilesListItem?TIGCC_QLL_Filter TIGCCAllFilter:
  category==oFilesListItem?TIGCC_O_Filter TIGCCAllFilter:
  category==aFilesListItem?TIGCC_A_Filter TIGCCAllFilter:
  category==txtFilesListItem?TIGCC_TXT_Filter TIGCCAllFilter:
  TIGCCAllFilter
  ,"Save Source File",this);
  ListViewFile *theFile=static_cast<ListViewFile *>(theItem);
  if (saveFileName.isEmpty()
      || (!IS_EDITABLE_CATEGORY(category)
          && !saveFileName.compare(theFile->fileName)))
    return;
  if (saveFileName.compare(theFile->fileName)
      && !checkFileName(saveFileName,extractAllFileNames())) {
    KMessageBox::error(this,"The name you chose conflicts with that of another file.");
    return;
  }
  if (theFile->fileName[0]=='/')
    KDirWatch::self()->removeFile(theFile->fileName);
  if (IS_EDITABLE_CATEGORY(category)
      ?saveFileText(saveFileName,theFile->kateView?theFile->kateView->getDoc()->text():theFile->textBuffer)
      :copyFile(theFile->fileName,saveFileName)) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    if (IS_EDITABLE_CATEGORY(category) && theFile->fileName[0]=='/')
      KDirWatch::self()->addFile(theFile->fileName);
  } else {
    if (IS_EDITABLE_CATEGORY(category) && saveFileName.compare(theFile->fileName)
        && theFile->kateView) {
      // Update the file name for printing.
      unsigned int line,col,hlMode;
      QString fileText=theFile->kateView->getDoc()->text();
      hlMode=theFile->kateView->getDoc()->hlMode();
      theFile->kateView->cursorPositionReal(&line,&col);
      theFile->kateView->getDoc()->setModified(FALSE);
      if (theFile->kateView->getDoc()->openStream("text/plain",saveFileName))
        theFile->kateView->getDoc()->closeStream();
      SET_TEXT_SAFE(theFile->kateView->getDoc(),fileText);
      theFile->kateView->getDoc()->clearUndo();
      theFile->kateView->getDoc()->clearRedo();
      theFile->kateView->getDoc()->setHlMode(hlMode);
      theFile->kateView->setCursorPositionReal(line,col);
    }
    theFile->fileName=saveFileName;
    if (IS_EDITABLE_CATEGORY(category)) {
      KDirWatch::self()->addFile(saveFileName);
      if (theFile->kateView) {
        removeTrailingSpacesFromView(theFile->kateView);
        theFile->kateView->getDoc()->setModified(FALSE);
      }
    }
    theFile->isNew=FALSE;
    updateRightStatusLabel();
    projectIsDirty=TRUE;
    projectNeedsRelink=TRUE;
  }
}

//loadList also saves the file contents
void MainForm::fileSave_loadList(QListViewItem *category,void *fileListV,const QString &base_dir,void *dir_new,QString *open_file)
{
  if (!category)
    return;
  TPRFileList *fileList=(TPRFileList*)fileListV;
  KURL *new_dir=(KURL*)dir_new;
  KURL tmpPath;
  QListViewItem *item=category->firstChild();
  QListViewItem *next;
  QString folderSpec=QString::null;
  int o;
  while (item)
  {
    if (IS_FILE(item))
    {
      ListViewFile *theFile=static_cast<ListViewFile *>(item);
      QString absPath=theFile->fileName;
      QString relPath=KURL::relativePath(base_dir,absPath);
      if (relPath.startsWith("./"))
      {
        relPath=relPath.mid(2);
      }
      else if (relPath.startsWith("../"))
      {
        relPath=absPath;
      }
      
      tmpPath=*new_dir;
      kurlNewFileName(tmpPath,relPath);
      if (theFile->fileName[0]=='/')
        KDirWatch::self()->removeFile(theFile->fileName);
      if (tmpPath.path().compare(theFile->fileName)
          || (IS_EDITABLE_CATEGORY(category)
              && ((theFile->kateView && theFile->kateView->getDoc()->isModified()) || theFile->isNew))) {
        if (IS_EDITABLE_CATEGORY(category)
            ?saveFileText(tmpPath.path(),theFile->kateView?theFile->kateView->getDoc()->text():theFile->textBuffer)
            :copyFile(theFile->fileName,tmpPath.path())) {
          KMessageBox::error(this,QString("Can't save to \'%1\'").arg(tmpPath.path()));
          if (IS_EDITABLE_CATEGORY(category) && theFile->fileName[0]=='/')
            KDirWatch::self()->addFile(theFile->fileName);
        } else {
          QString saveFileName=tmpPath.path();
          if (IS_EDITABLE_CATEGORY(category) && saveFileName.compare(theFile->fileName)
              && theFile->kateView) {
            // Update the file name for printing.
            unsigned int line,col,hlMode;
            QString fileText=theFile->kateView->getDoc()->text();
            hlMode=theFile->kateView->getDoc()->hlMode();
            theFile->kateView->cursorPositionReal(&line,&col);
            theFile->kateView->getDoc()->setModified(FALSE);
            if (theFile->kateView->getDoc()->openStream("text/plain",saveFileName))
              theFile->kateView->getDoc()->closeStream();
            SET_TEXT_SAFE(theFile->kateView->getDoc(),fileText);
            theFile->kateView->getDoc()->clearUndo();
            theFile->kateView->getDoc()->clearRedo();
            theFile->kateView->getDoc()->setHlMode(hlMode);
            theFile->kateView->setCursorPositionReal(line,col);
          }
          theFile->fileName=saveFileName;
          if (IS_EDITABLE_CATEGORY(category)) {
            KDirWatch::self()->addFile(theFile->fileName);
            if (theFile->kateView) {
              removeTrailingSpacesFromView(theFile->kateView);
              theFile->kateView->getDoc()->setModified(FALSE);
            }
          }
          theFile->isNew=FALSE;
          projectIsDirty=TRUE; // in case saving the project fails
          projectNeedsRelink=TRUE;
        }
      }
      
      fileList->path << relPath;
      fileList->folder << folderSpec;
      
      if (item==currentListItem)
        *open_file=theFile->fileName;
    }
    else if (IS_FOLDER(item))
    {
      next=item->firstChild();
      if (next)
      {
        if (folderSpec.isEmpty())
          folderSpec=item->text(0);
        else
        {
          folderSpec+='\\';
          folderSpec+=item->text(0);
        }
        item=next;
        continue;
      }
    }
fsll_seeknext:
    next=item->nextSibling();
    if (!next)
    {
      next=item->parent();
      if (next==category||!next)
        break;
      item=next;
      o=folderSpec.findRev('\\');
      if (o>=0)
        folderSpec.truncate(o);
      else
        folderSpec.truncate(0);
      goto fsll_seeknext;
    }
    item=next;
  }
}


void MainForm::fileSave_fromto(const QString &lastProj,const QString &nextProj)
{
  TPRDataStruct TPRData;
  QString open_file;
  KURL base_dir_k(lastProj);
  base_dir_k.setFileName("");
  QString base_dir=base_dir_k.path();
  KURL new_dir(nextProj);
  
  fileSave_loadList(hFilesListItem,&TPRData.h_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(cFilesListItem,&TPRData.c_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(qllFilesListItem,&TPRData.quill_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(sFilesListItem,&TPRData.s_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(asmFilesListItem,&TPRData.asm_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(oFilesListItem,&TPRData.o_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(aFilesListItem,&TPRData.a_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(txtFilesListItem,&TPRData.txt_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(othFilesListItem,&TPRData.oth_files,base_dir,&new_dir,&open_file);
  TPRData.prj_name=rootListItem->text(0);
  TPRData.open_file=open_file;
  TPRData.settings=settings;
  TPRData.libopts=libopts;
  
  if (saveTPR(nextProj,&TPRData)) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(nextProj));
    updateRightStatusLabel();
    return;
  } else {
    projectFileName=nextProj;
    projectIsDirty=FALSE;
    addRecent(nextProj);
  }
  updateRightStatusLabel();
  QPtrListIterator<SourceFile> sfit(sourceFiles);
  for (SourceFile *sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
    sourceFile->fileSave();
  }
}

bool MainForm::fileSave()
{
  if (projectFileName.isEmpty())
    return fileSaveAs();
  else {
    fileSave_fromto(projectFileName,projectFileName);
    return TRUE;
  }
}

bool MainForm::fileSaveAs()
{
  QString fileName=SGetFileName(KFileDialog::Saving,TIGCC_TPR_Filter TIGCCAllFilter,"Save Project",this);
  if (fileName.isEmpty())
    return FALSE;
  fileSave_fromto(projectFileName.isEmpty()?fileName:projectFileName,fileName);
  return TRUE;
}

void MainForm::filePrint()
{
  if (CURRENT_VIEW) CURRENT_VIEW->getDoc()->printDialog();
}


void MainForm::filePrintQuickly()
{
  if (CURRENT_VIEW) CURRENT_VIEW->getDoc()->print();
}

void MainForm::filePreferences()
{
  if (showPreferencesDialog(this)==QDialog::Accepted) {
    // Apply the KatePart preferences and treeview icons.
    QListViewItemIterator it(fileTree);
    QListViewItem *item;
    KParts::Factory *factory = (KParts::Factory *)
      KLibLoader::self()->factory ("libkatepart");
    if (!factory) qFatal("Failed to load KatePart");
    Kate::Document *doc = (Kate::Document *)
      factory->createPart( 0, "", this, "", "Kate::Document" );
    KTextEditor::ConfigInterfaceExtension *confInterfaceExt = KTextEditor::configInterfaceExtension(doc);
    unsigned numConfigPages=confInterfaceExt->configPages();
    for (unsigned i=0; i<numConfigPages; i++) {
      if (!confInterfaceExt->configPageName(i).compare("Fonts & Colors")) {
        KTextEditor::ConfigPage *configPage=confInterfaceExt->configPage(i);
        configPage->apply();
        delete configPage;
        break;
      }
    }
    delete doc;
    for (item=it.current();item;item=(++it).current()) {
      if (item == rootListItem) {
        item->setPixmap(0,SYSICON("exec","tpr.png"));
      } else if (IS_FOLDER(item)) {
        // Bluecurve's "folder_open" isn't actually more open than "folder".
        item->setPixmap(0,(item==currentListItem)?SYSICON(KIconTheme::current().compare("Bluecurve")?"folder_open":"folder-accept","folder2.png")
                                                 :SYSICON("folder","folder1.png"));
      } else if (IS_FILE(item)) {
        Kate::View *kateView=static_cast<ListViewFile *>(item)->kateView;
        if (kateView) {
          QString fileText=kateView->getDoc()->text();
          CATEGORY_OF(category,item);
          if (preferences.removeTrailingSpaces)
            kateView->getDoc()->setConfigFlags(kateView->getDoc()->configFlags()|(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
          else
            kateView->getDoc()->setConfigFlags(kateView->getDoc()->configFlags()&~(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
          kateView->setTabWidth(
            (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&(fileText[0]=='|'||fileText[0]==';'))))?preferences.tabWidthAsm:
            (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
            8
          );
        }
        CATEGORY_OF(category,item);
        item->setPixmap(0,
          category==cFilesListItem||category==qllFilesListItem?SYSICON("source_c","filec.png"):
          category==hFilesListItem?SYSICON("source_h","fileh.png"):
          category==sFilesListItem||category==asmFilesListItem?SYSICON("source_s","files.png"):
          category==txtFilesListItem?SYSICON("txt","filet.png"):
          category==oFilesListItem||category==aFilesListItem?SYSICON("binary","fileo.png"):
          SYSICON("unknown","filex.png"));
      } else qWarning("Internal error: What's this item?");
    }
    if (CURRENT_VIEW) {
      // Force redrawing to get the tab width right, repaint() is ignored for some reason.
      Kate::View *currView=CURRENT_VIEW;
      currView->hide();
      currView->show();
    }
    // Apply the icon preferences.
    setUsesBigPixmaps(preferences.useSystemIcons);
    if (preferences.useSystemIcons) {
      fileNewActionGroup->setIconSet(LOAD_ICON("filenew"));
      fileMenu->changeItem(fileMenu->idAt(0),LOAD_ICON("filenew"),"&New");
      fileOpenAction->setIconSet(LOAD_ICON("fileopen"));
      fileOpenActionGroup->setIconSet(LOAD_ICON("fileopen"));
      fileSaveAllAction->setIconSet(LOAD_ICON("filesave"));
      filePrintAction->setIconSet(LOAD_ICON("fileprint"));
      filePrintQuicklyAction->setIconSet(LOAD_ICON("fileprint"));
      editClearAction->setIconSet(LOAD_ICON("editdelete"));
      editCutAction->setIconSet(LOAD_ICON("editcut"));
      editCopyAction->setIconSet(LOAD_ICON("editcopy"));
      editPasteAction->setIconSet(LOAD_ICON("editpaste"));
      projectAddFilesAction->setIconSet(LOAD_ICON("edit_add"));
      projectCompileAction->setIconSet(LOAD_ICON("compfile"));
      projectMakeAction->setIconSet(LOAD_ICON("make_kdevelop"));
      projectBuildAction->setIconSet(LOAD_ICON("rebuild"));
      helpContentsAction->setIconSet(LOAD_ICON("help"));
      helpDocumentationAction->setIconSet(LOAD_ICON("help"));
      helpSearchAction->setIconSet(LOAD_ICON("filefind"));
      findFindAction->setIconSet(LOAD_ICON("filefind"));
      if (KGlobal::iconLoader()->iconPath("stock-find-and-replace",KIcon::Small,TRUE).isEmpty()) {
        QIconSet fileReplaceIconSet(QPixmap::fromMimeSource("filereplace.png"));
        int smallSize=IconSize(KIcon::Small);
        fileReplaceIconSet.setIconSize(QIconSet::Small,QSize(smallSize,smallSize));
        int largeSize=IconSize(KIcon::MainToolbar);
        fileReplaceIconSet.setIconSize(QIconSet::Large,QSize(largeSize,largeSize));
        findReplaceAction->setIconSet(fileReplaceIconSet);
      } else
        findReplaceAction->setIconSet(LOAD_ICON("stock-find-and-replace"));
      helpIndexAction->setIconSet(LOAD_ICON("contents"));
      editUndoAction->setIconSet(LOAD_ICON("undo"));
      editRedoAction->setIconSet(LOAD_ICON("redo"));
      findFunctionsAction->setIconSet(LOAD_ICON("view_tree"));
      editIncreaseIndentAction->setIconSet(LOAD_ICON("indent"));
      editDecreaseIndentAction->setIconSet(LOAD_ICON("unindent"));
      projectStopCompilationAction->setIconSet(LOAD_ICON("stop"));
      projectForceQuitAction->setIconSet(LOAD_ICON("button_cancel"));
      helpNewsAction->setIconSet(LOAD_ICON("kontact_news"));
      debugRunAction->setIconSet(LOAD_ICON("player_play"));
      debugPauseAction->setIconSet(LOAD_ICON("player_pause"));
      toolsConfigureAction->setIconSet(LOAD_ICON("configure"));
      debugResetAction->setIconSet(LOAD_ICON("player_stop"));
    } else {
      fileNewActionGroup->setIconSet(QIconSet(QPixmap::fromMimeSource("00")));
      fileMenu->changeItem(fileMenu->idAt(0),QIconSet(QPixmap::fromMimeSource("00")),"&New");
      fileOpenAction->setIconSet(QIconSet(QPixmap::fromMimeSource("01")));
      fileOpenActionGroup->setIconSet(QIconSet(QPixmap::fromMimeSource("01")));
      fileSaveAllAction->setIconSet(QIconSet(QPixmap::fromMimeSource("02")));
      filePrintAction->setIconSet(QIconSet(QPixmap::fromMimeSource("03")));
      filePrintQuicklyAction->setIconSet(QIconSet(QPixmap::fromMimeSource("03")));
      editClearAction->setIconSet(QIconSet(QPixmap::fromMimeSource("04")));
      editCutAction->setIconSet(QIconSet(QPixmap::fromMimeSource("05")));
      editCopyAction->setIconSet(QIconSet(QPixmap::fromMimeSource("06")));
      editPasteAction->setIconSet(QIconSet(QPixmap::fromMimeSource("07")));
      projectAddFilesAction->setIconSet(QIconSet(QPixmap::fromMimeSource("08")));
      projectCompileAction->setIconSet(QIconSet(QPixmap::fromMimeSource("09")));
      projectMakeAction->setIconSet(QIconSet(QPixmap::fromMimeSource("10")));
      projectBuildAction->setIconSet(QIconSet(QPixmap::fromMimeSource("11")));
      helpContentsAction->setIconSet(QIconSet(QPixmap::fromMimeSource("12")));
      helpDocumentationAction->setIconSet(QIconSet(QPixmap::fromMimeSource("12")));
      helpSearchAction->setIconSet(QIconSet(QPixmap::fromMimeSource("13")));
      findFindAction->setIconSet(QIconSet(QPixmap::fromMimeSource("13")));
      findReplaceAction->setIconSet(QIconSet(QPixmap::fromMimeSource("14")));
      helpIndexAction->setIconSet(QIconSet(QPixmap::fromMimeSource("15")));
      editUndoAction->setIconSet(QIconSet(QPixmap::fromMimeSource("16")));
      editRedoAction->setIconSet(QIconSet(QPixmap::fromMimeSource("17")));
      findFunctionsAction->setIconSet(QIconSet(QPixmap::fromMimeSource("18")));
      editIncreaseIndentAction->setIconSet(QIconSet(QPixmap::fromMimeSource("19")));
      editDecreaseIndentAction->setIconSet(QIconSet(QPixmap::fromMimeSource("20")));
      projectStopCompilationAction->setIconSet(QIconSet(QPixmap::fromMimeSource("21")));
      projectForceQuitAction->setIconSet(QIconSet(QPixmap::fromMimeSource("22")));
      helpNewsAction->setIconSet(QIconSet(QPixmap::fromMimeSource("23")));
      debugRunAction->setIconSet(QIconSet(QPixmap::fromMimeSource("24")));
      debugPauseAction->setIconSet(QIconSet(QPixmap::fromMimeSource("25")));
      toolsConfigureAction->setIconSet(QIconSet(QPixmap::fromMimeSource("26")));
      debugResetAction->setIconSet(QIconSet(QPixmap::fromMimeSource("27")));
    }
    it=QListViewItemIterator(errorList->errorListView);
    for (item=it.current();item;item=(++it).current()) {
      switch(item->rtti()) {
        case etError:
          item->setPixmap(0,SYSICON("messagebox_critical","error.png"));
          break;
        case etWarning:
          item->setPixmap(0,SYSICON("messagebox_warning","warning.png"));
          break;
        default:
          item->setPixmap(0,SYSICON("messagebox_info","info.png"));
          break;
      }
    }
    // Apply the preferences to the source file windows.
    QPtrListIterator<SourceFile> sfit(sourceFiles);
    SourceFile *sourceFile;
    for (sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
      sourceFile->applyPreferences();
    }
    // Apply the preferences to the debug menu.
    debugPauseAction->setEnabled(!compiling&&preferences.linkTarget==LT_TIEMU);
    debugResetAction->setEnabled(!compiling&&preferences.linkTarget==LT_TIEMU);
    bool runnable=!settings.archive&&!settings.flash_os&&preferences.linkTarget!=LT_NONE;
    menuBar()->setItemVisible(5,runnable); //debugMenu
    debugRunAction->setVisible(runnable);
    debugPauseAction->setVisible(runnable);
  }
}

void MainForm::editUndo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->undo();
}

void MainForm::editRedo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->redo();
}

void MainForm::editClear()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->removeSelectedText();
}

void MainForm::editCut()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->cut();
}

void MainForm::editCopy()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->copy();
}

void MainForm::editPaste()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->paste();
}

void MainForm::editSelectAll()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->selectAll();
}

void MainForm::editIncreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->indent();
}

void MainForm::editDecreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->unIndent();
}

void MainForm::findFind()
{
  if (kfinddialog)
    KWin::activateWindow(kfinddialog->winId());
  else {
    // Never set hasSelection because finding in selection doesn't really make
    // sense with my non-modal find dialog setup.
    kfinddialog=new KFindDialog(false,this,0,KFindDialog::FromCursor);
    connect(kfinddialog, SIGNAL(okClicked()), this, SLOT(findFind_next()));
    connect(kfinddialog, SIGNAL(cancelClicked()), this, SLOT(findFind_stop()));
    kfinddialog->show();
  }
}

void MainForm::findFind_next()
{
  // Use a local KFind object. The search will need to be restarted next time
  // this function is called because of the non-modality of the find dialog.
  KFind *kfind=new KFind(kfinddialog->pattern(),kfinddialog->options(),this,kfinddialog);

  // Initialize.
  bool findBackwards=!!(kfinddialog->options()&KFindDialog::FindBackwards);
  int findCurrentCol;
  kfind=new KFind(kfinddialog->pattern(),kfinddialog->options(),this,kfinddialog);
  kfind->closeFindNextDialog(); // don't use this, a non-modal KFindDialog is used instead
  connect(kfind,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findFind_highlight(const QString &,int,int)));
  // Make sure we have a valid currentListItem.
  if (!currentListItem) fileTreeClicked(rootListItem);
  findCurrentDocument=currentListItem;
  if (CURRENT_VIEW) {
    if (kfinddialog->options()&KFindDialog::FromCursor) {
      if (CURRENT_VIEW->getDoc()->hasSelection()) {
        if (findBackwards) {
          findCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
          findCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
          if (findCurrentCol==-1) {
            if (!findCurrentLine) goto skip_data;
            findCurrentLine--;
          }
        } else {
          findCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
          findCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
        }
      } else {
        findCurrentLine=CURRENT_VIEW->cursorLine();
        findCurrentCol=CURRENT_VIEW->cursorColumnReal();
      }
    } else {
      findCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
      findCurrentCol=-1;
    }
    kfind->setData(CURRENT_VIEW->getDoc()->textLine(findCurrentLine),findCurrentCol);
  } else findCurrentLine=0;
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  // We never have a currBuffer here, the current list item is always either
  // non-editable or instantiated.
  QStringList currBuffer;
  unsigned currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines)) {
        if (findBackwards) {
          // Traverse the file tree in order backwards, restart from the end if
          // the first file was reached. Stop at currentListItem.
          if (findCurrentDocument) {
            QListViewItemIterator lvit(fileTree);
            QPtrList<QListViewItem> lst;
            QListViewItem *item;
            for (item=lvit.current();item&&item!=findCurrentDocument;
                 item=(++lvit).current()) {
              lst.prepend(item);
            }
            QPtrListIterator<QListViewItem> it(lst);
            for (findCurrentDocument=it.current();
                 findCurrentDocument&&findCurrentDocument!=currentListItem;
                 findCurrentDocument=++it) {
              if (IS_FILE(findCurrentDocument)) {
                CATEGORY_OF(category,findCurrentDocument);
                if (IS_EDITABLE_CATEGORY(category)) goto file_found;
              }
            }
            if (findCurrentDocument) goto not_found;
          }
          QListViewItemIterator lvit(currentListItem);
          QPtrList<QListViewItem> lst;
          QListViewItem *item;
          for (item=(++lvit).current();item;item=(++lvit).current()) {
            lst.prepend(item);
          }
          QPtrListIterator<QListViewItem> it(lst);
          for (findCurrentDocument=it.current();findCurrentDocument;
               findCurrentDocument=++it) {
            if (IS_FILE(findCurrentDocument)) {
              CATEGORY_OF(category,findCurrentDocument);
              if (IS_EDITABLE_CATEGORY(category)) goto file_found;
            }
          }
          goto not_found;
        } else {
          // Traverse the file tree in order, restart from the beginning if
          // the last file was reached. Stop at currentListItem.
          if (findCurrentDocument) {
            QListViewItemIterator it(findCurrentDocument);
            for (findCurrentDocument=(++it).current();
                 findCurrentDocument&&findCurrentDocument!=currentListItem;
                 findCurrentDocument=(++it).current()) {
              if (IS_FILE(findCurrentDocument)) {
                CATEGORY_OF(category,findCurrentDocument);
                if (IS_EDITABLE_CATEGORY(category)) goto file_found;
              }
            }
            if (findCurrentDocument) goto not_found;
          }
          {
            QListViewItemIterator it(fileTree);
            for (findCurrentDocument=it.current();
                 findCurrentDocument&&findCurrentDocument!=currentListItem;
                 findCurrentDocument=(++it).current()) {
              if (IS_FILE(findCurrentDocument)) {
                CATEGORY_OF(category,findCurrentDocument);
                if (IS_EDITABLE_CATEGORY(category)) goto file_found;
              }
            }
          }
          not_found:
            // No find in all files. Try currentListItem again because it hasn't
            // been fully searched yet.
            if (IS_FILE(currentListItem)) {
              // currentListItem is always either instantiated or not editable
              currView=static_cast<ListViewFile *>(currentListItem)->kateView;
              if (currView) {
                currNumLines=currView->getDoc()->numLines();
                findCurrentLine=findBackwards?currNumLines-1:0;
                do {
                  if (kfind->needData()) {
                    if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines))
                      goto not_found_current;
                    if (findBackwards) findCurrentLine--; else findCurrentLine++;
                    kfind->setData(currView->getDoc()->textLine(findCurrentLine));
                  }
                  result=kfind->find();
                } while (result==KFind::NoMatch);
                break;
              }
            }
            not_found_current:
              KMessageBox::error(this,QString("Text \'%1\' not found").arg(kfinddialog->pattern()));
              delete kfind;
              return;
          file_found:
            currView=static_cast<ListViewFile *>(findCurrentDocument)->kateView;
            if (currView) {
              currNumLines=currView->getDoc()->numLines();
            } else {
              currBuffer=QStringList::split('\n',
                static_cast<ListViewFile *>(findCurrentDocument)->textBuffer,TRUE);
              currNumLines=currBuffer.count();
            }
            findCurrentLine=findBackwards?currNumLines-1:0;
        }
      } else if (findBackwards) findCurrentLine--; else findCurrentLine++;
      if (currView)
        kfind->setData(currView->getDoc()->textLine(findCurrentLine));
      else
        kfind->setData(currBuffer[findCurrentLine]);
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

#define unused_text text __attribute__((unused))
void MainForm::findFind_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  if (currentListItem!=findCurrentDocument) fileTreeClicked(findCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  CURRENT_VIEW->setCursorPositionReal(findCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(findCurrentLine,matchingindex,
                                       findCurrentLine,matchingindex+matchedlength);
}

void MainForm::findFind_stop()
{
  if (kfinddialog) kfinddialog->deleteLater();
  kfinddialog=static_cast<KFindDialog *>(NULL);
}

void MainForm::findReplace()
{
  if (kreplace) {
    KDialogBase *replaceNextDialog=kreplace->replaceNextDialog();
    if (replaceNextDialog)
      KWin::activateWindow(replaceNextDialog->winId());
    return;
  }
  KReplaceDialog kreplacedialog(this,0,((CURRENT_VIEW&&CURRENT_VIEW->getDoc()->hasSelection()
                                        &&CURRENT_VIEW->getDoc()->selStartLine()!=CURRENT_VIEW->getDoc()->selEndLine())?
                                        KFindDialog::SelectedText:0)|KFindDialog::FromCursor,
                                       QStringList(),QStringList(),
                                       CURRENT_VIEW&&CURRENT_VIEW->getDoc()->hasSelection());
  if (kreplacedialog.exec()!=QDialog::Accepted)
    return;
  kreplace=new KReplaceWithSelection(kreplacedialog.pattern(),kreplacedialog.replacement(),
                                     kreplacedialog.options(),this);
  // Connect signals to code which handles highlighting of found text, and
  // on-the-fly replacement.
  connect(kreplace,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findReplace_highlight(const QString &,int,int)));
  // Connect findNext signal - called when pressing the button in the dialog.
  connect(kreplace,SIGNAL(findNext()),this,SLOT(findReplace_next()));
  // Connect replace signal - called when doing a replacement.
  connect(kreplace,SIGNAL(replace(const QString &,int,int,int)),
          this,SLOT(findReplace_replace(const QString &,int,int,int)));
  // Connect dialogClosed signal - called when closing the Replace Next dialog.
  connect(kreplace,SIGNAL(dialogClosed()),this,SLOT(findReplace_stop()));
  // Initialize.
  bool findBackwards=!!(kreplace->options()&KFindDialog::FindBackwards);
  int replaceCurrentCol;
  // Make sure we have a valid currentListItem.
  if (!currentListItem) fileTreeClicked(rootListItem);
  replaceCurrentDocument=currentListItem;
  if (CURRENT_VIEW) {
    if (kreplace->options()&KFindDialog::SelectedText) {
      kreplace->setSelection(CURRENT_VIEW->getDoc()->selStartLine(),
                             CURRENT_VIEW->getDoc()->selStartCol(),
                             CURRENT_VIEW->getDoc()->selEndLine(),
                             CURRENT_VIEW->getDoc()->selEndCol());
      if (findBackwards) {
        replaceCurrentLine=kreplace->selEndLine();
        replaceCurrentCol=kreplace->selEndCol();
      } else {
        replaceCurrentLine=kreplace->selStartLine();
        replaceCurrentCol=kreplace->selStartCol();
      }
      kreplace->setOptions(kreplace->options()&~KFindDialog::FromCursor);
    } else if (kreplace->options()&KFindDialog::FromCursor) {
      if (CURRENT_VIEW->getDoc()->hasSelection()) {
        if (findBackwards) {
          replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
          if (replaceCurrentCol==-1) {
            if (!replaceCurrentLine) goto skip_data;
            replaceCurrentLine--;
          }
        } else {
          replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
        }
      } else {
        replaceCurrentLine=CURRENT_VIEW->cursorLine();
        replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(replaceCurrentLine==(CURRENT_VIEW->getDoc()->numLines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->getDoc()->lineLength(replaceCurrentLine)))
                         :(!replaceCurrentLine&&!replaceCurrentCol))
          kreplace->setOptions(kreplace->options()&~KFindDialog::FromCursor);
      }
    } else {
      replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
      replaceCurrentCol=-1;
    }
    kreplace->setData(CURRENT_VIEW->getDoc()->textLine(replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:
    // Now find the next occurrence.
    findReplace_next(TRUE);
}

void MainForm::findReplace_next()
{
  findReplace_next(FALSE);
}


void MainForm::findReplace_next(bool firstTime)
{
  // Replace All only works on the current file. Also, if we have a selection,
  // we only want to work on the file containing the selection.
  bool global=(kreplace->options()&KReplaceDialog::PromptOnReplace)&&!kreplace->haveSelection();
  bool findBackwards=!!(kreplace->options()&KFindDialog::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Make sure we have a valid currentListItem.
    if (!currentListItem) fileTreeClicked(rootListItem);
    replaceCurrentDocument=currentListItem;
    if (CURRENT_VIEW) {
      // Non-first-time always continues from cursor.
      if (CURRENT_VIEW->getDoc()->hasSelection()) {
        if (findBackwards) {
          replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
          if (replaceCurrentCol==-1) {
            if (!replaceCurrentLine) goto skip_data;
            replaceCurrentLine--;
          }
        } else {
          replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
        }
      } else {
        replaceCurrentLine=CURRENT_VIEW->cursorLine();
        replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
      }
      kreplace->setData(CURRENT_VIEW->getDoc()->textLine(replaceCurrentLine),replaceCurrentCol);
    } else replaceCurrentLine=0;
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  // We never have a currBuffer here, the current list item is always either
  // non-editable or instantiated.
  QStringList currBuffer;
  unsigned currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (kreplace->needData()) {
      if (global) {
        if (findBackwards?!replaceCurrentLine:(replaceCurrentLine>=currNumLines)) {
          if (findBackwards) {
            // Traverse the file tree in order backwards, restart from the end if
            // the first file was reached. Stop at currentListItem.
            if (replaceCurrentDocument) {
              QListViewItemIterator lvit(fileTree);
              QPtrList<QListViewItem> lst;
              QListViewItem *item;
              for (item=lvit.current();item&&item!=replaceCurrentDocument;
                   item=(++lvit).current()) {
                lst.prepend(item);
              }
              QPtrListIterator<QListViewItem> it(lst);
              for (replaceCurrentDocument=it.current();
                   replaceCurrentDocument&&replaceCurrentDocument!=currentListItem;
                   replaceCurrentDocument=++it) {
                if (IS_FILE(replaceCurrentDocument)) {
                  CATEGORY_OF(category,replaceCurrentDocument);
                  if (IS_EDITABLE_CATEGORY(category)) goto file_found;
                }
              }
              if (replaceCurrentDocument) goto not_found;
            }
            QListViewItemIterator lvit(currentListItem);
            QPtrList<QListViewItem> lst;
            QListViewItem *item;
            for (item=(++lvit).current();item;item=(++lvit).current()) {
              lst.prepend(item);
            }
            QPtrListIterator<QListViewItem> it(lst);
            for (replaceCurrentDocument=it.current();replaceCurrentDocument;
                 replaceCurrentDocument=++it) {
              if (IS_FILE(replaceCurrentDocument)) {
                CATEGORY_OF(category,replaceCurrentDocument);
                if (IS_EDITABLE_CATEGORY(category)) goto file_found;
              }
            }
            goto not_found;
          } else {
            // Traverse the file tree in order, restart from the beginning if
            // the last file was reached. Stop at currentListItem.
            if (replaceCurrentDocument) {
              QListViewItemIterator it(replaceCurrentDocument);
              for (replaceCurrentDocument=(++it).current();
                   replaceCurrentDocument&&replaceCurrentDocument!=currentListItem;
                   replaceCurrentDocument=(++it).current()) {
                if (IS_FILE(replaceCurrentDocument)) {
                  CATEGORY_OF(category,replaceCurrentDocument);
                  if (IS_EDITABLE_CATEGORY(category)) goto file_found;
                }
              }
              if (replaceCurrentDocument) goto not_found;
            }
            {
              QListViewItemIterator it(fileTree);
              for (replaceCurrentDocument=it.current();
                   replaceCurrentDocument&&replaceCurrentDocument!=currentListItem;
                   replaceCurrentDocument=(++it).current()) {
                if (IS_FILE(replaceCurrentDocument)) {
                  CATEGORY_OF(category,replaceCurrentDocument);
                  if (IS_EDITABLE_CATEGORY(category)) goto file_found;
                }
              }
            }
            not_found:
              // No find in all files. Try currentListItem again because it hasn't
              // been fully searched yet.
              if (IS_FILE(currentListItem)) {
                // currentListItem is always either instantiated or not editable
                currView=static_cast<ListViewFile *>(currentListItem)->kateView;
                if (currView) {
                  currNumLines=currView->getDoc()->numLines();
                  replaceCurrentLine=findBackwards?currNumLines-1:0;
                  do {
                    if (kreplace->needData()) {
                      if (findBackwards?!replaceCurrentLine:(replaceCurrentLine>=currNumLines))
                        goto not_found_current;
                      if (findBackwards) replaceCurrentLine--; else replaceCurrentLine++;
                      kreplace->setData(currView->getDoc()->textLine(replaceCurrentLine));
                    }
                    result=kreplace->replace();
                  } while (result==KFind::NoMatch);
                  break;
                }
              }
              not_found_current:
                kreplace->displayFinalDialog();
                findReplace_stop();
                return;
            file_found:
              currView=static_cast<ListViewFile *>(replaceCurrentDocument)->kateView;
              if (currView) {
                currNumLines=currView->getDoc()->numLines();
              } else {
                currBuffer=QStringList::split('\n',
                  static_cast<ListViewFile *>(replaceCurrentDocument)->textBuffer,TRUE);
                currNumLines=currBuffer.count();
              }
              replaceCurrentLine=findBackwards?currNumLines-1:0;
          }
        } else if (findBackwards) replaceCurrentLine--; else replaceCurrentLine++;
        if (currView)
          kreplace->setData(currView->getDoc()->textLine(replaceCurrentLine));
        else
          kreplace->setData(currBuffer[replaceCurrentLine]);
      } else { // if not global
        if (kreplace->haveSelection()
            ?(findBackwards?(replaceCurrentLine<=kreplace->selStartLine())
                           :(replaceCurrentLine>=kreplace->selEndLine()))
            :(findBackwards?!replaceCurrentLine:(replaceCurrentLine>=currNumLines))) {
          // It makes no sense to prompt for restarting if we aren't actually
          // searching anywhere.
          if (CURRENT_VIEW) {
            if (kreplace->shouldRestart()) {
              // Drop "From cursor" and "Selected text" options.
              kreplace->setOptions(kreplace->options()&~(KFindDialog::FromCursor
                                                         |KFindDialog::SelectedText));
              kreplace->invalidateSelection();
              // Reinitialize.
              replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
              kreplace->setData(CURRENT_VIEW->getDoc()->textLine(replaceCurrentLine));
              // Start again as if it was the first time.
              findReplace_next(TRUE);
              return;
            } else {
              findReplace_stop();
              return;
            }
          } else goto not_found_current;
        } else if (findBackwards) replaceCurrentLine--; else replaceCurrentLine++;
        if (currView)
          kreplace->setData(currView->getDoc()->textLine(replaceCurrentLine));
        else
          kreplace->setData(currBuffer[replaceCurrentLine]);
      }
    }
    result=kreplace->replace();
  } while (result==KFind::NoMatch);
}

void MainForm::findReplace_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  if (currentListItem!=replaceCurrentDocument) fileTreeClicked(replaceCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  CURRENT_VIEW->setCursorPositionReal(replaceCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(replaceCurrentLine,matchingindex,
                                       replaceCurrentLine,matchingindex+matchedlength);
}

void MainForm::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
  if (currentListItem!=replaceCurrentDocument) fileTreeClicked(replaceCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  bool update=!!(kreplace->options()&KReplaceDialog::PromptOnReplace);
  bool haveSelection=kreplace->haveSelection();
  // The initializations are redundant, but g++ doesn't understand this, and the
  // self-initialization trick doesn't work either (-Wno-init-self is ignored).
  unsigned selStartLine=0, selStartCol=0, selEndLine=0, selEndCol=0;
  if (haveSelection) {
    selStartLine=kreplace->selStartLine();
    selStartCol=kreplace->selStartCol();
    selEndLine=kreplace->selEndLine();
    selEndCol=kreplace->selEndCol();
  }
  KTextEditor::EditInterfaceExt *editinterfaceext=KTextEditor::editInterfaceExt(CURRENT_VIEW->getDoc());
  editinterfaceext->editBegin();
  CURRENT_VIEW->getDoc()->insertText(replaceCurrentLine,replacementIndex,
                                     text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  bool updateCursor=(CURRENT_VIEW->cursorLine()==replaceCurrentLine
                     && CURRENT_VIEW->cursorColumnReal()==(unsigned)replacementIndex+(unsigned)replacedLength);
  CURRENT_VIEW->getDoc()->removeText(replaceCurrentLine,replacementIndex+replacedLength,
                                     replaceCurrentLine,replacementIndex+replacedLength+matchedLength);
  editinterfaceext->editEnd();
  if (updateCursor)
    CURRENT_VIEW->setCursorPositionReal(replaceCurrentLine,replacementIndex);
  if (update) {
    CURRENT_VIEW->setCursorPositionReal(replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->getDoc()->setSelection(replaceCurrentLine,replacementIndex,
                                         replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->repaint();
  }
  if (haveSelection) {
    // Restore selection, updating coordinates if necessary.
    kreplace->setSelection(selStartLine,selStartCol,selEndLine,
                           (replaceCurrentLine==selEndLine)
                           ?(selEndCol+replacedLength-matchedLength)
                           :selEndCol);
  }
}

void MainForm::findReplace_stop()
{
  if (kreplace) kreplace->deleteLater();
  kreplace=static_cast<KReplaceWithSelection *>(NULL);
}

static SourceFileFunctions sourceFileFunctions;
static FunctionDialog *functionDialog;

void MainForm::findFunctions()
{
  if (CURRENT_VIEW) {
    functionDialog=new FunctionDialog(this);
    connect(functionDialog->functionListBox,SIGNAL(highlighted(int)),
            this,SLOT(findFunctions_functionListBox_highlighted(int)));
    connect(functionDialog->functionListBox,SIGNAL(selected(int)),
            this,SLOT(findFunctions_functionListBox_selected(int)));
    connect(functionDialog->prototypeButton,SIGNAL(clicked()),
            this,SLOT(findFunctions_prototypeButton_clicked()));
    connect(functionDialog->implementationButton,SIGNAL(clicked()),
            this,SLOT(findFunctions_implementationButton_clicked()));
    functionDialog->functionListBox->clear();
    CATEGORY_OF(category,currentListItem);
    sourceFileFunctions=getFunctions(CURRENT_VIEW->getDoc()->text(),
      category==asmFilesListItem||category==sFilesListItem);
    for (SourceFileFunctions::Iterator it=sourceFileFunctions.begin();
         it!=sourceFileFunctions.end(); ++it)
      functionDialog->functionListBox->insertItem((*it).name);
    functionDialog->exec();
    delete functionDialog;
  }
}

void MainForm::findFunctions_functionListBox_highlighted(int index)
{
  if (index>=0) {
    functionDialog->prototypeButton->setEnabled(
      sourceFileFunctions[index].prototypeLine>=0);
    functionDialog->implementationButton->setEnabled(
      sourceFileFunctions[index].implementationLine>=0);
  } else {
    functionDialog->prototypeButton->setEnabled(FALSE);
    functionDialog->implementationButton->setEnabled(FALSE);
  }
}

void MainForm::findFunctions_functionListBox_selected(int index)
{
  if (index>=0) {
    int line=sourceFileFunctions[index].implementationLine>=0
             ?sourceFileFunctions[index].implementationLine
             :sourceFileFunctions[index].prototypeLine;
    CURRENT_VIEW->setCursorPositionReal(line,0);
    functionDialog->accept();
  }
}

void MainForm::findFunctions_prototypeButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].prototypeLine>=0) {
    CURRENT_VIEW->setCursorPositionReal(
      sourceFileFunctions[index].prototypeLine,0);
    functionDialog->accept();
  }
}

void MainForm::findFunctions_implementationButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].implementationLine>=0) {
    CURRENT_VIEW->setCursorPositionReal(
      sourceFileFunctions[index].implementationLine,0);
    functionDialog->accept();
  }
}

void MainForm::findFunctionsPopup_aboutToShow()
{
  findFunctionsPopup->clear();
  if (CURRENT_VIEW) {
    CATEGORY_OF(category,currentListItem);
    sourceFileFunctions=getFunctions(CURRENT_VIEW->getDoc()->text(),
      category==asmFilesListItem||category==sFilesListItem);
    int idx=0;
    for (SourceFileFunctions::Iterator it=sourceFileFunctions.begin();
         it!=sourceFileFunctions.end(); ++it,++idx)
      findFunctionsPopup->insertItem((*it).name,idx);
  }
}

void MainForm::findFunctionsPopup_aboutToHide()
{
  QTimer::singleShot(0,this,SLOT(findFunctionsPopup_aboutToHide_async()));
}

void MainForm::findFunctionsPopup_aboutToHide_async()
{
  findFunctionsPopup->clear();
}

void MainForm::findFunctionsPopup_activated(int id)
{
  if (CURRENT_VIEW) {
    int line=sourceFileFunctions[id].implementationLine>=0
             ?sourceFileFunctions[id].implementationLine
             :sourceFileFunctions[id].prototypeLine;
    CURRENT_VIEW->setCursorPositionReal(line,0);
  }
}

void MainForm::findAndOpenFile(const QString &fileName, void *category)
{
  // Look for the source file with the given name.
  QString fileNameNoPath=QFileInfo(fileName).fileName();
  bool inProject;
  void *sourceFile;
  if (findSourceFile(inProject,sourceFile,fileNameNoPath)) {
    if (inProject)
      fileTreeClicked(reinterpret_cast<ListViewFile *>(sourceFile));
    else
      KWin::activateWindow(reinterpret_cast<SourceFile *>(sourceFile)->winId());
  } else {
    // Not found. Try to open it instead.
    // Don't do this if the name ends with ".tpr" because that would cause
    // openProject to close the current project and load the new one instead.
    if (!fileName.endsWith(".tpr",FALSE)) {
      QString fileNameFull=QFileInfo(projectFileName).dir().filePath(fileName);
      if (getPathType(fileNameFull)==PATH_FILE) {
        openProject(fileNameFull);
        if (findSourceFile(inProject,sourceFile,fileNameFull) && !inProject)
           KWin::activateWindow(reinterpret_cast<SourceFile *>(sourceFile)->winId());
      } else {
        QListViewItem *cat=reinterpret_cast<QListViewItem *>(category);
        QString includeDir=(cat==asmFilesListItem)?"asm":
                           (cat==sFilesListItem)?"s":"c";
        fileNameFull=QDir(QString("%1/include/%2/").arg(tigcc_base)
                          .arg(includeDir)).filePath(fileName);
        if (getPathType(fileNameFull)==PATH_FILE) {
          openProject(fileNameFull);
          if (findSourceFile(inProject,sourceFile,fileNameFull) && !inProject)
             KWin::activateWindow(reinterpret_cast<SourceFile *>(sourceFile)->winId());
        } else {
          KMessageBox::error(this,QString("File \'%1\' not found.").arg(fileName),
                             "Search Failed");
        }
      }
    }
  }
}

void MainForm::findOpenFileAtCursor()
{
  if (CURRENT_VIEW && IS_FILE(currentListItem)) {
    unsigned line,col,i;
    CURRENT_VIEW->cursorPositionReal(&line,&col);
    QString textLine=CURRENT_VIEW->getDoc()->textLine(line);
    unsigned l=textLine.length();
    bool quotesInLine=textLine.contains("\"");
    QString fileName;
    for (i=col;i<l;i--) {
      QChar c=textLine[i];
      if (!((quotesInLine && c==' ') || (c>='A' && c<="Z") || (c>='a' && c<='z')
            || (c>='0' && c<='9') || QString("_-./\\:").contains(c)))
        break;
      fileName.prepend(c);
    }
    for (i=col+1;i<l;i++) {
      QChar c=textLine[i];
      if (!((quotesInLine && c==' ') || (c>='A' && c<="Z") || (c>='a' && c<='z')
            || (c>='0' && c<='9') || QString("_-./\\:").contains(c)))
        break;
      fileName.append(c);
    }
    CATEGORY_OF(category,currentListItem);
    findAndOpenFile(fileName,category);
  }
}

void MainForm::findFindSymbolDeclaration()
{

}

//returns 1 on success
int MainForm::projectAddFiles_oneFile(const QString &fileName)
{
  QListViewItem *category=othFilesListItem;
  QString suffix,caption;
  int p;
  
  p=fileName.findRev('/');
  if (p<0) p=-1;
  caption=fileName.mid(p+1);
  p=caption.findRev('.');
  if (p>=0) {
    suffix=caption.mid(p+1);
    caption.truncate(p);
  }
  
  if (!checkFileName(fileName,extractAllFileNames())) {
    KMessageBox::error(this,QString("The file \'%1\' is already included in the project.").arg(caption));
    return 0;
  }

  if (!suffix.compare("h"))
    category=hFilesListItem;
  else if (!suffix.compare("c"))
    category=cFilesListItem;
  else if (!suffix.compare("s"))
    category=sFilesListItem;
  else if (!suffix.compare("asm"))
    category=asmFilesListItem;
  else if (!suffix.compare("qll"))
    category=qllFilesListItem;
  else if (!suffix.compare("o"))
    category=oFilesListItem;
  else if (!suffix.compare("a"))
    category=aFilesListItem;
  else if (!suffix.compare("txt"))
    category=txtFilesListItem;
  
  if (category && category==qllFilesListItem && qllFileCount) {
    KMessageBox::error(this,"There may be only one Quill source file in each project.","Quill Error");
    return 0;
  }

  if (!category)
    category=othFilesListItem;
  
  if (openFile(category,category,caption,fileName))
    return 1;
  return 0;
}

void MainForm::projectAddFiles()
{
  unsigned long i,e;
  int projectChanged=0;
  if (compiling) return;
  QStringList result=SGetFileName_Multiple(findFilter(TIGCCAddFilesFilter),"Add Files",this);
  e=result.count();
  for (i=0;i<e;i++) {
    if (projectAddFiles_oneFile(result[i]))
      projectChanged=TRUE;
  }
  if (projectChanged) {
    projectIsDirty=TRUE;
    projectNeedsRelink=TRUE;
  }
}

static bool stopCompilingFlag, errorsCompilingFlag;
static bool ti89_targeted, ti92p_targeted, v200_targeted, dataFileGenerated;
static QString compileStats;
static QDateTime newestHeaderTimestamp;
static QStringList objectFiles;
static QStringList deletableObjectFiles;
static QStringList deletableAsmFiles;
static KProcIO *procio;

QString MainForm::writeTempSourceFile(void *srcFile, bool inProject)
{
  const QString *origFileName;
  QListViewItem *category;
  QString fileName;
  QString fileText;
  LineStartList *pLineStartList=0;
  if (inProject) {
    ListViewFile *sourceFile=reinterpret_cast<ListViewFile *>(srcFile);
    origFileName=&(sourceFile->fileName);
    CATEGORY_OF(cat,sourceFile);
    category=cat;
    QString folder;
    QListViewItem *item=sourceFile->parent();
    while (!IS_CATEGORY(item)) {
      folder.prepend('/');
      folder.prepend(item->text(0));
      item=item->parent();
    }
    QString ext;
    // Some categories need fixed extensions.
    if (category==cFilesListItem) {
      ext=".c";
    } else if (category==sFilesListItem) {
      ext=".s";
    } else if (category==asmFilesListItem) {
      ext=".asm";
    } else if (category==qllFilesListItem) {
      ext=".qll";
    } else if (category==oFilesListItem) {
      ext=".o";
    } else if (category==aFilesListItem) {
      ext=".a";
    } else {
      // For others (header files in particular), we need to keep whatever
      // extension the user supplied.
      int dotPos=origFileName->findRev('.');
      int slashPos=origFileName->findRev('/');
      if (dotPos>slashPos) ext=origFileName->mid(dotPos);
    }
    fileName=QString("%1/%2%3%4").arg(tempdir).arg(folder)
                                 .arg(sourceFile->text(0)).arg(ext);
    if (IS_EDITABLE_CATEGORY(category)) {
      if (sourceFile->kateView) {
        fileText=sourceFile->kateView->getDoc()->text();
      } else {
        fileText=sourceFile->textBuffer;
      }
      pLineStartList=&(sourceFile->lineStartList);
    } else {
      if (copyFile(origFileName->ascii(),fileName.ascii())) {
        KMessageBox::error(this,"Failed to copy file to temporary directory.");
        stopCompilingFlag=TRUE;
      }
      return fileName;
    }
  } else {
    SourceFile *sourceFile=reinterpret_cast<SourceFile *>(srcFile);
    origFileName=&(sourceFile->fileName);
    category=reinterpret_cast<QListViewItem *>(sourceFile->category);
    fileName=QString("%1%2").arg(tempdir)
                            .arg(origFileName->mid(origFileName->findRev('/')));
    fileText=sourceFile->kateView->getDoc()->text();
    pLineStartList=&(sourceFile->lineStartList);
  }
  if (saveAndSplitFileText(fileName.ascii(),fileText,(category==cFilesListItem),
                           (category==cFilesListItem||category==qllFilesListItem),
                           (category==sFilesListItem),*origFileName,pLineStartList)) {
    KMessageBox::error(this,"Failed to write source file to temporary directory.");
    stopCompilingFlag=TRUE;
  }
  return fileName;
}

void MainForm::startCompiling()
{
  if (preferences.autoSave) {
    if (!fileSave()) {
      stopCompilingFlag=TRUE;
      return;
    }
  } else {
    QPtrListIterator<SourceFile> sfit(sourceFiles);
    for (SourceFile *sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
      sourceFile->fileSave();
    }
  }
  fileNewActionGroup->setEnabled(FALSE);
  fileOpenAction->setEnabled(FALSE);
  fileRecent1Action->setEnabled(FALSE);
  fileRecent2Action->setEnabled(FALSE);
  fileRecent3Action->setEnabled(FALSE);
  fileRecent4Action->setEnabled(FALSE);
  fileOpenActionGroup->setEnabled(FALSE);
  fileExitAction->setEnabled(FALSE);
  projectAddFilesAction->setEnabled(FALSE);
  projectCompileAction->setVisible(FALSE);
  projectMakeAction->setVisible(FALSE);
  projectBuildAction->setVisible(FALSE);
  projectCompileAction->setEnabled(FALSE);
  projectMakeAction->setEnabled(FALSE);
  projectBuildAction->setEnabled(FALSE);
  projectStopCompilationAction->setEnabled(TRUE);
  projectForceQuitAction->setEnabled(TRUE);
  projectStopCompilationAction->setVisible(TRUE);
  projectForceQuitAction->setVisible(TRUE);
  debugRunAction->setEnabled(FALSE);
  debugPauseAction->setEnabled(FALSE);
  debugResetAction->setEnabled(FALSE);
  QPtrListIterator<SourceFile> sfit(sourceFiles);
  for (SourceFile *sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
    sourceFile->fileAddToProjectAction->setEnabled(FALSE);
    sourceFile->fileCompileAction->setEnabled(FALSE);
  }
  compiling=TRUE;
  stopCompilingFlag=FALSE;
  errorsCompilingFlag=FALSE;
  newestHeaderTimestamp=QDateTime();
  objectFiles.clear();
  deletableObjectFiles.clear();
  deletableAsmFiles.clear();
  errorList->errorListView->clear();
  programOutput=QString::null;
  ti89_targeted=ti92p_targeted=v200_targeted=dataFileGenerated=FALSE;
  compileStats=QString::null;
  projectProgramOutputAction->setEnabled(FALSE);
  procio=static_cast<KProcIO *>(NULL);
  // Write all the headers and incbin files to the temporary directory.
  QListViewItemIterator lvit(hFilesListItem);
  QListViewItem *item;
  for (item=(++lvit).current();item&&!IS_CATEGORY(item);
       item=(++lvit).current()) {
    if (IS_FILE(item)) {
      if (static_cast<ListViewFile *>(item)->modifiedSinceLastCompile)
        headersModified=TRUE;
      if (!headersModified) {
        QDateTime headerTimestamp=QFileInfo(static_cast<ListViewFile *>(item)->fileName).lastModified();
        if (!newestHeaderTimestamp.isValid()
            || (headerTimestamp.isValid()
                && headerTimestamp>newestHeaderTimestamp))
          newestHeaderTimestamp=headerTimestamp;
      }
      writeTempSourceFile(static_cast<ListViewFile *>(item),TRUE);
      if (stopCompilingFlag) return;
    }
  }
  lvit=QListViewItemIterator(othFilesListItem);
  for (item=(++lvit).current();item&&!IS_CATEGORY(item);
       item=(++lvit).current()) {
    if (IS_FILE(item)) {
      if (!headersModified) {
        QDateTime headerTimestamp=QFileInfo(static_cast<ListViewFile *>(item)->fileName).lastModified();
        if (!newestHeaderTimestamp.isValid()
            || (headerTimestamp.isValid()
                && headerTimestamp>newestHeaderTimestamp))
          newestHeaderTimestamp=headerTimestamp;
      }
      writeTempSourceFile(static_cast<ListViewFile *>(item),TRUE);
      if (stopCompilingFlag) return;
    }
  }
}

void MainForm::stopCompiling()
{
  clear_temp_dir();
  stopCompilingFlag=FALSE;
  errorsCompilingFlag=FALSE;
  compiling=FALSE;
  QPtrListIterator<SourceFile> sfit(sourceFiles);
  for (SourceFile *sourceFile=sfit.current();sourceFile;sourceFile=++sfit) {
    sourceFile->fileAddToProjectAction->setEnabled(TRUE);
    sourceFile->fileCompileAction->setEnabled(TRUE);
    sourceFile->fileCloseAction->setEnabled(TRUE);
  }
  debugRunAction->setEnabled(TRUE);
  debugPauseAction->setEnabled(preferences.linkTarget==LT_TIEMU);
  debugResetAction->setEnabled(preferences.linkTarget==LT_TIEMU);
  projectStopCompilationAction->setVisible(FALSE);
  projectForceQuitAction->setVisible(FALSE);
  projectStopCompilationAction->setEnabled(FALSE);
  projectForceQuitAction->setEnabled(FALSE);
  projectCompileAction->setEnabled(TRUE);
  projectMakeAction->setEnabled(TRUE);
  projectBuildAction->setEnabled(TRUE);
  projectCompileAction->setVisible(TRUE);
  projectMakeAction->setVisible(TRUE);
  projectBuildAction->setVisible(TRUE);
  projectAddFilesAction->setEnabled(TRUE);
  fileExitAction->setEnabled(TRUE);
  fileOpenActionGroup->setEnabled(TRUE);
  fileRecent1Action->setEnabled(TRUE);
  fileRecent2Action->setEnabled(TRUE);
  fileRecent3Action->setEnabled(TRUE);
  fileRecent4Action->setEnabled(TRUE);
  fileOpenAction->setEnabled(TRUE);
  fileNewActionGroup->setEnabled(TRUE);
  statusBar()->clear();
}

static QString errorFunction;
static bool errorFlag;
static unsigned ldTigccStatPhase=0;

void MainForm::procio_processExited()
{
  // If we're in a modal dialog, let it complete or exiting the event loop will
  // crash.
  if (QApplication::eventLoop()->loopLevel()>2) {
    QTimer::singleShot(100,this,SLOT(procio_processExited()));
    return;
  }
  errorFunction=QString::null;
  ldTigccStatPhase=0;
  QApplication::eventLoop()->exitLoop();
}


void MainForm::procio_readReady()
{
  static unsigned a68kErrorLine=0; // A68k errors are split onto several lines.
  static int errorLine, errorColumn;
  static QString errorFile;
  QString line;
  while (procio->readln(line)>=0) {
    // ld-tigcc doesn't currently know the difference between host charset and
    // calculator charset, so the variable name won't display properly if it
    // contains non-ASCII characters. Fix that up.
    if (ldTigccStatPhase==1
        && line.stripWhiteSpace().startsWith("Program Variable Name:",FALSE)) {
      int pos=line.find(':');
      while (line[++pos]==' ');
      line.truncate(pos);
      if (!rootListItem->text(0).contains('\\')) line.append("main\\");
      line.append(rootListItem->text(0));
    }
    programOutput.append(line);
    programOutput.append('\n');
    projectProgramOutputAction->setEnabled(TRUE);
    if (line.isEmpty()) continue;
    // Parse the error message
    switch (ldTigccStatPhase) {
      default: // should not happen
        ldTigccStatPhase=0;
      case 0:
        switch (a68kErrorLine) {
          default: // should not happen
            a68kErrorLine=0;
          case 0:
            errorLine=-1;
            errorColumn=-1;
            errorFile=QString::null;
          case 1:
            if (line.contains(QRegExp(" line [0-9]+$",FALSE))) {
              if (line.contains("(user macro)",FALSE))
                errorColumn=0;
              else if (errorFile.isEmpty()) {
                errorFile=QFileInfo(line.left(line.findRev(" line ",-1,FALSE)))
                          .fileName();
                errorLine=line.mid(line.findRev(' ')+1).toInt()-1;
              }
              errorFunction=QString::null;
              a68kErrorLine=1;
            } else if (a68kErrorLine==1) {
              // This line contains only a copy of the source line, skip it.
              a68kErrorLine=2;
            } else {
              if (line.contains("Fatal errors - assembly aborted",FALSE)) break;
              // Not an A68k error, so parse it as a standard *nix-style error.
              // Normalize characters and strip whitespace
              line=line.stripWhiteSpace();
              line.replace('`','\''); // backtick
              line.replace(QChar(0xb4),'\''); // forward apostrophe
              line.replace('\"','\''); // quote
              if (line.contains("assembler messages:",FALSE)
                  || line.startsWith("from ",FALSE)
                  || (!preferences.allowImplicitDeclaration
                      && (line.contains("previous implicit declaration",FALSE)
                          || line.contains("previously implicitly declared",FALSE))))
                break;
              // ld-tigcc statistics start here, don't display them as errors.
              if (line.lower()=="target calculators:") {
                ldTigccStatPhase=1;
                break;
              }
              if (line.startsWith("in file",FALSE))
                errorFunction=QString::null;
              else {
                // The regex also supports Windows file names, in case KTIGCC ever
                // encounters them. Hopefully nobody will try giving it MacOS <=9
                // file names.
                int pos=line.find(QRegExp(":(?![/\\\\])"));
                if (pos>=0) errorFile=line.left(pos);
                if (errorFile.isEmpty()) {
                  if (!line.endsWith("."))
                    line.append('.');
                  errorFlag=TRUE;
                  new ErrorListItem(this,line.startsWith("please fill out ",FALSE)?
                                    etInfo:etError,QString::null,QString::null,line,
                                    errorLine,errorColumn);
                } else {
                  QString errorMessage;
                  if (errorFile.lower()=="error"||errorFile.lower()=="warning") {
                    errorFile=QString::null;
                    errorMessage=line;
                  } else {
                    errorFile=QFileInfo(errorFile).fileName();
                    errorMessage=line.mid(pos+1);
                  }
                  if (!errorMessage.isEmpty() && errorMessage[0]>='0'
                                              && errorMessage[0]<='9') {
                    pos=errorMessage.find(':');
                    if (pos>=0) {
                      bool ok;
                      errorLine=errorMessage.left(pos).toInt(&ok)-1;
                      if (ok) {
                        errorMessage.remove(0,pos+1);
                        if (!errorMessage.isEmpty() && errorMessage[0]>='0'
                                                    && errorMessage[0]<='9') {
                          pos=errorMessage.find(':');
                          if (pos>=0) {
                            errorColumn=errorMessage.left(pos).toInt(&ok)-1;
                            if (ok) errorMessage.remove(0,pos+1);
                          }
                        }
                      }
                    }
                    errorMessage=errorMessage.stripWhiteSpace();
                    ErrorTypes errorType=etError;
                    if (errorMessage.startsWith("warning:",FALSE)) {
                      errorMessage.remove(0,8);
                      errorMessage=errorMessage.stripWhiteSpace();
                      errorType=etWarning;
                    } else if (errorMessage.startsWith("error:",FALSE)) {
                      errorMessage.remove(0,6);
                      errorMessage=errorMessage.stripWhiteSpace();
                    }
                    if (errorMessage.startsWith("#warning ",FALSE)) {
                      errorMessage.remove(0,9);
                      errorType=etWarning;
                    } else if (errorMessage.startsWith("#error ",FALSE))
                      errorMessage.remove(0,7);
                    if (errorMessage.startsWith("previous declaration of ",FALSE)
                        || errorMessage.startsWith("possible real start of ",FALSE)
                        || errorMessage.startsWith("unused variable ",FALSE)
                        || errorMessage.startsWith("unused parameter ",FALSE)
                        || errorMessage.contains("previously declared here",FALSE)
                        || errorMessage.contains("location of the previous definition",FALSE))
                      errorType=etInfo;
                    if (!preferences.allowImplicitDeclaration)
                      errorMessage.replace(QRegExp("^implicit declaration of ",FALSE),
                                           "Undefined reference to ");
                    if (!errorMessage.endsWith("."))
                      errorMessage.append('.');
                    if (errorType==etError) errorFlag=TRUE;
                    new ErrorListItem(this,errorType,errorFile,errorFunction,
                                      errorMessage,errorLine,errorColumn);
                  } else {
                    if (errorMessage.startsWith(" in function \'",FALSE)
                        && errorMessage.contains('\'')>1) {
                      errorFunction=errorMessage.mid(14,errorMessage.find('\'',14)-14);
                    } else if (errorMessage.startsWith(" at top level",FALSE)) {
                      errorFunction=QString::null;
                    } else {
                      ErrorTypes errorType=etError;
                      if (errorMessage.startsWith("warning:",FALSE)) {
                        errorMessage.remove(0,8);
                        errorMessage=errorMessage.stripWhiteSpace();
                        errorType=etWarning;
                      } else if (errorMessage.startsWith("error:",FALSE)) {
                        errorMessage.remove(0,6);
                        errorMessage=errorMessage.stripWhiteSpace();
                      }
                      if (!errorMessage.endsWith("."))
                        errorMessage.append('.');
                      if (errorType==etError) errorFlag=TRUE;
                      new ErrorListItem(this,errorType,errorFile,QString::null,
                                        errorMessage,errorLine,errorColumn);
                    }
                  }
                }
              }
            }
            break;
          case 2:
            if (line.contains(QRegExp("^\\s*\\^"))) {
              int caretPos=line.find('^');
              if (errorLine>=0 && errorColumn<0)
                errorColumn=caretPos-1; // there's an extra tab at the beginning
              QString errorMessage=line.mid(caretPos+2);
              if (!errorMessage.endsWith("."))
                errorMessage.append('.');
              errorFlag=TRUE;
              new ErrorListItem(this,etError,errorFile,QString::null,errorMessage,
                                errorLine,errorColumn);
            }
            a68kErrorLine=0;
            break;
        }
        break;
      case 1:
        line=line.stripWhiteSpace();
        // Collect targets (the data file renaming needs to know them).
        if (line=="TI-89") ti89_targeted=TRUE;
        else if (line=="TI-92 Plus") ti92p_targeted=TRUE;
        else if (line=="V200") v200_targeted=TRUE;
        // The next line is the first one to display in the dialog. (TIGCC IDE
        // does the same.)
        else if (line.startsWith("Program Variable Name:",FALSE))
          ldTigccStatPhase=2;
        break;
      case 2:
        compileStats.append(line.simplifyWhiteSpace());
        compileStats.append('\n');
        if (line.stripWhiteSpace().startsWith("Data Variable Size:",FALSE))
          dataFileGenerated=TRUE;
        break;
    }
  }
  if (errorFlag) {
    errorsCompilingFlag=TRUE;
    if (preferences.stopAtFirstError) stopCompilingFlag=TRUE;
  }
}

void MainForm::procio_readReady_recordOnly()
{
  QString line;
  while (procio->readln(line)>=0) {
    programOutput.append(line);
    programOutput.append('\n');
    projectProgramOutputAction->setEnabled(TRUE);
  }
}

void MainForm::compileFile(void *srcFile, bool inProject, bool force)
{
  QListViewItem *category;
  const QString *origFileName;
  bool modified=FALSE;
  QString shortFileName;
  if (stopCompilingFlag) return;
  errorFlag=FALSE;
  if (inProject) {
    ListViewFile *sourceFile=reinterpret_cast<ListViewFile *>(srcFile);
    CATEGORY_OF(cat,sourceFile);
    category=cat;
    origFileName=&(sourceFile->fileName);
    if (sourceFile->modifiedSinceLastCompile)
      modified=TRUE;
    shortFileName=sourceFile->text(0);
  } else {
    SourceFile *sourceFile=reinterpret_cast<SourceFile *>(srcFile);
    category=reinterpret_cast<QListViewItem *>(sourceFile->category);
    origFileName=&(sourceFile->fileName);
    if (sourceFile->kateView->getDoc()->isModified())
      modified=TRUE;
    shortFileName=sourceFile->fileName;
  }
  if (category==cFilesListItem || category==sFilesListItem
      || category==asmFilesListItem || category==qllFilesListItem) {
    QString objectFile=*origFileName;
    int dotPos=origFileName->findRev('.');
    int slashPos=origFileName->findRev('/');
    if (dotPos>slashPos) objectFile.truncate(dotPos);
    // Compute correct file names for unsaved files.
    if (objectFile[0]!='/') {
      if (inProject) {
        ListViewFile *sourceFile=reinterpret_cast<ListViewFile *>(srcFile);
        QListViewItem *item=sourceFile->parent();
        while (!IS_CATEGORY(item)) {
          objectFile.prepend('/');
          objectFile.prepend(item->text(0));
          item=item->parent();
        }
      }
      if (!projectFileName.isEmpty()) {
        objectFile.prepend('/');
        objectFile.prepend(QFileInfo(projectFileName).dirPath(TRUE));
      }
    }
    QString asmFile=objectFile;
    objectFile.append(".o");
    deletableObjectFiles.append(objectFile);
    objectFiles.append(objectFile);
    asmFile.append(".s");
    mkdir_multi(objectFile);
    if (category==cFilesListItem || category==qllFilesListItem)
      deletableAsmFiles.append(asmFile);
    // Figure out whether to recompile the file.
    if (!force && !modified && !headersModified) {
      QFileInfo objectFileInfo(objectFile);
      if (objectFileInfo.exists()) {
        QDateTime timestamp=objectFileInfo.lastModified();
        if (timestamp.isValid() && (!newestHeaderTimestamp.isValid()
                                    || timestamp>=newestHeaderTimestamp)) {
          QDateTime sourceTimestamp=QFileInfo(*origFileName).lastModified();
          if (sourceTimestamp.isValid() && timestamp>=sourceTimestamp) {
            return;
          }
        }
      }
    }
    if (category==sFilesListItem||category==asmFilesListItem)
      statusBar()->message(QString("Assembling File \'%1\'...").arg(shortFileName));
    else
      statusBar()->message(QString("Compiling File \'%1\'...").arg(shortFileName));
    projectNeedsRelink=TRUE;
    QString fileName=writeTempSourceFile(srcFile,inProject);
    if (stopCompilingFlag) return;
    QString tempObjectFile=fileName;
    dotPos=fileName.findRev('.');
    slashPos=fileName.findRev('/');
    if (dotPos>slashPos) tempObjectFile.truncate(dotPos);
    QString tempAsmFile=tempObjectFile;
    tempObjectFile.append(".o");
    tempAsmFile.append(".s");
    QString fileDir=QFileInfo(fileName).dirPath(TRUE);
    QDir qdir;
    if (category==asmFilesListItem) {
      // Assemble A68k file
      int err;
      QStringList args=KShell::splitArgs(settings.a68k_switches,
                                         KShell::TildeExpand|KShell::AbortOnMeta,
                                         &err);
      if (err) {
        new ErrorListItem(this,etError,QString::null,QString::null,
                          "Invalid A68k assembler command line options.",-1,-1);
        stopCompilingFlag=TRUE;
      }
      if (!stopCompilingFlag) {
        // The QTextCodec has to be passed explicitly, or it will default to
        // ISO-8859-1 regardless of the locale, which is just broken.
        procio=new KProcIO(QTextCodec::codecForLocale());
        // Use MergedStderr instead of Stderr so the messages get ordered
        // properly.
        procio->setComm(static_cast<KProcess::Communication>(
          KProcess::Stdout|KProcess::MergedStderr));
        procio->setWorkingDirectory(fileDir);
        *procio<<(QString("%1/bin/a68k").arg(tigcc_base))
               <<fileName<<(QString("-o%1").arg(tempObjectFile))
               <<(QString("-i%1/include/asm/").arg(tigcc_base))<<"-q"<<args;
        if (settings.cut_ranges||settings.archive)
          *procio<<"-a"; // all relocs
        if (settings.optimize_returns||settings.archive)
          *procio<<"-d"; // keep locals
        connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
        connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
        procio->start();
        // We need to block here, but events still need to be handled. The most
        // effective way to do this is to enter the event loop recursively,
        // even though it is not recommended by Qt.
        QApplication::eventLoop()->enterLoop();
        // This will be reached only after exitLoop() is called.
        delete procio;
        procio=static_cast<KProcIO *>(NULL);
      }
    } else {
      bool deleteTempAsmFile=FALSE;
      QString fileNameToAssemble;
      if (category==sFilesListItem) {
        fileNameToAssemble=fileName;
      } else /* C or Quill */ {
        // Compile C/Quill file to assembly
        int err;
        QStringList args=KShell::splitArgs(settings.cc_switches,
                                           KShell::TildeExpand|KShell::AbortOnMeta,
                                           &err);
        if (err) {
          new ErrorListItem(this,etError,QString::null,QString::null,
                            "Invalid C compiler command line options.",-1,-1);
          stopCompilingFlag=TRUE;
        }
        if (!stopCompilingFlag) {
          // The QTextCodec has to be passed explicitly, or it will default to
          // ISO-8859-1 regardless of the locale, which is just broken.
          procio=new KProcIO(QTextCodec::codecForLocale());
          // Use MergedStderr instead of Stderr so the messages get ordered
          // properly.
          procio->setComm(static_cast<KProcess::Communication>(
            KProcess::Stdout|KProcess::MergedStderr));
          procio->setWorkingDirectory(fileDir);
          *procio<<(QString("%1/bin/gcc").arg(tigcc_base))
                 <<"-S"<<"-I"<<fileDir
                 <<"-B"<<(QString("%1/bin/").arg(tigcc_base))<<"-I-"
                 <<"-I"<<(QString("%1/include/c").arg(tigcc_base))<<args;
          if (category==qllFilesListItem) { // Quill needs special switches.
            *procio<<"-I"<<(QString("%1/include/quill").arg(tigcc_base))
                   <<"-include"<<quill_drv;
          }
          if (settings.use_data_var)
            *procio<<"-mno-merge-sections";
          if (!preferences.allowImplicitDeclaration)
            *procio<<"-Werror-implicit-function-declaration";
          if (settings.debug_info)
            *procio<<"-gdwarf-2"<<"-g3"<<"-fasynchronous-unwind-tables";
          if (settings.fargo)
            *procio<<"-DFARGO";
          else if (settings.flash_os)
            *procio<<"-DFLASH_OS";
          else if (!settings.archive) { // This leaves only regular programs.
            *procio<<process_libopts();
          }
          *procio<<fileName<<"-o"<<tempAsmFile;
          connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
          connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
          procio->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QApplication::eventLoop()->enterLoop();
          // This will be reached only after exitLoop() is called.
          delete procio;
          procio=static_cast<KProcIO *>(NULL);
        }
        if (!stopCompilingFlag && qdir.exists(tempAsmFile)) {
          // Run patcher.
          // The QTextCodec has to be passed explicitly, or it will default to
          // ISO-8859-1 regardless of the locale, which is just broken.
          procio=new KProcIO(QTextCodec::codecForLocale());
          // Use MergedStderr instead of Stderr so the messages get ordered
          // properly.
          procio->setComm(static_cast<KProcess::Communication>(
            KProcess::Stdout|KProcess::MergedStderr));
          procio->setWorkingDirectory(fileDir);
          *procio<<(QString("%1/bin/patcher").arg(tigcc_base))
                 <<tempAsmFile<<"-o"<<tempAsmFile;
          connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
          connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
          procio->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QApplication::eventLoop()->enterLoop();
          // This will be reached only after exitLoop() is called.
          delete procio;
          procio=static_cast<KProcIO *>(NULL);
        }
        if (qdir.exists(tempAsmFile)) {
          if (!stopCompilingFlag) {
            fileNameToAssemble=tempAsmFile;
            qdir.remove(asmFile);
            if (copyFile(tempAsmFile.ascii(),asmFile.ascii())) {
              new ErrorListItem(this,etError,QString::null,QString::null,
                                "Failed to copy assembly file from temporary directory.",
                                -1,-1);
              stopCompilingFlag=TRUE;
            }
          }
          deleteTempAsmFile=TRUE;
        } else {
          errorFlag=TRUE;
          errorsCompilingFlag=TRUE;
          if (preferences.stopAtFirstError) stopCompilingFlag=TRUE;
        }
      }
      // Assemble GNU as file
      if (!stopCompilingFlag && !fileNameToAssemble.isNull()) {
        int err;
        QStringList args=KShell::splitArgs(settings.as_switches,
                                           KShell::TildeExpand|KShell::AbortOnMeta,
                                           &err);
        if (err) {
          new ErrorListItem(this,etError,QString::null,QString::null,
                            "Invalid GNU assembler command line options.",-1,-1);
          stopCompilingFlag=TRUE;
        }
        if (!stopCompilingFlag) {
          // The QTextCodec has to be passed explicitly, or it will default to
          // ISO-8859-1 regardless of the locale, which is just broken.
          procio=new KProcIO(QTextCodec::codecForLocale());
          // Use MergedStderr instead of Stderr so the messages get ordered
          // properly.
          procio->setComm(static_cast<KProcess::Communication>(
            KProcess::Stdout|KProcess::MergedStderr));
          procio->setWorkingDirectory(fileDir);
          *procio<<(QString("%1/bin/as").arg(tigcc_base))
                 <<"-I"<<fileDir<<"-mc68000"
                 <<"-I"<<(QString("%1/include/s").arg(tigcc_base))<<args;
          if (settings.cut_ranges||settings.archive)
            *procio<<"--all-relocs";
          if (settings.optimize_returns||settings.archive)
            *procio<<"--keep-locals";
          if (settings.debug_info)
            *procio<<"--gdwarf2";
          *procio<<fileNameToAssemble<<"-o"<<tempObjectFile;
          connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
          connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
          procio->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QApplication::eventLoop()->enterLoop();
          // This will be reached only after exitLoop() is called.
          delete procio;
          procio=static_cast<KProcIO *>(NULL);
        }
      }
      if (deleteTempAsmFile) qdir.remove(tempAsmFile);
    }
    qdir.remove(fileName);
    if (qdir.exists(tempObjectFile)) {
      qdir.remove(objectFile);
      if (copyFile(tempObjectFile.ascii(),objectFile.ascii())) {
        new ErrorListItem(this,etError,QString::null,QString::null,
                          "Failed to copy object file from temporary directory.",
                          -1,-1);
        stopCompilingFlag=TRUE;
      }
      qdir.remove(tempObjectFile);
    } else {
      errorFlag=TRUE;
      errorsCompilingFlag=TRUE;
      if (preferences.stopAtFirstError) stopCompilingFlag=TRUE;
    }
  } else if (category==oFilesListItem || category==aFilesListItem) {
    objectFiles.append(*origFileName);
  } // else do nothing (ignore all other categories)
  if (inProject && !errorFlag && !stopCompilingFlag)
    reinterpret_cast<ListViewFile *>(srcFile)->modifiedSinceLastCompile=FALSE;
}

void MainForm::compileProject(bool forceAll)
{
  QListViewItemIterator lvit(fileTree);
  QListViewItem *item;
  for (item=lvit.current(); item&&!stopCompilingFlag; item=(++lvit).current()) {
    if (IS_FILE(item))
      compileFile(static_cast<ListViewFile *>(item),TRUE,forceAll);
  }
  if (!errorsCompilingFlag && !stopCompilingFlag) headersModified=FALSE;
}

void MainForm::linkProject()
{
  errorFlag=FALSE;
  QString projectBaseName=projectFileName;
  if (projectBaseName.endsWith(".tpr",FALSE))
    projectBaseName.truncate(projectBaseName.length()-4);
  QString projectDir=QFileInfo(projectFileName).dirPath(TRUE);
  statusBar()->message(QString("Linking Project \'%1\'...")
                       .arg(QFileInfo(projectFileName).baseName()));
  if (settings.archive) {
    // Link static library using ar-tigcc.
    // The QTextCodec has to be passed explicitly, or it will default to
    // ISO-8859-1 regardless of the locale, which is just broken.
    procio=new KProcIO(QTextCodec::codecForLocale());
    // Use MergedStderr instead of Stderr so the messages get ordered
    // properly.
    procio->setComm(static_cast<KProcess::Communication>(
      KProcess::Stdout|KProcess::MergedStderr));
    procio->setWorkingDirectory(projectDir);
    *procio<<(QString("%1/bin/ar-tigcc").arg(tigcc_base))
           <<"-o"<<(projectBaseName+".a")<<"--no-names"<<objectFiles;
    connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
    connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
    procio->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QApplication::eventLoop()->enterLoop();
    // This will be reached only after exitLoop() is called.
    delete procio;
    procio=static_cast<KProcIO *>(NULL);
    if (errorsCompilingFlag || stopCompilingFlag) return;
    QFileInfo fileInfo(projectBaseName+".a");
    if (fileInfo.exists())
      compileStats=QString("Archive Size: %1 Bytes\n").arg(fileInfo.size());
    else
      errorsCompilingFlag=TRUE;
  } else {
    // Link executable using ld-tigcc.
    QString linkOutput=settings.pack?QString("%1/tempprog").arg(tempdir)
                                    :projectBaseName;
    QCString projectName, dataVarName, packFolder, packName;
    QStringList linkerOptions=process_settings(rootListItem->text(0),
                                               projectName,dataVarName,
                                               packFolder,packName);
    // The QTextCodec has to be passed explicitly, or it will default to
    // ISO-8859-1 regardless of the locale, which is just broken.
    procio=new KProcIO(QTextCodec::codecForLocale());
    // Use MergedStderr instead of Stderr so the messages get ordered
    // properly.
    procio->setComm(static_cast<KProcess::Communication>(
      KProcess::Stdout|KProcess::MergedStderr));
    procio->setWorkingDirectory(projectDir);
    *procio<<(QString("%1/bin/ld-tigcc").arg(tigcc_base))
           <<"-v"<<"-o"<<linkOutput<<"-n"<<projectName;
    if (!dataVarName.isNull()) *procio<<"-d"<<dataVarName;
    *procio<<linkerOptions<<objectFiles;
    if (settings.std_lib)
      *procio<<QString(settings.flash_os?"%1/lib/flashos.a"
                                        :(settings.fargo?"%1/lib/fargo.a"
                                                        :"%1/lib/tigcc.a"))
               .arg(tigcc_base);
    connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
    connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
    procio->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QApplication::eventLoop()->enterLoop();
    // This will be reached only after exitLoop() is called.
    delete procio;
    procio=static_cast<KProcIO *>(NULL);
    if (errorsCompilingFlag || stopCompilingFlag) return;
    // Rename the data file so it doesn't conflict with PPGs.
    if (dataFileGenerated)  {
      QDir qdir;
      const int numTargets=3;
      bool targeted[numTargets]={ti89_targeted,ti92p_targeted,v200_targeted};
      char dextsbin[numTargets][5]={".y89",".y9x",".yv2"};
      char dextswrapped[numTargets][5]={".89y",".9xy",".v2y"};
      char (*dexts)[5]=(settings.outputbin&&!settings.pack)?dextsbin:dextswrapped;
      for (int target=0; target<numTargets; target++) {
        if (targeted[numTargets]) {
          qdir.remove(projectBaseName+"-data"+dexts[target]);
          if (!qdir.rename(linkOutput+dexts[target],
                           projectBaseName+"-data"+dexts[target])) {
            new ErrorListItem(this,etError,QString::null,QString::null,
                              "Failed to rename data file.",
                              -1,-1);
            errorsCompilingFlag=TRUE;
          }
          if (errorsCompilingFlag || stopCompilingFlag) return;
        }
      }
    }
    if (settings.pack) {
      // Copy over the .dbg file from the temporary directory if it exists.
      // Current TiEmu can't handle compressed programs, but maybe some day.
      if (QFileInfo(linkOutput+".dbg").exists()) {
        if (copyFile(linkOutput+".dbg",
                     projectBaseName+".dbg")) {
          new ErrorListItem(this,etError,QString::null,QString::null,
                            "Failed to copy debug info file from temporary directory.",
                            -1,-1);
          errorsCompilingFlag=TRUE;
        }
      }
      if (errorsCompilingFlag || stopCompilingFlag) return;
      statusBar()->message("Compressing...");
      const int numTargets=3;
      char binexts[numTargets][5]={".z89",".z9x",".zv2"};
      char exts[numTargets][5]={".89z",".9xz",".v2z"};
      char cbinexts[numTargets][5]={".y89",".y9x",".yv2"};
      char cexts[numTargets][5]={".89y",".9xy",".v2y"};
      // ttbin2oth STILL has no V200 support. Work around that.
      char ttbin2othflag[numTargets][4]={"-89","-92","-92"};
      char ttbin2othexts[numTargets][5]={".89y",".9xy",".9xy"};
      bool targeted[numTargets];
      for (int target=0; target<numTargets; target++) {
        targeted[target]=QFileInfo(linkOutput+binexts[target]).exists();
        if (targeted[target]) {
          // Compress binary using ttpack.
          // The QTextCodec has to be passed explicitly, or it will default to
          // ISO-8859-1 regardless of the locale, which is just broken.
          procio=new KProcIO(QTextCodec::codecForLocale());
          // Use MergedStderr instead of Stderr so the messages get ordered
          // properly.
          procio->setComm(static_cast<KProcess::Communication>(
            KProcess::Stdout|KProcess::MergedStderr));
          procio->setWorkingDirectory(tempdir);
          // It is not possible to pass an absolute POSIX path to ttpack because
          // it mistakes it for a switch.
          *procio<<(QString("%1/bin/ttpack").arg(tigcc_base))
                 <<"-quiet"<<(QString("tempprog")+binexts[target])
                 <<(QString("tempprog")+cbinexts[target]);
          connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
          connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
          procio->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QApplication::eventLoop()->enterLoop();
          // This will be reached only after exitLoop() is called.
          delete procio;
          procio=static_cast<KProcIO *>(NULL);
          if (!QFileInfo(linkOutput+cbinexts[target]).exists())
            errorsCompilingFlag=TRUE;
          if (errorsCompilingFlag || stopCompilingFlag) return;
          // Wrap binary using ttbin2oth.
          // The QTextCodec has to be passed explicitly, or it will default to
          // ISO-8859-1 regardless of the locale, which is just broken.
          procio=new KProcIO(QTextCodec::codecForLocale());
          // Use MergedStderr instead of Stderr so the messages get ordered
          // properly.
          procio->setComm(static_cast<KProcess::Communication>(
            KProcess::Stdout|KProcess::MergedStderr));
          procio->setWorkingDirectory(tempdir);
          // ttbin2oth tries to use the calculator file name as the host file
          // name. This is bad because the calculator charset is not necessarily
          // the same as the host charset, and files which are not valid host
          // charset names may be corrupted by the file system if it uses
          // Unicode internally (e.g. FAT). So use "tempprog" and then change
          // the name. Moreover, it is not possible to pass an absolute POSIX
          // path to ttbin2oth because it mistakes it for a switch.
          *procio<<(QString("%1/bin/ttbin2oth").arg(tigcc_base))
                 <<"-quiet"<<ttbin2othflag[target]<<"ppg"
                 <<(QString("tempprog")+cbinexts[target])<<"tempprog";
          if (!packFolder.isNull()) *procio<<packFolder;
          connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
          connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
          procio->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QApplication::eventLoop()->enterLoop();
          // This will be reached only after exitLoop() is called.
          delete procio;
          procio=static_cast<KProcIO *>(NULL);
          if (!QFileInfo(linkOutput+ttbin2othexts[target]).exists())
            errorsCompilingFlag=TRUE;
          if (errorsCompilingFlag || stopCompilingFlag) return;
          if (insertName(linkOutput+ttbin2othexts[target],
                         projectBaseName+cexts[target],packName)) {
            new ErrorListItem(this,etError,QString::null,QString::null,
                              "Failed to copy PPG from temporary directory.",
                              -1,-1);
            errorsCompilingFlag=TRUE;
          }
          if (errorsCompilingFlag || stopCompilingFlag) return;
        }
      }
      // Prepare the pstarter.
      QString pstarterBaseName=QString("%1/pstarter").arg(tempdir);
      if (insertName(QString("%1/lib/pstarter.o").arg(tigcc_base),
                     pstarterBaseName+".o",packName)) {
        new ErrorListItem(this,etError,QString::null,QString::null,
                          "Failed to copy pstarter.o to temporary directory.",
                          -1,-1);
        errorsCompilingFlag=TRUE;
      }
      if (errorsCompilingFlag || stopCompilingFlag) return;
      // Link the pstarter with ld-tigcc.
      // The QTextCodec has to be passed explicitly, or it will default to
      // ISO-8859-1 regardless of the locale, which is just broken.
      procio=new KProcIO(QTextCodec::codecForLocale());
      // Use MergedStderr instead of Stderr so the messages get ordered
      // properly.
      procio->setComm(static_cast<KProcess::Communication>(
        KProcess::Stdout|KProcess::MergedStderr));
      procio->setWorkingDirectory(projectDir);
      *procio<<(QString("%1/bin/ld-tigcc").arg(tigcc_base))
             <<"-o"<<pstarterBaseName<<"-n"<<projectName
             <<(pstarterBaseName+".o");
      connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
      connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady()));
      procio->start();
      // We need to block here, but events still need to be handled. The most
      // effective way to do this is to enter the event loop recursively,
      // even though it is not recommended by Qt.
      QApplication::eventLoop()->enterLoop();
      // This will be reached only after exitLoop() is called.
      delete procio;
      procio=static_cast<KProcIO *>(NULL);
      if (errorsCompilingFlag || stopCompilingFlag) return;
      // Now copy those pstarters actually requested.
      for (int target=0; target<numTargets; target++) {
        if (targeted[target]) {
          if (copyFile(pstarterBaseName+exts[target],
                       projectBaseName+exts[target])) {
            new ErrorListItem(this,etError,QString::null,QString::null,
                              "Failed to copy pstarter from temporary directory.",
                              -1,-1);
            errorsCompilingFlag=TRUE;
          }
          if (errorsCompilingFlag || stopCompilingFlag) return;
        }
      }
      // Remove any leftover "-titanium" launchers if the TI-89 is targeted.
      if (*targeted) {
        QDir qdir;
        qdir.remove(projectBaseName+"-titanium.89z");
        qdir.remove(projectBaseName+"-Titanium.89z");
      }
    }
  }
  if (errorsCompilingFlag || stopCompilingFlag) return;
  if (!settings.post_build.isEmpty()) {
    // Post-build processing.
    statusBar()->message("Calling User-Defined Program...");
    QString postBuild=settings.post_build;
    // Remove double quotes for TIGCC IDE compatibility. Single quotes will be
    // added instead.
    postBuild.replace("\"($TI89FILE)\"","($TI89FILE)",FALSE);
    postBuild.replace("\"($TI89TIFILE)\"","($TI89TIFILE)",FALSE);
    postBuild.replace("\"($TI92PLUSFILE)\"","($TI92PLUSFILE)",FALSE);
    postBuild.replace("\"($V200FILE)\"","($V200FILE)",FALSE);
    postBuild.replace("\"($TI92FILE)\"","($TI92FILE)",FALSE);
    postBuild.replace("($TI89FILE)",
      KProcess::quote(libopts.use_ti89?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-89.tib":settings.pack?".89y":
        settings.outputbin?".z89":".89z"
      ):""),FALSE);
    postBuild.replace("($TI89TIFILE)",
      KProcess::quote(libopts.use_ti89?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-89ti.tib":settings.pack?".89y":
        settings.outputbin?".z89":".89z"
      ):""),FALSE);
    postBuild.replace("($TI92PLUSFILE)",
      KProcess::quote(libopts.use_ti92p?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-9x.tib":settings.pack?".9xy":
        settings.outputbin?".z9x":".9xz"
      ):""),FALSE);
    postBuild.replace("($V200FILE)",
      KProcess::quote(libopts.use_v200?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-v2.tib":settings.pack?".v2y":
        settings.outputbin?".zv2":".v2z"
      ):""),FALSE);
    postBuild.replace("($TI92FILE)",
      KProcess::quote(settings.fargo?projectBaseName+(
        settings.outputbin?".p92":".92p"
      ):""),FALSE);
    int err;
    QStringList args=KShell::splitArgs(postBuild,
                                       KShell::TildeExpand|KShell::AbortOnMeta,
                                       &err);
    if (err) {
      new ErrorListItem(this,etError,QString::null,QString::null,
                        "Invalid post-build command line.",-1,-1);
      errorsCompilingFlag=TRUE;
    }
    if (errorsCompilingFlag || stopCompilingFlag) return;
    // The QTextCodec has to be passed explicitly, or it will default to
    // ISO-8859-1 regardless of the locale, which is just broken.
    procio=new KProcIO(QTextCodec::codecForLocale());
    // Use MergedStderr instead of Stderr so the messages get ordered
    // properly.
    procio->setComm(static_cast<KProcess::Communication>(
      KProcess::Stdout|KProcess::MergedStderr));
    procio->setWorkingDirectory(projectDir);
    *procio<<args;
    connect(procio,SIGNAL(processExited(KProcess*)),this,SLOT(procio_processExited()));
    connect(procio,SIGNAL(readReady(KProcIO*)),this,SLOT(procio_readReady_recordOnly()));
    procio->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QApplication::eventLoop()->enterLoop();
    // This will be reached only after exitLoop() is called.
    delete procio;
    procio=static_cast<KProcIO *>(NULL);
    if (errorsCompilingFlag || stopCompilingFlag) return;
  }
  // Delete object and/or assembly files.
  QDir qdir;
  if (preferences.deleteObjFiles) {
    for (QStringList::Iterator it=deletableObjectFiles.begin();
         it!=deletableObjectFiles.end(); ++it) {
      qdir.remove(*it);
    }
  }
  if (preferences.deleteAsmFiles) {
    for (QStringList::Iterator it=deletableAsmFiles.begin();
         it!=deletableAsmFiles.end(); ++it) {
      qdir.remove(*it);
    }
  }
  projectNeedsRelink=FALSE;
}

void MainForm::showStats()
{
  // KMessageBox also takes plain text, but it interprets \n as paragraph breaks
  // and always renders them as \n\n, no matter whether there is actually just
  // one, two or more than two \n characters. Thus the
  // QStyleSheet::convertFromPlainText. Sadly, that replaces the spaces with
  // strange non-BMP characters (from a Private Use Area) which don't display
  // properly in KMessageBox, so fix up those surrogate pairs.
  if (KMessageBox::questionYesNo(this,QStyleSheet::convertFromPlainText(
        QString("The project has been compiled successfully.\n\n%1\n"
        "Do you want to open the project folder?").arg(compileStats))
        .replace(QString(QChar(56319))+QString(QChar(56992))," "),
        "Compilation Successful")==KMessageBox::Yes) {
    KURL projectDir=KURL::fromPathOrURL(projectFileName);
    projectDir.setFileName("");
    KRun::runURL(projectDir.url(),"inode/directory");
  }
}

void MainForm::projectCompile()
{
  if (compiling) return;
  startCompiling();
  compileProject(FALSE);
  stopCompiling();
}

void MainForm::projectMake()
{
  if (compiling) return;
  // Can't link a project without saving it first.
  if (projectFileName.isEmpty()) {
    if (!fileSave() || projectFileName.isEmpty()) return;
  }
  startCompiling();
  compileProject(FALSE);
  if (!errorsCompilingFlag && !stopCompilingFlag) linkProject();
  bool success=(!errorsCompilingFlag && !stopCompilingFlag);
  stopCompiling();
  if (success && preferences.successMessage)
    showStats();
}

void MainForm::projectBuild()
{
  if (compiling) return;
  // Can't link a project without saving it first.
  if (projectFileName.isEmpty()) {
    if (!fileSave() || projectFileName.isEmpty()) return;
  }
  startCompiling();
  compileProject(TRUE);
  if (!errorsCompilingFlag && !stopCompilingFlag) linkProject();
  bool success=(!errorsCompilingFlag && !stopCompilingFlag);
  stopCompiling();
  if (success && preferences.successMessage)
    showStats();
}

void MainForm::compileSourceFile(void *srcFile)
{
  if (compiling) return;
  SourceFile *sourceFile=reinterpret_cast<SourceFile *>(srcFile);
  sourceFile->fileCloseAction->setEnabled(FALSE);
  startCompiling();
  compileFile(sourceFile,FALSE,TRUE);
  stopCompiling();
}

void MainForm::projectStopCompilation()
{
  if (!compiling) return;
  stopCompilingFlag=TRUE;
  projectStopCompilationAction->setEnabled(FALSE);
}

void MainForm::projectForceQuit()
{
  if (!compiling) return;
  stopCompilingFlag=TRUE;
  if (procio && procio->isRunning()) {
    procio->kill();
  }
}

void MainForm::projectErrorsAndWarnings(bool on)
{
  static bool lock=FALSE;
  if (!lock) {
    lock=TRUE;
    if (!projectErrorsAndWarningsAction->isEnabled()) on=FALSE;
    projectErrorsAndWarningsAction->setOn(on);
    if (on)
      errorListDock->show();
    else
      errorListDock->hide();
    lock=FALSE;
  }
}

void MainForm::projectProgramOutput()
{
  ProgramOutput programOutputDialog;
  programOutputDialog.textBrowser->setText(programOutput);
  programOutputDialog.exec();
}

void MainForm::projectOptions()
{
  ProjectOptions *projectoptions=new ProjectOptions(this);
  projectoptions->exec();
  if (projectoptions->result()==QDialog::Accepted) {
    projectIsDirty=TRUE;
    projectNeedsRelink=TRUE;
    headersModified=TRUE; // force complete rebuild
  }
  delete(projectoptions);
  bool runnable=!settings.archive&&!settings.flash_os&&preferences.linkTarget!=LT_NONE;
  menuBar()->setItemVisible(5,runnable); //debugMenu
  debugRunAction->setVisible(runnable);
  debugPauseAction->setVisible(runnable);
}

bool MainForm::tiemuInstance(QCString &instanceName)
{
  instanceName=QCString();
  DCOPClient *dcopClient=kapp->dcopClient();
  if (!dcopClient->isAttached() && !dcopClient->attach()) {
    KMessageBox::error(this,"Can\'t attach to DCOP.");
    return FALSE;
  }
  QCStringList applist=dcopClient->registeredApplications();
  QCString appname;
  QCStringList::iterator it;
  for (it = applist.begin(); it != applist.end(); ++it) {
    if ((*it).contains(QRegExp("^tiemu-"))) {
      instanceName = (*it);
      break;
    }
  }
  return TRUE;
}

// Same values as TIGCC, except for CALCULATOR_INVALID.
enum CalculatorModels {CALCULATOR_TI89, CALCULATOR_TI92P, CALCULATOR_TI92,
                       CALCULATOR_V200, CALCULATOR_INVALID=-1};

QString MainForm::tilibsErrorMessage(int err)
{
  if (!err) return "No error. (This is a bug, please report.)";

  char *msg=0;
  if ((err=ticables_error_get(err,&msg))
      && (err=tifiles_error_get(err,&msg))
      && (err=ticalcs_error_get(err,&msg)))
    return "Invalid error code. (This is a bug, please report.)";

  QString ret=msg;
  g_free(msg);
  return ret;
}

void MainForm::debugRun()
{
  if (compiling) return;
  // Can't link a project without saving it first.
  if (projectFileName.isEmpty()) {
    if (!fileSave() || projectFileName.isEmpty()) return;
  }
  startCompiling();
  compileProject(FALSE);
  if (!errorsCompilingFlag && !stopCompilingFlag && projectNeedsRelink)
    linkProject();
  bool success=(!errorsCompilingFlag && !stopCompilingFlag);
  stopCompiling();
  QString projectBaseName=projectFileName;
  if (projectBaseName.endsWith(".tpr",FALSE))
    projectBaseName.truncate(projectBaseName.length()-4);
  if (success && (QFileInfo(projectBaseName+".89z").exists()
                  || QFileInfo(projectBaseName+".9xz").exists()
                  || QFileInfo(projectBaseName+".v2z").exists()
                  || QFileInfo(projectBaseName+".92p").exists())) {
    QStringList files;
    if (settings.fargo)
      files.append(projectBaseName+".92p");
    else {
      files.append(projectBaseName+".89z");
      if (settings.pack) files.append(projectBaseName+".89y");
      if (settings.use_data_var) files.append(projectBaseName+"-data.89y");
    }
    if (files.isEmpty()) return;
  
    // Detect model of linked calculator.
    CalculatorModels model=CALCULATOR_INVALID;
    QCString instanceName;
    TiEmuDCOP_stub *tiemuDCOP=0;
    CableHandle *cable=0;
    CalcHandle *calc=0;
    switch (preferences.linkTarget) {
      case LT_TIEMU:
        {
          // Fire up TiEmu if it isn't running yet.
          if (!tiemuInstance(instanceName)) return;
          if (instanceName.isNull()) {
            if (!KRun::runCommand("tiemu")) {
              KMessageBox::error(this,"Can't run TiEmu.");
              return;
            }
            do {
              usleep(100000);
              QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,100);
              if (!tiemuInstance(instanceName)) return;
            } while (instanceName.isNull());
          }
          tiemuDCOP=new TiEmuDCOP_stub(instanceName,"TiEmuDCOP");
          // Wait for TiEmu to get ready.
          bool ready;
          do {
            usleep(100000);
            QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,100);
            ready=tiemuDCOP->ready_for_transfers();
            if (!tiemuDCOP->ok()) {
              KMessageBox::error(this,"DCOP function call failed.");
              delete tiemuDCOP;
              return;
            }
          } while (!ready);
          // Now obtain the model from TiEmu.
          int tiemuCalcType=tiemuDCOP->emulated_calc_type();
          if (!tiemuCalcType || !tiemuDCOP->ok()) {
            KMessageBox::error(this,"DCOP function call failed.");
            delete tiemuDCOP;
            return;
          }
          switch (tiemuCalcType) {
            case TIEMU_CALC_TI89:
            case TIEMU_CALC_TI89t:
              model=CALCULATOR_TI89;
              break;
            case TIEMU_CALC_TI92p:
              model=CALCULATOR_TI92P;
              break;
            case TIEMU_CALC_V200:
              model=CALCULATOR_V200;
              break;
            case TIEMU_CALC_TI92:
              model=CALCULATOR_TI92;
              break;
            default:
              KMessageBox::error(this,"Unsupported calculator model.");
              return;
          }
        }
        break;
      case LT_REALCALC:
        {
          // Probe for the connected model.
          CalcModel tilibsCalcModel=CALC_NONE;
          int err;
          if ((err=ticalcs_probe(preferences.linkCable,preferences.linkPort,
                                 &tilibsCalcModel,TRUE))) {
            KMessageBox::error(this,tilibsErrorMessage(err));
            return;
          }
          switch (tilibsCalcModel) {
            case CALC_TI89:
            case CALC_TI89T:
            case CALC_TI89T_USB:
              model=CALCULATOR_TI89;
              break;
            case CALC_TI92P:
              model=CALCULATOR_TI92P;
              break;
            case CALC_V200:
              model=CALCULATOR_V200;
              break;
            case CALC_TI92:
              model=CALCULATOR_TI92;
              break;
            default:
              KMessageBox::error(this,"Unsupported calculator model.");
              return;
          }
          // Allocate handle for the cable.
          cable=ticables_handle_new(preferences.linkCable,preferences.linkPort);
          if (!cable) {
            KMessageBox::error(this,"Failed to allocate cable handle.");
            return;
          }
          // Allocate handle for the calculator.
          calc=ticalcs_handle_new(tilibsCalcModel);
          if (!calc) {
            KMessageBox::error(this,"Failed to allocate calculator handle.");
            ticables_handle_del(cable);
            return;
          }
          // Attach the calculator handle to the cable handle.
          if ((err=ticalcs_cable_attach(calc,cable))) {
            KMessageBox::error(this,tilibsErrorMessage(err));
            ticalcs_handle_del(calc);
            ticables_handle_del(cable);
            return;
          }
        }
        break;
      default:
        qWarning("Bug: debugRun called with no link target.");
        return;
    }
  
    // Select the correct files for the model.
    if (model!=CALCULATOR_TI92 && settings.fargo) {
      KMessageBox::error(this,"Can't send Fargo program to a TI-89/89Ti/92+/V200.");
      return;
    }
    if ((model==CALCULATOR_TI92 || model==CALCULATOR_INVALID) && !settings.fargo) {
      KMessageBox::error(this,"Can't send AMS program to a TI-92.");
      return;
    }
    for (QStringList::Iterator it=files.begin(); it!=files.end(); ++it) {
      if (model==CALCULATOR_TI92P) (*it).replace(QRegExp("\\.89(?=.$)"),".9x");
      else if (model==CALCULATOR_V200) (*it).replace(QRegExp("\\.89(?=.$)"),".v2");
      if (!QFileInfo(*it).exists()) {
        KMessageBox::error(this,"The program was not compiled for the linked calculator.");
        return;
      }
    }
  
    // Now send the files.
    switch (preferences.linkTarget) {
      case LT_TIEMU:
        // Send the files.
        if (settings.debug_info && !settings.pack
            && QFileInfo(projectBaseName+".dbg").exists()) {
          QString mainFile=files.first();
          files.pop_front();
          if (!tiemuDCOP->debug_file(mainFile) || !tiemuDCOP->ok()) {
            KMessageBox::error(this,"DCOP function call failed.");
            delete tiemuDCOP;
            return;
          }
        }
        if (!tiemuDCOP->send_files(files) || !tiemuDCOP->ok()) {
          KMessageBox::error(this,"DCOP function call failed.");
          delete tiemuDCOP;
          return;
        }
        break;
      case LT_REALCALC:
        ticables_options_set_timeout(cable,DFLT_TIMEOUT<<2);
        ticalcs_update_set(calc,&ticalcsUpdate);
        for (QStringList::Iterator it=files.begin(); it!=files.end(); ++it) {
          const char *file=*it;
          int err;
          // libticalcs2 does NO validation on the file, so better do it now.
          if (!tifiles_file_is_single(file)
              || !tifiles_calc_is_ti9x(tifiles_file_get_model(file))) {
            KMessageBox::error(this,QString("File \'%1\' has an invalid format.").arg(file));
            ticalcs_handle_del(calc);
            ticables_handle_del(cable);
            return;
          }
          // Send the file.
          callbacksInit(this);
          if ((err=ticalcs_calc_send_var2(calc,MODE_NORMAL,file))) {
            bool cancelled=ticalcsUpdate.cancel;
            callbacksCleanup();
            if (!cancelled) KMessageBox::error(this,tilibsErrorMessage(err));
            ticalcs_handle_del(calc);
            ticables_handle_del(cable);
            return;
          }
          callbacksCleanup();
        }
        break;
      default:
        qFatal("Bug: This code in debugRun should be unreachable!");
        return;
    }

    // And finally run the program.
    QString command=(QString("%1%2(%3)")
      .arg(rootListItem->text(0).contains('\\')?"":"main\\")
      .arg(rootListItem->text(0)).arg(settings.cmd_line));
    switch (preferences.linkTarget) {
      case LT_TIEMU:
        // Execute the command.
        if (!tiemuDCOP->execute_command(command) || !tiemuDCOP->ok())
          KMessageBox::error(this,"DCOP function call failed.");
        delete tiemuDCOP;
        break;
      case LT_REALCALC:
        {
          int err;
          // Convert the command to the calculator charset.
          char *ti=ticonv_charset_utf16_to_ti(CALC_TI89,command.ucs2());
          // Send it character by character.
          #define SEND_CHAR(c) \
            if ((err=ticalcs_calc_send_key(calc,(c)))) { \
              KMessageBox::error(this,tilibsErrorMessage(err)); \
              g_free(ti); \
              ticalcs_handle_del(calc); \
              ticables_handle_del(cable); \
              return; \
            }
          SEND_CHAR(264); // KEY_ESC
          SEND_CHAR(264); // KEY_ESC
          SEND_CHAR(model?8273:277); // KEY_HOME
          SEND_CHAR(263); // KEY_CLEAR
          SEND_CHAR(263); // KEY_CLEAR
          for (unsigned char *p=reinterpret_cast<unsigned char *>(ti); *p; p++) {
            SEND_CHAR(*p);
          }
          SEND_CHAR(13); // KEY_ENTER
          #undef SEND_CHAR
          g_free(ti);
        }
        ticalcs_handle_del(calc);
        ticables_handle_del(cable);
        break;
      default:
        qFatal("Bug: This code in debugRun should be unreachable!");
        return;
    }
  }
}

void MainForm::debugPause()
{
  // This is enabled only for LT_TIEMU. Run the TiEmu debugger.
  QCString instanceName;
  if (!tiemuInstance(instanceName) || instanceName.isNull()) return;
  TiEmuDCOP_stub tiemuDCOP(instanceName,"TiEmuDCOP");
  tiemuDCOP.enter_debugger();
  if (!tiemuDCOP.ok())
    KMessageBox::error(this,"DCOP function call failed.");
}

void MainForm::debugReset()
{
  // This is enabled only for LT_TIEMU. Reset TiEmu.
  QCString instanceName;
  if (!tiemuInstance(instanceName) || instanceName.isNull()) return;
  TiEmuDCOP_stub tiemuDCOP(instanceName,"TiEmuDCOP");
  tiemuDCOP.reset_calc(FALSE);
  if (!tiemuDCOP.ok())
    KMessageBox::error(this,"DCOP function call failed.");
}

void MainForm::toolsConfigure()
{
  ToolsDialog toolsDialog(this);
  toolsDialog.exec();
  if (toolsDialog.result()==QDialog::Accepted) {
    pconfig->setGroup("Tools");
    unsigned toolCount=tools.count();
    pconfig->writeEntry("Count",toolCount);
    for (unsigned idx=0; idx<toolCount; idx++) {
      pconfig->setGroup(QString("Tool %1").arg(idx));
      Tool &tool=tools[idx];
      pconfig->writeEntry("Title",tool.title);
      pconfig->writeEntry("Command Line",tool.commandLine);
      pconfig->writeEntry("Working Directory",tool.workingDirectory);
      pconfig->writeEntry("Terminal",tool.runInTerminal);
      pconfig->sync();
    }
    updateToolsMenu();
  }
}

void MainForm::updateToolsMenu()
{
  toolsMenu->clear();
  toolsConfigureAction->addTo(toolsMenu);
  unsigned toolCount=tools.count();
  if (toolCount) toolsMenu->insertSeparator();
  for (unsigned idx=0; idx<toolCount; idx++)
    toolsMenu->insertItem(tools[idx].title,idx);
  disconnect(toolsMenu,SIGNAL(highlighted(int)),0,0);
  connect(toolsMenu,SIGNAL(highlighted(int)),
          this,SLOT(toolsMenu_highlighted(int)));
  disconnect(toolsMenu,SIGNAL(aboutToHide()),0,0);
  connect(toolsMenu,SIGNAL(aboutToHide()),this,SLOT(toolsMenu_aboutToHide()));
}

void MainForm::toolsMenu_highlighted(int id)
{
  if (id==toolsMenu->idAt(0))
    statusBar()->message(toolsConfigureAction->statusTip());
  else if (id!=toolsMenu->idAt(1))
    statusBar()->message(tools[id].commandLine);
}

void MainForm::toolsMenu_activated(int id)
{
  if (id!=toolsMenu->idAt(0) && id!=toolsMenu->idAt(1)) {
    const Tool &tool=tools[id];
    KProcess process(this);
    int err;
    QStringList args=KShell::splitArgs(tool.commandLine,
                                       KShell::TildeExpand|KShell::AbortOnMeta,
                                       &err);
    if (err) {
      KMessageBox::error(this,"Invalid command line.");
      return;
    }
    if (tool.runInTerminal) {
      KConfigGroup globalConfigGeneral(KGlobal::config(),"General");
      QString terminal=globalConfigGeneral.readPathEntry("TerminalApplication",
                                                         "konsole");
      if (terminal=="konsole")
        terminal+=" -caption=%c";
      QMap<QChar,QString> map;
      map.insert('c',tool.title);
      map.insert('i',"");
      map.insert('m',"");
      terminal=KMacroExpander::expandMacrosShellQuote(terminal,map);
      if (terminal.isNull()) {
        KMessageBox::error(this,"Invalid terminal specification.");
        return;
      }
      QStringList termargs=KShell::splitArgs(terminal,
                                             KShell::TildeExpand|KShell::AbortOnMeta,
                                             &err);
      if (err) {
        KMessageBox::error(this,"Invalid terminal specification.");
        return;
      }
      process << termargs << "-e" << "sh" << "-c"
              << (tool.commandLine+" ; echo Press Return to close... ; read");
    } else
      process << args;
    if (!tool.workingDirectory.isEmpty())
      process.setWorkingDirectory(tool.workingDirectory);
    if (!process.start(KProcess::DontCare))
      KMessageBox::error(this,QString("Can't run \'%1\'.").arg(tool.commandLine));
  }
}

void MainForm::toolsMenu_aboutToHide()
{
  statusBar()->clear();
}

void MainForm::helpDocumentation()
{
  assistant->openAssistant();
}

void MainForm::helpContents()
{
  force_qt_assistant_page(0);
  assistant->openAssistant();
}

void MainForm::helpIndex()
{
  force_qt_assistant_page(1);
  assistant->openAssistant();
}

void MainForm::helpSearch()
{
  force_qt_assistant_page(3);
  assistant->openAssistant();
}

void MainForm::helpNews()
{
  NewsDialog newsDialog(this);
  newsDialog.loadNews();
  newsDialog.exec();
}

void MainForm::helpAbout()
{
  khelpmenu->aboutApplication();
}

void MainForm::updateSizes()
{
  int leftSize=splitter->sizes().first();
  int rightSize=splitter->sizes().last();
  int totalSize=leftSize+rightSize;
  int mySize=size().width();
  leftStatusLabel->setMaximumWidth(leftSize*mySize/totalSize);
  rightStatusLabel->setMaximumWidth(rightSize*mySize/totalSize-10>0?
                                    rightSize*mySize/totalSize-10:0);
}

void MainForm::resizeEvent(QResizeEvent *event)
{
  QMainWindow::resizeEvent(event);
  if (event->size()==event->oldSize()) return;
  updateSizes();
}

void MainForm::timerEvent(QTimerEvent *event)
{
  static int lastSplitterPos=-1;
  QMainWindow::timerEvent(event);
  if (lastSplitterPos==splitter->sizes().first()) return;
  lastSplitterPos=splitter->sizes().first();
  updateSizes();
}


void MainForm::fileTreeClicked(QListViewItem *item)
{
  if (!item) return;
  if (fileTree->selectedItem()!=item) {
    fileTree->setSelected(item,TRUE);
    fileTree->ensureItemVisible(item);
  }
  if (IS_FOLDER(currentListItem))
    currentListItem->setPixmap(0,SYSICON("folder","folder1.png"));
  if (IS_FILE(currentListItem)) {
    CATEGORY_OF(category,currentListItem);
    if (IS_EDITABLE_CATEGORY(category) && static_cast<ListViewFile *>(currentListItem)->kateView) {
      static_cast<ListViewFile *>(currentListItem)->kateView->hide();
      widgetStack->removeWidget(static_cast<ListViewFile *>(currentListItem)->kateView);
      widgetStack->raiseWidget(-1);
    }
  }
  if (IS_FOLDER(item)) {
    // Bluecurve's "folder_open" isn't actually more open than "folder".
    item->setPixmap(0,SYSICON(KIconTheme::current().compare("Bluecurve")?"folder_open":"folder-accept","folder2.png"));
    fileNewFolderAction->setEnabled(TRUE);
    filePrintAction->setEnabled(FALSE);
    filePrintQuicklyAction->setEnabled(FALSE);
    editUndoAction->setEnabled(FALSE);
    editRedoAction->setEnabled(FALSE);
    editClearAction->setEnabled(FALSE);
    editCutAction->setEnabled(FALSE);
    editCopyAction->setEnabled(FALSE);
    editPasteAction->setEnabled(FALSE);
    editSelectAllAction->setEnabled(FALSE);
    editIncreaseIndentAction->setEnabled(FALSE);
    editDecreaseIndentAction->setEnabled(FALSE);
    findFunctionsAction->setEnabled(FALSE);
    findOpenFileAtCursorAction->setEnabled(FALSE);
    findFindSymbolDeclarationAction->setEnabled(FALSE);
    accel->setItemEnabled(0,FALSE);
    accel->setItemEnabled(1,FALSE);
    accel->setItemEnabled(2,FALSE);
    accel->setItemEnabled(3,FALSE);
    accel->setItemEnabled(4,FALSE);
    accel->setItemEnabled(5,FALSE);
    accel->setItemEnabled(6,FALSE);
    accel->setItemEnabled(7,FALSE);
  } else if (IS_FILE(item)) {
    fileNewFolderAction->setEnabled(TRUE);
    CATEGORY_OF(category,item->parent());
    if (IS_EDITABLE_CATEGORY(category)) {
      Kate::View *kateView=static_cast<ListViewFile *>(item)->kateView;
      if (!kateView) { // lazy loading
        kateView=reinterpret_cast<Kate::View *>(createView(static_cast<ListViewFile *>(item)->fileName,static_cast<ListViewFile *>(item)->textBuffer,category));
        static_cast<ListViewFile *>(item)->textBuffer=QString::null;
        static_cast<ListViewFile *>(item)->kateView=kateView;
        MainForm::createErrorCursorsForSourceFile(item);
      }
      filePrintAction->setEnabled(TRUE);
      filePrintQuicklyAction->setEnabled(TRUE);
      widgetStack->addWidget(kateView);
      kateView->show();
      widgetStack->raiseWidget(kateView);
      editUndoAction->setEnabled(!!(kateView->getDoc()->undoCount()));
      editRedoAction->setEnabled(!!(kateView->getDoc()->redoCount()));
      editClearAction->setEnabled(kateView->getDoc()->hasSelection());
      editCutAction->setEnabled(kateView->getDoc()->hasSelection());
      editCopyAction->setEnabled(kateView->getDoc()->hasSelection());
      editPasteAction->setEnabled(!clipboard->text().isNull());
      editSelectAllAction->setEnabled(TRUE);
      editIncreaseIndentAction->setEnabled(TRUE);
      editDecreaseIndentAction->setEnabled(TRUE);
      findFunctionsAction->setEnabled(category!=txtFilesListItem);
      findOpenFileAtCursorAction->setEnabled(TRUE);
      findFindSymbolDeclarationAction->setEnabled(TRUE);
      accel->setItemEnabled(0,!!(kateView->getDoc()->undoCount()));
      accel->setItemEnabled(1,!!(kateView->getDoc()->redoCount()));
      accel->setItemEnabled(2,kateView->getDoc()->hasSelection());
      accel->setItemEnabled(3,kateView->getDoc()->hasSelection());
      accel->setItemEnabled(4,!clipboard->text().isNull());
      accel->setItemEnabled(5,TRUE);
      accel->setItemEnabled(6,TRUE);
      accel->setItemEnabled(7,TRUE);
    } else {
      filePrintAction->setEnabled(FALSE);
      filePrintQuicklyAction->setEnabled(FALSE);
      editUndoAction->setEnabled(FALSE);
      editRedoAction->setEnabled(FALSE);
      editClearAction->setEnabled(FALSE);
      editCutAction->setEnabled(FALSE);
      editCopyAction->setEnabled(FALSE);
      editPasteAction->setEnabled(FALSE);
      editSelectAllAction->setEnabled(FALSE);
      editIncreaseIndentAction->setEnabled(FALSE);
      editDecreaseIndentAction->setEnabled(FALSE);
      findFunctionsAction->setEnabled(FALSE);
      findOpenFileAtCursorAction->setEnabled(FALSE);
      findFindSymbolDeclarationAction->setEnabled(FALSE);
      accel->setItemEnabled(0,FALSE);
      accel->setItemEnabled(1,FALSE);
      accel->setItemEnabled(2,FALSE);
      accel->setItemEnabled(3,FALSE);
      accel->setItemEnabled(4,FALSE);
      accel->setItemEnabled(5,FALSE);
      accel->setItemEnabled(6,FALSE);
      accel->setItemEnabled(7,FALSE);
    }
  } else {
    fileNewFolderAction->setEnabled(FALSE);
    filePrintAction->setEnabled(FALSE);
    filePrintQuicklyAction->setEnabled(FALSE);
    editUndoAction->setEnabled(FALSE);
    editRedoAction->setEnabled(FALSE);
    editClearAction->setEnabled(FALSE);
    editCutAction->setEnabled(FALSE);
    editCopyAction->setEnabled(FALSE);
    editPasteAction->setEnabled(FALSE);
    editSelectAllAction->setEnabled(FALSE);
    editIncreaseIndentAction->setEnabled(FALSE);
    editDecreaseIndentAction->setEnabled(FALSE);
    findFunctionsAction->setEnabled(FALSE);
    findOpenFileAtCursorAction->setEnabled(FALSE);
    findFindSymbolDeclarationAction->setEnabled(FALSE);
    accel->setItemEnabled(0,FALSE);
    accel->setItemEnabled(1,FALSE);
    accel->setItemEnabled(2,FALSE);
    accel->setItemEnabled(3,FALSE);
    accel->setItemEnabled(4,FALSE);
    accel->setItemEnabled(5,FALSE);
    accel->setItemEnabled(6,FALSE);
    accel->setItemEnabled(7,FALSE);
  }
  currentListItem=item;
  updateLeftStatusLabel();
  updateRightStatusLabel();
}

void MainForm::fileNewFolder()
{
  if (compiling) return;
  if (IS_FILE(currentListItem))
    currentListItem=currentListItem->parent();
  QListViewItem *item=NULL, *next=currentListItem->firstChild();
  for (; next; next=item->nextSibling())
  {
    item=next;
  }
  QListViewItem *newFolder=item?new ListViewFolder(currentListItem,item)
                           :new ListViewFolder(currentListItem);
  newFolder->setText(0,"NewFolder");
  newFolder->setRenameEnabled(0,TRUE);
  currentListItem->setOpen(TRUE);
  fileTreeClicked(newFolder);
  newFolder->startRename(0);
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
}

bool MainForm::removeItem(QListViewItem *item)
{
  if (IS_FOLDER(item) && !IS_CATEGORY(item)) {
    QListViewItem *child=item->firstChild();
    while (child) {
      QListViewItem *nextChild=child->nextSibling();
      if (!removeItem(child)) return FALSE;
      child=nextChild;
    }
    delete item;
    currentListItem=NULL;
    fileTreeClicked(fileTree->currentItem());
    projectIsDirty=TRUE;
    projectNeedsRelink=TRUE;
    return TRUE;
  } else if (IS_FILE(item)) {
    if (!fileSavePrompt(item)) {
      delete item;
      currentListItem=NULL;
      fileTreeClicked(fileTree->currentItem());
      projectIsDirty=TRUE;
      projectNeedsRelink=TRUE;
      return TRUE;
    } else return FALSE;
  } else return FALSE;
}

#define unused_col __attribute__((unused)) col /* stupid QT designer... */
void MainForm::fileTreeContextMenuRequested(QListViewItem *item,
                                            const QPoint &pos,
                                            int unused_col)
{
  fileTreeClicked(item);
  if (IS_FOLDER(item)) {
    QPopupMenu menu;
    menu.insertItem("New &Folder",0);
    menu.insertItem("New F&ile",1);
    if (compiling) {
      menu.setItemEnabled(0,FALSE);
      menu.setItemEnabled(1,FALSE);
    }
    CATEGORY_OF(category,item);
    if (!IS_EDITABLE_CATEGORY(category))
      menu.setItemEnabled(1,FALSE);
    if (!IS_CATEGORY(item)) {
      menu.insertSeparator();
      menu.insertItem("&Remove",2);
      if (compiling) menu.setItemEnabled(2,FALSE);
      menu.insertItem("Re&name",3);
    }
    switch (menu.exec(pos)) {
      case 0:
        fileNewFolder();
        break;
      case 1:
        newFile(item);
        break;
      case 2:
        removeItem(item);
        break;
      case 3:
        item->startRename(0);
    }
  } else if (IS_FILE(item)) {
    QPopupMenu menu;
    ListViewFile *theFile=static_cast<ListViewFile *>(item);
    menu.insertItem("&Save",0);
    CATEGORY_OF(category,item);
    if (!IS_EDITABLE_CATEGORY(category))
      menu.setItemEnabled(0,FALSE);
    menu.insertItem("Save &As...",1);
    menu.insertSeparator();
    menu.insertItem("&Compile",2);
    if (compiling || category==txtFilesListItem
                  || !IS_EDITABLE_CATEGORY(category))
      menu.setItemEnabled(2,FALSE);
    menu.insertSeparator();
    menu.insertItem("&Remove",3);
    menu.insertItem("&Delete",4);
    if (compiling) {
      menu.setItemEnabled(3,FALSE);
      menu.setItemEnabled(4,FALSE);
    }
    if (theFile->isNew)
      menu.setItemEnabled(4,FALSE);
    menu.insertSeparator();
    menu.insertItem("Re&name",5);
    switch (menu.exec(pos)) {
      case 0:
        fileSave_save(item);
        break;
      case 1:
        fileSave_saveAs(item);
        break;
      case 2:
        if (compiling) return;
        startCompiling();
        compileFile(theFile,TRUE,TRUE);
        stopCompiling();
        break;
      case 3:
        removeItem(item);
        break;
      case 4:
        if (KMessageBox::questionYesNo(this,
              "Are you sure you want to delete this source file? "
              "You cannot undo this operation.","Confirm Deletion")
              ==KMessageBox::Yes) {
          QString fileName=theFile->fileName;
          KDirWatch::self()->removeFile(fileName);
          if (QDir().remove(fileName)) {
            delete item;
            currentListItem=NULL;
            fileTreeClicked(fileTree->currentItem());
            projectIsDirty=TRUE;
            projectNeedsRelink=TRUE;
          } else {
            KMessageBox::error(this,
              QString("Error deleting file \'%1\'").arg(fileName));
            if (IS_EDITABLE_CATEGORY(category) && fileName[0]=='/')
              KDirWatch::self()->addFile(fileName);
          }
        }
        break;
      case 5:
        item->startRename(0);
    }
  }
}

QStringList MainForm::extractAllFileNames(void)
{
  QListViewItem *item=rootListItem->firstChild(),*next;
  QStringList allFiles;
  while (item)
  {
    if (IS_FOLDER(item))
    {
      next=item->firstChild();
      if (next)
      {
        item=next;
        continue;
      }
    }
    if (IS_FILE(item))
    {
      allFiles << (static_cast<ListViewFile *>(item)->fileName);
    }
    next=item->nextSibling();
    while (!next)
    {
      next=item->parent();
      if (next==rootListItem||!next)
      {
        return allFiles;
      }
      item=next;
      next=item->nextSibling();
    }
    item=next;
  }
  return allFiles;
}

//you put in parent, and it gives you the rest, but you must have a place to put it all.
void MainForm::extractFileTreeInfo(QListViewItem *parent,QListViewItem **p_category,QString *p_folderPath)
{
  QListViewItem *item,*next;
  QString tmp=QString::null;
  int o;
  CATEGORY_OF(category,parent);
  *p_category=category;
  item=category->firstChild();
  while (item)
  {
    if (item==parent)
    {
      if (!tmp.isEmpty())
        tmp+='/';
      tmp+=item->text(0);
      *p_folderPath=tmp;
    }
    if (IS_FOLDER(item))
    {
      next=item->firstChild();
      if (next)
      {
        if (tmp.isEmpty())
          tmp=item->text(0);
        else
        {
          tmp+='/';
          tmp+=item->text(0);
        }
        item=next;
        continue;
      }
    }
mfnf_seeknext:
    next=item->nextSibling();
    if (!next)
    {
      next=item->parent();
      if (next==category||!next)
        break;
      item=next;
      o=tmp.findRev('/');
      if (o>=0)
        tmp.truncate(o);
      else
        tmp.truncate(0);
      goto mfnf_seeknext;
    }
    item=next;
  }
}

void MainForm::newFile(QListViewItem *parent, QString text, const QPixmap &pixmap)
{
  QListViewItem *item=NULL, *next=parent->firstChild();
  QString tmp,oldtmp,suffix,caption;
  QStringList allFiles=extractAllFileNames();
  QListViewItem *category;
  KURL tmpK;
  int tryNum;
  for (; IS_FILE(next); next=item->nextSibling())
    item=next;
  extractFileTreeInfo(parent,&category,&tmp);
  
  suffix="";
  if (category==hFilesListItem)
	suffix="h";
  else if (category==cFilesListItem)
    suffix="c";
  else if (category==sFilesListItem)
    suffix="s";
  else if (category==asmFilesListItem)
    suffix="asm";
  else if (category==qllFilesListItem) {
    if (qllFileCount) {
      KMessageBox::error(this,"There may be only one Quill source file in each project.","Quill Error");
      return;
    }
    suffix="qll";
  } else if (category==oFilesListItem)
    suffix="o";
  else if (category==aFilesListItem)
    suffix="a";
  else if (category==txtFilesListItem)
    suffix="txt";
  suffix='.'+suffix;
  
  if (!tmp.isEmpty())
    tmp+='/';
  tmp+="New File";
  tmpK.setPath(projectFileName);
  kurlNewFileName(tmpK,tmp);
  tmp=tmpK.path();
  if (projectFileName.isEmpty())
  {
    short o=0;
    if (tmp[0]=='.')
      o=1;
    if (tmp[o]=='/')
      tmp=tmp.mid(o+1);
  }
  
  caption="New File";
  oldtmp=tmp+' ';
  tmp+=suffix;
  tryNum=1;
  while (!checkFileName(tmp,allFiles))
  {
    tryNum++;
    tmp=oldtmp+QString("%1").arg(tryNum)+suffix;
    caption="New File "+QString("%1").arg(tryNum);
  }
  
  ListViewFile *newFile=item?new ListViewFile(parent,item)
                        :new ListViewFile(parent);
  
  newFile->fileName=tmp;
  if (IS_EDITABLE_CATEGORY(category) && tmp[0]=='/')
    KDirWatch::self()->addFile(tmp);
  
  newFile->setText(0,caption);
  newFile->setPixmap(0,pixmap);
  parent->setOpen(TRUE);
  newFile->kateView=reinterpret_cast<Kate::View *>(createView(tmp,text,category));
  fileTreeClicked(newFile);
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
  newFile->startRename(0);
  
  fileCount++;
  
  COUNTER_FOR_CATEGORY(category)++;
  updateLeftStatusLabel();
}

void MainForm::newFile( QListViewItem *parent )
{
  CATEGORY_OF(category,parent);
  newFile(parent,category==txtFilesListItem?"":
                 ((category==hFilesListItem?"// Header File\n//":
                  category==cFilesListItem?"// C Source File\n//":
                  category==sFilesListItem?"| Assembly Source File\n|":
                  category==asmFilesListItem?"; Assembly Source File\n;":
                  category==qllFilesListItem?"// Quill Source File\n//":"???\n")
                 +QString(" Created ")
                 +QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n"+
                 QString(category==cFilesListItem?(cFileCount?
                 "\n#include <tigcclib.h>\n":
                 "\n// Delete or comment out the items you do not need.\n"
                 "#define COMMENT_STRING         \"Place your comment here.\"\n"
                 "#define COMMENT_PROGRAM_NAME   "
                 "\"Place your program name here.\"\n"
                 "#define COMMENT_VERSION_STRING "
                 "\"Place your version string here.\"\n"
                 "#define COMMENT_VERSION_NUMBER 0,0,0,0 "
                 "/* major, minor, revision, subrevision */\n"
                 "#define COMMENT_AUTHORS        "
                 "\"Place your author name(s) here.\"\n"
                 "#define COMMENT_BW_ICON \\\n"
                 "\t{0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000}\n"
                 "#define COMMENT_GRAY_ICON \\\n"
                 "\t{0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000}, \\\n"
                 "\t{0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000, \\\n"
                 "\t 0b0000000000000000}\n\n#include <tigcclib.h>\n\n"
                 "// Main Function\nvoid _main(void)\n{\n"
                 "\t// Place your code here.\n}\n"):"")),
                 category==cFilesListItem||category==qllFilesListItem
                                                    ?SYSICON("source_c","filec.png"):
                 category==hFilesListItem?SYSICON("source_h","fileh.png"):
                 category==sFilesListItem||category==asmFilesListItem
                                                    ?SYSICON("source_s","files.png"):
                 category==txtFilesListItem?SYSICON("txt","filet.png"):SYSICON("unknown","filex.png"));
}

QListViewItem *MainForm::resolveParent(QListViewItem *category)
{
  QListViewItem *ret=currentListItem;
  if (!IS_FILE(ret)&&!IS_FOLDER(ret))
    return category;
  if (IS_FILE(ret))
    ret=ret->parent();
  QListViewItem *actualCategory=ret;
  while (IS_FOLDER(actualCategory->parent())) actualCategory=actualCategory->parent();
  if (actualCategory!=category)
    return category;
  return ret;
}

void MainForm::fileNewCHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"// Header File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_h","fileh.png"));
}


void MainForm::fileNewGNUAssemblyHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"| Header File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_h","fileh.png"));
}


void MainForm::fileNewA68kAssemblyHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"; Header File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_h","fileh.png"));
}


void MainForm::fileNewCSourceFile()
{
  if (compiling) return;
  newFile(resolveParent(cFilesListItem));
}


void MainForm::fileNewGNUAssemblySourceFile()
{
  if (compiling) return;
  newFile(resolveParent(sFilesListItem),"| Assembly Source File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_s","files.png"));
}


void MainForm::fileNewA68kAssemblySourceFile()
{
  if (compiling) return;
  newFile(resolveParent(asmFilesListItem),"; Assembly Source File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_s","files.png"));
}


void MainForm::fileNewQuillSourceFile()
{
  if (compiling) return;
  newFile(resolveParent(qllFilesListItem),"// Quill Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("source_c","filec.png"));
}


void MainForm::fileNewTextFile()
{
  if (compiling) return;
  newFile(resolveParent(txtFilesListItem),"",SYSICON("txt","filet.png"));
}


void MainForm::updateLeftStatusLabel()
{
  QString text=QString::number(fileCount)+QString(" File")
               +QString(fileCount!=1?"s":"")+QString(" Total");
  if (IS_FOLDER(currentListItem)||IS_FILE(currentListItem)) {
    CATEGORY_OF(category,currentListItem);
    text+=QString(", ")+QString::number(COUNTER_FOR_CATEGORY(category))
          +QString(" in Category");
  }
  leftStatusLabel->setText(text);
}

void MainForm::statusBar_messageChanged(const QString & message)
{
  if (message.isNull())
    // Make sure no labels which should be hidden are shown.
    updateRightStatusLabel();
}

void MainForm::updateRightStatusLabel()
{
  int leftSize=splitter->sizes().first();
  int rightSize=splitter->sizes().last();
  int totalSize=leftSize+rightSize;
  int mySize=size().width();
  int rightStatusSize=rightSize*mySize/totalSize-10>0?
                      rightSize*mySize/totalSize-10:0;
  if (currentListItem==rootListItem) {
    rowStatusLabel->hide();
    colStatusLabel->hide();
    charsStatusLabel->hide();
    rightStatusLabel->setMaximumWidth(rightStatusSize);
    rightStatusLabel->setText(projectFileName);
  } else if (IS_FOLDER(currentListItem)) {
    rowStatusLabel->hide();
    colStatusLabel->hide();
    charsStatusLabel->hide();
    rightStatusLabel->setMaximumWidth(rightStatusSize);
    rightStatusLabel->setText("");
  } else if (IS_FILE(currentListItem)) {
    CATEGORY_OF(category,currentListItem);
    if (IS_EDITABLE_CATEGORY(category) && CURRENT_VIEW) {
      unsigned int line, col;
      CURRENT_VIEW->cursorPositionReal(&line,&col);
      rowStatusLabel->show();
      rowStatusLabel->setMaximumWidth(30);
      rowStatusLabel->setText(QString("%1").arg(line+1));
      colStatusLabel->show();
      colStatusLabel->setMaximumWidth(30);
      colStatusLabel->setText(QString("%1").arg(col+1));
      charsStatusLabel->show();
      charsStatusLabel->setMaximumWidth(100);
      charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
      rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
    } else {
      rowStatusLabel->hide();
      colStatusLabel->hide();
      charsStatusLabel->hide();
      rightStatusLabel->setMaximumWidth(rightStatusSize);
    }
    rightStatusLabel->setText(static_cast<ListViewFile *>(currentListItem)->fileName);
  }
}

void MainForm::current_view_cursorPositionChanged()
{
  if (CURRENT_VIEW) {
    unsigned int line, col;
    CURRENT_VIEW->cursorPositionReal(&line,&col);
    rowStatusLabel->setText(QString("%1").arg(line+1));
    colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void MainForm::current_view_textChanged()
{
  if (CURRENT_VIEW) {
    charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
    if (IS_FILE(currentListItem))
      static_cast<ListViewFile *>(currentListItem)->modifiedSinceLastCompile=TRUE;
    if (preferences.deleteOverwrittenErrors && IS_FILE(currentListItem)
        && CURRENT_VIEW==static_cast<ListViewFile *>(currentListItem)->kateView) {
      ListViewFile *lvFile=static_cast<ListViewFile *>(currentListItem);
      QListViewItemIterator lvit(errorList->errorListView);
      ErrorListItem *errorItem;
      while ((errorItem=static_cast<ErrorListItem *>(lvit.current()))) {
        ++lvit;
        if (errorItem->lvFile==lvFile && errorItem->cursor) {
          unsigned line,col;
          errorItem->cursor->position(&line,&col);
          if (lvFile->kateView->cursorLine()==line
              && lvFile->kateView->cursorColumnReal()==col)
            delete errorItem;
        }
      }
    }
  }
  if (kreplace) kreplace->invalidateSelection();
}

void MainForm::current_view_undoChanged()
{
  if (CURRENT_VIEW) {
    editUndoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->undoCount()));
    editRedoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->redoCount()));
    accel->setItemEnabled(0,!!(CURRENT_VIEW->getDoc()->undoCount()));
    accel->setItemEnabled(1,!!(CURRENT_VIEW->getDoc()->redoCount()));
  }
}

void MainForm::current_view_selectionChanged()
{
  if (CURRENT_VIEW) {
    editClearAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCutAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCopyAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    accel->setItemEnabled(2,CURRENT_VIEW->getDoc()->hasSelection());
    accel->setItemEnabled(3,CURRENT_VIEW->getDoc()->hasSelection());
  }
}

void MainForm::current_view_charactersInteractivelyInserted(int line, int col, const QString &characters)
{
  if (CURRENT_VIEW && preferences.autoBlocks && !characters.compare("{")
      && col==CURRENT_VIEW->getDoc()->lineLength(line)-1) {
    Kate::Document *doc=CURRENT_VIEW->getDoc();
    CATEGORY_OF(category,currentListItem);
    QString fileText=doc->text();
    // Only for C files.
    if (category==cFilesListItem||category==qllFilesListItem
        ||(category==hFilesListItem&&(fileText.isNull()||fileText.isEmpty()||(fileText[0]!='|'&&fileText[0]!=';')))) {
      QString indent=doc->textLine(line);
      // Only if the line was all whitespace, otherwise wait for Enter to be
      // pressed (prevents annoying the user while typing a string or something).
      if (indent.contains(QRegExp("^\\s*\\{$"))) {
        indent=indent.remove('{');
        QString cursorLine=indent+"\t";
        KTextEditor::EditInterfaceExt *editExt=KTextEditor::editInterfaceExt(doc);
        editExt->editBegin();
        doc->insertLine(line+1,cursorLine);
        doc->insertLine(line+2,indent+"}");
        editExt->editEnd();
        CURRENT_VIEW->setCursorPositionReal(line+1,cursorLine.length());
      }
    }
  }
}

void MainForm::current_view_newLineHook()
{
  unsigned line,col;
  CURRENT_VIEW->cursorPositionReal(&line,&col);
  Kate::Document *doc=CURRENT_VIEW->getDoc();
  if (preferences.autoBlocks && line && doc->textLine(line-1).endsWith("{")) {
    CATEGORY_OF(category,currentListItem);
    QString fileText=doc->text();
    // Only for C files.
    if (category==cFilesListItem||category==qllFilesListItem
        ||(category==hFilesListItem&&(fileText.isNull()||fileText.isEmpty()||(fileText[0]!='|'&&fileText[0]!=';')))) {
      QString indent=doc->textLine(line-1);
      // Remove everything starting from the first non-whitespace character.
      indent=indent.remove(QRegExp("(?!\\s).*$"));
      QString cursorLine=indent+"\t";
      KTextEditor::EditInterfaceExt *editExt=KTextEditor::editInterfaceExt(doc);
      editExt->editBegin();
      doc->insertLine(line,cursorLine);
      doc->insertText(line+1,0,indent+"}");
      editExt->editEnd();
      CURRENT_VIEW->setCursorPositionReal(line,cursorLine.length());
    }
  }
}

void MainForm::clipboard_dataChanged()
{
  if (CURRENT_VIEW) {
    editPasteAction->setEnabled(!clipboard->text().isNull());
    accel->setItemEnabled(4,!clipboard->text().isNull());
  }
}

void MainForm::fileTreeItemRenamed( QListViewItem *item, const QString &newName, int col)
{
  if (col)
    return;
  if (item==rootListItem) {
    // validate name, fix if invalid
    QValueList<QChar> validInVarname;
    #define V(i) validInVarname.append(QChar(i))
    #define VR(m,n) for(unsigned i=m;i<=n;i++)V(i)
    VR(48,57); // 0..9
    VR(65,90); // A..Z
    V(95); // _
    VR(97,122); // a..z
    V(181); // mu
    VR(192,214); // À..Ö
    VR(216,246); // Ø..ö
    VR(248,255); // ø..ÿ
    VR(0x3b1,0x3b6);V(0x3b8);V(0x3bb);V(0x3be);V(0x3c1);V(0x3c3);V(0x3c4);
    V(0x3c6);V(0x3c8);V(0x3c9); // small Greek letters
    V(0x393);V(0x394);V(0x3a0);V(0x3a3);V(0x3a9); // capital Greek letters
    #undef VR
    #undef V
    bool hasFolder=false;
    int i;
    QString prjName=newName;
    for (i=prjName.length();i>=0;i--) {
      if (prjName[i]=='\\') {
        if (hasFolder)
          prjName.remove(i,1);
        else
          hasFolder=true;
      } else if (!validInVarname.contains(prjName[i]))
        prjName.remove(i,1);
    }
    if (prjName[0]=='\\') prjName.remove(0,1);
    if (!prjName.length()) prjName="Project1";
    if ((prjName[0]>='0'&&prjName[0]<='9')||prjName[0]=='_')
      prjName.prepend('X');
    i=prjName.find('\\');
    if (i>=0) {
      if (i>8) {
        prjName.remove(8,i-8);
        i=8;
      }
      if ((prjName[i+1]>='0'&&prjName[i+1]<='9')||prjName[i+1]=='_')
        prjName.insert(i+1,'X');
      prjName.truncate(i+9);
      if (prjName.length()==(unsigned)i+1) prjName.append("Project1");
    } else prjName.truncate(8);
    item->setText(0,prjName);
    projectIsDirty=true;
    projectNeedsRelink=TRUE;
    return;
  }
  if (!IS_FILE(item))
    return;
  ListViewFile *theFile=static_cast<ListViewFile *>(item);
  QString suffix;
  QString oldLabel;
  QString &fileNameRef=theFile->fileName;
  QString oldFileName=fileNameRef;
  QString newFileName=fileNameRef;
  int o,s;
  
  o=oldFileName.findRev('.');
  s=oldFileName.findRev('/');
  if (o>=0&&(s<0||o>s)) {
    suffix=oldFileName.mid(o+1);
    newFileName.truncate(o);
  } else {
    suffix=QString::null;
  }
  if (s>=0) {
    oldLabel=newFileName.mid(s+1);
    newFileName.truncate(s+1);
  } else {
    oldLabel=newFileName;
    newFileName.truncate(0);
  }
  if (!oldLabel.compare(newName))
    return; //no changes are needed, and we don't want it to complain about the file conflicting with itself!
  newFileName+=newName;
  newFileName+='.';
  newFileName+=suffix;
  
  if (checkFileName(newFileName,extractAllFileNames())) {
    CATEGORY_OF(category,item);
    if (oldFileName[0]=='/')
      KDirWatch::self()->removeFile(oldFileName);
    if (!theFile->isNew && !QDir().rename(oldFileName,newFileName)) {
      KMessageBox::error(this,"Failed to rename the file.");
      theFile->setText(0,oldLabel);
      if (IS_EDITABLE_CATEGORY(category) && oldFileName[0]=='/')
        KDirWatch::self()->addFile(oldFileName);
    } else {
      fileNameRef=newFileName;
      if (theFile->kateView) {
        // Update the file name for printing.
        unsigned int line,col,hlMode,modified;
        modified=theFile->kateView->getDoc()->isModified();
        QString fileText=theFile->kateView->getDoc()->text();
        hlMode=theFile->kateView->getDoc()->hlMode();
        theFile->kateView->cursorPositionReal(&line,&col);
        theFile->kateView->getDoc()->setModified(FALSE);
        if (theFile->kateView->getDoc()->openStream("text/plain",newFileName))
          theFile->kateView->getDoc()->closeStream();
        SET_TEXT_SAFE(theFile->kateView->getDoc(),fileText);
        theFile->kateView->getDoc()->clearUndo();
        theFile->kateView->getDoc()->clearRedo();
        theFile->kateView->getDoc()->setHlMode(hlMode);
        theFile->kateView->setCursorPositionReal(line,col);
        theFile->kateView->getDoc()->setModified(modified);
      }
      if (IS_EDITABLE_CATEGORY(category) && newFileName[0]=='/')
        KDirWatch::self()->addFile(newFileName);
      projectIsDirty=TRUE;
      projectNeedsRelink=TRUE;
    }
  } else {
    KMessageBox::error(this,"The name you chose conflicts with that of another file.");
    theFile->setText(0,oldLabel);
  }
  
  updateRightStatusLabel();
}

void MainForm::closeEvent(QCloseEvent *e)
{
  if (compiling || savePrompt())
    e->ignore();
  else {
    clearProject();
    e->accept();
  }
}

void MainForm::KDirWatch_dirty(const QString &fileName)
{
  QListViewItem *item=rootListItem->firstChild(),*next;
  QStringList allFiles;
  while (item) {
    if (IS_FOLDER(item)) {
      next=item->firstChild();
      if (next) {
        item=next;
        continue;
      }
    }
    if (IS_FILE(item)) {
      if (!fileName.compare(static_cast<ListViewFile *>(item)->fileName)) {
        CATEGORY_OF(category,item);
        if (!IS_EDITABLE_CATEGORY(category)) {
          qWarning("KDirWatch_dirty called for non-editable file");
          return;
        }
        if (KMessageBox::questionYesNo(this,
              QString("The file \'%1\' has been changed by another program. "
                      "Do you want to reload it?").arg(fileName),"File Changed")
              ==KMessageBox::Yes) {
          QString fileText=loadFileText(fileName);
          if (fileText.isNull()) {
            KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
            return;
          }
          static_cast<ListViewFile *>(item)->isNew=FALSE;
          if (static_cast<ListViewFile *>(item)->kateView) {
            SET_TEXT_SAFE(static_cast<ListViewFile *>(item)->kateView->getDoc(),fileText);
            static_cast<ListViewFile *>(item)->kateView->getDoc()->setModified(FALSE);
            static_cast<ListViewFile *>(item)->kateView->getDoc()->clearUndo();
            static_cast<ListViewFile *>(item)->kateView->getDoc()->clearRedo();
          } else {
            static_cast<ListViewFile *>(item)->textBuffer=fileText;
          }
        }
        return;
      }
    }
    next=item->nextSibling();
    while (!next) {
      next=item->parent();
      if (next==rootListItem||!next) {
        return;
      }
      item=next;
      next=item->nextSibling();
    }
    item=next;
  }
  return;
}

// Yes, this is an ugly hack... Any better suggestions?
#define KListView DnDListView
