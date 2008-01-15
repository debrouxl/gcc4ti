/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2007 Kevin Kofler
   Copyright (C) 2006 Joey Adams
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

#include <k3listview.h>
class DnDListView : public K3ListView {
  private:
  public:
  DnDListView (QWidget * parent = 0) : K3ListView(parent) {}
  // Make my hand-coded drag&drop code work (public part).
  // Maybe the built-in drag&drop support in K3ListView could be made to work as
  // expected, but for now just bypass it to use the existing code I wrote for
  // QListView.
  virtual void takeItem(Q3ListViewItem *i) {Q3ListView::takeItem(i);}
  virtual void setAcceptDrops(bool on) {Q3ListView::setAcceptDrops(on);}
  protected:
  virtual Q3DragObject *dragObject();
  virtual void dropEvent (QDropEvent *e);
  virtual void dragEnterEvent (QDragEnterEvent *e);
  virtual void dragMoveEvent (QDragMoveEvent *e);
  // Make my hand-coded drag&drop code work (protected part).
  virtual void contentsDragMoveEvent(QDragMoveEvent *e) {
    Q3ListView::contentsDragMoveEvent(e);
  }
  virtual void contentsMouseMoveEvent(QMouseEvent *e) {
    Q3ListView::contentsMouseMoveEvent(e);
  }
  virtual void contentsDragLeaveEvent(QDragLeaveEvent *e) {
    Q3ListView::contentsDragLeaveEvent(e);
  }
  virtual void contentsDropEvent(QDropEvent *e) {
    Q3ListView::contentsDropEvent(e);
  }
  virtual void contentsDragEnterEvent(QDragEnterEvent *e) {
    Q3ListView::contentsDragEnterEvent(e);
  }
  virtual void startDrag() {
    Q3ListView::startDrag();
  }
  virtual void rename(Q3ListViewItem *item, int c);
  virtual void keyPressEvent(QKeyEvent *e);
};

// Yes, this is an ugly hack... Any better suggestions?
#define K3ListView DnDListView
#include "mainform.h"
#undef K3ListView

#include <QVariant>
#include <QHash>
#include <QImage>
#include <QString>
#include <QStringList>
#include <QByteArray>
#include <QPair>
#include <QRegExp>
#include <QApplication>
#include <QLabel>
#include <QStatusBar>
#include <QTimer>
#include <QDateTime>
#include <Q3DragObject>
#include <QDir>
#include <QClipboard>
#include <QShortcut>
#include <QEventLoop>
#include <QCoreApplication>
#include <QDockWidget>
#include <QFileInfo>
#include <QDateTime>
#include <QTextCodec>
#include <QTextDocument>
#include <QTimer>
#include <QToolButton>
#include <Q3ListBox>
#include <QTimerEvent>
#include <Q3PtrList>
#include <QList>
#include <QVector>
#include <QDragMoveEvent>
#include <QDragLeaveEvent>
#include <QKeyEvent>
#include <QResizeEvent>
#include <QEvent>
#include <QDropEvent>
#include <Q3PopupMenu>
#include <QDragEnterEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QAssistantClient>
#include <QDBusConnection>
#include <QDBusReply>
#include <QAction>
#include <kapplication.h>
#include <kparts/factory.h>
#include <klibloader.h>
#include <ktexteditor/editor.h>
#include <ktexteditor/document.h>
#include <ktexteditor/view.h>
#include <ktexteditor/cursor.h>
#include <ktexteditor/range.h>
#include <ktexteditor/smartcursor.h>
#include <ktexteditor/smartinterface.h>
#include <ktexteditor/commandinterface.h>
#include <ktexteditor/configinterface.h>
#include <kconfig.h>
#include <kconfiggroup.h>
#include <ktexteditor/configpage.h>
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
#include <kglobal.h>
#include <kicontheme.h>
#include <kicon.h>
#include <kiconloader.h>
#include <kprocess.h>
#include <kshell.h>
#include <ktextbrowser.h>
#include <krun.h>
#include <kpushbutton.h>
#include <kmacroexpander.h>
#include <kstandardaction.h>
#include <kactioncollection.h>
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
#include "srcfilewin.h"
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
#include "completion.h"

#if defined(Q_WS_X11)
#include <kwindowsystem.h>
#define ACTIVATE_WINDOW(winid) KWindowSystem::activateWindow(winid)
#elif defined(Q_WS_WIN)
#include <windows.h>
#define ACTIVATE_WINDOW(winid) SetForegroundWindow(winid)
#else
#define ACTIVATE_WINDOW(winid) ((void)0)
#endif

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
#define CATEGORY_OF(category,item) Q3ListViewItem *category=(item); \
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
#define CURRENT_VIEW (static_cast<KTextEditor::View *>(widgetStack->visibleWidget()))

#define SYSICON(sysname,name) (preferences.useSystemIcons?KIconLoader::global()->loadIcon((sysname),KIconLoader::Small,KIconLoader::SizeSmall):QPixmap(":/images/" name))

#define SET_TEXT_SAFE(doc,text) do { \
    disableViewEvents=TRUE; \
    (doc)->setText((text)); \
    disableViewEvents=FALSE; \
  } while(0)

static Q3ListViewItem *currentListItem;
static Q3ListViewItem *replaceCurrentDocument;
static int replaceCurrentLine;
static bool compiling;
class KReplaceWithSelection : public KReplace {
  public:
    KReplaceWithSelection(const QString &pattern, const QString &replacement,
                          long options, QWidget *parent=0) :
      KReplace(pattern,replacement,options,parent), m_haveSelection(FALSE) {}
    void setSelection(int selStartLine, int selStartCol,
                      int selEndLine, int selEndCol)
    {
      m_haveSelection=TRUE;
      m_selStartLine=selStartLine;
      m_selStartCol=selStartCol;
      m_selEndLine=selEndLine;
      m_selEndCol=selEndCol;
    }
    void setSelection(const KTextEditor::Range &selRange)
    {
      m_haveSelection=TRUE;
      selRange.start().position(m_selStartLine,m_selStartCol);
      selRange.end().position(m_selEndLine,m_selEndCol);
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
    int selStartLine() {return m_selStartLine;}
    int selStartCol() {return m_selStartCol;}
    int selEndLine() {return m_selEndLine;}
    int selEndCol() {return m_selEndCol;}
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
        return (index>=m_selStartCol)&&(index+matchedlength<=m_selEndCol);
      else if (replaceCurrentLine==m_selStartLine)
        return (index>=m_selStartCol);
      else if (replaceCurrentLine==m_selEndLine)
        return (index+matchedlength<=m_selEndCol);
      else
        return (replaceCurrentLine>=m_selStartLine&&replaceCurrentLine<=m_selEndLine);
    }
  private:
    bool m_haveSelection;
    int m_selStartLine, m_selStartCol, m_selEndLine, m_selEndCol;
};
static KReplaceWithSelection *kreplace;

// All the methods are inline because otherwise QT Designer will mistake them
// for slots of the main form.
class ListViewFolder : public K3ListViewItem {
  public:
  ListViewFolder(Q3ListView *parent) : K3ListViewItem(parent)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(Q3ListViewItem *parent) : K3ListViewItem(parent)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(Q3ListView *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
  }
  ListViewFolder(Q3ListViewItem *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after)
  {
    setPixmap(0,SYSICON("folder","folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  virtual int rtti(void) const {return 0x716CC0;}
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. K3ListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<K3ListView *>(listView())->rename(this,col);
  }
  protected:
};

class ListViewFile : public K3ListViewItem {
  public:
  ListViewFile(Q3ListView *parent) : K3ListViewItem(parent),
                                    kateView(NULL), isNew(TRUE),
                                    modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(Q3ListViewItem *parent) : K3ListViewItem(parent),
                                        kateView(NULL), isNew(TRUE),
                                        modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(Q3ListView *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after), kateView(NULL), isNew(TRUE),
            modifiedSinceLastCompile(TRUE)
  {
    setPixmap(0,SYSICON("unknown","filex.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(Q3ListViewItem *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after), kateView(NULL),
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
      replaceCurrentDocument=static_cast<Q3ListViewItem *>(NULL);
      kreplace->invalidateSelection();
    }
    if (!fileName.isEmpty() && fileName[0]=='/')
      KDirWatch::self()->removeFile(fileName);
    if (kateView) {
      KTextEditor::Document *doc=kateView->document();
      delete kateView;
      delete doc;
    }
  }
  virtual int rtti(void) const {return 0x716CC1;}
  KTextEditor::View *kateView;
  QString fileName; // full name of the file
  bool isNew;
  bool modifiedSinceLastCompile;
  LineStartList lineStartList;
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. K3ListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<K3ListView *>(listView())->rename(this,col);
  }
  protected:
};

class ListViewRoot : public K3ListViewItem {
  public:
  ListViewRoot(Q3ListView *parent) : K3ListViewItem(parent)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(Q3ListViewItem *parent) : K3ListViewItem(parent)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(Q3ListView *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  ListViewRoot(Q3ListViewItem *parent, Q3ListViewItem *after)
          : K3ListViewItem(parent, after)
  {
    setRenameEnabled(0,TRUE);
    // dragging not really allowed, but don't let the cursor run around when dragged
    setDragEnabled(TRUE);
  }
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. K3ListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<K3ListView *>(listView())->rename(this,col);
  }
  protected:
};

// These should be instance variables in clean C++, but QT Designer won't let me
// touch the class definition, so this is all I can do. And there is only one
// instance of MainForm anyway.
static Q3ListViewItem *rootListItem;
static Q3ListViewItem *hFilesListItem;
static Q3ListViewItem *cFilesListItem;
static Q3ListViewItem *sFilesListItem;
static Q3ListViewItem *asmFilesListItem;
static Q3ListViewItem *qllFilesListItem;
static Q3ListViewItem *oFilesListItem;
static Q3ListViewItem *aFilesListItem;
static Q3ListViewItem *txtFilesListItem;
static Q3ListViewItem *othFilesListItem;
static bool projectIsDirty, projectNeedsRelink;
static QLabel *leftStatusLabel;
static QLabel *rowStatusLabel;
static QLabel *colStatusLabel;
static QLabel *charsStatusLabel;
static QLabel *rightStatusLabel;
static KHelpMenu *khelpmenu;
static Q3PopupMenu *te_popup;
static bool headersModified;
static QDockWidget *errorListDock;
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
static QShortcut *shortcuts[18];
static KFindDialog *kfinddialog;
QStringList findHistory, replacementHistory;
static Q3ListViewItem *findCurrentDocument;
static int findCurrentLine;
QList<SourceFileWindow *> sourceFiles;
static Q3PopupMenu *findFunctionsPopup;
bool have_usb;
Tools tools, tempTools;
int toolIndex;
bool disableViewEvents=FALSE;

Q3DragObject *DnDListView::dragObject() {
  Q3ListViewItem *currItem=selectedItem();
  if (currItem==rootListItem || currItem->parent()==rootListItem)
    return NULL;
  Q3StoredDrag *storedDrag=new Q3StoredDrag("x-ktigcc-dnd", this);
  static QByteArray data(sizeof(Q3ListViewItem*));
  data.duplicate(reinterpret_cast<char *>(&currItem),
                 sizeof(Q3ListViewItem*));
  storedDrag->setEncodedData(data);
  return storedDrag;
}

void DnDListView::dropEvent(QDropEvent *e) {
  if (!compiling && e->source()==this && e->provides("x-ktigcc-dnd")) {
    Q3ListViewItem *currItem;
    currItem = *reinterpret_cast<Q3ListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
    if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
      // dropping folder
      // can only drop on folder or category
      Q3ListViewItem *item=itemAt(e->pos());
      if (IS_FOLDER(item)) {
        // need same category
        CATEGORY_OF(srcCategory,currItem);
        CATEGORY_OF(destCategory,item);
        if (srcCategory == destCategory) {
          // can't move folder into itself
          for (Q3ListViewItem *destFolder=item; IS_FOLDER(destFolder); destFolder=destFolder->parent()) {
            if (destFolder==currItem) goto ignore;
          }
          // move folder
          e->accept();
          currItem->parent()->takeItem(currItem);
          item->insertItem(currItem);
          // put it at the right place
          if (currItem->nextSibling()) {
            Q3ListViewItem *lastItem=currItem->nextSibling();
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
      Q3ListViewItem *item=itemAt(e->pos());
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
            if (!static_cast<ListViewFile *>(currItem)->fileName.isEmpty()
                && static_cast<ListViewFile *>(currItem)->fileName[0]=='/')
              KDirWatch::self()->removeFile(static_cast<ListViewFile *>(currItem)->fileName);
          }
          // moving from non-editable to editable category
          if (!IS_EDITABLE_CATEGORY(srcCategory)
              && IS_EDITABLE_CATEGORY(destCategory)) {
            static_cast<ListViewFile *>(currItem)->kateView=reinterpret_cast<KTextEditor::View *>(static_cast<MainForm *>(parent()->parent()->parent())->createView(static_cast<ListViewFile *>(currItem)->fileName,QString::null,destCategory));
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
            Q3ListViewItem *lastItem=currItem->nextSibling();
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
            if (!static_cast<ListViewFile *>(currItem)->fileName.isEmpty()
                && static_cast<ListViewFile *>(currItem)->fileName[0]=='/')
              KDirWatch::self()->addFile(static_cast<ListViewFile *>(currItem)->fileName);
          }
          // moving from editable to non-editable category
          if (IS_EDITABLE_CATEGORY(srcCategory)
              && !IS_EDITABLE_CATEGORY(destCategory)) {
            KTextEditor::Document *doc=static_cast<ListViewFile *>(currItem)->kateView->document();
            delete static_cast<ListViewFile *>(currItem)->kateView;
            delete doc;
            static_cast<ListViewFile *>(currItem)->kateView=NULL;
          }
          // moving from editable to editable category
          if (IS_EDITABLE_CATEGORY(srcCategory)
              && IS_EDITABLE_CATEGORY(destCategory)
              && srcCategory!=destCategory) {
            // update highlighting mode
            QString fileText=static_cast<ListViewFile *>(currItem)->kateView->document()->text();
            static_cast<ListViewFile *>(currItem)->kateView->document()->setHighlightingMode(
              (destCategory==qllFilesListItem?
                QLL_HL_MODE:
              (destCategory==sFilesListItem||(destCategory==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
                S_HL_MODE:
              (destCategory==asmFilesListItem||(destCategory==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
                ASM_HL_MODE:
              (destCategory==cFilesListItem||destCategory==hFilesListItem)?
                C_HL_MODE:
              "None"));
          }
          // update icon
          currItem->setPixmap(0,
            destCategory==cFilesListItem||destCategory==qllFilesListItem?SYSICON("text-x-csrc","filec.png"):
            destCategory==hFilesListItem?SYSICON("text-x-chdr","fileh.png"):
            destCategory==sFilesListItem||destCategory==asmFilesListItem?SYSICON("text-x-hex","files.png"):
            destCategory==txtFilesListItem?SYSICON("text-plain","filet.png"):
            destCategory==oFilesListItem||destCategory==aFilesListItem?SYSICON("application-x-object","fileo.png"):
            SYSICON("unknown","filex.png"));
        }
      } else if (IS_FILE(item)) {
        // drop on file
        // need same parent, but different items
        if (currItem->parent() == item->parent()
            && currItem != item) {
          // reorder files
          // figure out which one is the first
          for (Q3ListViewItem *i=currItem->parent()->firstChild();i;
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

void DnDListView::dragEnterEvent(QDragEnterEvent *e) {
  if (!compiling && e->source()==this&&(e->provides("x-ktigcc-dnd")))
        e->accept();
}

void DnDListView::dragMoveEvent(QDragMoveEvent *e) {
  if (!compiling && e->source()==this && e->provides("x-ktigcc-dnd")) {
    Q3ListViewItem *currItem;
    currItem = *reinterpret_cast<Q3ListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
    if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
      // dropping folder
      // can only drop on folder or category
      Q3ListViewItem *item=itemAt(e->pos());
      if (IS_FOLDER(item)) {
        // need same category
        CATEGORY_OF(srcCategory,currItem);
        CATEGORY_OF(destCategory,item);
        if (srcCategory == destCategory) {
          // can't move folder into itself
          for (Q3ListViewItem *destFolder=item; IS_FOLDER(destFolder); destFolder=destFolder->parent()) {
            if (destFolder==currItem) goto ignore;
          }
          e->accept();
        } else {ignore: e->ignore();}
      } else e->ignore();
    } else if (IS_FILE(currItem)) {
      // dropping file
      Q3ListViewItem *item=itemAt(e->pos());
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

// K3ListView::rename won't work properly if I don't do this. :-/
void DnDListView::rename(Q3ListViewItem *item, int c) {
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,1000);
  ensureItemVisible(item);
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,1000);
  K3ListView::rename(item,c);
}

void DnDListView::keyPressEvent(QKeyEvent *e)
{
  if (e->key()==Qt::Key_Delete && e->state()==0) {
    Q3ListViewItem *item=currentItem();
    if (item)
      static_cast<MainForm *>(parent()->parent()->parent())->removeItem(item);
    e->accept();
  } else QWidget::keyPressEvent(e);
}

enum ErrorTypes {etError, etWarning, etInfo};
class ErrorListItem : public K3ListViewItem {
  public:
  ErrorListItem(MainForm *pMainForm, ErrorTypes errType,
                const QString &errFile, const QString &errFunc,
                const QString &errMsg, int errLine, int errColumn)
    : K3ListViewItem(errorList->errorListView,
                    errorList->errorListView->lastItem()),
      lvFile(0), srcFile(0), cursor(0), errorLine(-1), errorColumn(0),
      mainForm(pMainForm), errorType(errType)
  {
    QString errMessage=errMsg.trimmed();
    if (!errMessage.isEmpty()) errMessage[0]=errMessage[0].toUpper();
    switch(errType) {
      case etError:
        setPixmap(0,SYSICON("dialog-error","error.png"));
        break;
      case etWarning:
        setPixmap(0,SYSICON("dialog-warning","warning.png"));
        break;
      default:
        setPixmap(0,SYSICON("dialog-information","info.png"));
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
    if (errLine!=-1 && (lvFile || srcFile)) {
      if (errColumn==-1) errColumn=0;
      const LineStartList &lineStartList=lvFile?lvFile->lineStartList
                                               :srcFile->lineStartList;
      if (lineStartList.isEmpty()) {
        errorLine=errLine;
        errorColumn=errColumn;
      } else if (errLine<lineStartList.count()) {
        QPair<unsigned,unsigned> pos=lineStartList[errLine];
        errorLine=(int)pos.first;
        errorColumn=(int)pos.second+errColumn;
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
    if (errorLine!=-1) {
      KTextEditor::View *kateView=lvFile?lvFile->kateView:(srcFile?srcFile->kateView
                             :static_cast<KTextEditor::View *>(NULL));
      if (kateView && errorLine<kateView->document()->lines()) {
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
              int i=errorColumn;
              QString textLine=kateView->document()->line(errorLine);
              int lineLength=kateView->document()->lineLength(errorLine);
              int tokenLength=token.length();
              while ((i<lineLength) && textLine[i].isSpace()
                     && textLine.mid(i,tokenLength)!=token) i++;
              if (textLine.mid(i,tokenLength)==token) errorColumn=i;
            }
          }
        }
        KTextEditor::SmartInterface* smart =
          qobject_cast<KTextEditor::SmartInterface*>(kateView->document());
        cursor=smart->newSmartCursor(errorLine,errorColumn);
      }
    }
  }
  void jumpToLocation(void)
  {
    // If the error corresponds to a list view file, select it. This will also
    // instantiate the Kate view and call createCursor for us.
    if (lvFile) mainForm->fileTreeClicked(lvFile);
    // If it corresponds to an external source file, activate the window.
    if (srcFile) ACTIVATE_WINDOW(srcFile->winId());
    // Now jump to the cursor's location if we have one.
    KTextEditor::View *kateView=lvFile?lvFile->kateView:(srcFile?srcFile->kateView
                           :static_cast<KTextEditor::View *>(NULL));
    if (cursor && kateView)
      kateView->setCursorPosition(*cursor);
  }
  ListViewFile *lvFile;
  SourceFileWindow *srcFile;
  KTextEditor::SmartCursor *cursor;
  private:
  int errorLine;
  int errorColumn;
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
        srcFile=reinterpret_cast<SourceFileWindow *>(sourceFile);
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
void MainForm::deleteErrorsForLVFile(Q3ListViewItem *item)
{
  Q3ListViewItemIterator lvit(errorList->errorListView);
  Q3ListViewItem *errorItem;
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
  Q3ListViewItemIterator lvit(errorList->errorListView);
  Q3ListViewItem *errorItem;
  while ((errorItem=lvit.current())) {
    ++lvit;
    if (static_cast<ErrorListItem *>(errorItem)->srcFile
        ==reinterpret_cast<SourceFileWindow *>(srcFile))
      delete errorItem;
  }
}

// And another.
void MainForm::createErrorCursorsForSourceFile(Q3ListViewItem *item)
{
  Q3ListViewItemIterator lvit(errorList->errorListView);
  Q3ListViewItem *errorItem;
  for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
    if (static_cast<ErrorListItem *>(errorItem)->lvFile
        ==static_cast<ListViewFile *>(item))
      static_cast<ErrorListItem *>(errorItem)->createCursor();
  }
}

// And the last.
void MainForm::deleteOverwrittenErrorsIn(void *srcFile)
{
  SourceFileWindow *sourceFile=reinterpret_cast<SourceFileWindow *>(srcFile);
  Q3ListViewItemIterator lvit(errorList->errorListView);
  ErrorListItem *errorItem;
  while ((errorItem=static_cast<ErrorListItem *>(lvit.current()))) {
    ++lvit;
    if (errorItem->srcFile==sourceFile && errorItem->cursor) {
      int line,col;
      errorItem->cursor->position(line,col);
      int curline,curcol;
      sourceFile->kateView->cursorPosition().position(curline,curcol);
      if (curline==line && curcol==col)
        delete errorItem;
    }
  }
}

void MainForm::errorListView_clicked(Q3ListViewItem *item)
{
  if (item) {
    static_cast<ErrorListItem *>(item)->jumpToLocation();
    errorList->setFocus();
  }
}

bool MainForm::findSourceFile(bool &inProject, void *&srcFile, const QString &fileName)
{
  bool compareAbsPaths=fileName.contains('/');
  Q3ListViewItemIterator lvit(fileTree);
  Q3ListViewItem *item;
  for (item=lvit.current();item;item=(++lvit).current()) {
    if (IS_FILE(item)
        && (compareAbsPaths?fileName==static_cast<ListViewFile *>(item)->fileName
                           :fileName==QFileInfo(static_cast<ListViewFile *>(item)->fileName).fileName())) {
      inProject=TRUE;
      srcFile=static_cast<ListViewFile *>(item);
      return TRUE;
    }
  }
  foreach (SourceFileWindow *sourceFile, sourceFiles) {
    if (compareAbsPaths?fileName==sourceFile->fileName
                       :fileName==QFileInfo(sourceFile->fileName).fileName()) {
      inProject=FALSE;
      srcFile=sourceFile;
      return TRUE;
    }
  }
  return FALSE;
}

void sendCommand(KTextEditor::View *view, const QString &cmd)
{
  KTextEditor::CommandInterface *cmdIface=
    qobject_cast<KTextEditor::CommandInterface*>(view->document()->editor());
  KTextEditor::Command *command=cmdIface->queryCommand(cmd);
  QString msg; // thrown away
  command->exec(view,cmd,msg);
}

void setTabWidth(KTextEditor::View *view, unsigned tabWidth)
{
  sendCommand(view,QString("set-tab-width %1").arg(tabWidth));
}

MainForm::MainForm(QWidget* parent, const char* name, Qt::WindowFlags fl)
  : QMainWindow(parent, name, fl)
{
  setupUi(this);

  (void)statusBar();

  QPixmap smallIcon(":/images/ktigcc.png");
  QPixmap largeIcon(":/images/icon.png");
  QIcon windowIcon(smallIcon);
  windowIcon.addPixmap(largeIcon);
  setWindowIcon(windowIcon);
#ifdef Q_WS_X11
  KWindowSystem::setIcons(winId(),largeIcon,smallIcon);
#endif
  ticables_library_init();
  tifiles_library_init();
  ticalcs_library_init();
  have_usb=ticables_is_usb_enabled();
  compiling=FALSE;
  headersModified=FALSE;
  loadPreferences();
  loadSystemHeaderCompletion();
  fileNewFolderAction->setEnabled(FALSE);
  te_popup = new Q3PopupMenu(this);
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
  QList<int> list;
  list.append(150);
  list.append(500);
  splitter->setSizes(list);
  leftStatusLabel=new QLabel("0 Files Total",this);
  leftStatusLabel->setMaximumWidth(splitter->sizes().first());
  statusBar()->addWidget(leftStatusLabel,1);
  rowStatusLabel=new QLabel("",this);
  rowStatusLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  statusBar()->addWidget(rowStatusLabel,1);
  rowStatusLabel->hide();
  colStatusLabel=new QLabel("",this);
  colStatusLabel->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
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
  fileTree->setColumnWidthMode(0,Q3ListView::Maximum);
  fileTree->header()->hide();
  fileTree->setAlternateBackground(QColor());
  rootListItem=new ListViewRoot(fileTree);
  rootListItem->setText(0,"Project1");
  rootListItem->setPixmap(0,SYSICON("system-run","tpr.png"));
  rootListItem->setOpen(TRUE);
  Q3ListViewItem *folderListItem=new ListViewFolder(rootListItem);
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
    asmFilesListItem=NULL;
    fileNewA68kAssemblyHeaderAction->setVisible(FALSE);
    fileNewA68kAssemblySourceFileAction->setVisible(FALSE);
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
  assistant=new QAssistantClient("",this);
  QStringList args(QString("-profile"));
  args.append(QString("%1/doc/html/qt-assistant.adp").arg(tigcc_base));
  assistant->setArguments(args);
  lastDirectory=QString("%1/tigcc-projects").arg(QDir::homePath());
  if (!QDir(lastDirectory).exists() && !QDir().mkdir(lastDirectory))
    lastDirectory=QString("%1/projects").arg(tigcc_base);
  projectFileName="";
  projectIsDirty=FALSE;
  projectNeedsRelink=FALSE;
  connect(KDirWatch::self(),SIGNAL(created(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  connect(KDirWatch::self(),SIGNAL(dirty(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  KDirWatch::self()->startScan();
  clipboard=QApplication::clipboard();
  connect(clipboard,SIGNAL(dataChanged()),this,SLOT(clipboard_dataChanged()));
  shortcuts[0]=new QShortcut(Qt::ALT+Qt::Key_Backspace,this);
  shortcuts[0]->setEnabled(FALSE);
  shortcuts[1]=new QShortcut(Qt::SHIFT+Qt::ALT+Qt::Key_Backspace,this);
  shortcuts[1]->setEnabled(FALSE);
  shortcuts[2]=new QShortcut(Qt::SHIFT+Qt::Key_Delete,this);
  shortcuts[2]->setEnabled(FALSE);
  shortcuts[3]=new QShortcut(Qt::CTRL+Qt::Key_Insert,this);
  shortcuts[3]->setEnabled(FALSE);
  shortcuts[4]=new QShortcut(Qt::SHIFT+Qt::Key_Insert,this);
  shortcuts[4]->setEnabled(FALSE);
  shortcuts[5]=new QShortcut(Qt::Key_F1,this);
  shortcuts[5]->setEnabled(FALSE);
  shortcuts[6]=new QShortcut(Qt::Key_Enter,this);
  shortcuts[6]->setEnabled(FALSE);
  shortcuts[7]=new QShortcut(Qt::Key_Return,this);
  shortcuts[7]->setEnabled(FALSE);
  shortcuts[8]=new QShortcut(Qt::CTRL+Qt::Key_J,this);
  shortcuts[8]->setEnabled(FALSE);
  shortcuts[9]=new QShortcut(Qt::CTRL+Qt::Key_Space,this);
  shortcuts[9]->setEnabled(FALSE);
  shortcuts[10]=new QShortcut(Qt::CTRL+Qt::Key_M,this);
  shortcuts[10]->setEnabled(FALSE);
  shortcuts[11]=new QShortcut(Qt::CTRL+Qt::Key_Tab,this);
  shortcuts[11]->setEnabled(TRUE);
  shortcuts[12]=new QShortcut(Qt::CTRL+Qt::Key_G,this);
  shortcuts[12]->setEnabled(TRUE);
  shortcuts[13]=new QShortcut(Qt::SHIFT+Qt::CTRL+Qt::ALT+Qt::Key_F9,this);
  shortcuts[13]->setEnabled(TRUE);
  shortcuts[14]=new QShortcut(Qt::SHIFT+Qt::ALT+Qt::Key_F8,this);
  shortcuts[14]->setEnabled(TRUE);
  shortcuts[15]=new QShortcut(Qt::CTRL+Qt::ALT+Qt::Key_F9,this);
  shortcuts[15]->setEnabled(TRUE);
  shortcuts[16]=new QShortcut(Qt::CTRL+Qt::Key_F9,this);
  shortcuts[16]->setEnabled(TRUE);
  shortcuts[17]=new QShortcut(Qt::CTRL+Qt::Key_Return,this);
  shortcuts[17]->setEnabled(FALSE);
  connect(shortcuts[0],SIGNAL(activated()),this,SLOT(shortcut_0_activated()));
  connect(shortcuts[1],SIGNAL(activated()),this,SLOT(shortcut_1_activated()));
  connect(shortcuts[2],SIGNAL(activated()),this,SLOT(shortcut_2_activated()));
  connect(shortcuts[3],SIGNAL(activated()),this,SLOT(shortcut_3_activated()));
  connect(shortcuts[4],SIGNAL(activated()),this,SLOT(shortcut_4_activated()));
  connect(shortcuts[5],SIGNAL(activated()),this,SLOT(shortcut_5_activated()));
  connect(shortcuts[6],SIGNAL(activated()),this,SLOT(shortcut_6_activated()));
  connect(shortcuts[7],SIGNAL(activated()),this,SLOT(shortcut_7_activated()));
  connect(shortcuts[8],SIGNAL(activated()),this,SLOT(shortcut_8_activated()));
  connect(shortcuts[9],SIGNAL(activated()),this,SLOT(shortcut_9_activated()));
  connect(shortcuts[10],SIGNAL(activated()),this,SLOT(shortcut_10_activated()));
  connect(shortcuts[11],SIGNAL(activated()),this,SLOT(shortcut_11_activated()));
  connect(shortcuts[12],SIGNAL(activated()),this,SLOT(shortcut_12_activated()));
  connect(shortcuts[13],SIGNAL(activated()),this,SLOT(shortcut_13_activated()));
  connect(shortcuts[14],SIGNAL(activated()),this,SLOT(shortcut_14_activated()));
  connect(shortcuts[15],SIGNAL(activated()),this,SLOT(shortcut_15_activated()));
  connect(shortcuts[16],SIGNAL(activated()),this,SLOT(shortcut_16_activated()));
  connect(shortcuts[17],SIGNAL(activated()),this,SLOT(findOpenFileAtCursor()));
  kfinddialog = static_cast<KFindDialog *>(NULL);
  kreplace = static_cast<KReplaceWithSelection *>(NULL);
  connect(fileNewAction,SIGNAL(triggered()),this,SLOT(fileNewProject()));
  if (preferences.useSystemIcons) {
    // Set the preferred icon size so system toolbar icons don't get annoying
    // padding.
    int toolbarIconSize=KIconLoader().currentSize(KIconLoader::MainToolbar);
    setIconSize(QSize(toolbarIconSize,toolbarIconSize));
    fileNewAction->setIcon(KIcon("document-new"));
    fileNewMenu->menuAction()->setIcon(KIcon("document-new"));
    fileOpenAction->setIcon(KIcon("document-open"));
    fileSaveAllAction->setIcon(KIcon("document-save"));
    filePrintAction->setIcon(KIcon("document-print"));
    filePrintQuicklyAction->setIcon(KIcon("document-print"));
    editClearAction->setIcon(KIcon("edit-delete"));
    editCutAction->setIcon(KIcon("edit-cut"));
    editCopyAction->setIcon(KIcon("edit-copy"));
    editPasteAction->setIcon(KIcon("edit-paste"));
    projectAddFilesAction->setIcon(KIcon("list-add"));
    projectCompileAction->setIcon(KIcon("run-build-file"));
    projectMakeAction->setIcon(KIcon("run-build"));
    projectBuildAction->setIcon(KIcon("view-refresh"));
    helpContentsAction->setIcon(KIcon("help-contents"));
    helpDocumentationAction->setIcon(KIcon("help-contents"));
    helpSearchAction->setIcon(KIcon("system-search"));
    findFindAction->setIcon(KIcon("edit-find"));
    findReplaceAction->setIcon(KIcon("edit-find-replace"));
    helpIndexAction->setIcon(KIcon("view-list-text"));
    editUndoAction->setIcon(KIcon("edit-undo"));
    editRedoAction->setIcon(KIcon("edit-redo"));
    findFunctionsAction->setIcon(KIcon("view-list-tree"));
    editIncreaseIndentAction->setIcon(KIcon("format-indent-more"));
    editDecreaseIndentAction->setIcon(KIcon("format-indent-less"));
    projectStopCompilationAction->setIcon(KIcon("process-stop"));
    projectForceQuitAction->setIcon(KIcon("dialog-cancel"));
    helpNewsAction->setIcon(KIcon("view-pim-news"));
    debugRunAction->setIcon(KIcon("media-playback-start"));
    debugPauseAction->setIcon(KIcon("media-playback-pause"));
    toolsConfigureAction->setIcon(KIcon("configure"));
    debugResetAction->setIcon(KIcon("media-playback-stop"));
  } else fileNewMenu->menuAction()->setIcon(QIcon(QPixmap(":/images/00.png")));
  QToolButton *fileNewButton=static_cast<QToolButton *>(toolBar
    ->widgetForAction(fileNewAction));
  fileNewButton->setPopupMode(QToolButton::MenuButtonPopup);
  fileNewButton->setMenu(fileNewMenu);
  QToolButton *fileOpenButton=static_cast<QToolButton *>(toolBar
    ->widgetForAction(fileOpenAction));
  QMenu *fileOpenMenu=new QMenu(this);
  fileOpenMenu->setObjectName(QString::fromUtf8("fileOpenMenu"));
  fileOpenMenu->addAction(fileRecent1Action);
  fileOpenMenu->addAction(fileRecent2Action);
  fileOpenMenu->addAction(fileRecent3Action);
  fileOpenMenu->addAction(fileRecent4Action);
  fileOpenButton->setPopupMode(QToolButton::MenuButtonPopup);
  fileOpenButton->setMenu(fileOpenMenu);
  QToolButton *findFunctionsButton=static_cast<QToolButton *>(toolBar
    ->widgetForAction(findFunctionsAction));
  findFunctionsPopup=new Q3PopupMenu(findFunctionsButton);
  connect(findFunctionsPopup,SIGNAL(aboutToShow()),
          this,SLOT(findFunctionsPopup_aboutToShow()));
  connect(findFunctionsPopup,SIGNAL(aboutToHide()),
          this,SLOT(findFunctionsPopup_aboutToHide()));
  connect(findFunctionsPopup,SIGNAL(activated(int)),
          this,SLOT(findFunctionsPopup_activated(int)));
  findFunctionsButton->setPopupMode(QToolButton::MenuButtonPopup);
  findFunctionsButton->setMenu(findFunctionsPopup);
  errorListDock=new QDockWidget("Errors and Warnings",this);
  errorListDock->setFloating(false);
  errorListDock->setFeatures(QDockWidget::DockWidgetClosable);
  errorListDock->setAllowedAreas(Qt::BottomDockWidgetArea);
  errorList=new ErrorList(errorListDock);
  errorListDock->setWidget(errorList);
  addDockWidget(Qt::BottomDockWidgetArea,errorListDock);
  errorList->show();
  errorListDock->hide();
  connect(errorListDock,SIGNAL(visibilityChanged(bool)),
          this,SLOT(projectErrorsAndWarnings(bool)));
  errorList->errorListView->setSorting(-1);
  errorList->errorListView->setAlternateBackground(QColor());
  connect(errorList->errorListView,SIGNAL(clicked(Q3ListViewItem *)),
          this,SLOT(errorListView_clicked(Q3ListViewItem *)));
  if (preferences.linkTarget!=LT_TIEMU) {
    debugPauseAction->setEnabled(FALSE);
    debugResetAction->setEnabled(FALSE);
  }
  if (preferences.linkTarget==LT_NONE) {
    menuBar()->setItemVisible(5,FALSE); //debugMenu
    debugRunAction->setVisible(FALSE);
    debugPauseAction->setVisible(FALSE);
  }
  KConfigGroup recentFiles=pconfig->group("Recent files");
  if (parg) {
    QString fileName=QDir().absoluteFilePath(parg);
    if (!openProject(fileName)) goto openRecent;
  } else {
    openRecent:;
    QString mostrecent=recentFiles.readEntry("Current project");
    if (!mostrecent.isNull() && !mostrecent.isEmpty())
      openProject(mostrecent);
  }
  updateRecent();
  KConfigGroup toolsConfig=pconfig->group("Tools");
  unsigned toolCount=toolsConfig.readEntry("Count",0u);
  tools.resize(toolCount);
  for (unsigned idx=0; idx<toolCount; idx++) {
    KConfigGroup toolConfig=pconfig->group(QString("Tool %1").arg(idx));
    Tool &tool=tools[idx];
    tool.title=toolConfig.readEntry("Title");
    tool.commandLine=toolConfig.readEntry("Command Line");
    tool.workingDirectory=toolConfig.readEntry("Working Directory");
    tool.runInTerminal=toolConfig.readEntry("Terminal",false);
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

MainForm::~MainForm()
{
  while (!sourceFiles.isEmpty()) {
    delete sourceFiles.first();
  }
  if (kreplace) delete kreplace;
  if (kfinddialog) delete kfinddialog;
  for (int i=0; i<18; i++) delete shortcuts[i];
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

void MainForm::shortcutActivated(int index)
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
        int line,col,i;
        CURRENT_VIEW->cursorPosition().position(line,col);
        QString textLine=CURRENT_VIEW->document()->line(line);
        QString wordUnderCursor;
        for (i=col-1;i>=0;i--) {
          QChar c=textLine[i];
          if ((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9')
              || c=='_' || c=='$' || c=='#')
            wordUnderCursor.prepend(c);
        }
        int len=textLine.length();
        for (i=col;i<len;i++) {
          QChar c=textLine[i];
          if ((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9')
              || c=='_' || c=='$' || c=='#')
            wordUnderCursor.append(c);
        }
        // always open at least the index
        force_qt_assistant_page(1);
        assistant->openAssistant();
        if (wordUnderCursor.isEmpty()) return;
        QString docFile=lookup_doc_keyword(wordUnderCursor);
        if (docFile.isEmpty()) return;
        // wait for Qt Assistant to actually open
        while (!assistant->isOpen())
          QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,1000);
        assistant->showPage(QString(tigcc_base)+QString("/doc/html/")+docFile);
        break;
      }
      case 6:
      case 7:
        // keyReturn is not in any interface, but it's a public slot...
        CURRENT_VIEW->qt_metacall(QMetaObject::InvokeMetaMethod,
          CURRENT_VIEW->metaObject()->indexOfMethod("keyReturn()"),NULL);
        current_view_newLineHook();
        break;
      case 8:
        new TemplatePopup(CURRENT_VIEW);
        break;
      case 9:
      case 10:
        if (IS_FILE(currentListItem)
            && CURRENT_VIEW==static_cast<ListViewFile *>(currentListItem)->kateView) {
          QString fileText=CURRENT_VIEW->document()->text();
          CATEGORY_OF(category,currentListItem);
          // Completion only operates on C files.
          if (category==cFilesListItem || category==qllFilesListItem
              || (category==hFilesListItem && !fileText.isEmpty()
                  && fileText[0]!='|' && fileText[0]!=';')) {
            // Disable newLineHook.
            shortcuts[6]->setEnabled(FALSE);
            shortcuts[7]->setEnabled(FALSE);
            new CompletionPopup(CURRENT_VIEW,pathInProject(currentListItem),this,this);
          }
        }
        break;
      case 11: // next file
      case 12:
      case_11:
      {
        Q3ListViewItem *item=currentListItem;
        if (!item) item=rootListItem;
        Q3ListViewItem *origItem=item;
        do {
          item=item->itemBelow();
          if (!item) item=rootListItem;
          if (item==origItem) return; // no suitable items to select
        } while (item==rootListItem || IS_CATEGORY(item));
        fileTreeClicked(item);
        if (CURRENT_VIEW) CURRENT_VIEW->setFocus();
        break;
      }
      case 13: // switch transfer target
      case 14:
      case_13:
      {
        preferences.linkTarget=(preferences.linkTarget==LT_TIEMU)?LT_REALCALC:LT_TIEMU;
        savePreferences();
        // Apply the preferences to the debug menu.
        debugPauseAction->setEnabled(!compiling&&preferences.linkTarget==LT_TIEMU);
        debugResetAction->setEnabled(!compiling&&preferences.linkTarget==LT_TIEMU);
        bool runnable=!settings.archive&&!settings.flash_os&&preferences.linkTarget!=LT_NONE;
        menuBar()->setItemVisible(5,runnable); //debugMenu
        debugRunAction->setVisible(runnable);
        debugPauseAction->setVisible(runnable);
        break;        
      }
      case 15: projectCompile(); break;
      case 16: projectMake(); break;
      default: break;
    }
  } else {
    switch (index) {
      case 6:
      case 7:
      {
        QKeyEvent *keyEvent=new QKeyEvent(QEvent::KeyPress,Qt::Key_Return,'\n',0,"\n");
        QApplication::postEvent(focusWidget(),keyEvent);
        break;
      }
      case 11: goto case_11;
      case 12: goto case_11;
      case 13: goto case_13;
      case 14: goto case_13;
      case 15: projectCompile(); break;
      case 16: projectMake(); break;
      default: break;
    }
  }
}

void MainForm::completionPopup_closed()
{
  if (IS_FILE(currentListItem)) {
    CATEGORY_OF(category,currentListItem->parent());
    if (IS_EDITABLE_CATEGORY(category)) {
      // Restore newLineHook.
      shortcuts[6]->setEnabled(TRUE);
      shortcuts[7]->setEnabled(TRUE);
    }
  }
}

void MainForm::shortcut_0_activated()
{
  shortcutActivated(0);
}

void MainForm::shortcut_1_activated()
{
  shortcutActivated(1);
}

void MainForm::shortcut_2_activated()
{
  shortcutActivated(2);
}

void MainForm::shortcut_3_activated()
{
  shortcutActivated(3);
}

void MainForm::shortcut_4_activated()
{
  shortcutActivated(4);
}

void MainForm::shortcut_5_activated()
{
  shortcutActivated(5);
}

void MainForm::shortcut_6_activated()
{
  shortcutActivated(6);
}

void MainForm::shortcut_7_activated()
{
  shortcutActivated(7);
}

void MainForm::shortcut_8_activated()
{
  shortcutActivated(8);
}

void MainForm::shortcut_9_activated()
{
  shortcutActivated(9);
}

void MainForm::shortcut_10_activated()
{
  shortcutActivated(10);
}

void MainForm::shortcut_11_activated()
{
  shortcutActivated(11);
}

void MainForm::shortcut_12_activated()
{
  shortcutActivated(12);
}

void MainForm::shortcut_13_activated()
{
  shortcutActivated(13);
}

void MainForm::shortcut_14_activated()
{
  shortcutActivated(14);
}

void MainForm::shortcut_15_activated()
{
  shortcutActivated(15);
}

void MainForm::shortcut_16_activated()
{
  shortcutActivated(16);
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
  Q3ListViewItem *f, *next;
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
  projectCompletion.clear();
  projectIsDirty=FALSE;
  projectNeedsRelink=FALSE;
  menuBar()->setItemVisible(5,preferences.linkTarget!=LT_NONE); //debugMenu
  debugRunAction->setVisible(preferences.linkTarget!=LT_NONE);
  debugPauseAction->setVisible(preferences.linkTarget!=LT_NONE);
  updateLeftStatusLabel();
}

void MainForm::fileNewProject()
{
  if (compiling || savePrompt())
    return;
  clearProject();
  KConfigGroup recentFiles=pconfig->group("Recent files");
  recentFiles.writeEntry("Current project","");
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
    KUrl dir;
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
    KUrl dir;
    dir.setPath(ret[0]);
    dir.setFileName("");
    lastDirectory=dir.path();
  }
  return ret;
}

void MainForm::updateRecent()
{
  KConfigGroup recentFiles=pconfig->group("Recent files");
  QString recent=recentFiles.readEntry("Recent file 1");
  if (recent.isNull())
    fileRecent1Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent1Action->setVisible(TRUE);
    fileRecent1Action->setText(recentcut);
    fileRecent1Action->setStatusTip(recent);
  }
  recent=recentFiles.readEntry("Recent file 2");
  if (recent.isNull())
    fileRecent2Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent2Action->setVisible(TRUE);
    fileRecent2Action->setText(recentcut);
    fileRecent2Action->setStatusTip(recent);
  }
  recent=recentFiles.readEntry("Recent file 3");
  if (recent.isNull())
    fileRecent3Action->setVisible(FALSE);
  else {
    QString recentcut=recent.mid(recent.findRev('/')+1);
    recentcut.truncate(recentcut.findRev('.'));
    fileRecent3Action->setVisible(TRUE);
    fileRecent3Action->setText(recentcut);
    fileRecent3Action->setStatusTip(recent);
  }
  recent=recentFiles.readEntry("Recent file 4");
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
  // Pick up any changes to the list of recent files from other instances
  pconfig->sync();
  pconfig->reparseConfiguration();
  KConfigGroup recentFiles=pconfig->group("Recent files");
  // Find recent file to overwrite. If it isn't one of the first 3, by
  // elimination, it is the last, thus the test only goes up to <4, not <=4.
  for (i=1;i<4;i++) {
    QString recenti=recentFiles.readEntry(QString("Recent file %1").arg(i));
    if (recenti.isNull() || !recenti.compare(fileName))
      break;
  }
  // Move entries up
  for (j=i;j>1;j--) {
    recentFiles.writeEntry(QString("Recent file %1").arg(j),recentFiles.readEntry(QString("Recent file %1").arg(j-1)));
  }
  // The first recent file is the current project.
  recentFiles.writeEntry("Recent file 1",fileName);
  recentFiles.writeEntry("Current project",fileName);
  pconfig->sync();
  updateRecent();
}

Q3ListViewItem * MainForm::openFile(Q3ListViewItem * category, Q3ListViewItem * parent, const QString &fileCaption, const QString &fileName)
{
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
  Q3ListViewItem *item=NULL, *next=parent->firstChild();
  for (; IS_FILE(next); next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(parent,item)
                        :new ListViewFile(parent);
  newFile->isNew=FALSE;
  newFile->modifiedSinceLastCompile=FALSE;
  newFile->setText(0,fileCaption);
  newFile->setPixmap(0,
    category==cFilesListItem||category==qllFilesListItem?SYSICON("text-x-csrc","filec.png"):
    category==hFilesListItem?SYSICON("text-x-chdr","fileh.png"):
    category==sFilesListItem||category==asmFilesListItem?SYSICON("text-x-hex","files.png"):
    category==txtFilesListItem?SYSICON("text-plain","filet.png"):
    category==oFilesListItem||category==aFilesListItem?SYSICON("application-x-object","fileo.png"):
    SYSICON("unknown","filex.png"));
  newFile->fileName=fileName;
  if (IS_EDITABLE_CATEGORY(category)) {
    newFile->kateView=reinterpret_cast<KTextEditor::View *>(createView(fileName,QString::null,category));
    KDirWatch::self()->addFile(fileName);
  }
  fileCount++;
  COUNTER_FOR_CATEGORY(category)++;
  return newFile;
}

Q3ListViewItem *MainForm::createFolder(Q3ListViewItem *parent,const QString &name)
{
  Q3ListViewItem *item=parent->firstChild();
  Q3ListViewItem *startItem=item;
  Q3ListViewItem *newItem;
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

void *MainForm::createView(const QString &fileName, const QString &fileText, Q3ListViewItem *category)
{
  // Create Document object.
  KParts::Factory *factory = (KParts::Factory *)
    KLibLoader::self()->factory ("katepart");
  if (!factory) qFatal("Failed to load KatePart");
  KTextEditor::Document *doc = (KTextEditor::Document *)
      factory->createPart(0,this,"KTextEditor::Document");
  // Open the file.
  doc->setEncoding(preferences.useCalcCharset?"TI-89":QTextCodec::codecForLocale()->name());
  if (!fileName.isNull()) doc->openUrl(KUrl(fileName));
  // Set text.
  if (!fileText.isNull()) {
    SET_TEXT_SAFE(doc,fileText);
    doc->setModified(FALSE);
    // clearUndo and clearRedo are not in any interface, but they are public slots...
    doc->qt_metacall(QMetaObject::InvokeMetaMethod,
      doc->metaObject()->indexOfMethod("clearUndo()"),NULL);
    doc->qt_metacall(QMetaObject::InvokeMetaMethod,
      doc->metaObject()->indexOfMethod("clearRedo()"),NULL);
  }
  // Create View object.
  KTextEditor::View *newView = (KTextEditor::View *) doc->createView(widgetStack);
  newView->hide();
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  // Set highlighting mode.
  QString firstLine;
  if (doc->lines()) firstLine=doc->line(0);
  newView->document()->setHighlightingMode(
    (category==qllFilesListItem?
      QLL_HL_MODE:
    (category==sFilesListItem||(category==hFilesListItem&&!firstLine.isEmpty()&&firstLine[0]=='|'))?
      S_HL_MODE:
    (category==asmFilesListItem||(category==hFilesListItem&&!firstLine.isEmpty()&&firstLine[0]==';'))?
      ASM_HL_MODE:
    (category==cFilesListItem||category==hFilesListItem)?
      C_HL_MODE:
    "None"));
  // Set options.
  KTextEditor::ConfigInterface *configiface
    =qobject_cast<KTextEditor::ConfigInterface*>(newView);
  configiface->setConfigValue("dynamic-word-wrap",false);
  if (preferences.removeTrailingSpaces) {
    sendCommand(newView,"set-remove-trailing-space 1");
    sendCommand(newView,"set-remove-trailing-space-save 1");
  } else {
    sendCommand(newView,"set-remove-trailing-space 0");
    sendCommand(newView,"set-remove-trailing-space-save 0");
  }
  setTabWidth(newView,
    (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!firstLine.isEmpty()&&(firstLine[0]=='|'||firstLine[0]==';'))))?preferences.tabWidthAsm:
    (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
    8
  );
  connect(newView,SIGNAL(cursorPositionChanged(KTextEditor::View*,const KTextEditor::Cursor&)),this,SLOT(current_view_cursorPositionChanged(KTextEditor::View*,const KTextEditor::Cursor&)));
  connect(newView,SIGNAL(textInserted(KTextEditor::View*,const KTextEditor::Cursor&,const QString&)),
          this,SLOT(current_view_textInserted(KTextEditor::View*,const KTextEditor::Cursor&,const QString&)));
  connect(newView,SIGNAL(selectionChanged(KTextEditor::View*)),this,SLOT(current_view_selectionChanged(KTextEditor::View*)));
  connect(newView->document(),SIGNAL(textChanged(KTextEditor::Document*)),this,SLOT(current_view_textChanged(KTextEditor::Document*)));
  connect(newView->document(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  newView->setContextMenu(te_popup);
  newView->setCursorPosition(KTextEditor::Cursor(0,0));
  // Clear unwanted KatePart shortcuts causing conflicts
  QList<QAction *>actions=newView->actionCollection()->actions();
  foreach(QAction *action, actions) action->setShortcuts(QKeySequence::UnknownKey);
  return newView;
}

void MainForm::adoptSourceFile(void *srcFile)
{
  if (compiling) return;
  SourceFileWindow *sourceFile=reinterpret_cast<SourceFileWindow *>(srcFile);
  QString fileName=sourceFile->fileName;
  KTextEditor::View *newView=sourceFile->kateView;
  // Determine category and caption.
  Q3ListViewItem *category=othFilesListItem;
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
  Q3ListViewItem *item=NULL, *next=category->firstChild();
  for (; IS_FILE(next); next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(category,item)
                            :new ListViewFile(category);
  newFile->isNew=FALSE;
  newFile->setText(0,caption);
  newFile->setPixmap(0,
    category==cFilesListItem||category==qllFilesListItem?SYSICON("text-x-csrc","filec.png"):
    category==hFilesListItem?SYSICON("text-x-chdr","fileh.png"):
    category==sFilesListItem||category==asmFilesListItem?SYSICON("text-x-hex","files.png"):
    SYSICON("text-plain","filet.png"));
  newFile->fileName=fileName;
  newFile->modifiedSinceLastCompile=newView->document()->isModified();
  // Adopt View object.
  newFile->kateView=newView;
  newView->hide();
  newView->reparent(widgetStack,QPoint());
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  KDirWatch::self()->addFile(fileName);
  fileCount++;
  COUNTER_FOR_CATEGORY(category)++;
  // Set highlighting mode.
  QString fileText=newView->document()->text();
  newView->document()->setHighlightingMode(
    (category==qllFilesListItem?
      QLL_HL_MODE:
    (category==sFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
      S_HL_MODE:
    (category==asmFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
      ASM_HL_MODE:
    (category==cFilesListItem||category==hFilesListItem)?
      C_HL_MODE:
    "None"));
  // Set options.
  setTabWidth(newView,
    (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&(fileText[0]=='|'||fileText[0]==';'))))?preferences.tabWidthAsm:
    (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
    8
  );
  connect(newView,SIGNAL(cursorPositionChanged(KTextEditor::View*,const KTextEditor::Cursor&)),this,SLOT(current_view_cursorPositionChanged(KTextEditor::View*,const KTextEditor::Cursor&)));
  connect(newView,SIGNAL(textInserted(KTextEditor::View*,const KTextEditor::Cursor&,const QString&)),
          this,SLOT(current_view_textInserted(KTextEditor::View*,const KTextEditor::Cursor&,const QString&)));
  connect(newView,SIGNAL(selectionChanged(KTextEditor::View*)),this,SLOT(current_view_selectionChanged(KTextEditor::View*)));
  connect(newView->document(),SIGNAL(textChanged(KTextEditor::Document*)),this,SLOT(current_view_textChanged(KTextEditor::Document*)));
  connect(newView->document(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  newView->setContextMenu(te_popup);
  // Mark project dirty.
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
  // Select file.
  fileTreeClicked(newFile);
  // Update errors to point to the in-project source file instead of the
  // external one.
  Q3ListViewItemIterator lvit(errorList->errorListView);
  Q3ListViewItem *errorItem;
  for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
    if (static_cast<ErrorListItem *>(errorItem)->srcFile==sourceFile) {
      static_cast<ErrorListItem *>(errorItem)->srcFile=static_cast<SourceFileWindow *>(NULL);
      static_cast<ErrorListItem *>(errorItem)->lvFile=newFile;
    }
  }
  // Close separate source view.
  sourceFile->kateView=static_cast<KTextEditor::View *>(NULL);
  sourceFile->deleteLater();
}

void MainForm::fileOpen_addList(Q3ListViewItem *category,void *fileListV,void *dir, const QString &open_file)
{
  int i,e;
  int p,pslash;
  KUrl tmp;
  TPRFileList *fileList=(TPRFileList*)fileListV;
  QString caption;
  QString treePath;
  Q3ListViewItem *parent;
  e=fileList->path.count();
  if (e) category->setOpen(TRUE);
  for (i=0;i<e;i++)
  {
    tmp=*reinterpret_cast<const KUrl *>(dir);
    kurlNewFileName(tmp,fileList->path[i]);
    caption=fileList->path[i];
    //fixed suffix truncation for file paths such as "/root/.dot/nodot" so it wouldn't truncate to "/root/"
    p=caption.findRev('.');
    pslash=caption.findRev('/');
    if (p>=0&&p>pslash) caption.truncate(p);
    if (pslash>=0) caption.remove(0,pslash+1);
    treePath=fileList->folder[i].trimmed();
    //check for a backslash at the end and remove it if it's there.
    if (!treePath.isEmpty() && treePath[treePath.length()-1]=='\\')
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
  KUrl dir;
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
    Q3ListViewItem *category=othFilesListItem;
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
    foreach (SourceFileWindow *sourceFile, sourceFiles) {
      if (!fileName.compare(sourceFile->fileName)) {
        ACTIVATE_WINDOW(sourceFile->winId());
        return FALSE;
      }
    }

    if (!suffix.compare("h"))
      category=hFilesListItem;
    else if (!suffix.compare("c"))
      category=cFilesListItem;
    else if (!suffix.compare("s"))
      category=sFilesListItem;
    else if (!suffix.compare("asm") && asmFilesListItem)
      category=asmFilesListItem;
    else if (!suffix.compare("qll") && qllFilesListItem)
      category=qllFilesListItem;
    else if (!suffix.compare("txt"))
      category=txtFilesListItem;
    else {
      KMessageBox::error(this,QString("\'%1\' is not a valid file for opening.").arg(fileName));
      return FALSE;
    }
    int firstChar=peekFirstChar(fileName);
    if (firstChar==-2) {
      KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
      return FALSE;
    }
    int type=((category==sFilesListItem||(category==hFilesListItem&&firstChar=='|'))?
                2:
              (category==asmFilesListItem||(category==hFilesListItem&&firstChar==';'))?
                3:
              (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?
                1:
              0);
    
    new SourceFileWindow(this,fileName,
                         (category==qllFilesListItem)?QLL_ENABLED_HL_MODE:
                         (type==2)?S_ENABLED_HL_MODE:
                         (type==3)?ASM_ENABLED_HL_MODE:
                         (type==1)?C_ENABLED_HL_MODE:
                         "None",
                         (category==qllFilesListItem)?&(preferences.synQll.enabled):
                         (type==2)?&(preferences.synS.enabled):
                         (type==3)?&(preferences.synAsm.enabled):
                         (type==1)?&(preferences.synC.enabled):
                         NULL,category,(type==1),(type>1),category==txtFilesListItem);
    return FALSE;
  }
}

void MainForm::fileOpen()
{
  if (compiling || savePrompt())
    return;
  QString fileName=SGetFileName(KFileDialog::Opening,findFilter(TIGCCOpenProjectFileFilter),"Open Project/File",this);
  KUrl dir;
  dir.setPath(fileName);
  if (fileName.isEmpty())
    return;
  openProject(fileName);
}

void MainForm::fileRecent1()
{
  QString recentFile=fileRecent1Action->statusTip();
  if (compiling || savePrompt())
    return;
  openProject(recentFile);
}

void MainForm::fileRecent2()
{
  QString recentFile=fileRecent2Action->statusTip();
  if (compiling || savePrompt())
    return;
  openProject(recentFile);
}

void MainForm::fileRecent3()
{
  QString recentFile=fileRecent3Action->statusTip();
  if (compiling || savePrompt())
    return;
  openProject(recentFile);
}

void MainForm::fileRecent4()
{
  QString recentFile=fileRecent4Action->statusTip();
  if (compiling || savePrompt())
    return;
  openProject(recentFile);
}

int MainForm::fileSavePrompt(Q3ListViewItem *fileItem)
{
  int result;
  ListViewFile *theFile=static_cast<ListViewFile *>(fileItem);
  if (!theFile->kateView) return 0;
  while (theFile->kateView->document()->isModified()) { // "while" in case saving fails!
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(theFile->text(0)),QString::null,KStandardGuiItem::save(),KStandardGuiItem::discard());
    if (result==KMessageBox::Yes)
      fileSave_save(fileItem);
    else if (result==KMessageBox::No)
      return 0;
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
    result=KMessageBox::questionYesNoCancel(this,"The current project has been modified.  Do you want to save the changes?",QString::null,KStandardGuiItem::save(),KStandardGuiItem::discard());
    if (result==KMessageBox::Yes)
      fileSave();
    else if (result==KMessageBox::No)
      return 0;
    else
      return 1;
  }
  
  Q3ListViewItem *item=rootListItem->firstChild(),*next;
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
  KTextEditor::View *kateView=reinterpret_cast<KTextEditor::View *>(view);
  KTextEditor::Document *doc=kateView->document();
  doc->startEditing();
  int numLines=doc->lines();
  for (int i=0; i<numLines; i++) {
    QString line=doc->line(i);
    int whitespace=line.find(QRegExp("\\s+$"));
    if (whitespace>=0) doc->removeText(KTextEditor::Range(i,whitespace,i,line.length()));
  }
  doc->endEditing();
}

void MainForm::fileSave_save(Q3ListViewItem *theItem)
{
  if (!IS_FILE(theItem))
    return;
  CATEGORY_OF(category,theItem);
  if (!IS_EDITABLE_CATEGORY(category))
    return;
  ListViewFile *theFile=static_cast<ListViewFile *>(theItem);
  if (!theFile->fileName.isEmpty() && theFile->fileName[0]!='/') {
    fileSave_saveAs(theFile);
  }
  else {
    KDirWatch::self()->removeFile(theFile->fileName);
    if (!theFile->kateView->document()->save()) {
      KMessageBox::error(this,QString("Can't save to \'%1\'").arg(theFile->text(0)));
      KDirWatch::self()->addFile(theFile->fileName);
    }
    else {
      KDirWatch::self()->addFile(theFile->fileName);
      theFile->isNew=FALSE;
      if (theFile->kateView) {
        removeTrailingSpacesFromView(theFile->kateView);
        theFile->kateView->document()->setModified(FALSE);
      }
      projectIsDirty=TRUE;
      projectNeedsRelink=TRUE;
    }
  }
}

void MainForm::fileSave_saveAs(Q3ListViewItem *theItem)
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
  if (!theFile->fileName.isEmpty() && theFile->fileName[0]=='/')
    KDirWatch::self()->removeFile(theFile->fileName);
  mkdir_multi(saveFileName);
  if (IS_EDITABLE_CATEGORY(category)
      ?!theFile->kateView->document()->saveAs(saveFileName)
      :copyFile(theFile->fileName,saveFileName)) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    if (IS_EDITABLE_CATEGORY(category) && !theFile->fileName.isEmpty() && theFile->fileName[0]=='/')
      KDirWatch::self()->addFile(theFile->fileName);
  } else {
    theFile->fileName=saveFileName;
    if (IS_EDITABLE_CATEGORY(category)) {
      KDirWatch::self()->addFile(saveFileName);
      removeTrailingSpacesFromView(theFile->kateView);
      theFile->kateView->document()->setModified(FALSE);
    }
    theFile->isNew=FALSE;
    updateRightStatusLabel();
    projectIsDirty=TRUE;
    projectNeedsRelink=TRUE;
  }
}

//loadList also saves the file contents
void MainForm::fileSave_loadList(Q3ListViewItem *category,void *fileListV,const QString &base_dir,void *dir_new,QString *open_file)
{
  if (!category)
    return;
  TPRFileList *fileList=(TPRFileList*)fileListV;
  KUrl *new_dir=(KUrl*)dir_new;
  KUrl tmpPath;
  Q3ListViewItem *item=category->firstChild();
  Q3ListViewItem *next;
  QString folderSpec=QString::null;
  int o;
  while (item)
  {
    if (IS_FILE(item))
    {
      ListViewFile *theFile=static_cast<ListViewFile *>(item);
      QString absPath=theFile->fileName;
      QString relPath=KUrl::relativePath(base_dir,absPath);
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
      if (!theFile->fileName.isEmpty() && theFile->fileName[0]=='/')
        KDirWatch::self()->removeFile(theFile->fileName);
      if (tmpPath.path().compare(theFile->fileName)
          || (IS_EDITABLE_CATEGORY(category)
              && (theFile->kateView->document()->isModified() || theFile->isNew))) {
        mkdir_multi(tmpPath.path());
        if (IS_EDITABLE_CATEGORY(category)
            ?!theFile->kateView->document()->saveAs(tmpPath.path())
            :copyFile(theFile->fileName,tmpPath.path())) {
          KMessageBox::error(this,QString("Can't save to \'%1\'").arg(tmpPath.path()));
          if (IS_EDITABLE_CATEGORY(category) && !theFile->fileName.isEmpty() && theFile->fileName[0]=='/')
            KDirWatch::self()->addFile(theFile->fileName);
        } else {
          QString saveFileName=tmpPath.path();
          theFile->fileName=saveFileName;
          if (IS_EDITABLE_CATEGORY(category)) {
            KDirWatch::self()->addFile(theFile->fileName);
            removeTrailingSpacesFromView(theFile->kateView);
            theFile->kateView->document()->setModified(FALSE);
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
  KUrl base_dir_k(lastProj);
  base_dir_k.setFileName("");
  QString base_dir=base_dir_k.path();
  KUrl new_dir(nextProj);
  
  fileSave_loadList(hFilesListItem,&TPRData.h_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(cFilesListItem,&TPRData.c_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(qllFilesListItem,&TPRData.quill_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(sFilesListItem,&TPRData.s_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(asmFilesListItem,&TPRData.asm_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(oFilesListItem,&TPRData.o_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(aFilesListItem,&TPRData.a_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(txtFilesListItem,&TPRData.txt_files,base_dir,&new_dir,&open_file);
  fileSave_loadList(othFilesListItem,&TPRData.oth_files,base_dir,&new_dir,&open_file);
  if (projectFileName.isEmpty() && rootListItem->text(0)=="Project1")
    fileTreeItemRenamed(rootListItem,QFileInfo(nextProj).baseName(),0);
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
  foreach (SourceFileWindow *sourceFile, sourceFiles) {
    if (sourceFile->kateView->document()->isModified())
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
  if (CURRENT_VIEW) CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Print))->trigger();
}


void MainForm::filePrintQuickly()
{
  // This still shows the print dialog, but then KDE 3.5 Kate did that too
  // despite having 2 nominally different APIs (print and printDialog).
  if (CURRENT_VIEW) CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Print))->trigger();
}

void MainForm::filePreferences()
{
  if (showPreferencesDialog(this,!!asmFilesListItem,!!qllFilesListItem)
      ==QDialog::Accepted) {
    // Apply the KatePart preferences and treeview icons.
    Q3ListViewItemIterator it(fileTree);
    Q3ListViewItem *item;
    // Kate seems really insisting on making it a pain to update syntax highlighting settings.
    for (item=it.current();item;item=(++it).current()) {
      if (IS_FILE(item)) {
        KTextEditor::View *kateView=static_cast<ListViewFile *>(item)->kateView;
        if (kateView) {
          kateView->document()->setHighlightingMode("None");
        }
      }
    }
    foreach (SourceFileWindow *sourceFile, sourceFiles) {
      sourceFile->kateView->document()->setHighlightingMode("None");
    }
    KParts::Factory *factory=(KParts::Factory *)
      KLibLoader::self()->factory("katepart");
    if (!factory) qFatal("Failed to load KatePart");
    KTextEditor::Document *doc=(KTextEditor::Document *)
      factory->createPart(0,this,"KTextEditor::Document");
    doc->setHighlightingMode("Asm6502"); // Don't ask...
    doc->setHighlightingMode("None");
    KTextEditor::Editor *editor=doc->editor();
    int numConfigPages=editor->configPages();
    for (int i=0; i<numConfigPages; i++) {
      if (editor->configPageName(i)=="Fonts & Colors") {
        KTextEditor::ConfigPage *configPage=editor->configPage(i,this);
        configPage->apply();
        delete configPage;
        break;
      }
    }
    delete doc;
    it=Q3ListViewItemIterator(fileTree);
    for (item=it.current();item;item=(++it).current()) {
      if (item == rootListItem) {
        item->setPixmap(0,SYSICON("system-run","tpr.png"));
      } else if (IS_FOLDER(item)) {
        // Bluecurve's "folder-open" isn't actually more open than "folder".
        item->setPixmap(0,(item==currentListItem)?SYSICON(KIconTheme::current()=="Bluecurve"?"folder-accept":"folder-open","folder2.png")
                                                 :SYSICON("folder","folder1.png"));
      } else if (IS_FILE(item)) {
        CATEGORY_OF(category,item);
        KTextEditor::View *kateView=static_cast<ListViewFile *>(item)->kateView;
        if (kateView) {
          QString fileText=kateView->document()->text();
          CATEGORY_OF(category,item);
          if (preferences.removeTrailingSpaces) {
            sendCommand(kateView,"set-remove-trailing-space 1");
            sendCommand(kateView,"set-remove-trailing-space-save 1");
          } else {
            sendCommand(kateView,"set-remove-trailing-space 0");
            sendCommand(kateView,"set-remove-trailing-space-save 0");
          }
          setTabWidth(kateView,
            (category==sFilesListItem||category==asmFilesListItem||((category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&(fileText[0]=='|'||fileText[0]==';'))))?preferences.tabWidthAsm:
            (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?preferences.tabWidthC:
            8
          );
          // Kate seems really insisting on making it a pain to update syntax highlighting settings.
          kateView->document()->setHighlightingMode(
            (category==qllFilesListItem?
              QLL_HL_MODE:
            (category==sFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]=='|'))?
              S_HL_MODE:
            (category==asmFilesListItem||(category==hFilesListItem&&!fileText.isNull()&&!fileText.isEmpty()&&fileText[0]==';'))?
              ASM_HL_MODE:
            (category==cFilesListItem||category==hFilesListItem)?
              C_HL_MODE:
            "None"));
        }
        item->setPixmap(0,
          category==cFilesListItem||category==qllFilesListItem?SYSICON("text-x-csrc","filec.png"):
          category==hFilesListItem?SYSICON("text-x-chdr","fileh.png"):
          category==sFilesListItem||category==asmFilesListItem?SYSICON("text-x-hex","files.png"):
          category==txtFilesListItem?SYSICON("text-plain","filet.png"):
          category==oFilesListItem||category==aFilesListItem?SYSICON("application-x-object","fileo.png"):
          SYSICON("unknown","filex.png"));
      } else qWarning("Internal error: What's this item?");
    }
    KTextEditor::View *currView=CURRENT_VIEW;
    if (currView) {
      // Force redrawing to get the tab width right, repaint() is ignored for some reason.
      currView->hide();
      currView->show();
    }
    // Apply the icon preferences.
    if (preferences.useSystemIcons) {
      // Set the preferred icon size so system toolbar icons don't get annoying
      // padding.
      int toolbarIconSize=KIconLoader().currentSize(KIconLoader::MainToolbar);
      setIconSize(QSize(toolbarIconSize,toolbarIconSize));
      fileNewAction->setIcon(KIcon("document-new"));
      fileNewMenu->menuAction()->setIcon(KIcon("document-new"));
      fileOpenAction->setIcon(KIcon("document-open"));
      fileSaveAllAction->setIcon(KIcon("document-save"));
      filePrintAction->setIcon(KIcon("document-print"));
      filePrintQuicklyAction->setIcon(KIcon("document-print"));
      editClearAction->setIcon(KIcon("edit-delete"));
      editCutAction->setIcon(KIcon("edit-cut"));
      editCopyAction->setIcon(KIcon("edit-copy"));
      editPasteAction->setIcon(KIcon("edit-paste"));
      projectAddFilesAction->setIcon(KIcon("list-add"));
      projectCompileAction->setIcon(KIcon("run-build-file"));
      projectMakeAction->setIcon(KIcon("run-build"));
      projectBuildAction->setIcon(KIcon("view-refresh"));
      helpContentsAction->setIcon(KIcon("help-contents"));
      helpDocumentationAction->setIcon(KIcon("help-contents"));
      helpSearchAction->setIcon(KIcon("system-search"));
      findFindAction->setIcon(KIcon("edit-find"));
      findReplaceAction->setIcon(KIcon("edit-find-replace"));
      helpIndexAction->setIcon(KIcon("view-list-text"));
      editUndoAction->setIcon(KIcon("edit-undo"));
      editRedoAction->setIcon(KIcon("edit-redo"));
      findFunctionsAction->setIcon(KIcon("view-list-tree"));
      editIncreaseIndentAction->setIcon(KIcon("format-indent-more"));
      editDecreaseIndentAction->setIcon(KIcon("format-indent-less"));
      projectStopCompilationAction->setIcon(KIcon("process-stop"));
      projectForceQuitAction->setIcon(KIcon("dialog-cancel"));
      helpNewsAction->setIcon(KIcon("view-pim-news"));
      debugRunAction->setIcon(KIcon("media-playback-start"));
      debugPauseAction->setIcon(KIcon("media-playback-pause"));
      toolsConfigureAction->setIcon(KIcon("configure"));
      debugResetAction->setIcon(KIcon("media-playback-stop"));
    } else {
      setIconSize(QSize(20,20));
      fileNewAction->setIcon(QIcon(QPixmap(":/images/00.png")));
      fileNewMenu->menuAction()->setIcon(QIcon(QPixmap(":/images/00.png")));
      fileOpenAction->setIcon(QIcon(QPixmap(":/images/01.png")));
      fileSaveAllAction->setIcon(QIcon(QPixmap(":/images/02.png")));
      filePrintAction->setIcon(QIcon(QPixmap(":/images/03.png")));
      filePrintQuicklyAction->setIcon(QIcon(QPixmap(":/images/03.png")));
      editClearAction->setIcon(QIcon(QPixmap(":/images/04.png")));
      editCutAction->setIcon(QIcon(QPixmap(":/images/05.png")));
      editCopyAction->setIcon(QIcon(QPixmap(":/images/06.png")));
      editPasteAction->setIcon(QIcon(QPixmap(":/images/07.png")));
      projectAddFilesAction->setIcon(QIcon(QPixmap(":/images/08.png")));
      projectCompileAction->setIcon(QIcon(QPixmap(":/images/09.png")));
      projectMakeAction->setIcon(QIcon(QPixmap(":/images/10.png")));
      projectBuildAction->setIcon(QIcon(QPixmap(":/images/11.png")));
      helpContentsAction->setIcon(QIcon(QPixmap(":/images/12.png")));
      helpDocumentationAction->setIcon(QIcon(QPixmap(":/images/12.png")));
      helpSearchAction->setIcon(QIcon(QPixmap(":/images/13.png")));
      findFindAction->setIcon(QIcon(QPixmap(":/images/13.png")));
      findReplaceAction->setIcon(QIcon(QPixmap(":/images/14.png")));
      helpIndexAction->setIcon(QIcon(QPixmap(":/images/15.png")));
      editUndoAction->setIcon(QIcon(QPixmap(":/images/16.png")));
      editRedoAction->setIcon(QIcon(QPixmap(":/images/17.png")));
      findFunctionsAction->setIcon(QIcon(QPixmap(":/images/18.png")));
      editIncreaseIndentAction->setIcon(QIcon(QPixmap(":/images/19.png")));
      editDecreaseIndentAction->setIcon(QIcon(QPixmap(":/images/20.png")));
      projectStopCompilationAction->setIcon(QIcon(QPixmap(":/images/21.png")));
      projectForceQuitAction->setIcon(QIcon(QPixmap(":/images/22.png")));
      helpNewsAction->setIcon(QIcon(QPixmap(":/images/23.png")));
      debugRunAction->setIcon(QIcon(QPixmap(":/images/24.png")));
      debugPauseAction->setIcon(QIcon(QPixmap(":/images/25.png")));
      toolsConfigureAction->setIcon(QIcon(QPixmap(":/images/26.png")));
      debugResetAction->setIcon(QIcon(QPixmap(":/images/27.png")));
    }
    it=Q3ListViewItemIterator(errorList->errorListView);
    for (item=it.current();item;item=(++it).current()) {
      switch(item->rtti()) {
        case etError:
          item->setPixmap(0,SYSICON("dialog-error","error.png"));
          break;
        case etWarning:
          item->setPixmap(0,SYSICON("dialog-warning","warning.png"));
          break;
        default:
          item->setPixmap(0,SYSICON("dialog-information","info.png"));
          break;
      }
    }
    // Apply the preferences to the source file windows.
    foreach (SourceFileWindow *sourceFile, sourceFiles) sourceFile->applyPreferences();
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
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->trigger();
}

void MainForm::editRedo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->trigger();
}

void MainForm::editClear()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->removeSelectionText();
}

void MainForm::editCut()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Cut))->trigger();
}

void MainForm::editCopy()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Copy))->trigger();
}

void MainForm::editPaste()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::PasteText))->trigger();
}

void MainForm::editSelectAll()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::SelectAll))->trigger();
}

void MainForm::editIncreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action("tools_indent")->trigger();
}

void MainForm::editDecreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action("tools_unindent")->trigger();
}

class NonModalFindDialog : public KFindDialog {
  public:
    explicit NonModalFindDialog(QWidget *parent=0, long options=0,
      const QStringList &findStrings=QStringList(), bool hasSelection=false)
      : KFindDialog(parent,options,findStrings,hasSelection)
    {
      setModal(false);
    }
    // Override accept() not to close the dialog.
    virtual void accept(void)
    {
      setResult(QDialog::Accepted);
    }
};

void MainForm::findFind()
{
  if (kfinddialog)
    ACTIVATE_WINDOW(kfinddialog->winId());
  else {
    // Never set hasSelection because finding in selection doesn't really make
    // sense with my non-modal find dialog setup.
    kfinddialog=new NonModalFindDialog(this,KFind::FromCursor,findHistory);
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
  bool findBackwards=!!(kfinddialog->options()&KFind::FindBackwards);
  int findCurrentCol;
  kfind=new KFind(kfinddialog->pattern(),kfinddialog->options(),this,kfinddialog);
  kfind->closeFindNextDialog(); // don't use this, a non-modal KFindDialog is used instead
  connect(kfind,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findFind_highlight(const QString &,int,int)));
  // Make sure we have a valid currentListItem.
  if (!currentListItem) fileTreeClicked(rootListItem);
  findCurrentDocument=currentListItem;
  if (CURRENT_VIEW) {
    if (kfinddialog->options()&KFind::FromCursor) {
      if (CURRENT_VIEW->selection()) {
        if (findBackwards) {
          CURRENT_VIEW->selectionRange().start().position(findCurrentLine,findCurrentCol);
          if ((--findCurrentCol)==-1) {
            if (!findCurrentLine) goto skip_data;
            findCurrentLine--;
          }
        } else {
          CURRENT_VIEW->selectionRange().end().position(findCurrentLine,findCurrentCol);
        }
      } else {
        CURRENT_VIEW->cursorPosition().position(findCurrentLine,findCurrentCol);
      }
    } else {
      findCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
      findCurrentCol=-1;
    }
    kfind->setData(CURRENT_VIEW->document()->line(findCurrentLine),findCurrentCol);
  } else findCurrentLine=0;
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  // We never have a currBuffer here, the current list item is always either
  // non-editable or instantiated.
  QStringList currBuffer;
  int currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines)) {
        if (findBackwards) {
          // Traverse the file tree in order backwards, restart from the end if
          // the first file was reached. Stop at currentListItem.
          if (findCurrentDocument) {
            Q3ListViewItemIterator lvit(fileTree);
            Q3PtrList<Q3ListViewItem> lst;
            Q3ListViewItem *item;
            for (item=lvit.current();item&&item!=findCurrentDocument;
                 item=(++lvit).current()) {
              lst.prepend(item);
            }
            Q3PtrListIterator<Q3ListViewItem> it(lst);
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
          Q3ListViewItemIterator lvit(currentListItem);
          Q3PtrList<Q3ListViewItem> lst;
          Q3ListViewItem *item;
          for (item=(++lvit).current();item;item=(++lvit).current()) {
            lst.prepend(item);
          }
          Q3PtrListIterator<Q3ListViewItem> it(lst);
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
            Q3ListViewItemIterator it(findCurrentDocument);
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
            Q3ListViewItemIterator it(fileTree);
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
                currNumLines=currView->document()->lines();
                findCurrentLine=findBackwards?currNumLines-1:0;
                do {
                  if (kfind->needData()) {
                    if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines))
                      goto not_found_current;
                    if (findBackwards) findCurrentLine--; else findCurrentLine++;
                    kfind->setData(currView->document()->line(findCurrentLine));
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
            currNumLines=currView->document()->lines();
            findCurrentLine=findBackwards?currNumLines-1:0;
        }
      } else if (findBackwards) findCurrentLine--; else findCurrentLine++;
      if (currView)
        kfind->setData(currView->document()->line(findCurrentLine));
      else
        kfind->setData(currBuffer[findCurrentLine]);
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

void MainForm::findFind_highlight(const QString &text __attribute__((unused)), int matchingindex, int matchedlength)
{
  if (currentListItem!=findCurrentDocument) fileTreeClicked(findCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(findCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(findCurrentLine,matchingindex,
                                                findCurrentLine,matchingindex+matchedlength));
}

void MainForm::findFind_stop()
{
  if (kfinddialog) {
    findHistory=kfinddialog->findHistory();
    kfinddialog->deleteLater();
  }
  kfinddialog=static_cast<KFindDialog *>(NULL);
}

void MainForm::findReplace()
{
  if (kreplace) {
    KDialog *replaceNextDialog=kreplace->replaceNextDialog();
    if (replaceNextDialog)
      ACTIVATE_WINDOW(replaceNextDialog->winId());
    return;
  }
  KReplaceDialog kreplacedialog(this,((CURRENT_VIEW&&CURRENT_VIEW->selection()
                                       &&!CURRENT_VIEW->selectionRange().onSingleLine())?
                                       KFind::SelectedText:0)|KFind::FromCursor,
                                      findHistory,replacementHistory,
                                      CURRENT_VIEW&&CURRENT_VIEW->selection());
  if (kreplacedialog.exec()!=QDialog::Accepted)
    return;
  findHistory=kreplacedialog.findHistory();
  replacementHistory=kreplacedialog.replacementHistory();
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
  bool findBackwards=!!(kreplace->options()&KFind::FindBackwards);
  int replaceCurrentCol;
  // Make sure we have a valid currentListItem.
  if (!currentListItem) fileTreeClicked(rootListItem);
  replaceCurrentDocument=currentListItem;
  if (CURRENT_VIEW) {
    if (kreplace->options()&KFind::SelectedText) {
      kreplace->setSelection(CURRENT_VIEW->selectionRange());
      if (findBackwards) {
        replaceCurrentLine=kreplace->selEndLine();
        replaceCurrentCol=kreplace->selEndCol();
      } else {
        replaceCurrentLine=kreplace->selStartLine();
        replaceCurrentCol=kreplace->selStartCol();
      }
      kreplace->setOptions(kreplace->options()&~KFind::FromCursor);
    } else if (kreplace->options()&KFind::FromCursor) {
      if (CURRENT_VIEW->selection()) {
        if (findBackwards) {
          CURRENT_VIEW->selectionRange().start().position(replaceCurrentLine,
                                                          replaceCurrentCol);
          if ((--replaceCurrentCol)==-1) {
            if (!replaceCurrentLine) goto skip_data;
            replaceCurrentLine--;
          }
        } else {
          CURRENT_VIEW->selectionRange().end().position(replaceCurrentLine,
                                                        replaceCurrentCol);
        }
      } else {
        CURRENT_VIEW->cursorPosition().position(replaceCurrentLine,replaceCurrentCol);
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(replaceCurrentLine==(CURRENT_VIEW->document()->lines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->document()->lineLength(replaceCurrentLine)))
                         :(!replaceCurrentLine&&!replaceCurrentCol))
          kreplace->setOptions(kreplace->options()&~KFind::FromCursor);
      }
    } else {
      replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
      replaceCurrentCol=-1;
    }
    kreplace->setData(CURRENT_VIEW->document()->line(replaceCurrentLine),replaceCurrentCol);
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
  bool findBackwards=!!(kreplace->options()&KFind::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Make sure we have a valid currentListItem.
    if (!currentListItem) fileTreeClicked(rootListItem);
    replaceCurrentDocument=currentListItem;
    if (CURRENT_VIEW) {
      // Non-first-time always continues from cursor.
      if (CURRENT_VIEW->selection()) {
        if (findBackwards) {
          CURRENT_VIEW->selectionRange().start().position(replaceCurrentLine,
                                                          replaceCurrentCol);
          if ((--replaceCurrentCol)==-1) {
            if (!replaceCurrentLine) goto skip_data;
            replaceCurrentLine--;
          }
        } else {
          CURRENT_VIEW->selectionRange().end().position(replaceCurrentLine,
                                                        replaceCurrentCol);
        }
      } else {
        CURRENT_VIEW->cursorPosition().position(replaceCurrentLine,replaceCurrentCol);
      }
      kreplace->setData(CURRENT_VIEW->document()->line(replaceCurrentLine),replaceCurrentCol);
    } else replaceCurrentLine=0;
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  // We never have a currBuffer here, the current list item is always either
  // non-editable or instantiated.
  QStringList currBuffer;
  int currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (kreplace->needData()) {
      if (global) {
        if (findBackwards?!replaceCurrentLine:(replaceCurrentLine>=currNumLines)) {
          if (findBackwards) {
            // Traverse the file tree in order backwards, restart from the end if
            // the first file was reached. Stop at currentListItem.
            if (replaceCurrentDocument) {
              Q3ListViewItemIterator lvit(fileTree);
              Q3PtrList<Q3ListViewItem> lst;
              Q3ListViewItem *item;
              for (item=lvit.current();item&&item!=replaceCurrentDocument;
                   item=(++lvit).current()) {
                lst.prepend(item);
              }
              Q3PtrListIterator<Q3ListViewItem> it(lst);
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
            Q3ListViewItemIterator lvit(currentListItem);
            Q3PtrList<Q3ListViewItem> lst;
            Q3ListViewItem *item;
            for (item=(++lvit).current();item;item=(++lvit).current()) {
              lst.prepend(item);
            }
            Q3PtrListIterator<Q3ListViewItem> it(lst);
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
              Q3ListViewItemIterator it(replaceCurrentDocument);
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
              Q3ListViewItemIterator it(fileTree);
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
                  currNumLines=currView->document()->lines();
                  replaceCurrentLine=findBackwards?currNumLines-1:0;
                  do {
                    if (kreplace->needData()) {
                      if (findBackwards?!replaceCurrentLine:(replaceCurrentLine>=currNumLines))
                        goto not_found_current;
                      if (findBackwards) replaceCurrentLine--; else replaceCurrentLine++;
                      kreplace->setData(currView->document()->line(replaceCurrentLine));
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
              currNumLines=currView->document()->lines();
              replaceCurrentLine=findBackwards?currNumLines-1:0;
          }
        } else if (findBackwards) replaceCurrentLine--; else replaceCurrentLine++;
        if (currView)
          kreplace->setData(currView->document()->line(replaceCurrentLine));
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
              kreplace->setOptions(kreplace->options()&~(KFind::FromCursor
                                                         |KFind::SelectedText));
              kreplace->invalidateSelection();
              // Reinitialize.
              replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
              kreplace->setData(CURRENT_VIEW->document()->line(replaceCurrentLine));
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
          kreplace->setData(currView->document()->line(replaceCurrentLine));
        else
          kreplace->setData(currBuffer[replaceCurrentLine]);
      }
    }
    result=kreplace->replace();
  } while (result==KFind::NoMatch);
}

void MainForm::findReplace_highlight(const QString &text __attribute__((unused)), int matchingindex, int matchedlength)
{
  if (currentListItem!=replaceCurrentDocument) fileTreeClicked(replaceCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(replaceCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(replaceCurrentLine,matchingindex,
                                                replaceCurrentLine,matchingindex+matchedlength));
}

void MainForm::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
  if (currentListItem!=replaceCurrentDocument) fileTreeClicked(replaceCurrentDocument);
  if (!CURRENT_VIEW) qFatal("CURRENT_VIEW should be set here!");
  bool update=!!(kreplace->options()&KReplaceDialog::PromptOnReplace);
  bool haveSelection=kreplace->haveSelection();
  // The initializations are redundant, but g++ doesn't understand this, and the
  // self-initialization trick doesn't work either (-Wno-init-self is ignored).
  int selStartLine=0, selStartCol=0, selEndLine=0, selEndCol=0;
  if (haveSelection) {
    selStartLine=kreplace->selStartLine();
    selStartCol=kreplace->selStartCol();
    selEndLine=kreplace->selEndLine();
    selEndCol=kreplace->selEndCol();
  }
  CURRENT_VIEW->document()->startEditing();
  CURRENT_VIEW->document()->insertText(KTextEditor::Cursor(replaceCurrentLine,replacementIndex),
                                       text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  bool updateCursor=(line==replaceCurrentLine && col==replacementIndex+replacedLength);
  CURRENT_VIEW->document()->removeText(KTextEditor::Range(replaceCurrentLine,replacementIndex+replacedLength,
                                       replaceCurrentLine,replacementIndex+replacedLength+matchedLength));
  CURRENT_VIEW->document()->endEditing();
  if (updateCursor)
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(replaceCurrentLine,replacementIndex));
  if (update) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(replaceCurrentLine,replacementIndex+replacedLength));
    CURRENT_VIEW->setSelection(KTextEditor::Range(replaceCurrentLine,replacementIndex,
                                                  replaceCurrentLine,replacementIndex+replacedLength));
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
    sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
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
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,0));
    functionDialog->accept();
  }
}

void MainForm::findFunctions_prototypeButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].prototypeLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      sourceFileFunctions[index].prototypeLine,0));
    functionDialog->accept();
  }
}

void MainForm::findFunctions_implementationButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].implementationLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      sourceFileFunctions[index].implementationLine,0));
    functionDialog->accept();
  }
}

void MainForm::findFunctionsPopup_aboutToShow()
{
  findFunctionsPopup->clear();
  if (CURRENT_VIEW) {
    CATEGORY_OF(category,currentListItem);
    sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
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
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,0));
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
      ACTIVATE_WINDOW(reinterpret_cast<SourceFileWindow *>(sourceFile)->winId());
  } else {
    // Not found. Try to open it instead.
    // Don't do this if the name ends with ".tpr" because that would cause
    // openProject to close the current project and load the new one instead.
    if (!fileName.endsWith(".tpr",FALSE)) {
      QString fileNameFull=QFileInfo(projectFileName).dir().filePath(fileName);
      if (getPathType(fileNameFull)==PATH_FILE) {
        openProject(fileNameFull);
        if (findSourceFile(inProject,sourceFile,fileNameFull) && !inProject)
           ACTIVATE_WINDOW(reinterpret_cast<SourceFileWindow *>(sourceFile)->winId());
      } else {
        Q3ListViewItem *cat=reinterpret_cast<Q3ListViewItem *>(category);
        QString includeDir=(cat==asmFilesListItem)?"asm":
                           (cat==sFilesListItem)?"s":"c";
        fileNameFull=QDir(QString("%1/include/%2/").arg(tigcc_base)
                          .arg(includeDir)).filePath(fileName);
        if (getPathType(fileNameFull)==PATH_FILE) {
          openProject(fileNameFull);
          if (findSourceFile(inProject,sourceFile,fileNameFull) && !inProject)
             ACTIVATE_WINDOW(reinterpret_cast<SourceFileWindow *>(sourceFile)->winId());
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
    int line,col,i;
    CURRENT_VIEW->cursorPosition().position(line,col);
    QString textLine=CURRENT_VIEW->document()->line(line);
    int l=textLine.length();
    bool quotesInLine=textLine.contains("\"");
    QString fileName;
    for (i=col;i>=0;i--) {
      QChar c=textLine[i];
      if (!((quotesInLine && c==' ') || (c>='A' && c<='Z') || (c>='a' && c<='z')
            || (c>='0' && c<='9') || QString("_-./\\:").contains(c)))
        break;
      fileName.prepend(c);
    }
    for (i=col+1;i<l;i++) {
      QChar c=textLine[i];
      if (!((quotesInLine && c==' ') || (c>='A' && c<='Z') || (c>='a' && c<='z')
            || (c>='0' && c<='9') || QString("_-./\\:").contains(c)))
        break;
      fileName.append(c);
    }
    CATEGORY_OF(category,currentListItem);
    findAndOpenFile(fileName,category);
  }
}

void MainForm::openHeader(const QString &fileName, bool systemHeader,
                          int lineno)
{
  if (systemHeader) {
    // Don't do this if the name ends with ".tpr" because that would cause
    // openProject to close the current project and load the new one instead.
    if (!fileName.endsWith(".tpr",FALSE)) {
      bool inProject;
      void *sourceFile;
      QString fileNameFull=QDir(QString("%1/include/c/").arg(tigcc_base))
                           .filePath(fileName);
      if (findSourceFile(inProject,sourceFile,fileNameFull)) {
        if (inProject) {
          fileTreeClicked(reinterpret_cast<ListViewFile *>(sourceFile));
          if (reinterpret_cast<ListViewFile *>(sourceFile)->kateView)
            reinterpret_cast<ListViewFile *>(sourceFile)->kateView->setCursorPosition(KTextEditor::Cursor(lineno,0));
        } else {
          reinterpret_cast<SourceFileWindow *>(sourceFile)->kateView->setCursorPosition(KTextEditor::Cursor(lineno,0));
          ACTIVATE_WINDOW(reinterpret_cast<SourceFileWindow *>(sourceFile)->winId());
        }
      } else {
        if (getPathType(fileNameFull)==PATH_FILE) {
          openProject(fileNameFull);
          if (findSourceFile(inProject,sourceFile,fileNameFull) && !inProject) {
            reinterpret_cast<SourceFileWindow *>(sourceFile)->kateView->setCursorPosition(KTextEditor::Cursor(lineno,0));
            ACTIVATE_WINDOW(reinterpret_cast<SourceFileWindow *>(sourceFile)->winId());
          }
        } else {
          KMessageBox::error(this,QString("File \'%1\' not found.").arg(fileName),
                             "Search Failed");
        }
      }
    }
  } else {
    QString name=fileName;
    int pos;
    Q3ListViewItem *item=hFilesListItem;
    while ((pos=name.find('/'))>=0) {
      QString folder=name.left(pos);
      name.remove(0,pos+1);
      for (item=item->firstChild();item;item=item->nextSibling()) {
        if (IS_FOLDER(item)) {
          if (item->text(0)==folder) break;
        }
      }
      if (!item) return;
    }
    for (item=item->firstChild();item;item=item->nextSibling()) {
      if (IS_FILE(item)) {
        ListViewFile *fileItem=static_cast<ListViewFile *>(item);
        if (QFileInfo(fileItem->fileName).fileName()==name) {
          fileTreeClicked(item);
          if (fileItem->kateView)
            fileItem->kateView->setCursorPosition(KTextEditor::Cursor(lineno,0));
          return;
        }
      }
    }
  }
}

void MainForm::findFindSymbolDeclaration()
{
  if (IS_FILE(currentListItem) && CURRENT_VIEW) {
    QString fileText=CURRENT_VIEW->document()->text();
    CATEGORY_OF(category,currentListItem);
    // "Find symbol declaration" only operates on C files.
    if (category==cFilesListItem || category==qllFilesListItem
        || (category==hFilesListItem && !fileText.isEmpty()
            && fileText[0]!='|' && fileText[0]!=';')) {
      QString fileName=pathInProject(currentListItem);
      QString symbolFile;
      unsigned symbolLine;
      bool systemHeader;
      int line,col,i;
      CURRENT_VIEW->cursorPosition().position(line,col);
      QString textLine=CURRENT_VIEW->document()->line(line);
      QString wordUnderCursor;
      for (i=col-1;i>=0;i--) {
        QChar c=textLine[i];
        if ((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9')
            || c=='_' || c=='$' || c=='#')
          wordUnderCursor.prepend(c);
      }
      int len=textLine.length();
      for (i=col;i<len;i++) {
        QChar c=textLine[i];
        if ((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9')
            || c=='_' || c=='$' || c=='#')
          wordUnderCursor.append(c);
      }
      if (findSymbolInFile(wordUnderCursor,fileText,fileName,this,symbolFile,
                           symbolLine,systemHeader)
          && !symbolFile.isNull()) {
        if (symbolFile==fileName)
          CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(symbolLine,0));
        else
          openHeader(symbolFile,systemHeader,symbolLine);
      }
    }
  }
}

//returns 1 on success
int MainForm::projectAddFiles_oneFile(const QString &fileName)
{
  Q3ListViewItem *category=othFilesListItem;
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
static QString compileStats;
static QDateTime newestHeaderTimestamp;
static QStringList objectFiles;
static QStringList deletableObjectFiles;
static QStringList deletableAsmFiles;
static KProcess *process;

QString MainForm::writeTempSourceFile(void *srcFile, bool inProject)
{
  const QString *origFileName;
  Q3ListViewItem *category;
  QString fileName;
  QString fileText;
  LineStartList *pLineStartList=0;
  if (inProject) {
    ListViewFile *sourceFile=reinterpret_cast<ListViewFile *>(srcFile);
    origFileName=&(sourceFile->fileName);
    CATEGORY_OF(cat,sourceFile);
    category=cat;
    QString folder;
    Q3ListViewItem *item=sourceFile->parent();
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
      fileText=sourceFile->kateView->document()->text();
      pLineStartList=&(sourceFile->lineStartList);
    } else {
      if (copyFile(origFileName->ascii(),fileName.ascii())) {
        KMessageBox::error(this,"Failed to copy file to temporary directory.");
        stopCompilingFlag=TRUE;
      }
      return fileName;
    }
  } else {
    SourceFileWindow *sourceFile=reinterpret_cast<SourceFileWindow *>(srcFile);
    origFileName=&(sourceFile->fileName);
    category=reinterpret_cast<Q3ListViewItem *>(sourceFile->category);
    fileName=QString("%1%2").arg(tempdir)
                            .arg(origFileName->mid(origFileName->findRev('/')));
    fileText=sourceFile->kateView->document()->text();
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
    foreach (SourceFileWindow *sourceFile, sourceFiles) sourceFile->fileSave();
  }
  fileNewMenu->menuAction()->setEnabled(FALSE);
  fileNewAction->setEnabled(FALSE);
  fileOpenAction->setEnabled(FALSE);
  fileRecent1Action->setEnabled(FALSE);
  fileRecent2Action->setEnabled(FALSE);
  fileRecent3Action->setEnabled(FALSE);
  fileRecent4Action->setEnabled(FALSE);
  fileExitAction->setEnabled(FALSE);
  projectAddFilesAction->setEnabled(FALSE);
  projectCompileAction->setVisible(FALSE);
  projectMakeAction->setVisible(FALSE);
  projectBuildAction->setVisible(FALSE);
  projectCompileAction->setEnabled(FALSE);
  shortcuts[15]->setEnabled(FALSE);
  projectMakeAction->setEnabled(FALSE);
  shortcuts[16]->setEnabled(FALSE);
  projectBuildAction->setEnabled(FALSE);
  projectStopCompilationAction->setEnabled(TRUE);
  projectForceQuitAction->setEnabled(TRUE);
  projectStopCompilationAction->setVisible(TRUE);
  projectForceQuitAction->setVisible(TRUE);
  debugRunAction->setEnabled(FALSE);
  debugPauseAction->setEnabled(FALSE);
  debugResetAction->setEnabled(FALSE);
  foreach (SourceFileWindow *sourceFile, sourceFiles) {
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
  compileStats=QString::null;
  projectProgramOutputAction->setEnabled(FALSE);
  process=static_cast<KProcess *>(NULL);
  // Write all the headers and incbin files to the temporary directory.
  Q3ListViewItemIterator lvit(hFilesListItem);
  Q3ListViewItem *item;
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
  lvit=Q3ListViewItemIterator(othFilesListItem);
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
  foreach (SourceFileWindow *sourceFile, sourceFiles) {
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
  shortcuts[15]->setEnabled(TRUE);
  projectMakeAction->setEnabled(TRUE);
  shortcuts[16]->setEnabled(TRUE);
  projectBuildAction->setEnabled(TRUE);
  projectCompileAction->setVisible(TRUE);
  projectMakeAction->setVisible(TRUE);
  projectBuildAction->setVisible(TRUE);
  projectAddFilesAction->setEnabled(TRUE);
  fileExitAction->setEnabled(TRUE);
  fileRecent1Action->setEnabled(TRUE);
  fileRecent2Action->setEnabled(TRUE);
  fileRecent3Action->setEnabled(TRUE);
  fileRecent4Action->setEnabled(TRUE);
  fileOpenAction->setEnabled(TRUE);
  fileNewAction->setEnabled(TRUE);
  fileNewMenu->menuAction()->setEnabled(TRUE);
  statusBar()->clear();
}

static QString errorFunction;
static bool errorFlag;
static unsigned ldTigccStatPhase=0;

void MainForm::process_finished()
{
  // If we're in a modal dialog, let it complete or exiting the event loop will
  // crash.
  if (QCoreApplication::loopLevel()>2) {
    QTimer::singleShot(100,this,SLOT(process_finished()));
    return;
  }
  errorFunction=QString::null;
  ldTigccStatPhase=0;
  QCoreApplication::exit_loop();
}


void MainForm::process_readyRead()
{
  static int a68kErrorLine=0; // A68k errors are split onto several lines.
  static int errorLine, errorColumn;
  static QString errorFile;
  while (process->canReadLine()) {
    QString line=process->readLine();
    line.chop(1); // zap newline
    // ld-tigcc doesn't currently know the difference between host charset and
    // calculator charset, so the variable name won't display properly if it
    // contains non-ASCII characters. Fix that up.
    if (ldTigccStatPhase==1
        && line.trimmed().startsWith("Program Variable Name:",FALSE)) {
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
              line=line.trimmed();
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
              if (line.toLower()=="target calculators:") {
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
                  if (errorFile.toLower()=="error"||errorFile.toLower()=="warning") {
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
                    errorMessage=errorMessage.trimmed();
                    ErrorTypes errorType=etError;
                    if (errorMessage.startsWith("warning:",FALSE)) {
                      errorMessage.remove(0,8);
                      errorMessage=errorMessage.trimmed();
                      errorType=etWarning;
                    } else if (errorMessage.startsWith("error:",FALSE)) {
                      errorMessage.remove(0,6);
                      errorMessage=errorMessage.trimmed();
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
                        && errorMessage.count('\'')>1) {
                      errorFunction=errorMessage.mid(14,errorMessage.find('\'',14)-14);
                    } else if (errorMessage.startsWith(" at top level",FALSE)) {
                      errorFunction=QString::null;
                    } else {
                      ErrorTypes errorType=etError;
                      if (errorMessage.startsWith("warning:",FALSE)) {
                        errorMessage.remove(0,8);
                        errorMessage=errorMessage.trimmed();
                        errorType=etWarning;
                      } else if (errorMessage.startsWith("error:",FALSE)) {
                        errorMessage.remove(0,6);
                        errorMessage=errorMessage.trimmed();
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
        line=line.trimmed();
        // The next line is the first one to display in the dialog. (TIGCC IDE
        // does the same.)
        if (line.startsWith("Program Variable Name:",FALSE))
          ldTigccStatPhase=2;
        break;
      case 2:
        compileStats.append(line.simplified());
        compileStats.append('\n');
        break;
    }
  }
  if (errorFlag) {
    errorsCompilingFlag=TRUE;
    if (preferences.stopAtFirstError) stopCompilingFlag=TRUE;
  }
}

void MainForm::process_readyRead_recordOnly()
{
  QString line;
  while (process->canReadLine()) {
    programOutput.append(process->readLine());
    projectProgramOutputAction->setEnabled(TRUE);
  }
}

void MainForm::compileFile(void *srcFile, bool inProject, bool force)
{
  Q3ListViewItem *category;
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
    SourceFileWindow *sourceFile=reinterpret_cast<SourceFileWindow *>(srcFile);
    category=reinterpret_cast<Q3ListViewItem *>(sourceFile->category);
    origFileName=&(sourceFile->fileName);
    if (sourceFile->kateView->document()->isModified())
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
    if (!objectFile.isEmpty() && objectFile[0]!='/') {
      if (inProject) {
        ListViewFile *sourceFile=reinterpret_cast<ListViewFile *>(srcFile);
        Q3ListViewItem *item=sourceFile->parent();
        while (!IS_CATEGORY(item)) {
          objectFile.prepend('/');
          objectFile.prepend(item->text(0));
          item=item->parent();
        }
      }
      if (!projectFileName.isEmpty()) {
        objectFile.prepend('/');
        objectFile.prepend(QFileInfo(projectFileName).absolutePath());
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
    QString fileDir=QFileInfo(fileName).absolutePath();
    QDir qdir;
    if (category==asmFilesListItem) {
      // Assemble A68k file
      KShell::Errors err;
      QStringList args=KShell::splitArgs(settings.a68k_switches,
                                         KShell::TildeExpand|KShell::AbortOnMeta,
                                         &err);
      if (err) {
        new ErrorListItem(this,etError,QString::null,QString::null,
                          "Invalid A68k assembler command line options.",-1,-1);
        stopCompilingFlag=TRUE;
      }
      if (!stopCompilingFlag) {
        process=new KProcess();
        process->setOutputChannelMode(KProcess::MergedChannels);
        process->setWorkingDirectory(fileDir);
        *process<<(QString("%1/bin/a68k").arg(tigcc_base))
                <<fileName<<(QString("-o%1").arg(tempObjectFile))
                <<(QString("-i%1/include/asm/").arg(tigcc_base))<<"-q"<<args;
        if (settings.cut_ranges||settings.archive)
          *process<<"-a"; // all relocs
        if (settings.optimize_returns||settings.archive)
          *process<<"-d"; // keep locals
        connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
        connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
        process->start();
        // We need to block here, but events still need to be handled. The most
        // effective way to do this is to enter the event loop recursively,
        // even though it is not recommended by Qt.
        QCoreApplication::enter_loop();
        // This will be reached only after exitLoop() is called.
        delete process;
        process=static_cast<KProcess *>(NULL);
      }
    } else {
      bool deleteTempAsmFile=FALSE;
      QString fileNameToAssemble;
      if (category==sFilesListItem) {
        fileNameToAssemble=fileName;
      } else /* C or Quill */ {
        // Compile C/Quill file to assembly
        KShell::Errors err;
        QStringList args=KShell::splitArgs(settings.cc_switches,
                                           KShell::TildeExpand|KShell::AbortOnMeta,
                                           &err);
        if (err) {
          new ErrorListItem(this,etError,QString::null,QString::null,
                            "Invalid C compiler command line options.",-1,-1);
          stopCompilingFlag=TRUE;
        }
        if (!stopCompilingFlag) {
          process=new KProcess();
          process->setOutputChannelMode(KProcess::MergedChannels);
          process->setWorkingDirectory(fileDir);
          *process<<(QString("%1/bin/gcc").arg(tigcc_base))
                  <<"-S"<<"-I"<<fileDir
                  <<"-B"<<(QString("%1/bin/").arg(tigcc_base))<<"-I-"
                  <<"-I"<<(QString("%1/include/c").arg(tigcc_base))<<args;
          if (category==qllFilesListItem) { // Quill needs special switches.
            *process<<"-I"<<(QString("%1/include/quill").arg(tigcc_base))
                    <<"-include"<<quill_drv;
          }
          if (settings.use_data_var)
            *process<<"-mno-merge-sections";
          if (!preferences.allowImplicitDeclaration)
            *process<<"-Werror-implicit-function-declaration";
          if (settings.debug_info)
            *process<<"-gdwarf-2"<<"-g3"<<"-fasynchronous-unwind-tables";
          if (settings.fargo)
            *process<<"-DFARGO";
          else if (settings.flash_os)
            *process<<"-DFLASH_OS";
          else if (!settings.archive) { // This leaves only regular programs.
            *process<<process_libopts();
          }
          *process<<fileName<<"-o"<<tempAsmFile;
          connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
          connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
          process->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QCoreApplication::enter_loop();
          // This will be reached only after exitLoop() is called.
          delete process;
          process=static_cast<KProcess *>(NULL);
        }
        if (!stopCompilingFlag && qdir.exists(tempAsmFile)) {
          // Run patcher.
          process=new KProcess();
          process->setOutputChannelMode(KProcess::MergedChannels);
          process->setWorkingDirectory(fileDir);
          *process<<(QString("%1/bin/patcher").arg(tigcc_base))
                  <<tempAsmFile<<"-o"<<tempAsmFile;
          connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
          connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
          process->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QCoreApplication::enter_loop();
          // This will be reached only after exitLoop() is called.
          delete process;
          process=static_cast<KProcess *>(NULL);
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
        KShell::Errors err;
        QStringList args=KShell::splitArgs(settings.as_switches,
                                           KShell::TildeExpand|KShell::AbortOnMeta,
                                           &err);
        if (err) {
          new ErrorListItem(this,etError,QString::null,QString::null,
                            "Invalid GNU assembler command line options.",-1,-1);
          stopCompilingFlag=TRUE;
        }
        if (!stopCompilingFlag) {
          process=new KProcess();
          process->setOutputChannelMode(KProcess::MergedChannels);
          process->setWorkingDirectory(fileDir);
          *process<<(QString("%1/bin/as").arg(tigcc_base))
                  <<"-I"<<fileDir<<"-mc68000"
                  <<"-I"<<(QString("%1/include/s").arg(tigcc_base))<<args;
          if (settings.cut_ranges||settings.archive)
            *process<<"--all-relocs";
          if (settings.optimize_returns||settings.archive)
            *process<<"--keep-locals";
          if (settings.debug_info)
            *process<<"--gdwarf2";
          *process<<fileNameToAssemble<<"-o"<<tempObjectFile;
          connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
          connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
          process->start();
          // We need to block here, but events still need to be handled. The most
          // effective way to do this is to enter the event loop recursively,
          // even though it is not recommended by Qt.
          QCoreApplication::enter_loop();
          // This will be reached only after exitLoop() is called.
          delete process;
          process=static_cast<KProcess *>(NULL);
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
  Q3ListViewItemIterator lvit(fileTree);
  Q3ListViewItem *item;
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
  QString projectDir=QFileInfo(projectFileName).absolutePath();
  statusBar()->message(QString("Linking Project \'%1\'...")
                       .arg(QFileInfo(projectFileName).baseName()));
  if (settings.archive) {
    // Link static library using ar-tigcc.
    process=new KProcess();
    process->setOutputChannelMode(KProcess::MergedChannels);
    process->setWorkingDirectory(projectDir);
    *process<<(QString("%1/bin/ar-tigcc").arg(tigcc_base))
            <<"-o"<<(projectBaseName+".a")<<"--no-names"<<objectFiles;
    connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
    connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
    process->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QCoreApplication::enter_loop();
    // This will be reached only after exitLoop() is called.
    delete process;
    process=static_cast<KProcess *>(NULL);
    if (errorsCompilingFlag || stopCompilingFlag) return;
    QFileInfo fileInfo(projectBaseName+".a");
    if (fileInfo.exists())
      compileStats=QString("Archive Size: %1 Bytes\n").arg(fileInfo.size());
    else
      errorsCompilingFlag=TRUE;
  } else {
    // Link executable using ld-tigcc.
    QByteArray packName;
    QString pstarterName; // urlencoded
    QStringList linkerOptions=process_settings(rootListItem->text(0),
                                               projectBaseName,
                                               pstarterName,packName);
    process=new KProcess();
    process->setOutputChannelMode(KProcess::MergedChannels);
    process->setWorkingDirectory(projectDir);
    *process<<(QString("%1/bin/ld-tigcc").arg(tigcc_base))
            <<"-v"<<linkerOptions<<objectFiles;
    if (settings.std_lib)
      *process<<QString(settings.flash_os?"%1/lib/flashos.a"
                                         :(settings.fargo?"%1/lib/fargo.a"
                                                         :"%1/lib/tigcc.a"))
               .arg(tigcc_base);
    connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
    connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
    process->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QCoreApplication::enter_loop();
    // This will be reached only after exitLoop() is called.
    delete process;
    process=static_cast<KProcess *>(NULL);
    if (errorsCompilingFlag || stopCompilingFlag) return;
    if (settings.pack) {
      if (errorsCompilingFlag || stopCompilingFlag) return;
      statusBar()->message("Creating launcher...");
      const int numTargets=3;
      static const char binexts[numTargets][5]={".z89",".z9x",".zv2"};
      static const char exts[numTargets][5]={".89z",".9xz",".v2z"};
      static const char cbinexts[numTargets][5]={".y89",".y9x",".yv2"};
      static const char cexts[numTargets][5]={".89y",".9xy",".v2y"};
      bool targeted[numTargets];
      for (int target=0; target<numTargets; target++) {
        targeted[target]=QFileInfo(projectBaseName
          +(settings.outputbin?cbinexts[target]:cexts[target])).exists();
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
      process=new KProcess();
      process->setOutputChannelMode(KProcess::MergedChannels);
      process->setWorkingDirectory(projectDir);
      *process<<(QString("%1/bin/ld-tigcc").arg(tigcc_base))
              <<"-o"<<pstarterBaseName<<"-n"<<pstarterName
              <<(pstarterBaseName+".o");
      if (settings.outputbin) *process<<"--outputbin";
      connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
      connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead()));
      process->start();
      // We need to block here, but events still need to be handled. The most
      // effective way to do this is to enter the event loop recursively,
      // even though it is not recommended by Qt.
      QCoreApplication::enter_loop();
      // This will be reached only after exitLoop() is called.
      delete process;
      process=static_cast<KProcess *>(NULL);
      if (errorsCompilingFlag || stopCompilingFlag) return;
      // Now copy those pstarters actually requested.
      for (int target=0; target<numTargets; target++) {
        if (targeted[target]) {
          if (copyFile(pstarterBaseName+(settings.outputbin?binexts[target]:exts[target]),
                       projectBaseName+(settings.outputbin?binexts[target]:exts[target]))) {
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
      KShell::quoteArg(libopts.use_ti89?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-89.tib":
        settings.pack?(settings.outputbin?".y89":".89y"):
        settings.outputbin?".z89":".89z"
      ):""),FALSE);
    postBuild.replace("($TI89TIFILE)",
      KShell::quoteArg(libopts.use_ti89?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-89ti.tib":
        settings.pack?(settings.outputbin?".y89":".89y"):
        settings.outputbin?".z89":".89z"
      ):""),FALSE);
    postBuild.replace("($TI92PLUSFILE)",
      KShell::quoteArg(libopts.use_ti92p?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-9x.tib":
        settings.pack?(settings.outputbin?".y9x":".9xy"):
        settings.outputbin?".z9x":".9xz"
      ):""),FALSE);
    postBuild.replace("($V200FILE)",
      KShell::quoteArg(libopts.use_v200?projectBaseName+(
        settings.archive?".a":settings.flash_os?"-v2.tib":
        settings.pack?(settings.outputbin?".yv2":".v2y"):
        settings.outputbin?".zv2":".v2z"
      ):""),FALSE);
    postBuild.replace("($TI92FILE)",
      KShell::quoteArg(settings.fargo?projectBaseName+(
        settings.outputbin?".p92":".92p"
      ):""),FALSE);
    KShell::Errors err;
    QStringList args=KShell::splitArgs(postBuild,
                                       KShell::TildeExpand|KShell::AbortOnMeta,
                                       &err);
    if (err) {
      new ErrorListItem(this,etError,QString::null,QString::null,
                        "Invalid post-build command line.",-1,-1);
      errorsCompilingFlag=TRUE;
    }
    if (errorsCompilingFlag || stopCompilingFlag) return;
    process=new KProcess();
    process->setOutputChannelMode(KProcess::MergedChannels);
    process->setWorkingDirectory(projectDir);
    *process<<args;
    connect(process,SIGNAL(finished(int,QProcess::ExitStatus)),this,SLOT(process_finished()));
    connect(process,SIGNAL(readyRead()),this,SLOT(process_readyRead_recordOnly()));
    process->start();
    // We need to block here, but events still need to be handled. The most
    // effective way to do this is to enter the event loop recursively,
    // even though it is not recommended by Qt.
    QCoreApplication::enter_loop();
    // This will be reached only after exitLoop() is called.
    delete process;
    process=static_cast<KProcess *>(NULL);
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
  // Qt::convertFromPlainText. Sadly, that replaces the spaces with character
  // #160 (&nbsp;) which doesn't display properly in KMessageBox, so fix it up.
  if (KMessageBox::questionYesNo(this,Qt::convertFromPlainText(
        QString("The project has been compiled successfully.\n\n%1\n"
        "Do you want to open the project folder?").arg(compileStats))
        .replace(QChar(160)," "),
        "Compilation Successful")==KMessageBox::Yes) {
    KUrl projectDir(projectFileName);
    projectDir.setFileName("");
    KRun::runUrl(projectDir,"inode/directory",this);
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
  SourceFileWindow *sourceFile=reinterpret_cast<SourceFileWindow *>(srcFile);
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
  if (process && process->state()!=QProcess::NotRunning) {
    process->terminate();
    QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,100);
    if (process && process->state()!=QProcess::NotRunning)
      process->kill();
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

bool MainForm::tiemuInstance(TiEmuDBus *&instance)
{
  QDBusConnection connection=QDBusConnection::sessionBus();
  if (!connection.isConnected()) {
    KMessageBox::error(this,"Can\'t attach to D-Bus.");
    instance=NULL;
    return FALSE;
  }
  instance=new TiEmuDBus("org.ticalc.lpg.tiemu.TiEmuDBus",
                         "/org/ticalc/lpg/tiemu/TiEmuDBus",connection,this);
  if (!instance->isValid()) {
    delete instance;
    instance=NULL;
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
    TiEmuDBus *tiemuDBus=0;
    CableHandle *cable=0;
    CalcHandle *calc=0;
    switch (preferences.linkTarget) {
      case LT_TIEMU:
        {
          // Fire up TiEmu if it isn't running yet.
          if (!tiemuInstance(tiemuDBus)) return;
          if (!tiemuDBus) {
            if (!KRun::runCommand("tiemu", this)) {
              KMessageBox::error(this,"Can't run TiEmu.");
              return;
            }
            do {
              usleep(100000);
              QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,100);
              if (!tiemuInstance(tiemuDBus)) return;
            } while (!tiemuDBus);
          }
          // Wait for TiEmu to get ready.
          bool ready=false;
          do {
            usleep(100000);
            QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,100);
            QDBusReply<bool> reply=tiemuDBus->ready_for_transfers();
            if (!reply.isValid()) {
              KMessageBox::error(this,"D-Bus function call failed.");
              delete tiemuDBus;
              return;
            }
            ready=reply.value();
          } while (!ready);
          // Now obtain the model from TiEmu.
          QDBusReply<int> reply=tiemuDBus->emulated_calc_type();
          int tiemuCalcType;
          if (!reply.isValid() || !(tiemuCalcType=reply.value())) {
            KMessageBox::error(this,"D-Bus function call failed.");
            delete tiemuDBus;
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
          QDBusReply<bool> reply=tiemuDBus->debug_file(mainFile);
          if (!reply.isValid() || !reply.value()) {
            KMessageBox::error(this,"D-Bus function call failed.");
            delete tiemuDBus;
            return;
          }
        }
        {
          QDBusReply<bool> reply=tiemuDBus->send_files(files);
          if (!reply.isValid() || !reply.value()) {
            KMessageBox::error(this,"D-Bus function call failed.");
            delete tiemuDBus;
            return;
          }
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
        {
          // Execute the command.
          QDBusReply<bool> reply=tiemuDBus->execute_command(command);
          if (!reply.isValid() || !reply.value())
            KMessageBox::error(this,"D-Bus function call failed.");
          delete tiemuDBus;
        }
        break;
      case LT_REALCALC:
        {
          int err;
          // Convert the command to the calculator charset.
          char *ti=ticonv_charset_utf16_to_ti(CALC_TI89,command.utf16());
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
  TiEmuDBus *tiemuDBus;
  if (!tiemuInstance(tiemuDBus) || !tiemuDBus) return;
  if (!tiemuDBus->enter_debugger().isValid())
    KMessageBox::error(this,"D-Bus function call failed.");
  delete tiemuDBus;
}

void MainForm::debugReset()
{
  // This is enabled only for LT_TIEMU. Reset TiEmu.
  TiEmuDBus *tiemuDBus;
  if (!tiemuInstance(tiemuDBus) || !tiemuDBus) return;
  if (!tiemuDBus->reset_calc(FALSE).isValid())
    KMessageBox::error(this,"D-Bus function call failed.");
  delete tiemuDBus;
}

void MainForm::toolsConfigure()
{
  ToolsDialog toolsDialog(this);
  toolsDialog.exec();
  if (toolsDialog.result()==QDialog::Accepted) {
    KConfigGroup toolsConfig=pconfig->group("Tools");
    unsigned toolCount=tools.count();
    toolsConfig.writeEntry("Count",toolCount);
    for (unsigned idx=0; idx<toolCount; idx++) {
      KConfigGroup toolConfig=pconfig->group(QString("Tool %1").arg(idx));
      Tool &tool=tools[idx];
      toolConfig.writeEntry("Title",tool.title);
      toolConfig.writeEntry("Command Line",tool.commandLine);
      toolConfig.writeEntry("Working Directory",tool.workingDirectory);
      toolConfig.writeEntry("Terminal",tool.runInTerminal);
    }
    pconfig->sync();
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
    KShell::Errors err;
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
      QHash<QChar,QString> map;
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
    if (!process.startDetached())
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


void MainForm::fileTreeClicked(Q3ListViewItem *item)
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
    if (IS_EDITABLE_CATEGORY(category)) {
      static_cast<ListViewFile *>(currentListItem)->kateView->hide();
      widgetStack->removeWidget(static_cast<ListViewFile *>(currentListItem)->kateView);
      widgetStack->raiseWidget(-1);
    }
  }
  if (IS_FOLDER(item)) {
    // Bluecurve's "folder-open" isn't actually more open than "folder".
    item->setPixmap(0,SYSICON(KIconTheme::current()=="Bluecurve"?"folder-accept":"folder-open","folder2.png"));
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
    shortcuts[0]->setEnabled(FALSE);
    shortcuts[1]->setEnabled(FALSE);
    shortcuts[2]->setEnabled(FALSE);
    shortcuts[3]->setEnabled(FALSE);
    shortcuts[4]->setEnabled(FALSE);
    shortcuts[5]->setEnabled(FALSE);
    shortcuts[6]->setEnabled(FALSE);
    shortcuts[7]->setEnabled(FALSE);
    shortcuts[8]->setEnabled(FALSE);
    shortcuts[9]->setEnabled(FALSE);
    shortcuts[10]->setEnabled(FALSE);
    shortcuts[17]->setEnabled(FALSE);
  } else if (IS_FILE(item)) {
    fileNewFolderAction->setEnabled(TRUE);
    CATEGORY_OF(category,item->parent());
    if (IS_EDITABLE_CATEGORY(category)) {
      KTextEditor::View *kateView=static_cast<ListViewFile *>(item)->kateView;
      filePrintAction->setEnabled(TRUE);
      filePrintQuicklyAction->setEnabled(TRUE);
      widgetStack->addWidget(kateView);
      kateView->show();
      widgetStack->raiseWidget(kateView);
      editUndoAction->setEnabled(kateView->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
      editRedoAction->setEnabled(kateView->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
      editClearAction->setEnabled(kateView->selection());
      editCutAction->setEnabled(kateView->selection());
      editCopyAction->setEnabled(kateView->selection());
      editPasteAction->setEnabled(!clipboard->text().isNull());
      editSelectAllAction->setEnabled(TRUE);
      editIncreaseIndentAction->setEnabled(TRUE);
      editDecreaseIndentAction->setEnabled(TRUE);
      findFunctionsAction->setEnabled(category!=txtFilesListItem);
      findOpenFileAtCursorAction->setEnabled(TRUE);
      findFindSymbolDeclarationAction->setEnabled(TRUE);
      shortcuts[0]->setEnabled(kateView->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
      shortcuts[1]->setEnabled(kateView->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
      shortcuts[2]->setEnabled(kateView->selection());
      shortcuts[3]->setEnabled(kateView->selection());
      shortcuts[4]->setEnabled(!clipboard->text().isNull());
      shortcuts[5]->setEnabled(TRUE);
      shortcuts[6]->setEnabled(TRUE);
      shortcuts[7]->setEnabled(TRUE);
      shortcuts[8]->setEnabled(TRUE);
      shortcuts[9]->setEnabled(TRUE);
      shortcuts[10]->setEnabled(TRUE);
      shortcuts[17]->setEnabled(TRUE);
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
      shortcuts[0]->setEnabled(FALSE);
      shortcuts[1]->setEnabled(FALSE);
      shortcuts[2]->setEnabled(FALSE);
      shortcuts[3]->setEnabled(FALSE);
      shortcuts[4]->setEnabled(FALSE);
      shortcuts[5]->setEnabled(FALSE);
      shortcuts[6]->setEnabled(FALSE);
      shortcuts[7]->setEnabled(FALSE);
      shortcuts[8]->setEnabled(FALSE);
      shortcuts[9]->setEnabled(FALSE);
      shortcuts[10]->setEnabled(FALSE);
      shortcuts[17]->setEnabled(FALSE);
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
    shortcuts[0]->setEnabled(FALSE);
    shortcuts[1]->setEnabled(FALSE);
    shortcuts[2]->setEnabled(FALSE);
    shortcuts[3]->setEnabled(FALSE);
    shortcuts[4]->setEnabled(FALSE);
    shortcuts[5]->setEnabled(FALSE);
    shortcuts[6]->setEnabled(FALSE);
    shortcuts[7]->setEnabled(FALSE);
    shortcuts[8]->setEnabled(FALSE);
    shortcuts[9]->setEnabled(FALSE);
    shortcuts[10]->setEnabled(FALSE);
    shortcuts[17]->setEnabled(FALSE);
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
  Q3ListViewItem *item=NULL, *next=currentListItem->firstChild();
  for (; next; next=item->nextSibling())
  {
    item=next;
  }
  Q3ListViewItem *newFolder=item?new ListViewFolder(currentListItem,item)
                           :new ListViewFolder(currentListItem);
  newFolder->setText(0,"NewFolder");
  newFolder->setRenameEnabled(0,TRUE);
  currentListItem->setOpen(TRUE);
  fileTreeClicked(newFolder);
  newFolder->startRename(0);
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
}

bool MainForm::removeItem(Q3ListViewItem *item)
{
  if (IS_FOLDER(item) && !IS_CATEGORY(item)) {
    Q3ListViewItem *child=item->firstChild();
    while (child) {
      Q3ListViewItem *nextChild=child->nextSibling();
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

void MainForm::fileTreeContextMenuRequested(Q3ListViewItem *item,
                                            const QPoint &pos,
                                            int col __attribute__((unused)))
{
  fileTreeClicked(item);
  if (IS_FOLDER(item)) {
    Q3PopupMenu menu;
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
    Q3PopupMenu menu;
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
            if (IS_EDITABLE_CATEGORY(category) && !fileName.isEmpty() && fileName[0]=='/')
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
  Q3ListViewItem *item=rootListItem->firstChild(),*next;
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
void MainForm::extractFileTreeInfo(Q3ListViewItem *parent,Q3ListViewItem **p_category,QString *p_folderPath)
{
  Q3ListViewItem *item,*next;
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

void MainForm::newFile(Q3ListViewItem *parent, QString text, const QPixmap &pixmap)
{
  Q3ListViewItem *item=NULL, *next=parent->firstChild();
  QString tmp,oldtmp,suffix,caption;
  QStringList allFiles=extractAllFileNames();
  Q3ListViewItem *category;
  KUrl tmpK;
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
  if (projectFileName.isEmpty() && !tmp.isEmpty())
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
  if (IS_EDITABLE_CATEGORY(category) && !tmp.isEmpty() && tmp[0]=='/')
    KDirWatch::self()->addFile(tmp);
  
  newFile->setText(0,caption);
  newFile->setPixmap(0,pixmap);
  parent->setOpen(TRUE);
  newFile->kateView=reinterpret_cast<KTextEditor::View *>(createView(QString::null,text,category));
  fileTreeClicked(newFile);
  projectIsDirty=TRUE;
  projectNeedsRelink=TRUE;
  newFile->startRename(0);
  
  fileCount++;
  
  COUNTER_FOR_CATEGORY(category)++;
  updateLeftStatusLabel();
}

void MainForm::newFile( Q3ListViewItem *parent )
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
                                                    ?SYSICON("text-x-csrc","filec.png"):
                 category==hFilesListItem?SYSICON("text-x-chdr","fileh.png"):
                 category==sFilesListItem||category==asmFilesListItem
                                                    ?SYSICON("text-x-hex","files.png"):
                 category==txtFilesListItem?SYSICON("text-plain","filet.png"):SYSICON("unknown","filex.png"));
}

Q3ListViewItem *MainForm::resolveParent(Q3ListViewItem *category)
{
  Q3ListViewItem *ret=currentListItem;
  if (!IS_FILE(ret)&&!IS_FOLDER(ret))
    return category;
  if (IS_FILE(ret))
    ret=ret->parent();
  Q3ListViewItem *actualCategory=ret;
  while (IS_FOLDER(actualCategory->parent())) actualCategory=actualCategory->parent();
  if (actualCategory!=category)
    return category;
  return ret;
}

void MainForm::fileNewCHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"// Header File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-chdr","fileh.png"));
}


void MainForm::fileNewGNUAssemblyHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"| Header File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-chdr","fileh.png"));
}


void MainForm::fileNewA68kAssemblyHeader()
{
  if (compiling) return;
  newFile(resolveParent(hFilesListItem),"; Header File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-chdr","fileh.png"));
}


void MainForm::fileNewCSourceFile()
{
  if (compiling) return;
  newFile(resolveParent(cFilesListItem));
}


void MainForm::fileNewGNUAssemblySourceFile()
{
  if (compiling) return;
  newFile(resolveParent(sFilesListItem),"| Assembly Source File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-hex","files.png"));
}


void MainForm::fileNewA68kAssemblySourceFile()
{
  if (compiling) return;
  newFile(resolveParent(asmFilesListItem),"; Assembly Source File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-hex","files.png"));
}


void MainForm::fileNewQuillSourceFile()
{
  if (compiling) return;
  newFile(resolveParent(qllFilesListItem),"// Quill Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n",SYSICON("text-x-csrc","filec.png"));
}


void MainForm::fileNewTextFile()
{
  if (compiling) return;
  newFile(resolveParent(txtFilesListItem),"",SYSICON("text-plain","filet.png"));
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
      int line, col;
      CURRENT_VIEW->cursorPosition().position(line,col);
      rowStatusLabel->show();
      rowStatusLabel->setMaximumWidth(30);
      rowStatusLabel->setText(QString("%1").arg(line+1));
      colStatusLabel->show();
      colStatusLabel->setMaximumWidth(30);
      colStatusLabel->setText(QString("%1").arg(col+1));
      charsStatusLabel->show();
      charsStatusLabel->setMaximumWidth(100);
      charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
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

void MainForm::current_view_cursorPositionChanged(KTextEditor::View *view, const KTextEditor::Cursor &newPosition)
{
  if (CURRENT_VIEW==view && !disableViewEvents) {
    int line, col;
    newPosition.position(line,col);
    rowStatusLabel->setText(QString("%1").arg(line+1));
    colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void MainForm::current_view_textChanged(KTextEditor::Document *document)
{
  if (disableViewEvents) return;
  if (CURRENT_VIEW && CURRENT_VIEW->document()==document) {
    charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
    if (IS_FILE(currentListItem)) {
      static_cast<ListViewFile *>(currentListItem)->modifiedSinceLastCompile=TRUE;
      QString fileName=pathInProject(currentListItem);
      if (projectCompletion.contains(fileName))
        projectCompletion[fileName].dirty=TRUE;
    }
    if (preferences.deleteOverwrittenErrors && IS_FILE(currentListItem)
        && CURRENT_VIEW==static_cast<ListViewFile *>(currentListItem)->kateView) {
      ListViewFile *lvFile=static_cast<ListViewFile *>(currentListItem);
      Q3ListViewItemIterator lvit(errorList->errorListView);
      ErrorListItem *errorItem;
      while ((errorItem=static_cast<ErrorListItem *>(lvit.current()))) {
        ++lvit;
        if (errorItem->lvFile==lvFile && errorItem->cursor) {
          int line,col;
          errorItem->cursor->position(line,col);
          int curline,curcol;
          lvFile->kateView->cursorPosition().position(curline,curcol);
          if (curline==line && curcol==col)
            delete errorItem;
        }
      }
    }
  }
  if (kreplace) kreplace->invalidateSelection();
}

void MainForm::current_view_undoChanged()
{
  if (CURRENT_VIEW && !disableViewEvents) {
    editUndoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    editRedoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
    shortcuts[0]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    shortcuts[1]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  }
}

void MainForm::current_view_selectionChanged(KTextEditor::View *view)
{
  if (CURRENT_VIEW==view && !disableViewEvents) {
    editClearAction->setEnabled(view->selection());
    editCutAction->setEnabled(view->selection());
    editCopyAction->setEnabled(view->selection());
    shortcuts[2]->setEnabled(view->selection());
    shortcuts[3]->setEnabled(view->selection());
  }
}

void MainForm::current_view_textInserted(KTextEditor::View *view, const KTextEditor::Cursor &position, const QString &text)
{
  int line,col;
  position.position(line,col);
  if (preferences.autoBlocks && text=="{"
      && col==view->document()->lineLength(line)-1) {
    KTextEditor::Document *doc=view->document();
    CATEGORY_OF(category,currentListItem);
    QString firstLine=doc->line(0);
    // Only for C files.
    if (category==cFilesListItem||category==qllFilesListItem
        || (category==hFilesListItem && !firstLine.isEmpty()
            && firstLine[0]!='|' && firstLine[0]!=';')) {
      QString indent=doc->line(line);
      // Only if the line was all whitespace, otherwise wait for Enter to be
      // pressed (prevents annoying the user while typing a string or something).
      if (indent.contains(QRegExp("^\\s*\\{$"))) {
        indent=indent.remove('{');
        QString cursorLine=indent+"\t";
        doc->startEditing();
        doc->insertLine(line+1,cursorLine);
        doc->insertLine(line+2,indent+"}");
        doc->endEditing();
        view->setCursorPosition(KTextEditor::Cursor(line+1,cursorLine.length()));
      }
    }
  }
  if (IS_FILE(currentListItem)
      && view==static_cast<ListViewFile *>(currentListItem)->kateView
      && text=="(") {
    QString firstLine=view->document()->line(0);
    CATEGORY_OF(category,currentListItem);
    // Completion only operates on C files.
    if (category==cFilesListItem || category==qllFilesListItem
        || (category==hFilesListItem && !firstLine.isEmpty()
            && firstLine[0]!='|' && firstLine[0]!=';')) {
      new ArgHintPopup(view,pathInProject(currentListItem),this);
    }
  }
}

void MainForm::current_view_newLineHook()
{
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  KTextEditor::Document *doc=CURRENT_VIEW->document();
  if (preferences.autoBlocks && line && doc->line(line-1).endsWith("{")) {
    CATEGORY_OF(category,currentListItem);
    QString firstLine=doc->line(0);
    // Only for C files.
    if (category==cFilesListItem || category==qllFilesListItem
        || (category==hFilesListItem && !firstLine.isEmpty()
            && firstLine[0]!='|' && firstLine[0]!=';')) {
      QString indent=doc->line(line-1);
      // Remove everything starting from the first non-whitespace character.
      indent=indent.remove(QRegExp("(?!\\s).*$"));
      QString cursorLine=indent+"\t";
      doc->startEditing();
      doc->removeText(KTextEditor::Range(line,0,line,col));
      doc->insertLine(line,cursorLine);
      doc->insertText(KTextEditor::Cursor(line+1,0),indent+"}");
      doc->endEditing();
      CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,cursorLine.length()));
    }
  }
}

void MainForm::clipboard_dataChanged()
{
  if (CURRENT_VIEW) {
    editPasteAction->setEnabled(!clipboard->text().isNull());
    shortcuts[4]->setEnabled(!clipboard->text().isNull());
  }
}

void MainForm::fileTreeItemRenamed( Q3ListViewItem *item, const QString &newName, int col)
{
  if (col)
    return;
  if (item==rootListItem) {
    // validate name, fix if invalid
    QVector<QChar> validInVarname;
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
    if (!prjName.isEmpty() && prjName[0]=='\\') prjName.remove(0,1);
    if (prjName.isEmpty()) prjName="Project1";
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
      if (prjName.length()==i+1) prjName.append("Project1");
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
    if (IS_EDITABLE_CATEGORY(category)) {
      if (!oldFileName.isEmpty() && oldFileName[0]=='/')
        KDirWatch::self()->removeFile(oldFileName);
      if (newFileName.isEmpty() || newFileName[0]!='/')
        newFileName.prepend(QFileInfo(projectFileName).absolutePath()+"/");
      mkdir_multi(newFileName);
      if (!theFile->kateView->document()->saveAs(newFileName)) {
        KMessageBox::error(this,"Failed to rename the file.");
        theFile->setText(0,oldLabel);
        if (!oldFileName.isEmpty() && oldFileName[0]=='/')
          KDirWatch::self()->addFile(oldFileName);
      } else {
        if (!theFile->isNew && !oldFileName.isEmpty() && oldFileName[0]=='/')
          QDir().remove(oldFileName);
        fileNameRef=newFileName;
        KDirWatch::self()->addFile(newFileName);
        removeTrailingSpacesFromView(theFile->kateView);
        theFile->kateView->document()->setModified(FALSE);
        theFile->isNew=FALSE;
        projectIsDirty=TRUE;
        projectNeedsRelink=TRUE;
      }
    } else {
      mkdir_multi(newFileName);
      if (!moveFile(oldFileName,newFileName)) {
        KMessageBox::error(this,"Failed to rename the file.");
        theFile->setText(0,oldLabel);
      } else {
        fileNameRef=newFileName;
        projectIsDirty=TRUE;
        projectNeedsRelink=TRUE;
      }
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
    foreach (SourceFileWindow *sourceFile, sourceFiles) {
      if (sourceFile->savePrompt()) {
        e->ignore();
        return;
      }
    }
    clearProject();
    e->accept();
  }
}

void MainForm::KDirWatch_dirty(const QString &fileName)
{
  Q3ListViewItem *item=rootListItem->firstChild(),*next;
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
          static_cast<ListViewFile *>(item)->isNew=FALSE;
          static_cast<ListViewFile *>(item)->kateView->document()->setModified(FALSE);
          disableViewEvents=TRUE;
          static_cast<ListViewFile *>(item)->kateView->document()->documentReload();
          disableViewEvents=FALSE;
          updateRightStatusLabel();
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

QString MainForm::pathInProject(Q3ListViewItem *item)
{
  QString path=QFileInfo(static_cast<ListViewFile *>(item)->fileName).fileName();
  item=item->parent();
  while (!IS_CATEGORY(item)) {
    path.prepend('/');
    path.prepend(item->text(0));
    item=item->parent();
  }
  return path;
}

QString MainForm::textForHeader(const QString &fileName)
{
  QString name=fileName;
  int pos;
  Q3ListViewItem *item=hFilesListItem;
  while ((pos=name.find('/'))>=0) {
    QString folder=name.left(pos);
    name.remove(0,pos+1);
    for (item=item->firstChild();item;item=item->nextSibling()) {
      if (IS_FOLDER(item)) {
        if (item->text(0)==folder) break;
      }
    }
    if (!item) return QString::null;
  }
  for (item=item->firstChild();item;item=item->nextSibling()) {
    if (IS_FILE(item)) {
      ListViewFile *fileItem=static_cast<ListViewFile *>(item);
      if (QFileInfo(fileItem->fileName).fileName()==name)
        return fileItem->kateView->document()->text();
    }
  }
  return QString::null;
}

void MainForm::languageChange()
{
  retranslateUi(this);
}
