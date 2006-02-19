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
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

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
#include <kparts/factory.h>
#include <klibloader.h>
#include <kate/document.h>
#include <kate/view.h>
#include <kconfig.h>
#include <ktexteditor/configinterfaceextension.h>
#include <ktexteditor/dynwordwrapinterface.h>
#include <kaboutdata.h>
#include <khelpmenu.h>
#include <kfiledialog.h>
#include <kurl.h>
#include <kmessagebox.h>
#include <kdirwatch.h>
#include <cstdio>
#include <cstdlib>
#include "ktigcc.h"
#include "tpr.h"

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

#define TIGCCProjectDirectory "/usr/local/tigcc/projects"

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

// All the methods are inline because otherwise QT Designer will mistake them
// for slots of the main form.
class ListViewFolder : public QListViewItem {
  public:
  ListViewFolder(QListView *parent) : QListViewItem(parent)
  {
    setPixmap(0,QPixmap::fromMimeSource("folder1.png"));    
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(QListViewItem *parent) : QListViewItem(parent)
  {
    setPixmap(0,QPixmap::fromMimeSource("folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  ListViewFolder(QListView *parent, QListViewItem *after)
          : QListViewItem(parent, after)
  {
    setPixmap(0,QPixmap::fromMimeSource("folder1.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
  }
  ListViewFolder(QListViewItem *parent, QListViewItem *after)
          : QListViewItem(parent, after)
  {
    setPixmap(0,QPixmap::fromMimeSource("folder1.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
  }
  virtual int rtti(void) const {return 0x716CC0;}
  protected:
};

class ListViewFile : public QListViewItem {
  public:
  ListViewFile(QListView *parent) : QListViewItem(parent),
                                    kateView(NULL), isNew(TRUE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));    
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent) : QListViewItem(parent),
                                        kateView(NULL), isNew(TRUE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListView *parent, QListViewItem *after)
          : QListViewItem(parent, after), kateView(NULL), isNew(TRUE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent, QListViewItem *after)
          : QListViewItem(parent, after), kateView(NULL),
            isNew(TRUE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  virtual ~ListViewFile()
  {
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
static QListViewItem *currentListItem;
static bool projectIsDirty;
static QLabel *leftStatusLabel;
static QLabel *rowStatusLabel;
static QLabel *colStatusLabel;
static QLabel *charsStatusLabel;
static QLabel *rightStatusLabel;
static KParts::Factory* factory;
static KHelpMenu *khelpmenu;
static QPopupMenu *te_popup;
static QAssistantClient *assistant;
static int fileCount=0, hFileCount=0, cFileCount=0, sFileCount=0, asmFileCount=0, qllFileCount=0, oFileCount=0, aFileCount=0, txtFileCount=0, othFileCount=0;
static tprSettings settings;
static tprLibOpts libopts;
static QString projectFileName;
static QString lastDirectory;
static QClipboard *clipboard;
static QAccel *accel;
static bool lazyLoading=true;

class DnDListView : public QListView {
  private:
  public:
  DnDListView ( QWidget * parent = 0, const char * name = 0, WFlags f = 0 )
          : QListView(parent,name,f) {}
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
    if (e->source()==this && e->provides("x-ktigcc-dnd")) {
      QListViewItem *currItem;
      currItem = *reinterpret_cast<QListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
      if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
        // dropping folder
        // can only drop on folder or category
        QPoint vp=contentsToViewport(e->pos());
        QListViewItem *item=itemAt(vp);
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
          } else {ignore: e->ignore();}
        } else e->ignore();
      } else if (IS_FILE(currItem)) {
        // dropping file
        QPoint vp=contentsToViewport(e->pos());
        QListViewItem *item=itemAt(vp);
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
                break;
              } else if (i==item) {
                // item is first, move currItem before item
                e->accept();
                currItem->moveItem(item);
                item->moveItem(currItem);
                projectIsDirty=TRUE;
                break;
              }
            }
          } else e->ignore();
        } else e->ignore();
      } else e->ignore();
    } else e->ignore();
  }
  virtual void dragEnterEvent (QDragEnterEvent *e) {
    if (e->source()==this&&(e->provides("x-ktigcc-dnd")))
	  e->accept();
  }
  virtual void dragMoveEvent (QDragMoveEvent *e) {
    if (e->source()==this && e->provides("x-ktigcc-dnd")) {
      QListViewItem *currItem;
      currItem = *reinterpret_cast<QListViewItem * const *>((const char *)e->encodedData("x-ktigcc-dnd"));
      if (IS_FOLDER(currItem) && !IS_CATEGORY(currItem)) {
        // dropping folder
        // can only drop on folder or category
        QPoint vp=contentsToViewport(e->pos());
        QListViewItem *item=itemAt(vp);
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
        QPoint vp=contentsToViewport(e->pos());
        QListViewItem *item=itemAt(vp);
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
};

void MainForm::init()
{  
  fileNewFolderAction->setEnabled(FALSE);
  factory = (KParts::Factory *)
      KLibLoader::self()->factory ("libkatepart");
  if (!factory) exit(1);
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
  rootListItem=new QListViewItem(fileTree);
  rootListItem->setText(0,"Project1");
  rootListItem->setPixmap(0,QPixmap::fromMimeSource("tpr.png"));
  rootListItem->setOpen(TRUE);
  rootListItem->setDragEnabled(TRUE);
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
  lastDirectory=TIGCCProjectDirectory;
  projectFileName="";
  projectIsDirty=FALSE;
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
  connect(accel,SIGNAL(activated(int)),this,SLOT(accel_activated(int)));
  pconfig->setGroup("Recent files");
  if (parg)
    openProject(parg);
  else {
    QString mostrecent=pconfig->readEntry("Current project");
    if (!mostrecent.isNull() && !mostrecent.isEmpty())
      openProject(mostrecent);
  }
  updateRecent();
  startTimer(100);
}

void MainForm::destroy()
{
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
  // FIXME: This doesn't work for the text box you get when renaming.
  switch (index) {
    case 0: editUndo(); break;
    case 1: editRedo(); break;
    case 2: editCut(); break;
    case 3: editCopy(); break;
    case 4: editPaste(); break;
    default: break;
  }
}

void MainForm::clearProject()
{
  rootListItem->setText(0,"Project1");
  projectFileName="";
  fileTreeClicked(rootListItem);
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
  updateLeftStatusLabel();
}

void MainForm::fileNewProject()
{
  if (savePrompt())
    return;
  clearProject();
  pconfig->setGroup("Recent files");
  pconfig->writeEntry("Current project","");
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
  updateRecent();
}

QListViewItem * MainForm::openFile(QListViewItem * category, QListViewItem * parent, const QString &fileCaption, const QString &fileName)
{
  QString fileText;
  if (getPathType(fileName)!=PATH_FILE) {
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
  newFile->setText(0,fileCaption);
  newFile->setPixmap(0,QPixmap::fromMimeSource(
    category==cFilesListItem||category==qllFilesListItem?"filec.png":
    category==hFilesListItem?"fileh.png":
    category==sFilesListItem||category==asmFilesListItem?"files.png":
    category==txtFilesListItem?"filet.png":"filex.png"));
  newFile->fileName=fileName;
  if (IS_EDITABLE_CATEGORY(category)) {
    if (lazyLoading)
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
  KTextEditor::Document *doc = (KTextEditor::Document *)
      factory->createPart( 0, "", this, "", "KTextEditor::Document" );
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
  dynWordWrapInterface(newView)->setDynWordWrap(FALSE);
  newView->setTabWidth(2);
  connect(newView,SIGNAL(cursorPositionChanged()),this,SLOT(current_view_cursorPositionChanged()));
  connect(newView->getDoc(),SIGNAL(textChanged()),this,SLOT(current_view_textChanged()));
  connect(newView->getDoc(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  connect(newView->getDoc(),SIGNAL(selectionChanged()),this,SLOT(current_view_selectionChanged()));
  newView->installPopup(te_popup);
  // Set text.
  newView->getDoc()->setText(fileText);
  newView->getDoc()->setModified(FALSE);
  newView->getDoc()->clearUndo();
  newView->getDoc()->clearRedo();
  newView->setCursorPositionReal(0,0);
  return newView;
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

void MainForm::openProject(const QString &fileName)
{
  TPRDataStruct TPRData;
  KURL dir;
  dir.setPath(fileName);
  if (getPathType(fileName)!=PATH_FILE) {
    KMessageBox::error(this,QString("\'%1\' is not a regular file").arg(fileName));
    return;
  }
  int ret=loadTPR(fileName, &TPRData);
  if (ret == -1) {
    KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
    return;
  }
  if (ret > 0) {
    KMessageBox::error(this,QString("Error at line %2 of \'%1\'").arg(fileName).arg(ret));
    return;
  }
  if (TPRData.asm_files.path.count() && !asmFilesListItem) {
    KMessageBox::error(this,"This project needs A68k, which is not installed.");
    return;
  }
  if (TPRData.quill_files.path.count() && !qllFilesListItem) {
    KMessageBox::error(this,"This project needs quill.drv, which is not installed.");
    return;
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
  updateLeftStatusLabel();
  updateRightStatusLabel();
  addRecent(fileName);
}

void MainForm::fileOpen()
{
  if (savePrompt())
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
  if (savePrompt())
    return;
  openProject(fileRecent1Action->statusTip());
}

void MainForm::fileRecent2()
{
  if (savePrompt())
    return;
  openProject(fileRecent2Action->statusTip());
}

void MainForm::fileRecent3()
{
  if (savePrompt())
    return;
  openProject(fileRecent3Action->statusTip());
}

void MainForm::fileRecent4()
{
  if (savePrompt())
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
      if (theFile->kateView)
        theFile->kateView->getDoc()->setModified(FALSE);
      projectIsDirty=TRUE;
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
      theFile->kateView->getDoc()->setText(fileText);
      theFile->kateView->getDoc()->clearUndo();
      theFile->kateView->getDoc()->clearRedo();
      theFile->kateView->getDoc()->setHlMode(hlMode);
      theFile->kateView->setCursorPositionReal(line,col);
    }
    theFile->fileName=saveFileName;
    if (IS_EDITABLE_CATEGORY(category)) {
      KDirWatch::self()->addFile(saveFileName);
      if (theFile->kateView)
        theFile->kateView->getDoc()->setModified(FALSE);
    }
    theFile->isNew=FALSE;
    updateRightStatusLabel();
    projectIsDirty=TRUE;
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
      if (relPath.find("./")==0)
      {
        relPath=relPath.mid(2);
      }
      else if (relPath.find("../")==0)
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
            theFile->kateView->getDoc()->setText(fileText);
            theFile->kateView->getDoc()->clearUndo();
            theFile->kateView->getDoc()->clearRedo();
            theFile->kateView->getDoc()->setHlMode(hlMode);
            theFile->kateView->setCursorPositionReal(line,col);
          }
          theFile->fileName=saveFileName;
          if (IS_EDITABLE_CATEGORY(category)) {
            KDirWatch::self()->addFile(theFile->fileName);
            if (theFile->kateView)
              theFile->kateView->getDoc()->setModified(FALSE);
          }
          theFile->isNew=FALSE;
          projectIsDirty=TRUE; // in case saving the project fails
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
  
  if (saveTPR(nextProj,&TPRData))
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(nextProj));
  else {
    projectFileName=nextProj;
    projectIsDirty=FALSE;
    addRecent(nextProj);
  }
  updateRightStatusLabel();
}

void MainForm::fileSave()
{
  if (projectFileName.isEmpty())
    fileSaveAs();
  else
    fileSave_fromto(projectFileName,projectFileName);
}

void MainForm::fileSaveAs()
{
  QString fileName=SGetFileName(KFileDialog::Saving,TIGCC_TPR_Filter TIGCCAllFilter,"Save Project",this);
  if (fileName.isEmpty())
    return;
  fileSave_fromto(projectFileName,fileName);
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
  
}

void MainForm::findReplace()
{
  
}

void MainForm::findFunctions()
{
  
}

void MainForm::findOpenFileAtCursor()
{
  
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
  
  if (qllFileCount) {
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
  QStringList result=SGetFileName_Multiple(findFilter(TIGCCAddFilesFilter),"Add Files",this);
  e=result.count();
  for (i=0;i<e;i++) {
    if (projectAddFiles_oneFile(result[i]))
      projectChanged=TRUE;
  }
  if (projectChanged) {
    projectIsDirty=TRUE;
  }
}

void MainForm::projectCompile()
{
  
}

void MainForm::projectMake()
{
  
}

void MainForm::projectBuild()
{
  
}

void MainForm::errorsAndWarnings()
{
  
}

void MainForm::projectProgramOutput()
{
  
}

void MainForm::projectOptions()
{
  
}

void MainForm::debugRun()
{
  
}

void MainForm::debugPause()
{
  
}

void MainForm::debugReset()
{
  
}

void MainForm::toolsConfigure()
{
  
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
  if (IS_FOLDER(currentListItem))
    currentListItem->setPixmap(0,QPixmap::fromMimeSource("folder1.png"));
  if (IS_FILE(currentListItem)) {
    CATEGORY_OF(category,currentListItem);
    if (IS_EDITABLE_CATEGORY(category) && static_cast<ListViewFile *>(currentListItem)->kateView) {
      static_cast<ListViewFile *>(currentListItem)->kateView->hide();
      widgetStack->removeWidget(static_cast<ListViewFile *>(currentListItem)->kateView);
      widgetStack->raiseWidget(-1);
    }
  }
  if (IS_FOLDER(item)) {
    item->setPixmap(0,QPixmap::fromMimeSource("folder2.png"));
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
    accel->setItemEnabled(0,FALSE);
    accel->setItemEnabled(1,FALSE);
    accel->setItemEnabled(2,FALSE);
    accel->setItemEnabled(3,FALSE);
    accel->setItemEnabled(4,FALSE);
  } else if (IS_FILE(item)) {
    fileNewFolderAction->setEnabled(TRUE);
    CATEGORY_OF(category,item->parent());
    if (IS_EDITABLE_CATEGORY(category)) {
      Kate::View *kateView=static_cast<ListViewFile *>(item)->kateView;
      if (!kateView) { // lazy loading
        kateView=reinterpret_cast<Kate::View *>(createView(static_cast<ListViewFile *>(item)->fileName,static_cast<ListViewFile *>(item)->textBuffer,category));
        static_cast<ListViewFile *>(item)->textBuffer=QString::null;
        static_cast<ListViewFile *>(item)->kateView=kateView;
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
      accel->setItemEnabled(0,!!(kateView->getDoc()->undoCount()));
      accel->setItemEnabled(1,!!(kateView->getDoc()->redoCount()));
      accel->setItemEnabled(2,kateView->getDoc()->hasSelection());
      accel->setItemEnabled(3,kateView->getDoc()->hasSelection());
      accel->setItemEnabled(4,!clipboard->text().isNull());
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
      accel->setItemEnabled(0,FALSE);
      accel->setItemEnabled(1,FALSE);
      accel->setItemEnabled(2,FALSE);
      accel->setItemEnabled(3,FALSE);
      accel->setItemEnabled(4,FALSE);
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
    accel->setItemEnabled(0,FALSE);
    accel->setItemEnabled(1,FALSE);
    accel->setItemEnabled(2,FALSE);
    accel->setItemEnabled(3,FALSE);
    accel->setItemEnabled(4,FALSE);
  }
  currentListItem=item;
  updateLeftStatusLabel();
  updateRightStatusLabel();
}

void MainForm::fileNewFolder()
{
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
    CATEGORY_OF(category,item);
    if (!IS_EDITABLE_CATEGORY(category))
      menu.setItemEnabled(1,FALSE);
    if (!IS_CATEGORY(item)) {
      menu.insertSeparator();
      menu.insertItem("&Remove",2);
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
        delete item;
        currentListItem=NULL;
        fileTreeClicked(fileTree->currentItem());
        projectIsDirty=TRUE;
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
    if (category==txtFilesListItem
        || !IS_EDITABLE_CATEGORY(category))
      menu.setItemEnabled(2,FALSE);
    menu.insertSeparator();
    menu.insertItem("&Remove",3);
    menu.insertItem("&Delete",4);
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
        // compiling not implemented yet!
        break;
      case 3:
        if (!fileSavePrompt(item)) {
          delete item;
          currentListItem=NULL;
          fileTreeClicked(fileTree->currentItem());
          projectIsDirty=TRUE;
        }
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

void MainForm::newFile( QListViewItem *parent, QString text, const char *iconName )
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
  newFile->setPixmap(0,QPixmap::fromMimeSource(iconName));
  parent->setOpen(TRUE);
  newFile->kateView=reinterpret_cast<Kate::View *>(createView(tmp,text,category));
  fileTreeClicked(newFile);
  projectIsDirty=TRUE;
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
                                                    ?"filec.png":
                 category==hFilesListItem?"fileh.png":
                 category==sFilesListItem||category==asmFilesListItem
                                                    ?"files.png":
                 category==txtFilesListItem?"filet.png":"filex.png");
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
  newFile(resolveParent(hFilesListItem),"// Header File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewGNUAssemblyHeader()
{
  newFile(resolveParent(hFilesListItem),"| Header File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewA68kAssemblyHeader()
{
  newFile(resolveParent(hFilesListItem),"; Header File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewCSourceFile()
{
  newFile(resolveParent(cFilesListItem),"// C Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","filec.png");
}


void MainForm::fileNewGNUAssemblySourceFile()
{
  newFile(resolveParent(sFilesListItem),"| Assembly Source File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","files.png");
}


void MainForm::fileNewA68kAssemblySourceFile()
{
  newFile(resolveParent(asmFilesListItem),"; Assembly Source File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","files.png");
}


void MainForm::fileNewQuillSourceFile()
{
  newFile(resolveParent(qllFilesListItem),"// Quill Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","filec.png");
}


void MainForm::fileNewTextFile()
{
  newFile(resolveParent(txtFilesListItem),"","filet.png");
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
  if (CURRENT_VIEW)
    charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
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

void MainForm::clipboard_dataChanged()
{
  if (CURRENT_VIEW) {
    editPasteAction->setEnabled(!clipboard->text().isNull());
    accel->setItemEnabled(4,!clipboard->text().isNull());
  }
}

void MainForm::fileTreeItemRenamed( QListViewItem *item, int col, const QString &newName)
{
  if (col)
    return;
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
        theFile->kateView->getDoc()->setText(fileText);
        theFile->kateView->getDoc()->clearUndo();
        theFile->kateView->getDoc()->clearRedo();
        theFile->kateView->getDoc()->setHlMode(hlMode);
        theFile->kateView->setCursorPositionReal(line,col);
        theFile->kateView->getDoc()->setModified(modified);
      }
      if (IS_EDITABLE_CATEGORY(category) && newFileName[0]=='/')
        KDirWatch::self()->addFile(newFileName);
      projectIsDirty=TRUE;
    }
  } else {
    KMessageBox::error(this,"The name you chose conflicts with that of another file.");
    theFile->setText(0,oldLabel);
  }
  
  updateRightStatusLabel();
}

void MainForm::closeEvent(QCloseEvent *e)
{
  if (savePrompt())
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
            static_cast<ListViewFile *>(item)->kateView->getDoc()->setText(fileText);
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
        qWarning("KDirWatch_dirty called for file not in project tree");
        return;
      }
      item=next;
      next=item->nextSibling();
    }
    item=next;
  }
  qWarning("KDirWatch_dirty called for file not in project tree");
  return;
}

// Yes, this is an ugly hack... Any better suggestions?
#define QListView DnDListView
