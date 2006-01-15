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

#include <qlabel.h>
#include <qstatusbar.h>
#include <qtimer.h>
#include <qdatetime.h>
#include <qdragobject.h>
#include <qassistantclient.h> 
#include <kparts/factory.h>
#include <klibloader.h>
#include <kate/document.h>
#include <kate/view.h>
#include <kconfig.h>
#include <ktexteditor/configinterfaceextension.h>
#include <kaboutdata.h>
#include <khelpmenu.h>
#include <kfiledialog.h>
#include <kurl.h>
#include <cstdio>
#include <cstdlib>
#include "tpr.h"

using std::puts;
using std::exit;

enum {TIGCCOpenProjectFileFilter=0,TIGCCSaveProjectFilter};
static const char *TIGCCFileFilters[2]=
{
    "*.tpr *.h *.c *.s *.asm *.txt|All TIGCC Files (*.tpr *.h *.c *.s *.asm *.txt)\n*.tpr|TIGCC Projects (*.tpr)\n*.h|Header Files (*.h)\n*.c|C Files (*.c)\n*.s|GNU Assembly Files (*.s)\n*.asm|A68k Assembly Files (*.asm)\nText Files (*.txt)\n*.*|All Files (*.*)",
    "*.tpr|TIGCC Projects (*.tpr)\n*.*|All Files (*.*)"
};
static const char *TIGCCProjectDirectory="/usr/local/tigcc/projects";
static QString lastDirectory;

extern const char *tigcc_base;
extern const char *quill_drv;
extern char tempdir[];
extern void write_temp_file(const char *filename, const char *data, const size_t len);
extern void delete_temp_file(const char *filename);
extern void force_qt_assistant_page(int n);
extern KAboutData *pabout;

#define IS_CATEGORY(item) ((item) && ((item)==hFilesListItem \
                                      || (item)==cFilesListItem \
                                      || (item)==sFilesListItem \
                                      || (item)==asmFilesListItem \
                                      || (item)==qllFilesListItem \
                                      || (item)==oFilesListItem \
                                      || (item)==aFilesListItem \
                                      || (item)==txtFilesListItem \
                                      || (item)==othFilesListItem))
#define IS_FOLDER(item) ((item) && (item)->rtti()==0x716CC0)
#define IS_FILE(item) ((item) && (item)->rtti()==0x716CC1)

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
                                    cursorLine(1), cursorCol(0)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));    
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent) : QListViewItem(parent),
                                        cursorLine(1), cursorCol(0)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListView *parent, QListViewItem *after)
          : QListViewItem(parent, after), cursorLine(1), cursorCol(0)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent, QListViewItem *after)
          : QListViewItem(parent, after), cursorLine(1), cursorCol(0)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  virtual int rtti(void) const {return 0x716CC1;}
  QString textBuffer;
  unsigned int cursorLine, cursorCol;
  QString fileName; // full name of the file
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
static QLabel *leftStatusLabel;
static QLabel *rowStatusLabel;
static QLabel *colStatusLabel;
static QLabel *charsStatusLabel;
static QLabel *rightStatusLabel;
static Kate::View* m_view;
static KHelpMenu *khelpmenu;
static QPopupMenu *te_popup;
static QAssistantClient *assistant;
static int fileCount=0, hFileCount=0, cFileCount=0, sFileCount=0, asmFileCount=0, qllFileCount=0, oFileCount=0, aFileCount=0, txtFileCount=0, othFileCount=0;

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
          QListViewItem *srcCategory=currItem;
          while (srcCategory->parent()->rtti()==0x716CC0) srcCategory=srcCategory->parent();
          QListViewItem *destCategory=item;
          while (destCategory->parent()->rtti()==0x716CC0) destCategory=destCategory->parent();
          if (srcCategory == destCategory) {
            // can't move folder into itself
            for (QListViewItem *destFolder=item; destFolder->rtti()==0x716CC0; destFolder=destFolder->parent()) {
              if (destFolder==currItem) goto ignore;
            }
            // move folder
            e->accept();
            currItem->parent()->takeItem(currItem);
            item->insertItem(currItem);
          } else {ignore: e->ignore();}
        } else e->ignore();
      } else if (IS_FILE(currItem)) {
        // dropping file
        QPoint vp=contentsToViewport(e->pos());
        QListViewItem *item=itemAt(vp);
        if (IS_FOLDER(item)) {
          // drop on folder
          // move file
          e->accept();
          currItem->parent()->takeItem(currItem);
          item->insertItem(currItem);
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
                break;
              } else if (i==item) {
                // item is first, move currItem before item
                e->accept();
                currItem->moveItem(item);
                item->moveItem(currItem);
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
          QListViewItem *srcCategory=currItem;
          while (srcCategory->parent()->rtti()==0x716CC0) srcCategory=srcCategory->parent();
          QListViewItem *destCategory=item;
          while (destCategory->parent()->rtti()==0x716CC0) destCategory=destCategory->parent();
          if (srcCategory == destCategory) {
            // can't move folder into itself
            for (QListViewItem *destFolder=item; destFolder->rtti()==0x716CC0; destFolder=destFolder->parent()) {
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
  KParts::Factory* factory = (KParts::Factory *)
      KLibLoader::self()->factory ("libkatepart");
  if (!factory) exit(1);
  KTextEditor::Document *doc = (KTextEditor::Document *)
      factory->createPart( 0, "", this, "", "KTextEditor::Document" );
  m_view = (Kate::View *) doc->createView( splitter, 0L );
  m_view->setEnabled(FALSE);
  m_view->setSizePolicy(QSizePolicy(QSizePolicy::Expanding,QSizePolicy::Expanding,0,0));
  write_temp_file("config.tmp","[Kate Renderer Defaults]\nSchema=ktigcc - Grayed Out\n",0);
  KConfig kconfig(QString(tempdir)+"/config.tmp",true);
  m_view->getDoc()->readConfig(&kconfig);
  delete_temp_file("config.tmp");
  m_view->getDoc()->setHlMode(0);
  connect(m_view,SIGNAL(cursorPositionChanged()),this,SLOT(m_view_cursorPositionChanged()));
  connect(m_view->getDoc(),SIGNAL(textChanged()),this,SLOT(m_view_textChanged()));
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
  m_view->installPopup(te_popup);
  connect(te_popup,SIGNAL(aboutToShow()),this,SLOT(te_popup_aboutToShow()));
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
  startTimer(100);
}

void MainForm::destroy()
{
  Kate::Document *doc=m_view->getDoc();
  delete m_view;
  delete doc;
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

void MainForm::fileNewProject()
{
  rootListItem->setText(0,"Project1");
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
  updateLeftStatusLabel();
}

QString MainForm::SGetFileName(int mode,short fileFilter,const QString &caption,QWidget *parent)
{
  QString ret;
  if (static_cast<KFileDialog::OperationMode>(mode)==KFileDialog::Opening)
    ret=KFileDialog::getOpenFileName(lastDirectory,TIGCCFileFilters[fileFilter],parent,caption);
  else
    ret=KFileDialog::getSaveFileName(lastDirectory,TIGCCFileFilters[fileFilter],parent,caption);
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
QStringList MainForm::SGetFileName_Multiple(short fileFilter,const QString &caption,QWidget *parent)
{
  QStringList ret;
  ret=KFileDialog::getOpenFileNames(lastDirectory,TIGCCFileFilters[fileFilter],parent,caption);
  if (!ret.empty())
  {
    KURL dir;
    dir.setPath(ret[0]);
    dir.setFileName("");
    lastDirectory=dir.path();
  }
  return ret;
}

void MainForm::openFile(QListViewItem * parent, const QString &fileCaption, const QString &fileName, const QString &text)
{
  QListViewItem *category=parent;
  parent->setOpen(TRUE);
  while (category->parent()->rtti()==0x716CC0) {
    category=category->parent();
    category->setOpen(TRUE);
  }
  QListViewItem *item=NULL, *next=parent->firstChild();
  for (; next; next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(parent,item)
                        :new ListViewFile(parent);
  newFile->setText(0,fileCaption);
  newFile->setPixmap(0,QPixmap::fromMimeSource(
    category==cFilesListItem||category==qllFilesListItem?"filec.png":
    category==hFilesListItem?"fileh.png":
    category==sFilesListItem||category==asmFilesListItem?"files.png":
    category==txtFilesListItem?"filet.png":"filex.png"));
  newFile->textBuffer=text;
  newFile->fileName=fileName;
  fileCount++;
  (category==hFilesListItem?hFileCount:category==cFilesListItem?cFileCount:
   category==sFilesListItem?sFileCount:category==asmFilesListItem?asmFileCount:
   category==qllFilesListItem?qllFileCount:category==oFilesListItem?oFileCount:
   category==aFilesListItem?aFileCount:category==txtFilesListItem?txtFileCount:
   othFileCount)++;
}

void MainForm::fileOpen_addList(QListViewItem **parent,void *fileListV,void *dir)
{
  int i,e;
  KURL tmp;
  TPRFileList *fileList=(TPRFileList*)fileListV;
  e=fileList->path.count();
  for (i=0;i<e;i++)
  {
    tmp=*reinterpret_cast<const KURL *>(dir);
    tmp.setFileName(fileList->path[i]);
    openFile(*parent,fileList->path[i],tmp.path(),loadFileText(tmp.path()));
  }
  updateLeftStatusLabel();
}

void MainForm::fileOpen()
{
  QString fileName=SGetFileName(KFileDialog::Opening,TIGCCOpenProjectFileFilter,"Open Project/File",this);
  KURL dir;
  dir.setPath(fileName);
  if (!loadTPR(fileName))
  {
    fileNewProject();
    fileOpen_addList(&hFilesListItem,&TPRData.h_files,&dir);
    fileOpen_addList(&cFilesListItem,&TPRData.c_files,&dir);
    fileOpen_addList(&qllFilesListItem,&TPRData.quill_files,&dir);
    fileOpen_addList(&sFilesListItem,&TPRData.s_files,&dir);
    fileOpen_addList(&asmFilesListItem,&TPRData.asm_files,&dir);
    fileOpen_addList(&oFilesListItem,&TPRData.o_files,&dir);
    fileOpen_addList(&aFilesListItem,&TPRData.a_files,&dir);
    fileOpen_addList(&txtFilesListItem,&TPRData.txt_files,&dir);
    fileOpen_addList(&othFilesListItem,&TPRData.oth_files,&dir);
  }
}

void MainForm::fileSave()
{
  
}

void MainForm::fileSaveAs()
{
  QString fileName=SGetFileName(KFileDialog::Saving,TIGCCSaveProjectFilter,"Save Project",this);
  
}

void MainForm::filePrint()
{
  
}

void MainForm::editUndo()
{
  
}

void MainForm::editRedo()
{
  
}

void MainForm::editCut()
{
  
}

void MainForm::editCopy()
{

}

void MainForm::editPaste()
{
  
}

void MainForm::editFind()
{
  
}

void MainForm::findFindSymbolDeclaration()
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
    static_cast<ListViewFile *>(currentListItem)->textBuffer=m_view->getDoc()->text();
    m_view->cursorPositionReal(&(static_cast<ListViewFile *>(currentListItem)->cursorLine),
                               &(static_cast<ListViewFile *>(currentListItem)->cursorCol));
  }
  if (IS_FOLDER(item)) {
    item->setPixmap(0,QPixmap::fromMimeSource("folder2.png"));
    fileNewFolderAction->setEnabled(TRUE);
    m_view->setEnabled(FALSE);
    m_view->getDoc()->setText("");
    write_temp_file("config.tmp","[Kate Renderer Defaults]\nSchema=ktigcc - Grayed Out\n",0);
    KConfig kconfig(QString(tempdir)+"/config.tmp",true);
    m_view->getDoc()->readConfig(&kconfig);
    delete_temp_file("config.tmp");
    m_view->getDoc()->setHlMode(0);
  } else if (IS_FILE(item)) {
    fileNewFolderAction->setEnabled(TRUE);
    m_view->setEnabled(TRUE);
    m_view->getDoc()->setText(static_cast<ListViewFile *>(item)->textBuffer);
    QListViewItem *category=item->parent();
    while (category->parent()->rtti()==0x716CC0) category=category->parent();
    const char *buffer=static_cast<ListViewFile *>(item)->textBuffer;
    write_temp_file("config.tmp","[Kate Renderer Defaults]\nSchema=kate - Normal\n",0);
    KConfig kconfig(QString(tempdir)+"/config.tmp",true);
    m_view->getDoc()->readConfig(&kconfig);
    delete_temp_file("config.tmp");
    uint cnt=m_view->getDoc()->hlModeCount(), i;
    for (i=0; i<cnt; i++) {
      if (!m_view->getDoc()->hlModeName(i).compare(
          ((category==sFilesListItem||(category==hFilesListItem&&buffer&&*buffer=='|'))?
             "GNU Assembler 68k":
           (category==asmFilesListItem||(category==hFilesListItem&&buffer&&*buffer==';'))?
             "Motorola Assembler 68k":
           (category==cFilesListItem||category==qllFilesListItem||category==hFilesListItem)?
             "C":
           "None"))) break;
    }
    if (i==cnt) i=0;
    m_view->getDoc()->setHlMode(i);
    m_view->setCursorPositionReal(static_cast<ListViewFile *>(item)->cursorLine,
                                  static_cast<ListViewFile *>(item)->cursorCol);
  } else {
    fileNewFolderAction->setEnabled(FALSE);
    m_view->setEnabled(FALSE);
    m_view->getDoc()->setText("");
    write_temp_file("config.tmp","[Kate Renderer Defaults]\nSchema=ktigcc - Grayed Out\n",0);
    KConfig kconfig(QString(tempdir)+"/config.tmp",true);
    m_view->getDoc()->readConfig(&kconfig);
    delete_temp_file("config.tmp");
    m_view->getDoc()->setHlMode(0);
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
  for (; IS_FOLDER(next); next=item->nextSibling())
    item=next;
  QListViewItem *newFolder=item?new ListViewFolder(currentListItem,item)
                           :new ListViewFolder(currentListItem);
  newFolder->setText(0,"NewFolder");
  newFolder->setRenameEnabled(0,TRUE);
  currentListItem->setOpen(TRUE);
  fileTreeClicked(newFolder);
  newFolder->startRename(0);
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
    QListViewItem *category=item;
    while (category->parent()->rtti()==0x716CC0) category=category->parent();
    if (category==oFilesListItem || category==aFilesListItem
        || category==othFilesListItem) menu.setItemEnabled(1,FALSE);
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
        break;
      case 3:
        item->startRename(0);
    }
  }
}

void MainForm::newFile( QListViewItem *parent, QString text, const char *iconName )
{
  QListViewItem *item=NULL, *next=parent->firstChild();
  for (; next; next=item->nextSibling())
    item=next;
  ListViewFile *newFile=item?new ListViewFile(parent,item)
                        :new ListViewFile(parent);
  newFile->setText(0,"New File");
  newFile->setPixmap(0,QPixmap::fromMimeSource(iconName));
  parent->setOpen(TRUE);
  newFile->textBuffer=text;
  fileTreeClicked(newFile);
  newFile->startRename(0);
  m_view->getDoc()->setText(text);
  fileCount++;
  QListViewItem *category=parent;
  while (category->parent()->rtti()==0x716CC0) category=category->parent();
  (category==hFilesListItem?hFileCount:category==cFilesListItem?cFileCount:
   category==sFilesListItem?sFileCount:category==asmFilesListItem?asmFileCount:
   category==qllFilesListItem?qllFileCount:category==oFilesListItem?oFileCount:
   category==aFilesListItem?aFileCount:category==txtFilesListItem?txtFileCount:
   othFileCount)++;
  updateLeftStatusLabel();
}

void MainForm::newFile( QListViewItem *parent )
{
  QListViewItem *category=parent;
  while (category->parent()->rtti()==0x716CC0) category=category->parent();
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


void MainForm::fileNewCHeader()
{
  newFile(hFilesListItem,"// Header File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewGNUAssemblyHeader()
{
  newFile(hFilesListItem,"| Header File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewA68kAssemblyHeader()
{
  newFile(hFilesListItem,"; Header File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","fileh.png");
}


void MainForm::fileNewCSourceFile()
{
  newFile(cFilesListItem,"// C Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","filec.png");
}


void MainForm::fileNewGNUAssemblySourceFile()
{
  newFile(sFilesListItem,"| Assembly Source File\n| Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","files.png");
}


void MainForm::fileNewA68kAssemblySourceFile()
{
  newFile(asmFilesListItem,"; Assembly Source File\n; Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","files.png");
}


void MainForm::fileNewQuillSourceFile()
{
  newFile(qllFilesListItem,"// Quill Source File\n// Created "+QDateTime::currentDateTime ().toString(Qt::LocalDate)+"\n","filec.png");
}


void MainForm::fileNewTextFile()
{
  newFile(txtFilesListItem,"","filet.png");
}


void MainForm::updateLeftStatusLabel()
{
  QString text=QString::number(fileCount)+QString(" File")
               +QString(fileCount!=1?"s":"")+QString(" Total");
  QListViewItem *category=currentListItem;
  if (IS_FOLDER(currentListItem)||IS_FILE(currentListItem)) {
    while (category->parent()->rtti()==0x716CC0) category=category->parent();
    text+=QString(", ")+QString::number(category==hFilesListItem?hFileCount:
                                        category==cFilesListItem?cFileCount:
                                        category==sFilesListItem?sFileCount:
                                        category==asmFilesListItem?asmFileCount:
                                        category==qllFilesListItem?qllFileCount:
                                        category==oFilesListItem?oFileCount:
                                        category==aFilesListItem?aFileCount:
                                        category==txtFilesListItem?txtFileCount:
                                        othFileCount)
          +QString(" in Category");
  }
  leftStatusLabel->setText(text);
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
    rightStatusLabel->setText("project file name");
  } else if (IS_FOLDER(currentListItem)) {
    rowStatusLabel->hide();
    colStatusLabel->hide();
    charsStatusLabel->hide();
    rightStatusLabel->setMaximumWidth(rightStatusSize);
    rightStatusLabel->setText("");
  } else if (IS_FILE(currentListItem)) {
    QListViewItem *category=currentListItem;
    while (category->parent()->rtti()==0x716CC0) category=category->parent();
    if (category==hFilesListItem||category==cFilesListItem
        ||category==sFilesListItem||category==asmFilesListItem
        ||category==qllFilesListItem||category==txtFilesListItem) {
      unsigned int line, col;
      m_view->cursorPositionReal(&line,&col);
      rowStatusLabel->show();
      rowStatusLabel->setMaximumWidth(30);
      rowStatusLabel->setText(QString("%1").arg(line));
      colStatusLabel->show();
      colStatusLabel->setMaximumWidth(30);
      colStatusLabel->setText(QString("%1").arg(col+1));
      charsStatusLabel->show();
      charsStatusLabel->setMaximumWidth(100);
      charsStatusLabel->setText(QString("%1 Characters").arg(m_view->getDoc()->text().length()));
      rightStatusLabel->setMaximumWidth(rightStatusSize-160);
    } else {
      rowStatusLabel->hide();
      colStatusLabel->hide();
      charsStatusLabel->hide();
      rightStatusLabel->setMaximumWidth(rightStatusSize);
    }
    rightStatusLabel->setText(static_cast<ListViewFile *>(currentListItem)->fileName);
  }
}

void MainForm::m_view_cursorPositionChanged()
{
  unsigned int line, col;
  m_view->cursorPositionReal(&line,&col);
  rowStatusLabel->setText(QString("%1").arg(line));
  colStatusLabel->setText(QString("%1").arg(col+1));
}

void MainForm::m_view_textChanged()
{
  charsStatusLabel->setText(QString("%1 Characters").arg(m_view->getDoc()->text().length()));
}

// Yes, this is an ugly hack... Any better suggestions?
#define QListView DnDListView
