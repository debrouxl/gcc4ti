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
#include <qdir.h>
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
#include <kmessagebox.h>
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
                                    cursorLine(1), cursorCol(0),
                                    isNew(TRUE), isDirty(FALSE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));    
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent) : QListViewItem(parent),
                                        cursorLine(1), cursorCol(0),
                                        isNew(TRUE), isDirty(FALSE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDragEnabled(TRUE);
    setDropEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListView *parent, QListViewItem *after)
          : QListViewItem(parent, after), cursorLine(1), cursorCol(0),
            isNew(TRUE), isDirty(FALSE)
  {
    setPixmap(0,QPixmap::fromMimeSource("filex.png"));
    setDropEnabled(TRUE);
    setDragEnabled(TRUE);
    setRenameEnabled(0,TRUE);
  }
  ListViewFile(QListViewItem *parent, QListViewItem *after)
          : QListViewItem(parent, after), cursorLine(1), cursorCol(0),
            isNew(TRUE), isDirty(FALSE)
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
  bool isNew;
  bool isDirty;
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
static bool currentListItemEditable;
static bool projectIsDirty;
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
static tprSettings settings;
static tprLibOpts libopts;
static QString projectFileName;
static QString lastDirectory;

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
          else {
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
            // we changed the counters
            static_cast<MainForm *>(parent())->updateLeftStatusLabel();
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
  projectFileName="";
  projectIsDirty=FALSE;
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
  clearProject();
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

QListViewItem * MainForm::openFile(QListViewItem * category, QListViewItem * parent, const QString &fileCaption, const QString &fileName)
{
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
  if (IS_EDITABLE_CATEGORY(category)) {
    QString fileText=loadFileText(fileName);
    if (fileText.isNull()) {
      KMessageBox::sorry(this,QString("Can't open \'%1\'").arg(fileName),"Warning");
      fileText="";
    }
    newFile->textBuffer=fileText;
  }
  newFile->fileName=fileName;
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

void MainForm::fileOpen_addList(QListViewItem *category,void *fileListV,void *dir, const QString &open_file)
{
  int i,e;
  int p;
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
    p=caption.findRev('.');
    if (p>=0) caption.truncate(p);
    p=caption.findRev('/');
    if (p>=0) caption.remove(0,p+1);
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
    if (!newFile->fileName.compare(open_file))
      fileTreeClicked(newFile);
  }
}

void MainForm::openProject(const QString &fileName)
{
  TPRDataStruct TPRData;
  KURL dir;
  dir.setPath(fileName);
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
}

void MainForm::fileOpen()
{
  QString fileName=SGetFileName(KFileDialog::Opening,findFilter(TIGCCOpenProjectFileFilter),"Open Project/File",this);
  KURL dir;
  dir.setPath(fileName);
  if (fileName.isEmpty())
    return;
  openProject(fileName);
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
  if (IS_EDITABLE_CATEGORY(category)
      ?saveFileText(saveFileName,theFile->textBuffer)
      :copyFile(theFile->fileName,saveFileName))
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
  else {
    theFile->fileName=saveFileName;
    theFile->isNew=FALSE;
    theFile->isDirty=FALSE;
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
      
      if (tmpPath.path().compare(theFile->fileName)
          || (IS_EDITABLE_CATEGORY(category)
              && (theFile->isDirty || theFile->isNew))) {
        tmpPath=*new_dir;
        kurlNewFileName(tmpPath,relPath);
        if (IS_EDITABLE_CATEGORY(category)
            ?saveFileText(tmpPath.path(),theFile->textBuffer)
            :copyFile(theFile->fileName,tmpPath.path()))
          KMessageBox::error(this,QString("Can't save to \'%1\'").arg(tmpPath.path()));
        else {
          theFile->fileName=tmpPath.path();
          theFile->isNew=FALSE;
          theFile->isDirty=FALSE;
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
  
  if (IS_FILE(currentListItem))
    static_cast<ListViewFile *>(currentListItem)->textBuffer=m_view->getDoc()->text();
    //we don't want to make it so you have to click to another file and back to save the current document properly ;)
  
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
  }
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
    if (currentListItemEditable) {
      static_cast<ListViewFile *>(currentListItem)->textBuffer=m_view->getDoc()->text();
      m_view->cursorPositionReal(&(static_cast<ListViewFile *>(currentListItem)->cursorLine),
                                 &(static_cast<ListViewFile *>(currentListItem)->cursorCol));
    }
    
    
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
    CATEGORY_OF(category,item->parent());
    if (IS_EDITABLE_CATEGORY(category)) {
      m_view->getDoc()->setText(static_cast<ListViewFile *>(item)->textBuffer);
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
      currentListItemEditable=TRUE;
    } else {
      m_view->getDoc()->setText("");
      write_temp_file("config.tmp","[Kate Renderer Defaults]\nSchema=ktigcc - Grayed Out\n",0);
      KConfig kconfig(QString(tempdir)+"/config.tmp",true);
      m_view->getDoc()->readConfig(&kconfig);
      delete_temp_file("config.tmp");
      m_view->getDoc()->setHlMode(0);
      currentListItemEditable=FALSE;
    }
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
  
  newFile->setText(0,caption);
  newFile->setPixmap(0,QPixmap::fromMimeSource(iconName));
  parent->setOpen(TRUE);
  newFile->textBuffer=text;
  fileTreeClicked(newFile);
  newFile->startRename(0);
  
  m_view->getDoc()->setText(text);
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
    if (IS_EDITABLE_CATEGORY(category)) {
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
  if (IS_FILE(currentListItem))
    static_cast<ListViewFile *>(currentListItem)->isDirty=TRUE;
  charsStatusLabel->setText(QString("%1 Characters").arg(m_view->getDoc()->text().length()));
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
    if (!theFile->isNew && !QDir().rename(oldFileName,newFileName)) {
      KMessageBox::error(this,"Failed to rename the file.");
      theFile->setText(0,oldLabel);
    } else {
      fileNameRef=newFileName;
    }
  } else {
    KMessageBox::error(this,"The name you chose conflicts with that of another file.");
    theFile->setText(0,oldLabel);
  }
  
  updateRightStatusLabel();
}

// Yes, this is an ugly hack... Any better suggestions?
#define QListView DnDListView
