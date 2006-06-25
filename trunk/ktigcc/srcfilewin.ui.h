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
#include <kparts/factory.h>
#include <klibloader.h>
#include <kate/document.h>
#include <kate/view.h>
#include <kconfig.h>
#include <ktexteditor/editinterfaceext.h>
#include <ktexteditor/configinterfaceextension.h>
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
#include <cstdio>
#include <cstdlib>
#include "ktigcc.h"
#include "mainform.h"
#include "tpr.h"
#include "preferences.h"
#include "projectoptions.h"

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

#define IS_FOLDER(item) ((item) && (item)->rtti()==0x716CC0)
#define IS_FILE(item) ((item) && (item)->rtti()==0x716CC1)
#define CURRENT_VIEW (static_cast<Kate::View *>(widgetStack->visibleWidget()))

#define LOAD_ICON(name) (QIconSet(KGlobal::iconLoader()->loadIcon((name),KIcon::Small),KGlobal::iconLoader()->loadIcon((name),KIcon::MainToolbar)))
#define SYSICON(sysname,name) (preferences.useSystemIcons?KGlobal::iconLoader()->loadIcon((sysname),KIcon::Small):QPixmap::fromMimeSource((name)))

// For some reason, this flag is not in the public ConfigFlags enum.
#define CF_REMOVE_TRAILING_DYN 0x4000000

class KReplaceWithSelectionS : public KReplace {
  public:
    KReplaceWithSelectionS(const QString &pattern, const QString &replacement,
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
    unsigned replaceCurrentLine;
    void invalidateSelection() {m_haveSelection=FALSE;}
    bool haveSelection() {return m_haveSelection;}
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

// FIXME: There may be multiple instances of SourceFileWindow, so these have no
// business of being here.
static KReplaceWithSelectionS *kreplace;
static QLabel *rowStatusLabel;
static QLabel *colStatusLabel;
static QLabel *charsStatusLabel;
static QLabel *rightStatusLabel;
static QPopupMenu *te_popup;
static QString fileName;
static QClipboard *clipboard;
static QAccel *accel;
static KFindDialog *kfinddialog;
static unsigned findCurrentLine;
static bool isCFile;
static bool isASMFile;

void SourceFileWindow::init()
{  
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
  Kate::View *kateView=reinterpret_cast<Kate::View *>(createView(fileName,"","",8));
  fileName="";
  int rightStatusSize=size().width();
  unsigned int line, col;
  CURRENT_VIEW->cursorPositionReal(&line,&col);
  rowStatusLabel=new QLabel(QString("%1").arg(line+1),this);
  rowStatusLabel->setAlignment(Qt::AlignRight);
  rowStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(rowStatusLabel,1);
  colStatusLabel=new QLabel(QString("%1").arg(col+1),this);
  colStatusLabel->setAlignment(Qt::AlignRight);
  colStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(colStatusLabel,1);
  charsStatusLabel=new QLabel(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()),this);
  charsStatusLabel->setMaximumWidth(100);
  statusBar()->addWidget(charsStatusLabel,1);
  rightStatusLabel=new QLabel(fileName,this);
  rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  statusBar()->addWidget(rightStatusLabel,1);
  statusBar()->setSizeGripEnabled(FALSE);
  connect(statusBar(),SIGNAL(messageChanged(const QString &)),this,SLOT(statusBar_messageChanged(const QString &)));
  connect(KDirWatch::self(),SIGNAL(created(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  connect(KDirWatch::self(),SIGNAL(dirty(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  KDirWatch::self()->startScan();
  clipboard=QApplication::clipboard();
  connect(clipboard,SIGNAL(dataChanged()),this,SLOT(clipboard_dataChanged()));
  widgetStack->addWidget(kateView);
  kateView->show();
  widgetStack->raiseWidget(kateView);
  editUndoAction->setEnabled(!!(kateView->getDoc()->undoCount()));
  editRedoAction->setEnabled(!!(kateView->getDoc()->redoCount()));
  editClearAction->setEnabled(kateView->getDoc()->hasSelection());
  editCutAction->setEnabled(kateView->getDoc()->hasSelection());
  editCopyAction->setEnabled(kateView->getDoc()->hasSelection());
  editPasteAction->setEnabled(!clipboard->text().isNull());
  accel=new QAccel(this);
  accel->insertItem(ALT+Key_Backspace,0);
  accel->insertItem(SHIFT+ALT+Key_Backspace,1);
  accel->insertItem(SHIFT+Key_Delete,2);
  accel->insertItem(CTRL+Key_Insert,3);
  accel->insertItem(SHIFT+Key_Insert,4);
  accel->insertItem(Key_F1,5);
  accel->insertItem(Key_Enter,6);
  accel->insertItem(Key_Return,7);
  accel->setItemEnabled(0,!!(kateView->getDoc()->undoCount()));
  accel->setItemEnabled(1,!!(kateView->getDoc()->redoCount()));
  accel->setItemEnabled(2,kateView->getDoc()->hasSelection());
  accel->setItemEnabled(3,kateView->getDoc()->hasSelection());
  accel->setItemEnabled(4,!clipboard->text().isNull());
  accel->setItemEnabled(5,TRUE);
  accel->setItemEnabled(6,TRUE);
  accel->setItemEnabled(7,TRUE);
  connect(accel,SIGNAL(activated(int)),this,SLOT(accel_activated(int)));
  startTimer(100);
  kfinddialog = static_cast<KFindDialog *>(NULL);
  kreplace = static_cast<KReplaceWithSelectionS *>(NULL);
  if (preferences.useSystemIcons) {
    setUsesBigPixmaps(TRUE);
    fileSaveAction->setIconSet(LOAD_ICON("filesave"));
    fileAddToProjectAction->setIconSet(LOAD_ICON("edit_add"));
    fileCompileAction->setIconSet(LOAD_ICON("compfile"));
    filePrintAction->setIconSet(LOAD_ICON("fileprint"));
    filePrintQuicklyAction->setIconSet(LOAD_ICON("fileprint"));
    editClearAction->setIconSet(LOAD_ICON("editdelete"));
    editCutAction->setIconSet(LOAD_ICON("editcut"));
    editCopyAction->setIconSet(LOAD_ICON("editcopy"));
    editPasteAction->setIconSet(LOAD_ICON("editpaste"));
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
    editUndoAction->setIconSet(LOAD_ICON("undo"));
    editRedoAction->setIconSet(LOAD_ICON("redo"));
    findFunctionsAction->setIconSet(LOAD_ICON("view_tree"));
    editIncreaseIndentAction->setIconSet(LOAD_ICON("indent"));
    editDecreaseIndentAction->setIconSet(LOAD_ICON("unindent"));
    // stop compilation: "stop"
    // force-quit compiler: "button_cancel"
  }
}

void SourceFileWindow::destroy()
{
  if (kreplace) delete kreplace;
  if (kfinddialog) delete kfinddialog;
  delete accel;
  delete te_popup;
  delete rowStatusLabel;
  delete colStatusLabel;
  delete charsStatusLabel;
  delete rightStatusLabel;
}

void SourceFileWindow::te_popup_aboutToShow()
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

void SourceFileWindow::te_popup_activated(int index)
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

void SourceFileWindow::accel_activated(int index)
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

void *SourceFileWindow::createView(const QString &fileName, const QString &fileText, const QString &hlModeName, unsigned tabWidth)
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
    if (!newView->getDoc()->hlModeName(i).compare(hlModeName)) break;
  }
  if (i==cnt) i=0;
  newView->getDoc()->setHlMode(i);
  // Set options.
  newView->setDynWordWrap(FALSE);
  if (preferences.removeTrailingSpaces)
    newView->getDoc()->setConfigFlags(newView->getDoc()->configFlags()|(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  else
    newView->getDoc()->setConfigFlags(newView->getDoc()->configFlags()&~(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  newView->setTabWidth(tabWidth);
  connect(newView,SIGNAL(cursorPositionChanged()),this,SLOT(current_view_cursorPositionChanged()));
  connect(newView->getDoc(),SIGNAL(textChanged()),this,SLOT(current_view_textChanged()));
  connect(newView->getDoc(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  connect(newView->getDoc(),SIGNAL(selectionChanged()),this,SLOT(current_view_selectionChanged()));
  connect(newView->getDoc(),SIGNAL(charactersInteractivelyInserted(int,int,const QString&)),this,SLOT(current_view_charactersInteractivelyInserted(int,int,const QString&)));
  newView->installPopup(te_popup);
  // Set text.
  newView->getDoc()->setText(fileText);
  newView->getDoc()->setModified(FALSE);
  newView->getDoc()->clearUndo();
  newView->getDoc()->clearRedo();
  newView->setCursorPositionReal(0,0);
  return newView;
}

//returns 1 if the current project data should not be cleared out, 0 if it can be cleared out.
int SourceFileWindow::savePrompt(void)
{
  int result;
  if (!CURRENT_VIEW) return 0;
  while (CURRENT_VIEW->getDoc()->isModified()) { // "while" in case saving fails!
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(fileName),QString::null,KStdGuiItem::save(),KStdGuiItem::discard());
    if (result==KMessageBox::Yes)
        fileSave_save();
    else if (result==KMessageBox::No)
      CURRENT_VIEW->getDoc()->setModified(FALSE);
    else
      return 1;
  }
  return 0;
}

void SourceFileWindow::removeTrailingSpacesFromView(void *view)
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

void SourceFileWindow::fileSave_save()
{
  KDirWatch::self()->removeFile(fileName);
  if (saveFileText(fileName,CURRENT_VIEW->getDoc()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(fileName));
    KDirWatch::self()->addFile(fileName);
  } else {
    KDirWatch::self()->addFile(fileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->getDoc()->setModified(FALSE);
  }
}

void SourceFileWindow::fileSave_saveAs()
{
  QString saveFileName=MainForm::SGetFileName(KFileDialog::Saving,
  TIGCC_H_Filter TIGCC_C_Filter TIGCC_S_Filter TIGCC_ASM_Filter TIGCC_QLL_Filter
  TIGCC_TXT_Filter TIGCCAllFilter,"Save Source File",this);
  if (saveFileName.isEmpty())
    return;
  if (fileName[0]=='/')
    KDirWatch::self()->removeFile(fileName);
  if (saveFileText(saveFileName,CURRENT_VIEW->getDoc()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    if (fileName[0]=='/')
      KDirWatch::self()->addFile(fileName);
  } else {
    if (saveFileName.compare(fileName)) {
      // Update the file name for printing.
      unsigned int line,col,hlMode;
      QString fileText=CURRENT_VIEW->getDoc()->text();
      hlMode=CURRENT_VIEW->getDoc()->hlMode();
      CURRENT_VIEW->cursorPositionReal(&line,&col);
      CURRENT_VIEW->getDoc()->setModified(FALSE);
      if (CURRENT_VIEW->getDoc()->openStream("text/plain",saveFileName))
        CURRENT_VIEW->getDoc()->closeStream();
      CURRENT_VIEW->getDoc()->setText(fileText);
      CURRENT_VIEW->getDoc()->clearUndo();
      CURRENT_VIEW->getDoc()->clearRedo();
      CURRENT_VIEW->getDoc()->setHlMode(hlMode);
      CURRENT_VIEW->setCursorPositionReal(line,col);
    }
    fileName=saveFileName;
    KDirWatch::self()->addFile(saveFileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->getDoc()->setModified(FALSE);
    updateRightStatusLabel();
  }
}

void SourceFileWindow::fileSave()
{
  fileSave_save();
}

void SourceFileWindow::fileSaveAs()
{
  fileSave_saveAs();
}

void SourceFileWindow::fileAddToProject()
{

}

void SourceFileWindow::fileCompile()
{

}

void SourceFileWindow::filePrint()
{
  if (CURRENT_VIEW) CURRENT_VIEW->getDoc()->printDialog();
}


void SourceFileWindow::filePrintQuickly()
{
  if (CURRENT_VIEW) CURRENT_VIEW->getDoc()->print();
}

void SourceFileWindow::applyPreferences()
{
  if (showPreferencesDialog(this)==QDialog::Accepted) {
    // Apply the KatePart preferences and treeview icons.
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
    Kate::View *kateView=CURRENT_VIEW;
    if (kateView) {
    }
    if (CURRENT_VIEW) {
      Kate::View *currView=CURRENT_VIEW;
      QString fileText=currView->getDoc()->text();
      if (preferences.removeTrailingSpaces)
        currView->getDoc()->setConfigFlags(currView->getDoc()->configFlags()|(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
      else
        currView->getDoc()->setConfigFlags(currView->getDoc()->configFlags()&~(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
      currView->setTabWidth(isASMFile?preferences.tabWidthAsm:
                            isCFile?preferences.tabWidthC:8);
      // Force redrawing to get the tab width right, repaint() is ignored for some reason.
      currView->hide();
      currView->show();
    }
    // Apply the icon preferences.
    setUsesBigPixmaps(preferences.useSystemIcons);
    if (preferences.useSystemIcons) {
      fileSaveAction->setIconSet(LOAD_ICON("filesave"));
      fileAddToProjectAction->setIconSet(LOAD_ICON("edit_add"));
      fileCompileAction->setIconSet(LOAD_ICON("compfile"));
      filePrintAction->setIconSet(LOAD_ICON("fileprint"));
      filePrintQuicklyAction->setIconSet(LOAD_ICON("fileprint"));
      editClearAction->setIconSet(LOAD_ICON("editdelete"));
      editCutAction->setIconSet(LOAD_ICON("editcut"));
      editCopyAction->setIconSet(LOAD_ICON("editcopy"));
      editPasteAction->setIconSet(LOAD_ICON("editpaste"));
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
      editUndoAction->setIconSet(LOAD_ICON("undo"));
      editRedoAction->setIconSet(LOAD_ICON("redo"));
      findFunctionsAction->setIconSet(LOAD_ICON("view_tree"));
      editIncreaseIndentAction->setIconSet(LOAD_ICON("indent"));
      editDecreaseIndentAction->setIconSet(LOAD_ICON("unindent"));
      // stop compilation: "stop"
      // force-quit compiler: "button_cancel"
    } else {
      fileSaveAction->setIconSet(QIconSet(QPixmap::fromMimeSource("02")));
      fileAddToProjectAction->setIconSet(QIconSet(QPixmap::fromMimeSource("08")));
      fileCompileAction->setIconSet(QIconSet(QPixmap::fromMimeSource("09")));
      filePrintAction->setIconSet(QIconSet(QPixmap::fromMimeSource("03")));
      filePrintQuicklyAction->setIconSet(QIconSet(QPixmap::fromMimeSource("03")));
      editClearAction->setIconSet(QIconSet(QPixmap::fromMimeSource("04")));
      editCutAction->setIconSet(QIconSet(QPixmap::fromMimeSource("05")));
      editCopyAction->setIconSet(QIconSet(QPixmap::fromMimeSource("06")));
      editPasteAction->setIconSet(QIconSet(QPixmap::fromMimeSource("07")));
      findFindAction->setIconSet(QIconSet(QPixmap::fromMimeSource("13")));
      findReplaceAction->setIconSet(QIconSet(QPixmap::fromMimeSource("14")));
      editUndoAction->setIconSet(QIconSet(QPixmap::fromMimeSource("16")));
      editRedoAction->setIconSet(QIconSet(QPixmap::fromMimeSource("17")));
      findFunctionsAction->setIconSet(QIconSet(QPixmap::fromMimeSource("18")));
      editIncreaseIndentAction->setIconSet(QIconSet(QPixmap::fromMimeSource("19")));
      editDecreaseIndentAction->setIconSet(QIconSet(QPixmap::fromMimeSource("20")));
      // stop compilation: "21"
      // force-quit compiler: "22"
    }
  }
}

void SourceFileWindow::editUndo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->undo();
}

void SourceFileWindow::editRedo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->redo();
}

void SourceFileWindow::editClear()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->removeSelectedText();
}

void SourceFileWindow::editCut()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->cut();
}

void SourceFileWindow::editCopy()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->copy();
}

void SourceFileWindow::editPaste()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->paste();
}

void SourceFileWindow::editSelectAll()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->getDoc()->selectAll();
}

void SourceFileWindow::editIncreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->indent();
}

void SourceFileWindow::editDecreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->unIndent();
}

void SourceFileWindow::findFind()
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

void SourceFileWindow::findFind_next()
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
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  unsigned currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines)) {
        // Try restarting the search.
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
        not_found_current:
          KMessageBox::error(this,QString("Text \'%1\' not found").arg(kfinddialog->pattern()));
          delete kfind;
          return;
      } else if (findBackwards) findCurrentLine--; else findCurrentLine++;
      kfind->setData(currView->getDoc()->textLine(findCurrentLine));
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

#define unused_text text __attribute__((unused))
void SourceFileWindow::findFind_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPositionReal(findCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(findCurrentLine,matchingindex,
                                       findCurrentLine,matchingindex+matchedlength);
}

void SourceFileWindow::findFind_stop()
{
  if (kfinddialog) kfinddialog->deleteLater();
  kfinddialog=static_cast<KFindDialog *>(NULL);
}

void SourceFileWindow::findReplace()
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
  kreplace=new KReplaceWithSelectionS(kreplacedialog.pattern(),kreplacedialog.replacement(),
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
  if (CURRENT_VIEW) {
    if (kreplace->options()&KFindDialog::SelectedText) {
      kreplace->setSelection(CURRENT_VIEW->getDoc()->selStartLine(),
                             CURRENT_VIEW->getDoc()->selStartCol(),
                             CURRENT_VIEW->getDoc()->selEndLine(),
                             CURRENT_VIEW->getDoc()->selEndCol());
      if (findBackwards) {
        kreplace->replaceCurrentLine=kreplace->selEndLine();
        replaceCurrentCol=kreplace->selEndCol();
      } else {
        kreplace->replaceCurrentLine=kreplace->selStartLine();
        replaceCurrentCol=kreplace->selStartCol();
      }
      kreplace->setOptions(kreplace->options()&~KFindDialog::FromCursor);
    } else if (kreplace->options()&KFindDialog::FromCursor) {
      if (CURRENT_VIEW->getDoc()->hasSelection()) {
        if (findBackwards) {
          kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
          if (replaceCurrentCol==-1) {
            if (!kreplace->replaceCurrentLine) goto skip_data;
            kreplace->replaceCurrentLine--;
          }
        } else {
          kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
        }
      } else {
        kreplace->replaceCurrentLine=CURRENT_VIEW->cursorLine();
        replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(kreplace->replaceCurrentLine==(CURRENT_VIEW->getDoc()->numLines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->getDoc()->lineLength(kreplace->replaceCurrentLine)))
                         :(!kreplace->replaceCurrentLine&&!replaceCurrentCol))
          kreplace->setOptions(kreplace->options()&~KFindDialog::FromCursor);
      }
    } else {
      kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
      replaceCurrentCol=-1;
    }
    kreplace->setData(CURRENT_VIEW->getDoc()->textLine(kreplace->replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:
    // Now find the next occurrence.
    findReplace_next(TRUE);
}

void SourceFileWindow::findReplace_next()
{
  findReplace_next(FALSE);
}


void SourceFileWindow::findReplace_next(bool firstTime)
{
  bool findBackwards=!!(kreplace->options()&KFindDialog::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Non-first-time always continues from cursor.
    if (CURRENT_VIEW->getDoc()->hasSelection()) {
      if (findBackwards) {
        kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
        replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
        if (replaceCurrentCol==-1) {
          if (!kreplace->replaceCurrentLine) goto skip_data;
          kreplace->replaceCurrentLine--;
        }
      } else {
        kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
        replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
      }
    } else {
      kreplace->replaceCurrentLine=CURRENT_VIEW->cursorLine();
      replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
    }
    kreplace->setData(CURRENT_VIEW->getDoc()->textLine(kreplace->replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  unsigned currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (kreplace->needData()) {
      if (kreplace->haveSelection()
          ?(findBackwards?(kreplace->replaceCurrentLine<=kreplace->selStartLine())
                         :(kreplace->replaceCurrentLine>=kreplace->selEndLine()))
          :(findBackwards?!kreplace->replaceCurrentLine:(kreplace->replaceCurrentLine>=currNumLines))) {
        if (kreplace->shouldRestart()) {
          // Drop "From cursor" and "Selected text" options.
          kreplace->setOptions(kreplace->options()&~(KFindDialog::FromCursor
                                                     |KFindDialog::SelectedText));
          kreplace->invalidateSelection();
          // Reinitialize.
          kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
          kreplace->setData(CURRENT_VIEW->getDoc()->textLine(kreplace->replaceCurrentLine));
          // Start again as if it was the first time.
          findReplace_next(TRUE);
          return;
        } else {
          findReplace_stop();
          return;
        }
      } else if (findBackwards) kreplace->replaceCurrentLine--; else kreplace->replaceCurrentLine++;
      kreplace->setData(currView->getDoc()->textLine(kreplace->replaceCurrentLine));
    }
    result=kreplace->replace();
  } while (result==KFind::NoMatch);
}

void SourceFileWindow::findReplace_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPositionReal(kreplace->replaceCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(kreplace->replaceCurrentLine,matchingindex,
                                       kreplace->replaceCurrentLine,matchingindex+matchedlength);
}

void SourceFileWindow::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
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
  CURRENT_VIEW->getDoc()->insertText(kreplace->replaceCurrentLine,replacementIndex,
                                     text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  bool updateCursor=(CURRENT_VIEW->cursorLine()==kreplace->replaceCurrentLine
                     && CURRENT_VIEW->cursorColumnReal()==(unsigned)replacementIndex+(unsigned)replacedLength);
  CURRENT_VIEW->getDoc()->removeText(kreplace->replaceCurrentLine,replacementIndex+replacedLength,
                                     kreplace->replaceCurrentLine,replacementIndex+replacedLength+matchedLength);
  editinterfaceext->editEnd();
  if (updateCursor)
    CURRENT_VIEW->setCursorPositionReal(kreplace->replaceCurrentLine,replacementIndex);
  if (update) {
    CURRENT_VIEW->setCursorPositionReal(kreplace->replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->getDoc()->setSelection(kreplace->replaceCurrentLine,replacementIndex,
                                         kreplace->replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->repaint();
  }
  if (haveSelection) {
    // Restore selection, updating coordinates if necessary.
    kreplace->setSelection(selStartLine,selStartCol,selEndLine,
                           (kreplace->replaceCurrentLine==selEndLine)
                           ?(selEndCol+replacedLength-matchedLength)
                           :selEndCol);
  }
}

void SourceFileWindow::findReplace_stop()
{
  if (kreplace) kreplace->deleteLater();
  kreplace=static_cast<KReplaceWithSelectionS *>(NULL);
}

void SourceFileWindow::findFunctions()
{
  
}

void SourceFileWindow::findOpenFileAtCursor()
{
  
}

void SourceFileWindow::findFindSymbolDeclaration()
{

}

void SourceFileWindow::updateSizes()
{
  int rightStatusSize=size().width();
  rowStatusLabel->setMaximumWidth(30);
  colStatusLabel->setMaximumWidth(30);
  charsStatusLabel->setMaximumWidth(100);
  rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
}

void SourceFileWindow::resizeEvent(QResizeEvent *event)
{
  QMainWindow::resizeEvent(event);
  if (event->size()==event->oldSize()) return;
  updateSizes();
}

void SourceFileWindow::timerEvent(QTimerEvent *event)
{
  static int lastSplitterPos=-1;
  QMainWindow::timerEvent(event);
  if (lastSplitterPos==splitter->sizes().first()) return;
  lastSplitterPos=splitter->sizes().first();
  updateSizes();
}

void SourceFileWindow::statusBar_messageChanged(const QString & message)
{
  if (message.isNull())
    // Make sure no labels which should be hidden are shown.
    updateRightStatusLabel();
}

void SourceFileWindow::updateRightStatusLabel()
{
  int rightStatusSize=size().width();
  unsigned int line, col;
  CURRENT_VIEW->cursorPositionReal(&line,&col);
  rowStatusLabel->setMaximumWidth(30);
  rowStatusLabel->setText(QString("%1").arg(line+1));
  colStatusLabel->setMaximumWidth(30);
  colStatusLabel->setText(QString("%1").arg(col+1));
  charsStatusLabel->setMaximumWidth(100);
  charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
  rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  rightStatusLabel->setText(fileName);
}

void SourceFileWindow::current_view_cursorPositionChanged()
{
  if (CURRENT_VIEW) {
    unsigned int line, col;
    CURRENT_VIEW->cursorPositionReal(&line,&col);
    rowStatusLabel->setText(QString("%1").arg(line+1));
    colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void SourceFileWindow::current_view_textChanged()
{
  if (CURRENT_VIEW)
    charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
  if (kreplace) kreplace->invalidateSelection();
}

void SourceFileWindow::current_view_undoChanged()
{
  if (CURRENT_VIEW) {
    editUndoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->undoCount()));
    editRedoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->redoCount()));
    accel->setItemEnabled(0,!!(CURRENT_VIEW->getDoc()->undoCount()));
    accel->setItemEnabled(1,!!(CURRENT_VIEW->getDoc()->redoCount()));
  }
}

void SourceFileWindow::current_view_selectionChanged()
{
  if (CURRENT_VIEW) {
    editClearAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCutAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCopyAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    accel->setItemEnabled(2,CURRENT_VIEW->getDoc()->hasSelection());
    accel->setItemEnabled(3,CURRENT_VIEW->getDoc()->hasSelection());
  }
}

void SourceFileWindow::current_view_charactersInteractivelyInserted(int line, int col, const QString &characters)
{
  if (CURRENT_VIEW && preferences.autoBlocks && !characters.compare("{")
      && col==CURRENT_VIEW->getDoc()->lineLength(line)-1) {
    Kate::Document *doc=CURRENT_VIEW->getDoc();
    QString fileText=doc->text();
    // Only for C files.
    if (isCFile) {
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

void SourceFileWindow::current_view_newLineHook()
{
  unsigned line,col;
  CURRENT_VIEW->cursorPositionReal(&line,&col);
  Kate::Document *doc=CURRENT_VIEW->getDoc();
  if (preferences.autoBlocks && line && doc->textLine(line-1).endsWith("{")) {
    QString fileText=doc->text();
    // Only for C files.
    if (isCFile) {
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

void SourceFileWindow::clipboard_dataChanged()
{
  if (CURRENT_VIEW) {
    editPasteAction->setEnabled(!clipboard->text().isNull());
    accel->setItemEnabled(4,!clipboard->text().isNull());
  }
}

void SourceFileWindow::closeEvent(QCloseEvent *e)
{
  if (savePrompt())
    e->ignore();
  else
    e->accept();
}

void SourceFileWindow::KDirWatch_dirty(const QString &fileName)
{
  if (KMessageBox::questionYesNo(this,
        QString("The file \'%1\' has been changed by another program. "
                "Do you want to reload it?").arg(fileName),"File Changed")
        ==KMessageBox::Yes) {
    QString fileText=loadFileText(fileName);
    if (fileText.isNull()) {
      KMessageBox::error(this,QString("Can't open \'%1\'").arg(fileName));
      return;
    }
    CURRENT_VIEW->getDoc()->setText(fileText);
    CURRENT_VIEW->getDoc()->setModified(FALSE);
    CURRENT_VIEW->getDoc()->clearUndo();
    CURRENT_VIEW->getDoc()->clearRedo();
  }
  return;
}
