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
#include <qlayout.h>
#include <qtoolbutton.h>
#include <qlistbox.h>
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
#include <kpushbutton.h>
#include <cstdio>
#include <cstdlib>
#include "ktigcc.h"
#include "mainform.h"
#include "tpr.h"
#include "preferences.h"
#include "projectoptions.h"
#include "srcfile.h"
#include "functions.h"

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

#define THIS (static_cast<SourceFile *>(this))
#define CURRENT_VIEW (THIS->kateView)
#define HL_MODE ((THIS->hlEnabled && *(THIS->hlEnabled))?THIS->hlMode:"None")

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

void SourceFileWindow::initBase()
{
  setIcon(QPixmap::fromMimeSource("icon.png"));
  KWin::setIcons(winId(),*(icon()),QPixmap::fromMimeSource("ktigcc.png"));
  sourceFiles.append(THIS);
  THIS->dirWatch=new KDirWatch(this);
  setCaption(caption()+" - "+THIS->fileName);
  THIS->te_popup = new QPopupMenu(this);
  THIS->te_popup->insertItem("&Open file at cursor",0);
  THIS->te_popup->insertItem("&Find symbol declaration",1);
  THIS->te_popup->insertSeparator();
  THIS->te_popup->insertItem("&Undo",2);
  THIS->te_popup->insertItem("&Redo",3);
  THIS->te_popup->insertSeparator();
  THIS->te_popup->insertItem("&Clear",4);
  THIS->te_popup->insertItem("Cu&t",5);
  THIS->te_popup->insertItem("Cop&y",6);
  THIS->te_popup->insertItem("&Paste",7);
  THIS->te_popup->insertSeparator();
  THIS->te_popup->insertItem("&Select all",8);
  THIS->te_popup->insertSeparator();
  THIS->te_popup->insertItem("&Increase indent",9);
  THIS->te_popup->insertItem("&Decrease indent",10);
  connect(THIS->te_popup,SIGNAL(aboutToShow()),this,SLOT(te_popup_aboutToShow()));
  connect(THIS->te_popup,SIGNAL(activated(int)),this,SLOT(te_popup_activated(int)));
  THIS->kfinddialog = static_cast<KFindDialog *>(NULL);
  THIS->kreplace = static_cast<KReplaceWithSelectionS *>(NULL);
  THIS->kateView=static_cast<Kate::View *>(NULL);
  THIS->kateView=reinterpret_cast<Kate::View *>(createView(THIS->fileName,THIS->fileText,HL_MODE,
    THIS->isASMFile?preferences.tabWidthAsm:THIS->isCFile?preferences.tabWidthC:8));
  THIS->fileText=QString::null;
  int rightStatusSize=size().width();
  unsigned int line, col;
  CURRENT_VIEW->cursorPositionReal(&line,&col);
  THIS->rowStatusLabel=new QLabel(QString("%1").arg(line+1),this);
  THIS->rowStatusLabel->setAlignment(Qt::AlignRight);
  THIS->rowStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(THIS->rowStatusLabel,1);
  THIS->colStatusLabel=new QLabel(QString("%1").arg(col+1),this);
  THIS->colStatusLabel->setAlignment(Qt::AlignRight);
  THIS->colStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(THIS->colStatusLabel,1);
  THIS->charsStatusLabel=new QLabel(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()),this);
  THIS->charsStatusLabel->setMaximumWidth(100);
  statusBar()->addWidget(THIS->charsStatusLabel,1);
  THIS->rightStatusLabel=new QLabel(THIS->fileName,this);
  THIS->rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  statusBar()->addWidget(THIS->rightStatusLabel,1);
  statusBar()->setSizeGripEnabled(FALSE);
  connect(statusBar(),SIGNAL(messageChanged(const QString &)),this,SLOT(statusBar_messageChanged(const QString &)));
  connect(THIS->dirWatch,SIGNAL(created(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  connect(THIS->dirWatch,SIGNAL(dirty(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  THIS->dirWatch->addFile(THIS->fileName);
  THIS->dirWatch->startScan();
  connect(clipboard,SIGNAL(dataChanged()),this,SLOT(clipboard_dataChanged()));
  centralWidget()->layout()->add(CURRENT_VIEW);
  CURRENT_VIEW->show();
  editUndoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->undoCount()));
  editRedoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->redoCount()));
  editClearAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
  editCutAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
  editCopyAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
  editPasteAction->setEnabled(!clipboard->text().isNull());
  THIS->accel=new QAccel(this);
  THIS->accel->insertItem(ALT+Key_Backspace,0);
  THIS->accel->insertItem(SHIFT+ALT+Key_Backspace,1);
  THIS->accel->insertItem(SHIFT+Key_Delete,2);
  THIS->accel->insertItem(CTRL+Key_Insert,3);
  THIS->accel->insertItem(SHIFT+Key_Insert,4);
  THIS->accel->insertItem(Key_F1,5);
  THIS->accel->insertItem(Key_Enter,6);
  THIS->accel->insertItem(Key_Return,7);
  THIS->accel->setItemEnabled(0,!!(CURRENT_VIEW->getDoc()->undoCount()));
  THIS->accel->setItemEnabled(1,!!(CURRENT_VIEW->getDoc()->redoCount()));
  THIS->accel->setItemEnabled(2,CURRENT_VIEW->getDoc()->hasSelection());
  THIS->accel->setItemEnabled(3,CURRENT_VIEW->getDoc()->hasSelection());
  THIS->accel->setItemEnabled(4,!clipboard->text().isNull());
  THIS->accel->setItemEnabled(5,TRUE);
  THIS->accel->setItemEnabled(6,TRUE);
  THIS->accel->setItemEnabled(7,TRUE);
  connect(THIS->accel,SIGNAL(activated(int)),this,SLOT(accel_activated(int)));
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
  }
  QToolButton *findFunctionsButton=static_cast<QToolButton *>(toolBar
    ->child("findFunctionsAction_action_button","QToolButton",FALSE));
  THIS->findFunctionsPopup=new QPopupMenu(findFunctionsButton);
  connect(THIS->findFunctionsPopup,SIGNAL(aboutToShow()),
          this,SLOT(findFunctionsPopup_aboutToShow()));
  connect(THIS->findFunctionsPopup,SIGNAL(aboutToHide()),
          this,SLOT(findFunctionsPopup_aboutToHide()));
  connect(THIS->findFunctionsPopup,SIGNAL(activated(int)),
          this,SLOT(findFunctionsPopup_activated(int)));
  findFunctionsButton->setPopupDelay(0);
  findFunctionsButton->setPopup(THIS->findFunctionsPopup);
  if (THIS->isTextFile) findFunctionsAction->setEnabled(FALSE);
}

void SourceFileWindow::destroy()
{
  MainForm::deleteErrorsForSrcFile(this);
  if (THIS->kreplace) delete THIS->kreplace;
  if (THIS->kfinddialog) delete THIS->kfinddialog;
  delete THIS->accel;
  delete THIS->te_popup;
  delete THIS->rowStatusLabel;
  delete THIS->colStatusLabel;
  delete THIS->charsStatusLabel;
  delete THIS->rightStatusLabel;
  THIS->dirWatch->removeFile(THIS->fileName);
  delete THIS->dirWatch;
  if (THIS->kateView) delete THIS->kateView->getDoc();
  sourceFiles.remove(THIS);
}

void SourceFileWindow::te_popup_aboutToShow()
{
  THIS->te_popup->setItemEnabled(0,findOpenFileAtCursorAction->isEnabled());
  THIS->te_popup->setItemEnabled(1,findFindSymbolDeclarationAction->isEnabled());
  THIS->te_popup->setItemEnabled(2,editUndoAction->isEnabled());
  THIS->te_popup->setItemEnabled(3,editRedoAction->isEnabled());
  THIS->te_popup->setItemEnabled(4,editClearAction->isEnabled());
  THIS->te_popup->setItemEnabled(5,editCutAction->isEnabled());
  THIS->te_popup->setItemEnabled(6,editCopyAction->isEnabled());
  THIS->te_popup->setItemEnabled(7,editPasteAction->isEnabled());
  THIS->te_popup->setItemEnabled(8,editSelectAllAction->isEnabled());
  THIS->te_popup->setItemEnabled(9,editIncreaseIndentAction->isEnabled());
  THIS->te_popup->setItemEnabled(10,editDecreaseIndentAction->isEnabled());
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
      factory->createPart( 0, "", THIS->mainForm, "", "Kate::Document" );
  // Set the file name for printing.
  doc->setModified(FALSE);
  if (doc->openStream("text/plain",fileName))
    doc->closeStream();
  // Create View object.
  Kate::View *newView = (Kate::View *) doc->createView( centralWidget(), 0L );
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
  newView->installPopup(THIS->te_popup);
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
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(THIS->fileName),QString::null,KStdGuiItem::save(),KStdGuiItem::discard());
    if (result==KMessageBox::Yes)
        fileSave();
    else if (result==KMessageBox::No)
      return 0;
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

void SourceFileWindow::fileSave()
{
  THIS->dirWatch->removeFile(THIS->fileName);
  if (saveFileText(THIS->fileName,CURRENT_VIEW->getDoc()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(THIS->fileName));
    THIS->dirWatch->addFile(THIS->fileName);
  } else {
    THIS->dirWatch->addFile(THIS->fileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->getDoc()->setModified(FALSE);
  }
}

void SourceFileWindow::fileSaveAs()
{
  QString saveFileName=MainForm::SGetFileName(KFileDialog::Saving,
  (THIS->fileName.endsWith(".h")?TIGCC_H_Filter TIGCCAllFilter:
  THIS->fileName.endsWith(".c")?TIGCC_C_Filter TIGCCAllFilter:
  THIS->fileName.endsWith(".s")?TIGCC_S_Filter TIGCCAllFilter:
  THIS->fileName.endsWith(".asm")?TIGCC_ASM_Filter TIGCCAllFilter:
  THIS->fileName.endsWith(".qll")?TIGCC_QLL_Filter TIGCCAllFilter:
  THIS->fileName.endsWith(".txt")?TIGCC_TXT_Filter TIGCCAllFilter:
  TIGCCAllFilter),"Save Source File",this);
  if (saveFileName.isEmpty())
    return;
  THIS->dirWatch->removeFile(THIS->fileName);
  if (saveFileText(saveFileName,CURRENT_VIEW->getDoc()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    THIS->dirWatch->addFile(THIS->fileName);
  } else {
    if (saveFileName.compare(THIS->fileName)) {
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
      // Update the caption
      setCaption(caption().left(caption().find('-')+2)+saveFileName);
    }
    THIS->fileName=saveFileName;
    THIS->dirWatch->addFile(saveFileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->getDoc()->setModified(FALSE);
    updateRightStatusLabel();
  }
}

void SourceFileWindow::fileAddToProject()
{
  THIS->mainForm->adoptSourceFile(THIS);
}

void SourceFileWindow::fileCompile()
{
  THIS->mainForm->compileSourceFile(THIS);
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
    QString fileText=kateView->getDoc()->text();
    if (preferences.removeTrailingSpaces)
      kateView->getDoc()->setConfigFlags(kateView->getDoc()->configFlags()|(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
    else
      kateView->getDoc()->setConfigFlags(kateView->getDoc()->configFlags()&~(Kate::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
    kateView->setTabWidth(THIS->isASMFile?preferences.tabWidthAsm:
                          THIS->isCFile?preferences.tabWidthC:8);
    // Kate seems really insisting on making it a pain to update syntax highlighting settings.
    unsigned cnt=kateView->getDoc()->hlModeCount(), i;
    for (i=0; i<cnt; i++) {
      if (kateView->getDoc()->hlModeName(i)==HL_MODE) break;
    }
    if (i==cnt) i=0;
    kateView->getDoc()->setHlMode(i);
    // Force redrawing to get the tab width right, repaint() is ignored for some reason.
    kateView->hide();
    kateView->show();
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
  if (THIS->kfinddialog)
    KWin::activateWindow(THIS->kfinddialog->winId());
  else {
    // Never set hasSelection because finding in selection doesn't really make
    // sense with my non-modal find dialog setup.
    THIS->kfinddialog=new KFindDialog(false,this,0,KFindDialog::FromCursor);
    connect(THIS->kfinddialog, SIGNAL(okClicked()), this, SLOT(findFind_next()));
    connect(THIS->kfinddialog, SIGNAL(cancelClicked()), this, SLOT(findFind_stop()));
    THIS->kfinddialog->show();
  }
}

void SourceFileWindow::findFind_next()
{
  // Use a local KFind object. The search will need to be restarted next time
  // this function is called because of the non-modality of the find dialog.
  KFind *kfind=new KFind(THIS->kfinddialog->pattern(),THIS->kfinddialog->options(),this,THIS->kfinddialog);

  // Initialize.
  bool findBackwards=!!(THIS->kfinddialog->options()&KFindDialog::FindBackwards);
  int findCurrentCol;
  kfind=new KFind(THIS->kfinddialog->pattern(),THIS->kfinddialog->options(),this,THIS->kfinddialog);
  kfind->closeFindNextDialog(); // don't use this, a non-modal KFindDialog is used instead
  connect(kfind,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findFind_highlight(const QString &,int,int)));
  if (THIS->kfinddialog->options()&KFindDialog::FromCursor) {
    if (CURRENT_VIEW->getDoc()->hasSelection()) {
      if (findBackwards) {
        THIS->findCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
        findCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
        if (findCurrentCol==-1) {
          if (!THIS->findCurrentLine) goto skip_data;
          THIS->findCurrentLine--;
        }
      } else {
        THIS->findCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
        findCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
      }
    } else {
      THIS->findCurrentLine=CURRENT_VIEW->cursorLine();
      findCurrentCol=CURRENT_VIEW->cursorColumnReal();
    }
  } else {
    THIS->findCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
    findCurrentCol=-1;
  }
  kfind->setData(CURRENT_VIEW->getDoc()->textLine(THIS->findCurrentLine),findCurrentCol);
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  unsigned currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!THIS->findCurrentLine:(THIS->findCurrentLine>=currNumLines)) {
        // Try restarting the search.
        currNumLines=currView->getDoc()->numLines();
        THIS->findCurrentLine=findBackwards?currNumLines-1:0;
        do {
          if (kfind->needData()) {
            if (findBackwards?!THIS->findCurrentLine:(THIS->findCurrentLine>=currNumLines))
              goto not_found_current;
            if (findBackwards) THIS->findCurrentLine--; else THIS->findCurrentLine++;
            kfind->setData(currView->getDoc()->textLine(THIS->findCurrentLine));
          }
          result=kfind->find();
        } while (result==KFind::NoMatch);
        break;
        not_found_current:
          KMessageBox::error(this,QString("Text \'%1\' not found").arg(THIS->kfinddialog->pattern()));
          delete kfind;
          return;
      } else if (findBackwards) THIS->findCurrentLine--; else THIS->findCurrentLine++;
      kfind->setData(currView->getDoc()->textLine(THIS->findCurrentLine));
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

#define unused_text text __attribute__((unused))
void SourceFileWindow::findFind_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPositionReal(THIS->findCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(THIS->findCurrentLine,matchingindex,
                                       THIS->findCurrentLine,matchingindex+matchedlength);
}

void SourceFileWindow::findFind_stop()
{
  if (THIS->kfinddialog) THIS->kfinddialog->deleteLater();
  THIS->kfinddialog=static_cast<KFindDialog *>(NULL);
}

void SourceFileWindow::findReplace()
{
  if (THIS->kreplace) {
    KDialogBase *replaceNextDialog=THIS->kreplace->replaceNextDialog();
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
  THIS->kreplace=new KReplaceWithSelectionS(kreplacedialog.pattern(),kreplacedialog.replacement(),
                                            kreplacedialog.options(),this);
  // Connect signals to code which handles highlighting of found text, and
  // on-the-fly replacement.
  connect(THIS->kreplace,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findReplace_highlight(const QString &,int,int)));
  // Connect findNext signal - called when pressing the button in the dialog.
  connect(THIS->kreplace,SIGNAL(findNext()),this,SLOT(findReplace_next()));
  // Connect replace signal - called when doing a replacement.
  connect(THIS->kreplace,SIGNAL(replace(const QString &,int,int,int)),
          this,SLOT(findReplace_replace(const QString &,int,int,int)));
  // Connect dialogClosed signal - called when closing the Replace Next dialog.
  connect(THIS->kreplace,SIGNAL(dialogClosed()),this,SLOT(findReplace_stop()));
  // Initialize.
  bool findBackwards=!!(THIS->kreplace->options()&KFindDialog::FindBackwards);
  int replaceCurrentCol;
  if (CURRENT_VIEW) {
    if (THIS->kreplace->options()&KFindDialog::SelectedText) {
      THIS->kreplace->setSelection(CURRENT_VIEW->getDoc()->selStartLine(),
                                   CURRENT_VIEW->getDoc()->selStartCol(),
                                   CURRENT_VIEW->getDoc()->selEndLine(),
                                   CURRENT_VIEW->getDoc()->selEndCol());
      if (findBackwards) {
        THIS->kreplace->replaceCurrentLine=THIS->kreplace->selEndLine();
        replaceCurrentCol=THIS->kreplace->selEndCol();
      } else {
        THIS->kreplace->replaceCurrentLine=THIS->kreplace->selStartLine();
        replaceCurrentCol=THIS->kreplace->selStartCol();
      }
      THIS->kreplace->setOptions(THIS->kreplace->options()&~KFindDialog::FromCursor);
    } else if (THIS->kreplace->options()&KFindDialog::FromCursor) {
      if (CURRENT_VIEW->getDoc()->hasSelection()) {
        if (findBackwards) {
          THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
          if (replaceCurrentCol==-1) {
            if (!THIS->kreplace->replaceCurrentLine) goto skip_data;
            THIS->kreplace->replaceCurrentLine--;
          }
        } else {
          THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
          replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
        }
      } else {
        THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->cursorLine();
        replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(THIS->kreplace->replaceCurrentLine==(CURRENT_VIEW->getDoc()->numLines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->getDoc()->lineLength(THIS->kreplace->replaceCurrentLine)))
                         :(!THIS->kreplace->replaceCurrentLine&&!replaceCurrentCol))
          THIS->kreplace->setOptions(THIS->kreplace->options()&~KFindDialog::FromCursor);
      }
    } else {
      THIS->kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
      replaceCurrentCol=-1;
    }
    THIS->kreplace->setData(CURRENT_VIEW->getDoc()->textLine(THIS->kreplace->replaceCurrentLine),replaceCurrentCol);
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
  bool findBackwards=!!(THIS->kreplace->options()&KFindDialog::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Non-first-time always continues from cursor.
    if (CURRENT_VIEW->getDoc()->hasSelection()) {
      if (findBackwards) {
        THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selStartLine();
        replaceCurrentCol=CURRENT_VIEW->getDoc()->selStartCol()-1;
        if (replaceCurrentCol==-1) {
          if (!THIS->kreplace->replaceCurrentLine) goto skip_data;
          THIS->kreplace->replaceCurrentLine--;
        }
      } else {
        THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->getDoc()->selEndLine();
        replaceCurrentCol=CURRENT_VIEW->getDoc()->selEndCol();
      }
    } else {
      THIS->kreplace->replaceCurrentLine=CURRENT_VIEW->cursorLine();
      replaceCurrentCol=CURRENT_VIEW->cursorColumnReal();
    }
    THIS->kreplace->setData(CURRENT_VIEW->getDoc()->textLine(THIS->kreplace->replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  Kate::View *currView=CURRENT_VIEW;
  unsigned currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->getDoc()->numLines();
  do {
    if (THIS->kreplace->needData()) {
      if (THIS->kreplace->haveSelection()
          ?(findBackwards?(THIS->kreplace->replaceCurrentLine<=THIS->kreplace->selStartLine())
                         :(THIS->kreplace->replaceCurrentLine>=THIS->kreplace->selEndLine()))
          :(findBackwards?!THIS->kreplace->replaceCurrentLine:(THIS->kreplace->replaceCurrentLine>=currNumLines))) {
        if (THIS->kreplace->shouldRestart()) {
          // Drop "From cursor" and "Selected text" options.
          THIS->kreplace->setOptions(THIS->kreplace->options()&~(KFindDialog::FromCursor
                                                                 |KFindDialog::SelectedText));
          THIS->kreplace->invalidateSelection();
          // Reinitialize.
          THIS->kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->getDoc()->numLines()-1):0;
          THIS->kreplace->setData(CURRENT_VIEW->getDoc()->textLine(THIS->kreplace->replaceCurrentLine));
          // Start again as if it was the first time.
          findReplace_next(TRUE);
          return;
        } else {
          findReplace_stop();
          return;
        }
      } else if (findBackwards) THIS->kreplace->replaceCurrentLine--; else THIS->kreplace->replaceCurrentLine++;
      THIS->kreplace->setData(currView->getDoc()->textLine(THIS->kreplace->replaceCurrentLine));
    }
    result=THIS->kreplace->replace();
  } while (result==KFind::NoMatch);
}

void SourceFileWindow::findReplace_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPositionReal(THIS->kreplace->replaceCurrentLine,matchingindex+matchedlength);
  CURRENT_VIEW->getDoc()->setSelection(THIS->kreplace->replaceCurrentLine,matchingindex,
                                       THIS->kreplace->replaceCurrentLine,matchingindex+matchedlength);
}

void SourceFileWindow::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
  bool update=!!(THIS->kreplace->options()&KReplaceDialog::PromptOnReplace);
  bool haveSelection=THIS->kreplace->haveSelection();
  // The initializations are redundant, but g++ doesn't understand this, and the
  // self-initialization trick doesn't work either (-Wno-init-self is ignored).
  unsigned selStartLine=0, selStartCol=0, selEndLine=0, selEndCol=0;
  if (haveSelection) {
    selStartLine=THIS->kreplace->selStartLine();
    selStartCol=THIS->kreplace->selStartCol();
    selEndLine=THIS->kreplace->selEndLine();
    selEndCol=THIS->kreplace->selEndCol();
  }
  KTextEditor::EditInterfaceExt *editinterfaceext=KTextEditor::editInterfaceExt(CURRENT_VIEW->getDoc());
  editinterfaceext->editBegin();
  CURRENT_VIEW->getDoc()->insertText(THIS->kreplace->replaceCurrentLine,replacementIndex,
                                     text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  bool updateCursor=(CURRENT_VIEW->cursorLine()==THIS->kreplace->replaceCurrentLine
                     && CURRENT_VIEW->cursorColumnReal()==(unsigned)replacementIndex+(unsigned)replacedLength);
  CURRENT_VIEW->getDoc()->removeText(THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength,
                                     THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength+matchedLength);
  editinterfaceext->editEnd();
  if (updateCursor)
    CURRENT_VIEW->setCursorPositionReal(THIS->kreplace->replaceCurrentLine,replacementIndex);
  if (update) {
    CURRENT_VIEW->setCursorPositionReal(THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->getDoc()->setSelection(THIS->kreplace->replaceCurrentLine,replacementIndex,
                                         THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength);
    CURRENT_VIEW->repaint();
  }
  if (haveSelection) {
    // Restore selection, updating coordinates if necessary.
    THIS->kreplace->setSelection(selStartLine,selStartCol,selEndLine,
                                 (THIS->kreplace->replaceCurrentLine==selEndLine)
                                 ?(selEndCol+replacedLength-matchedLength)
                                 :selEndCol);
  }
}

void SourceFileWindow::findReplace_stop()
{
  if (THIS->kreplace) THIS->kreplace->deleteLater();
  THIS->kreplace=static_cast<KReplaceWithSelectionS *>(NULL);
}

void SourceFileWindow::findFunctions()
{
  THIS->functionDialog=new FunctionDialog(this);
  connect(THIS->functionDialog->functionListBox,SIGNAL(highlighted(int)),
          this,SLOT(findFunctions_functionListBox_highlighted(int)));
  connect(THIS->functionDialog->functionListBox,SIGNAL(selected(int)),
          this,SLOT(findFunctions_functionListBox_selected(int)));
  connect(THIS->functionDialog->prototypeButton,SIGNAL(clicked()),
          this,SLOT(findFunctions_prototypeButton_clicked()));
  connect(THIS->functionDialog->implementationButton,SIGNAL(clicked()),
          this,SLOT(findFunctions_implementationButton_clicked()));
  THIS->functionDialog->functionListBox->clear();
  THIS->sourceFileFunctions=getFunctions(CURRENT_VIEW->getDoc()->text(),
                                         THIS->isASMFile);
  for (SourceFileFunctions::Iterator it=THIS->sourceFileFunctions.begin();
       it!=THIS->sourceFileFunctions.end(); ++it)
    THIS->functionDialog->functionListBox->insertItem((*it).name);
  THIS->functionDialog->exec();
  delete THIS->functionDialog;
}

void SourceFileWindow::findFunctions_functionListBox_highlighted(int index)
{
  if (index>=0) {
    THIS->functionDialog->prototypeButton->setEnabled(
      THIS->sourceFileFunctions[index].prototypeLine>=0);
    THIS->functionDialog->implementationButton->setEnabled(
      THIS->sourceFileFunctions[index].implementationLine>=0);
  } else {
    THIS->functionDialog->prototypeButton->setEnabled(FALSE);
    THIS->functionDialog->implementationButton->setEnabled(FALSE);
  }
}

void SourceFileWindow::findFunctions_functionListBox_selected(int index)
{
  if (index>=0) {
    int line=THIS->sourceFileFunctions[index].implementationLine>=0
             ?THIS->sourceFileFunctions[index].implementationLine
             :THIS->sourceFileFunctions[index].prototypeLine;
    CURRENT_VIEW->setCursorPositionReal(line,0);
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_prototypeButton_clicked()
{
  int index=THIS->functionDialog->functionListBox->currentItem();
  if (index>=0 && THIS->sourceFileFunctions[index].prototypeLine>=0) {
    CURRENT_VIEW->setCursorPositionReal(
      THIS->sourceFileFunctions[index].prototypeLine,0);
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_implementationButton_clicked()
{
  int index=THIS->functionDialog->functionListBox->currentItem();
  if (index>=0 && THIS->sourceFileFunctions[index].implementationLine>=0) {
    CURRENT_VIEW->setCursorPositionReal(
      THIS->sourceFileFunctions[index].implementationLine,0);
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctionsPopup_aboutToShow()
{
  THIS->findFunctionsPopup->clear();
  THIS->sourceFileFunctions=getFunctions(CURRENT_VIEW->getDoc()->text(),
                                         THIS->isASMFile);
  int idx=0;
  for (SourceFileFunctions::Iterator it=THIS->sourceFileFunctions.begin();
       it!=THIS->sourceFileFunctions.end(); ++it,++idx)
    THIS->findFunctionsPopup->insertItem((*it).name,idx);
}

void SourceFileWindow::findFunctionsPopup_aboutToHide()
{
  QTimer::singleShot(0,this,SLOT(findFunctionsPopup_aboutToHide_async()));
}

void SourceFileWindow::findFunctionsPopup_aboutToHide_async()
{
  THIS->findFunctionsPopup->clear();
}

void SourceFileWindow::findFunctionsPopup_activated(int id)
{
  int line=THIS->sourceFileFunctions[id].implementationLine>=0
           ?THIS->sourceFileFunctions[id].implementationLine
           :THIS->sourceFileFunctions[id].prototypeLine;
  CURRENT_VIEW->setCursorPositionReal(line,0);
}

void SourceFileWindow::findOpenFileAtCursor()
{
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
  THIS->mainForm->findAndOpenFile(fileName,THIS->category);
}

void SourceFileWindow::findFindSymbolDeclaration()
{

}

void SourceFileWindow::updateSizes()
{
  int rightStatusSize=size().width();
  THIS->rowStatusLabel->setMaximumWidth(30);
  THIS->colStatusLabel->setMaximumWidth(30);
  THIS->charsStatusLabel->setMaximumWidth(100);
  THIS->rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
}

void SourceFileWindow::resizeEvent(QResizeEvent *event)
{
  QMainWindow::resizeEvent(event);
  if (event->size()==event->oldSize()) return;
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
  THIS->rowStatusLabel->setMaximumWidth(30);
  THIS->rowStatusLabel->setText(QString("%1").arg(line+1));
  THIS->colStatusLabel->setMaximumWidth(30);
  THIS->colStatusLabel->setText(QString("%1").arg(col+1));
  THIS->charsStatusLabel->setMaximumWidth(100);
  THIS->charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
  THIS->rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  THIS->rightStatusLabel->setText(THIS->fileName);
}

void SourceFileWindow::current_view_cursorPositionChanged()
{
  if (CURRENT_VIEW) {
    unsigned int line, col;
    CURRENT_VIEW->cursorPositionReal(&line,&col);
    THIS->rowStatusLabel->setText(QString("%1").arg(line+1));
    THIS->colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void SourceFileWindow::current_view_textChanged()
{
  if (CURRENT_VIEW) {
    THIS->charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->getDoc()->text().length()));
    if (preferences.deleteOverwrittenErrors) MainForm::deleteOverwrittenErrorsIn(THIS);
  }
  if (THIS->kreplace) THIS->kreplace->invalidateSelection();
}

void SourceFileWindow::current_view_undoChanged()
{
  if (CURRENT_VIEW) {
    editUndoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->undoCount()));
    editRedoAction->setEnabled(!!(CURRENT_VIEW->getDoc()->redoCount()));
    THIS->accel->setItemEnabled(0,!!(CURRENT_VIEW->getDoc()->undoCount()));
    THIS->accel->setItemEnabled(1,!!(CURRENT_VIEW->getDoc()->redoCount()));
  }
}

void SourceFileWindow::current_view_selectionChanged()
{
  if (CURRENT_VIEW) {
    editClearAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCutAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    editCopyAction->setEnabled(CURRENT_VIEW->getDoc()->hasSelection());
    THIS->accel->setItemEnabled(2,CURRENT_VIEW->getDoc()->hasSelection());
    THIS->accel->setItemEnabled(3,CURRENT_VIEW->getDoc()->hasSelection());
  }
}

void SourceFileWindow::current_view_charactersInteractivelyInserted(int line, int col, const QString &characters)
{
  if (CURRENT_VIEW && preferences.autoBlocks && !characters.compare("{")
      && col==CURRENT_VIEW->getDoc()->lineLength(line)-1) {
    Kate::Document *doc=CURRENT_VIEW->getDoc();
    QString fileText=doc->text();
    // Only for C files.
    if (THIS->isCFile) {
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
    if (THIS->isCFile) {
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
    THIS->accel->setItemEnabled(4,!clipboard->text().isNull());
  }
}

void SourceFileWindow::closeEvent(QCloseEvent *e)
{
  if (!fileCloseAction->isEnabled() || savePrompt())
    e->ignore();
  else {
    e->accept();
    deleteLater();
  }
}

void SourceFileWindow::KDirWatch_dirty(const QString &fileName)
{
  if (!fileName.compare(THIS->fileName)) {
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
  }
}
