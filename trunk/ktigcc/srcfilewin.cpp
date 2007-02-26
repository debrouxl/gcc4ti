/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2007 Kevin Kofler
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

#include "srcfilewin.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>
#include <qstring.h>
#include <qregexp.h>
#include <qapplication.h>
#include <qlabel.h>
#include <qstatusbar.h>
#include <qtimer.h>
#include <qdatetime.h>
#include <q3dragobject.h>
#include <qdir.h>
#include <qclipboard.h>
#include <q3accel.h>
#include <qeventloop.h>
#include <qlayout.h>
#include <qtoolbutton.h>
#include <q3listbox.h>
#include <QKeyEvent>
#include <q3mimefactory.h>
#include <QResizeEvent>
#include <Q3PopupMenu>
#include <QEvent>
#include <QCloseEvent>
#include <QAssistantClient>
#include <kparts/factory.h>
#include <klibloader.h>
#include <ktexteditor/editor.h>
#include <ktexteditor/document.h>
#include <ktexteditor/view.h>
#include <ktexteditor/cursor.h>
#include <ktexteditor/range.h>
#include <ktexteditor/highlightinginterface.h>
#include <ktexteditor/configinterface.h>
#include <kconfig.h>
#include <ktexteditor/configpage.h>
#include <kfiledialog.h>
#include <kurl.h>
#include <kmessagebox.h>
#include <kdirwatch.h>
#include <kfinddialog.h>
#include <kfind.h>
#include <kreplacedialog.h>
#include <kreplace.h>
#include <kwin.h>
#include <kicontheme.h>
#include <kicon.h>
#include <kiconloader.h>
#include <kpushbutton.h>
#include <kstandardaction.h>
#include <cstdio>
#include <cstdlib>
#include "ktigcc.h"
#include "mainform.h"
#include "tpr.h"
#include "preferences.h"
#include "projectoptions.h"
#include "srcfile.h"
#include "functions.h"
#include "completion.h"

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

#define SET_TEXT_SAFE(doc,text) do { \
    disableViewEvents=TRUE; \
    (doc)->setText((text)); \
    disableViewEvents=FALSE; \
  } while(0)

// For some reason, this flag is not in the public ConfigFlags enum.
#define CF_REMOVE_TRAILING_DYN 0x4000000

class KReplaceWithSelectionS : public KReplace {
  public:
    KReplaceWithSelectionS(const QString &pattern, const QString &replacement,
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
    int replaceCurrentLine;
    void invalidateSelection() {m_haveSelection=FALSE;}
    bool haveSelection() {return m_haveSelection;}
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

void SourceFileWindow::initBase()
{
  setIcon(qPixmapFromMimeSource("icon.png"));
  KWin::setIcons(winId(),*(icon()),qPixmapFromMimeSource("ktigcc.png"));
  sourceFiles.append(THIS);
  THIS->dirWatch=new KDirWatch(this);
  setCaption(caption()+" - "+THIS->fileName);
  THIS->te_popup = new Q3PopupMenu(this);
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
  THIS->kateView=static_cast<KTextEditor::View *>(NULL);
  THIS->kateView=reinterpret_cast<KTextEditor::View *>(createView(THIS->fileName,THIS->fileText,HL_MODE,
    THIS->isASMFile?preferences.tabWidthAsm:THIS->isCFile?preferences.tabWidthC:8));
  THIS->fileText=QString::null;
  int rightStatusSize=size().width();
  int line, col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  THIS->rowStatusLabel=new QLabel(QString("%1").arg(line+1),this);
  THIS->rowStatusLabel->setAlignment(Qt::AlignRight);
  THIS->rowStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(THIS->rowStatusLabel,1);
  THIS->colStatusLabel=new QLabel(QString("%1").arg(col+1),this);
  THIS->colStatusLabel->setAlignment(Qt::AlignRight);
  THIS->colStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(THIS->colStatusLabel,1);
  THIS->charsStatusLabel=new QLabel(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()),this);
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
  editUndoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
  editRedoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  editClearAction->setEnabled(CURRENT_VIEW->selection());
  editCutAction->setEnabled(CURRENT_VIEW->selection());
  editCopyAction->setEnabled(CURRENT_VIEW->selection());
  editPasteAction->setEnabled(!clipboard->text().isNull());
  THIS->accel=new Q3Accel(this);
  THIS->accel->insertItem(Qt::ALT+Qt::Key_Backspace,0);
  THIS->accel->insertItem(Qt::SHIFT+Qt::ALT+Qt::Key_Backspace,1);
  THIS->accel->insertItem(Qt::SHIFT+Qt::Key_Delete,2);
  THIS->accel->insertItem(Qt::CTRL+Qt::Key_Insert,3);
  THIS->accel->insertItem(Qt::SHIFT+Qt::Key_Insert,4);
  THIS->accel->insertItem(Qt::Key_F1,5);
  THIS->accel->insertItem(Qt::Key_Enter,6);
  THIS->accel->insertItem(Qt::Key_Return,7);
  THIS->accel->insertItem(Qt::CTRL+Qt::Key_J,8);
  THIS->accel->insertItem(Qt::CTRL+Qt::Key_Space,9);
  THIS->accel->insertItem(Qt::CTRL+Qt::Key_M,10);
  THIS->accel->setItemEnabled(0,CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
  THIS->accel->setItemEnabled(1,CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  THIS->accel->setItemEnabled(2,CURRENT_VIEW->selection());
  THIS->accel->setItemEnabled(3,CURRENT_VIEW->selection());
  THIS->accel->setItemEnabled(4,!clipboard->text().isNull());
  THIS->accel->setItemEnabled(5,TRUE);
  THIS->accel->setItemEnabled(6,TRUE);
  THIS->accel->setItemEnabled(7,TRUE);
  THIS->accel->setItemEnabled(8,TRUE);
  THIS->accel->setItemEnabled(9,TRUE);
  THIS->accel->setItemEnabled(10,TRUE);
  connect(THIS->accel,SIGNAL(activated(int)),this,SLOT(accel_activated(int)));
  if (preferences.useSystemIcons) {
    setUsesBigPixmaps(TRUE);
    fileSaveAction->setIcon(KIcon("filesave"));
    fileAddToProjectAction->setIcon(KIcon("edit_add"));
    fileCompileAction->setIcon(KIcon("compfile"));
    filePrintAction->setIcon(KIcon("fileprint"));
    filePrintQuicklyAction->setIcon(KIcon("fileprint"));
    editClearAction->setIcon(KIcon("editdelete"));
    editCutAction->setIcon(KIcon("editcut"));
    editCopyAction->setIcon(KIcon("editcopy"));
    editPasteAction->setIcon(KIcon("editpaste"));
    findFindAction->setIcon(KIcon("filefind"));
    if (KIconLoader::global()->iconPath("stock-find-and-replace",K3Icon::Small,TRUE).isEmpty())
      findReplaceAction->setIcon(QIcon(qPixmapFromMimeSource("filereplace.png")));
    else
      findReplaceAction->setIcon(KIcon("stock-find-and-replace"));
    editUndoAction->setIcon(KIcon("undo"));
    editRedoAction->setIcon(KIcon("redo"));
    findFunctionsAction->setIcon(KIcon("view_tree"));
    editIncreaseIndentAction->setIcon(KIcon("indent"));
    editDecreaseIndentAction->setIcon(KIcon("unindent"));
  }
  QToolButton *findFunctionsButton=static_cast<QToolButton *>(toolBar
    ->child("findFunctionsAction_action_button","QToolButton",FALSE));
  THIS->findFunctionsPopup=new Q3PopupMenu(findFunctionsButton);
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
  if (THIS->kfinddialog) {
    findHistory=THIS->kfinddialog->findHistory();
    delete THIS->kfinddialog;
  }
  delete THIS->accel;
  delete THIS->te_popup;
  delete THIS->rowStatusLabel;
  delete THIS->colStatusLabel;
  delete THIS->charsStatusLabel;
  delete THIS->rightStatusLabel;
  THIS->dirWatch->removeFile(THIS->fileName);
  delete THIS->dirWatch;
  if (THIS->kateView) delete THIS->kateView->document();
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
        // FIXME: Send ENTER key in some way, or replace newLineHook with a
        //        better solution altogether.
        CURRENT_VIEW->document()->startEditing();
        CURRENT_VIEW->removeSelectionText();
        CURRENT_VIEW->insertText("\n");
        CURRENT_VIEW->document()->endEditing();
        current_view_newLineHook();
        break;
      case 8:
        new TemplatePopup(CURRENT_VIEW);
        break;
      case 9:
      case 10:
        // Completion only operates on C files.
        if (THIS->isCFile) {
          // Disable newLineHook.
          THIS->accel->setItemEnabled(6,FALSE);
          THIS->accel->setItemEnabled(7,FALSE);
          new CompletionPopup(CURRENT_VIEW,THIS->fileName,THIS->mainForm,this);
        }
        break;
      default: break;
    }
  } else if (index == 6 || index == 7) {
    QKeyEvent *keyEvent=new QKeyEvent(QEvent::KeyPress,Qt::Key_Return,'\n',0,"\n");
    QApplication::postEvent(focusWidget(),keyEvent);
  }
}

void SourceFileWindow::completionPopup_closed()
{
  // Restore newLineHook.
  THIS->accel->setItemEnabled(6,TRUE);
  THIS->accel->setItemEnabled(7,TRUE);
}

void *SourceFileWindow::createView(const QString &fileName, const QString &fileText, const QString &hlModeName, unsigned tabWidth)
{
  // Create Document object.
  KParts::Factory *factory = (KParts::Factory *)
    KLibLoader::self()->factory ("katepart");
  if (!factory) qFatal("Failed to load KatePart");
  KTextEditor::Document *doc = (KTextEditor::Document *)
      factory->createPart(0,THIS->mainForm,"KTextEditor::Document");
  // Set the file name for printing.
  doc->setModified(FALSE);
  if (doc->openStream("text/plain",fileName))
    doc->closeStream();
  // Create View object.
  KTextEditor::View *newView = (KTextEditor::View *) doc->createView(centralWidget());
  newView->hide();
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  // Set highlighting mode.
  KTextEditor::HighlightingInterface *hliface
    =qobject_cast<KTextEditor::HighlightingInterface*>(newView->document());
  hliface->setHighlighting(hlModeName);
  // Set options.
  KTextEditor::ConfigInterface *configiface
    =qobject_cast<KTextEditor::ConfigInterface*>(newView);
  configiface->setConfigValue("dynamic-word-wrap",false);
#if 0 // FIXME: remove spaces, tab width
  if (preferences.removeTrailingSpaces)
    newView->document()->setConfigFlags(newView->document()->configFlags()|(KTextEditor::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  else
    newView->document()->setConfigFlags(newView->document()->configFlags()&~(KTextEditor::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
  newView->setTabWidth(tabWidth);
#endif
  connect(newView,SIGNAL(cursorPositionChanged()),this,SLOT(current_view_cursorPositionChanged()));
  connect(newView->document(),SIGNAL(textChanged()),this,SLOT(current_view_textChanged()));
  connect(newView->document(),SIGNAL(undoChanged()),this,SLOT(current_view_undoChanged()));
  connect(newView->document(),SIGNAL(selectionChanged()),this,SLOT(current_view_selectionChanged()));
  connect(newView->document(),SIGNAL(charactersInteractivelyInserted(int,int,const QString&)),this,SLOT(current_view_charactersInteractivelyInserted(int,int,const QString&)));
  newView->setContextMenu(THIS->te_popup);
  // Set text.
  SET_TEXT_SAFE(newView->document(),fileText);
  newView->document()->setModified(FALSE);
// FIXME
//  newView->document()->clearUndo();
//  newView->document()->clearRedo();
  newView->setCursorPosition(KTextEditor::Cursor(0,0));
  return newView;
}

// Returns 1 if the source file should not be closed, 0 if it can be closed.
int SourceFileWindow::savePrompt(void)
{
  int result;
  if (!CURRENT_VIEW) return 0;
  while (CURRENT_VIEW->document()->isModified()) { // "while" in case saving fails!
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(THIS->fileName),QString::null,KStandardGuiItem::save(),KStandardGuiItem::discard());
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
  KTextEditor::View *kateView=reinterpret_cast<KTextEditor::View *>(view);
  KTextEditor::Document *doc=kateView->document();
  doc->startEditing();
  unsigned numLines=doc->lines();
  for (unsigned i=0; i<numLines; i++) {
    QString line=doc->line(i);
    int whitespace=line.find(QRegExp("\\s+$"));
    if (whitespace>=0) doc->removeText(KTextEditor::Range(i,whitespace,i,line.length()));
  }
  doc->endEditing();
}

void SourceFileWindow::fileSave()
{
  THIS->dirWatch->removeFile(THIS->fileName);
  if (saveFileText(THIS->fileName,CURRENT_VIEW->document()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(THIS->fileName));
    THIS->dirWatch->addFile(THIS->fileName);
  } else {
    THIS->dirWatch->addFile(THIS->fileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->document()->setModified(FALSE);
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
  if (saveFileText(saveFileName,CURRENT_VIEW->document()->text())) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    THIS->dirWatch->addFile(THIS->fileName);
  } else {
    if (saveFileName.compare(THIS->fileName)) {
      // Update the file name for printing.
      int line,col;
      QString fileText=CURRENT_VIEW->document()->text();
      KTextEditor::HighlightingInterface *hliface
        =qobject_cast<KTextEditor::HighlightingInterface*>(CURRENT_VIEW->document());
      QString hlMode=hliface->highlighting();
      CURRENT_VIEW->cursorPosition().position(line,col);
      CURRENT_VIEW->document()->setModified(FALSE);
      if (CURRENT_VIEW->document()->openStream("text/plain",saveFileName))
        CURRENT_VIEW->document()->closeStream();
      SET_TEXT_SAFE(CURRENT_VIEW->document(),fileText);
// FIXME
//      CURRENT_VIEW->document()->clearUndo();
//      CURRENT_VIEW->document()->clearRedo();
      hliface->setHighlighting(hlMode);
      CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,col));
      // Update the caption
      setCaption(caption().left(caption().find('-')+2)+saveFileName);
    }
    THIS->fileName=saveFileName;
    THIS->dirWatch->addFile(saveFileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->document()->setModified(FALSE);
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
  if (CURRENT_VIEW) CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Print))->trigger();
}


void SourceFileWindow::filePrintQuickly()
{
  // This still shows the print dialog, but then KDE 3.5 Kate did that too
  // despite having 2 nominally different APIs (print and printDialog).
  if (CURRENT_VIEW) CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Print))->trigger();
}

void SourceFileWindow::applyPreferences()
{
  // Apply the KatePart preferences and treeview icons.
  KParts::Factory *factory=(KParts::Factory *)
    KLibLoader::self()->factory("katepart");
  if (!factory) qFatal("Failed to load KatePart");
  KTextEditor::Document *doc=(KTextEditor::Document *)
    factory->createPart(0,this,"KTextEditor::Document");
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
  KTextEditor::View *kateView=CURRENT_VIEW;
  if (kateView) {
    QString fileText=kateView->document()->text();
#if 0 // FIXME: remove spaces, tab width
    if (preferences.removeTrailingSpaces)
      kateView->document()->setConfigFlags(kateView->document()->configFlags()|(KTextEditor::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
    else
      kateView->document()->setConfigFlags(kateView->document()->configFlags()&~(KTextEditor::Document::cfRemoveSpaces|CF_REMOVE_TRAILING_DYN));
    kateView->setTabWidth(THIS->isASMFile?preferences.tabWidthAsm:
                          THIS->isCFile?preferences.tabWidthC:8);
#endif
    // Kate seems really insisting on making it a pain to update syntax highlighting settings.
    KTextEditor::HighlightingInterface *hliface
      =qobject_cast<KTextEditor::HighlightingInterface*>(kateView->document());
    hliface->setHighlighting(HL_MODE);
    // Force redrawing to get the tab width right, repaint() is ignored for some reason.
    kateView->hide();
    kateView->show();
  }
  // Apply the icon preferences.
  setUsesBigPixmaps(preferences.useSystemIcons);
  if (preferences.useSystemIcons) {
    fileSaveAction->setIcon(KIcon("filesave"));
    fileAddToProjectAction->setIcon(KIcon("edit_add"));
    fileCompileAction->setIcon(KIcon("compfile"));
    filePrintAction->setIcon(KIcon("fileprint"));
    filePrintQuicklyAction->setIcon(KIcon("fileprint"));
    editClearAction->setIcon(KIcon("editdelete"));
    editCutAction->setIcon(KIcon("editcut"));
    editCopyAction->setIcon(KIcon("editcopy"));
    editPasteAction->setIcon(KIcon("editpaste"));
    findFindAction->setIcon(KIcon("filefind"));
    if (KIconLoader::global()->iconPath("stock-find-and-replace",K3Icon::Small,TRUE).isEmpty())
      findReplaceAction->setIcon(QIcon(qPixmapFromMimeSource("filereplace.png")));
    else
      findReplaceAction->setIcon(KIcon("stock-find-and-replace"));
    editUndoAction->setIcon(KIcon("undo"));
    editRedoAction->setIcon(KIcon("redo"));
    findFunctionsAction->setIcon(KIcon("view_tree"));
    editIncreaseIndentAction->setIcon(KIcon("indent"));
    editDecreaseIndentAction->setIcon(KIcon("unindent"));
  } else {
    fileSaveAction->setIcon(QIcon(qPixmapFromMimeSource("02")));
    fileAddToProjectAction->setIcon(QIcon(qPixmapFromMimeSource("08")));
    fileCompileAction->setIcon(QIcon(qPixmapFromMimeSource("09")));
    filePrintAction->setIcon(QIcon(qPixmapFromMimeSource("03")));
    filePrintQuicklyAction->setIcon(QIcon(qPixmapFromMimeSource("03")));
    editClearAction->setIcon(QIcon(qPixmapFromMimeSource("04")));
    editCutAction->setIcon(QIcon(qPixmapFromMimeSource("05")));
    editCopyAction->setIcon(QIcon(qPixmapFromMimeSource("06")));
    editPasteAction->setIcon(QIcon(qPixmapFromMimeSource("07")));
    findFindAction->setIcon(QIcon(qPixmapFromMimeSource("13")));
    findReplaceAction->setIcon(QIcon(qPixmapFromMimeSource("14")));
    editUndoAction->setIcon(QIcon(qPixmapFromMimeSource("16")));
    editRedoAction->setIcon(QIcon(qPixmapFromMimeSource("17")));
    findFunctionsAction->setIcon(QIcon(qPixmapFromMimeSource("18")));
    editIncreaseIndentAction->setIcon(QIcon(qPixmapFromMimeSource("19")));
    editDecreaseIndentAction->setIcon(QIcon(qPixmapFromMimeSource("20")));
  }
}

void SourceFileWindow::editUndo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->trigger();
}

void SourceFileWindow::editRedo()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->trigger();
}

void SourceFileWindow::editClear()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->removeSelectionText();
}

void SourceFileWindow::editCut()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Cut))->trigger();
}

void SourceFileWindow::editCopy()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Copy))->trigger();
}

void SourceFileWindow::editPaste()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::PasteText))->trigger();
}

void SourceFileWindow::editSelectAll()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action(KStandardAction::name(KStandardAction::SelectAll))->trigger();
}

void SourceFileWindow::editIncreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action("tools_indent")->trigger();
}

void SourceFileWindow::editDecreaseIndent()
{
  if (CURRENT_VIEW)
    CURRENT_VIEW->action("tools_unindent")->trigger();
}

void SourceFileWindow::findFind()
{
  if (THIS->kfinddialog)
    KWin::activateWindow(THIS->kfinddialog->winId());
  else {
    // Never set hasSelection because finding in selection doesn't really make
    // sense with my non-modal find dialog setup.
    THIS->kfinddialog=new KFindDialog(false,this,0,KFind::FromCursor,findHistory);
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
  bool findBackwards=!!(THIS->kfinddialog->options()&KFind::FindBackwards);
  int findCurrentCol;
  kfind=new KFind(THIS->kfinddialog->pattern(),THIS->kfinddialog->options(),this,THIS->kfinddialog);
  kfind->closeFindNextDialog(); // don't use this, a non-modal KFindDialog is used instead
  connect(kfind,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findFind_highlight(const QString &,int,int)));
  if (THIS->kfinddialog->options()&KFind::FromCursor) {
    if (CURRENT_VIEW->selection()) {
      if (findBackwards) {
        CURRENT_VIEW->selectionRange().start().position(THIS->findCurrentLine,findCurrentCol);
        if ((--findCurrentCol)==-1) {
          if (!THIS->findCurrentLine) goto skip_data;
          THIS->findCurrentLine--;
        }
      } else {
        CURRENT_VIEW->selectionRange().end().position(THIS->findCurrentLine,findCurrentCol);
      }
    } else {
      CURRENT_VIEW->cursorPosition().position(THIS->findCurrentLine,findCurrentCol);
    }
  } else {
    THIS->findCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
    findCurrentCol=-1;
  }
  kfind->setData(CURRENT_VIEW->document()->line(THIS->findCurrentLine),findCurrentCol);
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  int currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!THIS->findCurrentLine:(THIS->findCurrentLine>=currNumLines)) {
        // Try restarting the search.
        currNumLines=currView->document()->lines();
        THIS->findCurrentLine=findBackwards?currNumLines-1:0;
        do {
          if (kfind->needData()) {
            if (findBackwards?!THIS->findCurrentLine:(THIS->findCurrentLine>=currNumLines))
              goto not_found_current;
            if (findBackwards) THIS->findCurrentLine--; else THIS->findCurrentLine++;
            kfind->setData(currView->document()->line(THIS->findCurrentLine));
          }
          result=kfind->find();
        } while (result==KFind::NoMatch);
        break;
        not_found_current:
          KMessageBox::error(this,QString("Text \'%1\' not found").arg(THIS->kfinddialog->pattern()));
          delete kfind;
          return;
      } else if (findBackwards) THIS->findCurrentLine--; else THIS->findCurrentLine++;
      kfind->setData(currView->document()->line(THIS->findCurrentLine));
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

#define unused_text text __attribute__((unused))
void SourceFileWindow::findFind_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(THIS->findCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(THIS->findCurrentLine,matchingindex,
                                                THIS->findCurrentLine,matchingindex+matchedlength));
}

void SourceFileWindow::findFind_stop()
{
  if (THIS->kfinddialog) {
    findHistory=THIS->kfinddialog->findHistory();
    THIS->kfinddialog->deleteLater();
  }
  THIS->kfinddialog=static_cast<KFindDialog *>(NULL);
}

void SourceFileWindow::findReplace()
{
  if (THIS->kreplace) {
    KDialog *replaceNextDialog=THIS->kreplace->replaceNextDialog();
    if (replaceNextDialog)
      KWin::activateWindow(replaceNextDialog->winId());
    return;
  }
  KReplaceDialog kreplacedialog(this,0,((CURRENT_VIEW&&CURRENT_VIEW->selection()
                                         &&!CURRENT_VIEW->selectionRange().onSingleLine())?
                                         KFind::SelectedText:0)|KFind::FromCursor,
                                        findHistory,replacementHistory,
                                        CURRENT_VIEW&&CURRENT_VIEW->selection());
  if (kreplacedialog.exec()!=QDialog::Accepted)
    return;
  findHistory=kreplacedialog.findHistory();
  replacementHistory=kreplacedialog.replacementHistory();
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
  bool findBackwards=!!(THIS->kreplace->options()&KFind::FindBackwards);
  int replaceCurrentCol;
  if (CURRENT_VIEW) {
    if (THIS->kreplace->options()&KFind::SelectedText) {
      THIS->kreplace->setSelection(CURRENT_VIEW->selectionRange());
      if (findBackwards) {
        THIS->kreplace->replaceCurrentLine=THIS->kreplace->selEndLine();
        replaceCurrentCol=THIS->kreplace->selEndCol();
      } else {
        THIS->kreplace->replaceCurrentLine=THIS->kreplace->selStartLine();
        replaceCurrentCol=THIS->kreplace->selStartCol();
      }
      THIS->kreplace->setOptions(THIS->kreplace->options()&~KFind::FromCursor);
    } else if (THIS->kreplace->options()&KFind::FromCursor) {
      if (CURRENT_VIEW->selection()) {
        if (findBackwards) {
          CURRENT_VIEW->selectionRange().start().position(THIS->kreplace->replaceCurrentLine,
                                                          replaceCurrentCol);
          if ((--replaceCurrentCol)==-1) {
            if (!THIS->kreplace->replaceCurrentLine) goto skip_data;
            THIS->kreplace->replaceCurrentLine--;
          }
        } else {
          CURRENT_VIEW->selectionRange().end().position(THIS->kreplace->replaceCurrentLine,
                                                        replaceCurrentCol);
        }
      } else {
        CURRENT_VIEW->cursorPosition().position(THIS->kreplace->replaceCurrentLine,replaceCurrentCol);
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(THIS->kreplace->replaceCurrentLine==(CURRENT_VIEW->document()->lines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->document()->lineLength(THIS->kreplace->replaceCurrentLine)))
                         :(!THIS->kreplace->replaceCurrentLine&&!replaceCurrentCol))
          THIS->kreplace->setOptions(THIS->kreplace->options()&~KFind::FromCursor);
      }
    } else {
      THIS->kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
      replaceCurrentCol=-1;
    }
    THIS->kreplace->setData(CURRENT_VIEW->document()->line(THIS->kreplace->replaceCurrentLine),replaceCurrentCol);
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
  bool findBackwards=!!(THIS->kreplace->options()&KFind::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Non-first-time always continues from cursor.
    if (CURRENT_VIEW->selection()) {
      if (findBackwards) {
        CURRENT_VIEW->selectionRange().start().position(THIS->kreplace->replaceCurrentLine,
                                                        replaceCurrentCol);
        if ((--replaceCurrentCol)==-1) {
          if (!THIS->kreplace->replaceCurrentLine) goto skip_data;
          THIS->kreplace->replaceCurrentLine--;
        }
      } else {
        CURRENT_VIEW->selectionRange().end().position(THIS->kreplace->replaceCurrentLine,
                                                      replaceCurrentCol);
      }
    } else {
      CURRENT_VIEW->cursorPosition().position(THIS->kreplace->replaceCurrentLine,
                                              replaceCurrentCol);
    }
    THIS->kreplace->setData(CURRENT_VIEW->document()->line(THIS->kreplace->replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  int currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (THIS->kreplace->needData()) {
      if (THIS->kreplace->haveSelection()
          ?(findBackwards?(THIS->kreplace->replaceCurrentLine<=THIS->kreplace->selStartLine())
                         :(THIS->kreplace->replaceCurrentLine>=THIS->kreplace->selEndLine()))
          :(findBackwards?!THIS->kreplace->replaceCurrentLine:(THIS->kreplace->replaceCurrentLine>=currNumLines))) {
        if (THIS->kreplace->shouldRestart()) {
          // Drop "From cursor" and "Selected text" options.
          THIS->kreplace->setOptions(THIS->kreplace->options()&~(KFind::FromCursor
                                                                 |KFind::SelectedText));
          THIS->kreplace->invalidateSelection();
          // Reinitialize.
          THIS->kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
          THIS->kreplace->setData(CURRENT_VIEW->document()->line(THIS->kreplace->replaceCurrentLine));
          // Start again as if it was the first time.
          findReplace_next(TRUE);
          return;
        } else {
          findReplace_stop();
          return;
        }
      } else if (findBackwards) THIS->kreplace->replaceCurrentLine--; else THIS->kreplace->replaceCurrentLine++;
      THIS->kreplace->setData(currView->document()->line(THIS->kreplace->replaceCurrentLine));
    }
    result=THIS->kreplace->replace();
  } while (result==KFind::NoMatch);
}

void SourceFileWindow::findReplace_highlight(const QString &unused_text, int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(THIS->kreplace->replaceCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(THIS->kreplace->replaceCurrentLine,matchingindex,
                                                THIS->kreplace->replaceCurrentLine,matchingindex+matchedlength));
}

void SourceFileWindow::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
  bool update=!!(THIS->kreplace->options()&KReplaceDialog::PromptOnReplace);
  bool haveSelection=THIS->kreplace->haveSelection();
  // The initializations are redundant, but g++ doesn't understand this, and the
  // self-initialization trick doesn't work either (-Wno-init-self is ignored).
  int selStartLine=0, selStartCol=0, selEndLine=0, selEndCol=0;
  if (haveSelection) {
    selStartLine=THIS->kreplace->selStartLine();
    selStartCol=THIS->kreplace->selStartCol();
    selEndLine=THIS->kreplace->selEndLine();
    selEndCol=THIS->kreplace->selEndCol();
  }
  CURRENT_VIEW->document()->startEditing();
  CURRENT_VIEW->document()->insertText(KTextEditor::Cursor(THIS->kreplace->replaceCurrentLine,replacementIndex),
                                       text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  bool updateCursor=(line==THIS->kreplace->replaceCurrentLine
                     && col==replacementIndex+replacedLength);
  CURRENT_VIEW->document()->removeText(KTextEditor::Range(THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength,
                                       THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength+matchedLength));
  CURRENT_VIEW->document()->endEditing();
  if (updateCursor)
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(THIS->kreplace->replaceCurrentLine,replacementIndex));
  if (update) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength));
    CURRENT_VIEW->setSelection(KTextEditor::Range(THIS->kreplace->replaceCurrentLine,replacementIndex,
                                                  THIS->kreplace->replaceCurrentLine,replacementIndex+replacedLength));
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
  THIS->sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
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
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,0));
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_prototypeButton_clicked()
{
  int index=THIS->functionDialog->functionListBox->currentItem();
  if (index>=0 && THIS->sourceFileFunctions[index].prototypeLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      THIS->sourceFileFunctions[index].prototypeLine,0));
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_implementationButton_clicked()
{
  int index=THIS->functionDialog->functionListBox->currentItem();
  if (index>=0 && THIS->sourceFileFunctions[index].implementationLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      THIS->sourceFileFunctions[index].implementationLine,0));
    THIS->functionDialog->accept();
  }
}

void SourceFileWindow::findFunctionsPopup_aboutToShow()
{
  THIS->findFunctionsPopup->clear();
  THIS->sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
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
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,0));
}

void SourceFileWindow::findOpenFileAtCursor()
{
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
  THIS->mainForm->findAndOpenFile(fileName,THIS->category);
}

void SourceFileWindow::findFindSymbolDeclaration()
{
  QString fileText=CURRENT_VIEW->document()->text();
  // "Find symbol declaration" only operates on C files.
  if (THIS->isCFile) {
    QString fileName=THIS->fileName;
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
    if (findSymbolInFile(wordUnderCursor,fileText,fileName,THIS->mainForm,
                         symbolFile,symbolLine,systemHeader)
        && !symbolFile.isNull()) {
      if (symbolFile==fileName)
        CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(symbolLine,0));
      else {
        THIS->mainForm->openHeader(symbolFile,systemHeader,symbolLine);
        if (!systemHeader)
          KWin::activateWindow(THIS->mainForm->winId());
      }
    }
  }
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
  Q3MainWindow::resizeEvent(event);
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
  int line, col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  THIS->rowStatusLabel->setMaximumWidth(30);
  THIS->rowStatusLabel->setText(QString("%1").arg(line+1));
  THIS->colStatusLabel->setMaximumWidth(30);
  THIS->colStatusLabel->setText(QString("%1").arg(col+1));
  THIS->charsStatusLabel->setMaximumWidth(100);
  THIS->charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
  THIS->rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  THIS->rightStatusLabel->setText(THIS->fileName);
}

void SourceFileWindow::current_view_cursorPositionChanged()
{
  if (CURRENT_VIEW && !disableViewEvents) {
    int line, col;
    CURRENT_VIEW->cursorPosition().position(line,col);
    THIS->rowStatusLabel->setText(QString("%1").arg(line+1));
    THIS->colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void SourceFileWindow::current_view_textChanged()
{
  if (disableViewEvents) return;
  if (CURRENT_VIEW) {
    THIS->charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
    if (projectCompletion.contains(THIS->fileName))
      projectCompletion[THIS->fileName].dirty=TRUE;
    if (preferences.deleteOverwrittenErrors) MainForm::deleteOverwrittenErrorsIn(THIS);
  }
  if (THIS->kreplace) THIS->kreplace->invalidateSelection();
}

void SourceFileWindow::current_view_undoChanged()
{
  if (CURRENT_VIEW && !disableViewEvents) {
    editUndoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    editRedoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
    THIS->accel->setItemEnabled(0,CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    THIS->accel->setItemEnabled(1,CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  }
}

void SourceFileWindow::current_view_selectionChanged()
{
  if (CURRENT_VIEW && !disableViewEvents) {
    editClearAction->setEnabled(CURRENT_VIEW->selection());
    editCutAction->setEnabled(CURRENT_VIEW->selection());
    editCopyAction->setEnabled(CURRENT_VIEW->selection());
    THIS->accel->setItemEnabled(2,CURRENT_VIEW->selection());
    THIS->accel->setItemEnabled(3,CURRENT_VIEW->selection());
  }
}

void SourceFileWindow::current_view_charactersInteractivelyInserted(int line, int col, const QString &characters)
{
  if (CURRENT_VIEW) {
    if (preferences.autoBlocks && characters=="{"
        && col==CURRENT_VIEW->document()->lineLength(line)-1) {
      KTextEditor::Document *doc=CURRENT_VIEW->document();
      QString fileText=doc->text();
      // Only for C files.
      if (THIS->isCFile) {
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
          CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line+1,cursorLine.length()));
        }
      }
    }
    // Completion only operates on C files.
    if (characters=="(" && THIS->isCFile)
      new ArgHintPopup(CURRENT_VIEW,THIS->fileName,THIS->mainForm);
  }
}

void SourceFileWindow::current_view_newLineHook()
{
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  KTextEditor::Document *doc=CURRENT_VIEW->document();
  if (preferences.autoBlocks && line && doc->line(line-1).endsWith("{")) {
    QString fileText=doc->text();
    // Only for C files.
    if (THIS->isCFile) {
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
      SET_TEXT_SAFE(CURRENT_VIEW->document(),fileText);
      CURRENT_VIEW->document()->setModified(FALSE);
// FIXME
//      CURRENT_VIEW->document()->clearUndo();
//      CURRENT_VIEW->document()->clearRedo();
      updateRightStatusLabel();
    }
  }
}

/*
 *  Constructs a SourceFileWindow as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 */
SourceFileWindow::SourceFileWindow(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : Q3MainWindow(parent, name, fl)
{
    setupUi(this);

    (void)statusBar();
}

/*
 *  Destroys the object and frees any allocated resources
 */
SourceFileWindow::~SourceFileWindow()
{
    destroy();
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void SourceFileWindow::languageChange()
{
    retranslateUi(this);
}

