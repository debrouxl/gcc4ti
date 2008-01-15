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

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <QString>
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
#include <QLayout>
#include <QToolButton>
#include <Q3ListBox>
#include <QKeyEvent>
#include <QResizeEvent>
#include <Q3PopupMenu>
#include <QEvent>
#include <QCloseEvent>
#include <QTextCodec>
#include <QAssistantClient>
#include <QAction>
#include <kparts/factory.h>
#include <klibloader.h>
#include <ktexteditor/editor.h>
#include <ktexteditor/document.h>
#include <ktexteditor/view.h>
#include <ktexteditor/cursor.h>
#include <ktexteditor/range.h>
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
#include <kicontheme.h>
#include <kicon.h>
#include <kiconloader.h>
#include <kpushbutton.h>
#include <kstandardaction.h>
#include <kactioncollection.h>
#include <cstdio>
#include <cstdlib>
#include "ktigcc.h"
#include "mainform.h"
#include "tpr.h"
#include "preferences.h"
#include "projectoptions.h"
#include "functions.h"
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

#define CURRENT_VIEW (this->kateView)
#define HL_MODE ((hlEnabled && *hlEnabled)?hlMode:"None")

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

SourceFileWindow::SourceFileWindow(MainForm *mainfrm, const QString &fn,
                                   const QString &hlm, const bool *hle,
                                   void *cat, bool isc, bool isasm, bool istxt)
  : QMainWindow(), mainForm(mainfrm), fileName(fn), hlMode(hlm), hlEnabled(hle),
    category(cat), isCFile(isc), isASMFile(isasm), isTextFile(istxt)
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
  sourceFiles.append(this);
  dirWatch=new KDirWatch(this);
  setCaption(caption()+" - "+fileName);
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
  kfinddialog = static_cast<KFindDialog *>(NULL);
  kreplace = static_cast<KReplaceWithSelectionS *>(NULL);
  kateView=static_cast<KTextEditor::View *>(NULL);
  kateView=reinterpret_cast<KTextEditor::View *>(createView(fileName,HL_MODE,
    isASMFile?preferences.tabWidthAsm:isCFile?preferences.tabWidthC:8));
  int rightStatusSize=size().width();
  int line, col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  rowStatusLabel=new QLabel(QString("%1").arg(line+1),this);
  rowStatusLabel->setAlignment(Qt::AlignRight);
  rowStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(rowStatusLabel,1);
  colStatusLabel=new QLabel(QString("%1").arg(col+1),this);
  colStatusLabel->setAlignment(Qt::AlignRight);
  colStatusLabel->setMaximumWidth(30);
  statusBar()->addWidget(colStatusLabel,1);
  charsStatusLabel=new QLabel(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()),this);
  charsStatusLabel->setMaximumWidth(100);
  statusBar()->addWidget(charsStatusLabel,1);
  rightStatusLabel=new QLabel(fileName,this);
  rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  statusBar()->addWidget(rightStatusLabel,1);
  statusBar()->setSizeGripEnabled(FALSE);
  connect(statusBar(),SIGNAL(messageChanged(const QString &)),this,SLOT(statusBar_messageChanged(const QString &)));
  connect(dirWatch,SIGNAL(created(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  connect(dirWatch,SIGNAL(dirty(const QString &)),this,SLOT(KDirWatch_dirty(const QString &)));
  dirWatch->addFile(fileName);
  dirWatch->startScan();
  connect(clipboard,SIGNAL(dataChanged()),this,SLOT(clipboard_dataChanged()));
  centralWidget()->layout()->add(CURRENT_VIEW);
  CURRENT_VIEW->show();
  editUndoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
  editRedoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  editClearAction->setEnabled(CURRENT_VIEW->selection());
  editCutAction->setEnabled(CURRENT_VIEW->selection());
  editCopyAction->setEnabled(CURRENT_VIEW->selection());
  editPasteAction->setEnabled(!clipboard->text().isNull());
  shortcuts[0]=new QShortcut(Qt::ALT+Qt::Key_Backspace,this);
  shortcuts[1]=new QShortcut(Qt::SHIFT+Qt::ALT+Qt::Key_Backspace,this);
  shortcuts[2]=new QShortcut(Qt::SHIFT+Qt::Key_Delete,this);
  shortcuts[3]=new QShortcut(Qt::CTRL+Qt::Key_Insert,this);
  shortcuts[4]=new QShortcut(Qt::SHIFT+Qt::Key_Insert,this);
  shortcuts[5]=new QShortcut(Qt::Key_F1,this);
  shortcuts[6]=new QShortcut(Qt::Key_Enter,this);
  shortcuts[7]=new QShortcut(Qt::Key_Return,this);
  shortcuts[8]=new QShortcut(Qt::CTRL+Qt::Key_J,this);
  shortcuts[9]=new QShortcut(Qt::CTRL+Qt::Key_Space,this);
  shortcuts[10]=new QShortcut(Qt::CTRL+Qt::Key_M,this);
  shortcuts[11]=new QShortcut(Qt::CTRL+Qt::Key_Return,this);
  shortcuts[0]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
  shortcuts[1]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  shortcuts[2]->setEnabled(CURRENT_VIEW->selection());
  shortcuts[3]->setEnabled(CURRENT_VIEW->selection());
  shortcuts[4]->setEnabled(!clipboard->text().isNull());
  shortcuts[5]->setEnabled(TRUE);
  shortcuts[6]->setEnabled(TRUE);
  shortcuts[7]->setEnabled(TRUE);
  shortcuts[8]->setEnabled(TRUE);
  shortcuts[9]->setEnabled(TRUE);
  shortcuts[10]->setEnabled(TRUE);
  shortcuts[11]->setEnabled(TRUE);
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
  connect(shortcuts[11],SIGNAL(activated()),this,SLOT(findOpenFileAtCursor()));
  if (preferences.useSystemIcons) {
    // Set the preferred icon size so system toolbar icons don't get annoying
    // padding.
    int toolbarIconSize=KIconLoader().currentSize(KIconLoader::MainToolbar);
    setIconSize(QSize(toolbarIconSize,toolbarIconSize));
    fileSaveAction->setIcon(KIcon("document-save"));
    fileAddToProjectAction->setIcon(KIcon("list-add"));
    fileCompileAction->setIcon(KIcon("run-build-file"));
    filePrintAction->setIcon(KIcon("document-print"));
    filePrintQuicklyAction->setIcon(KIcon("document-print"));
    editClearAction->setIcon(KIcon("edit-delete"));
    editCutAction->setIcon(KIcon("edit-cut"));
    editCopyAction->setIcon(KIcon("edit-copy"));
    editPasteAction->setIcon(KIcon("edit-paste"));
    findFindAction->setIcon(KIcon("edit-find"));
    findReplaceAction->setIcon(KIcon("edit-find-replace"));
    editUndoAction->setIcon(KIcon("edit-undo"));
    editRedoAction->setIcon(KIcon("edit-redo"));
    findFunctionsAction->setIcon(KIcon("view-list-tree"));
    editIncreaseIndentAction->setIcon(KIcon("format-indent-more"));
    editDecreaseIndentAction->setIcon(KIcon("format-indent-less"));
  }
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
  if (isTextFile) findFunctionsAction->setEnabled(FALSE);
  show();
}

SourceFileWindow::~SourceFileWindow()
{
  MainForm::deleteErrorsForSrcFile(this);
  if (kreplace) delete kreplace;
  if (kfinddialog) {
    findHistory=kfinddialog->findHistory();
    delete kfinddialog;
  }
  for (int i=0; i<12; i++) delete shortcuts[i];
  delete te_popup;
  delete rowStatusLabel;
  delete colStatusLabel;
  delete charsStatusLabel;
  delete rightStatusLabel;
  dirWatch->removeFile(fileName);
  delete dirWatch;
  if (kateView) delete kateView->document();
  sourceFiles.remove(this);
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

void SourceFileWindow::shortcutActivated(int index)
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
        // Completion only operates on C files.
        if (isCFile) {
          // Disable newLineHook.
          shortcuts[6]->setEnabled(FALSE);
          shortcuts[7]->setEnabled(FALSE);
          new CompletionPopup(CURRENT_VIEW,fileName,mainForm,this);
        }
        break;
      default: break;
    }
  } else if (index == 6 || index == 7) {
    QKeyEvent *keyEvent=new QKeyEvent(QEvent::KeyPress,Qt::Key_Return,'\n',0,"\n");
    QApplication::postEvent(focusWidget(),keyEvent);
  }
}

void SourceFileWindow::shortcut_0_activated()
{
  shortcutActivated(0);
}

void SourceFileWindow::shortcut_1_activated()
{
  shortcutActivated(1);
}

void SourceFileWindow::shortcut_2_activated()
{
  shortcutActivated(2);
}

void SourceFileWindow::shortcut_3_activated()
{
  shortcutActivated(3);
}

void SourceFileWindow::shortcut_4_activated()
{
  shortcutActivated(4);
}

void SourceFileWindow::shortcut_5_activated()
{
  shortcutActivated(5);
}

void SourceFileWindow::shortcut_6_activated()
{
  shortcutActivated(6);
}

void SourceFileWindow::shortcut_7_activated()
{
  shortcutActivated(7);
}

void SourceFileWindow::shortcut_8_activated()
{
  shortcutActivated(8);
}

void SourceFileWindow::shortcut_9_activated()
{
  shortcutActivated(9);
}

void SourceFileWindow::shortcut_10_activated()
{
  shortcutActivated(10);
}

void SourceFileWindow::completionPopup_closed()
{
  // Restore newLineHook.
  shortcuts[6]->setEnabled(TRUE);
  shortcuts[7]->setEnabled(TRUE);
}

void *SourceFileWindow::createView(const QString &fileName, const QString &hlModeName, unsigned tabWidth)
{
  // Create Document object.
  KParts::Factory *factory = (KParts::Factory *)
    KLibLoader::self()->factory ("katepart");
  if (!factory) qFatal("Failed to load KatePart");
  KTextEditor::Document *doc = (KTextEditor::Document *)
      factory->createPart(0,mainForm,"KTextEditor::Document");
  // Open the file.
  doc->setEncoding(preferences.useCalcCharset?"TI-89":QTextCodec::codecForLocale()->name());
  doc->openUrl(KUrl(fileName));
  // Create View object.
  KTextEditor::View *newView = (KTextEditor::View *) doc->createView(centralWidget());
  newView->hide();
  newView->setSizePolicy(QSizePolicy(QSizePolicy::Ignored,QSizePolicy::Ignored,0,0));
  // Set highlighting mode.
  newView->document()->setHighlightingMode(hlModeName);
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
  setTabWidth(newView,tabWidth);
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

// Returns 1 if the source file should not be closed, 0 if it can be closed.
int SourceFileWindow::savePrompt(void)
{
  int result;
  if (!CURRENT_VIEW) return 0;
  while (CURRENT_VIEW->document()->isModified()) { // "while" in case saving fails!
    result=KMessageBox::questionYesNoCancel(this,QString("The file \'%1\' has been modified.  Do you want to save the changes?").arg(fileName),QString::null,KStandardGuiItem::save(),KStandardGuiItem::discard());
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
  dirWatch->removeFile(fileName);
  if (!CURRENT_VIEW->document()->save()) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(fileName));
    dirWatch->addFile(fileName);
  } else {
    dirWatch->addFile(fileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->document()->setModified(FALSE);
  }
}

void SourceFileWindow::fileSaveAs()
{
  QString saveFileName=MainForm::SGetFileName(KFileDialog::Saving,
  (fileName.endsWith(".h")?TIGCC_H_Filter TIGCCAllFilter:
  fileName.endsWith(".c")?TIGCC_C_Filter TIGCCAllFilter:
  fileName.endsWith(".s")?TIGCC_S_Filter TIGCCAllFilter:
  fileName.endsWith(".asm")?TIGCC_ASM_Filter TIGCCAllFilter:
  fileName.endsWith(".qll")?TIGCC_QLL_Filter TIGCCAllFilter:
  fileName.endsWith(".txt")?TIGCC_TXT_Filter TIGCCAllFilter:
  TIGCCAllFilter),"Save Source File",this);
  if (saveFileName.isEmpty())
    return;
  dirWatch->removeFile(fileName);
  mkdir_multi(saveFileName);
  if (!CURRENT_VIEW->document()->saveAs(saveFileName)) {
    KMessageBox::error(this,QString("Can't save to \'%1\'").arg(saveFileName));
    dirWatch->addFile(fileName);
  } else {
    fileName=saveFileName;
    dirWatch->addFile(saveFileName);
    removeTrailingSpacesFromView(CURRENT_VIEW);
    CURRENT_VIEW->document()->setModified(FALSE);
    updateRightStatusLabel();
  }
}

void SourceFileWindow::fileAddToProject()
{
  mainForm->adoptSourceFile(this);
}

void SourceFileWindow::fileCompile()
{
  mainForm->compileSourceFile(this);
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
    if (preferences.removeTrailingSpaces) {
      sendCommand(kateView,"set-remove-trailing-space 1");
      sendCommand(kateView,"set-remove-trailing-space-save 1");
    } else {
      sendCommand(kateView,"set-remove-trailing-space 0");
      sendCommand(kateView,"set-remove-trailing-space-save 0");
    }
    setTabWidth(kateView,isASMFile?preferences.tabWidthAsm:
                         isCFile?preferences.tabWidthC:8);
    // Kate seems really insisting on making it a pain to update syntax highlighting settings.
    kateView->document()->setHighlightingMode(HL_MODE);
    // Force redrawing to get the tab width right, repaint() is ignored for some reason.
    kateView->hide();
    kateView->show();
  }
  // Apply the icon preferences.
  if (preferences.useSystemIcons) {
    // Set the preferred icon size so system toolbar icons don't get annoying
    // padding.
    int toolbarIconSize=KIconLoader().currentSize(KIconLoader::MainToolbar);
    setIconSize(QSize(toolbarIconSize,toolbarIconSize));
    fileSaveAction->setIcon(KIcon("document-save"));
    fileAddToProjectAction->setIcon(KIcon("list-add"));
    fileCompileAction->setIcon(KIcon("run-build-file"));
    filePrintAction->setIcon(KIcon("document-print"));
    filePrintQuicklyAction->setIcon(KIcon("document-print"));
    editClearAction->setIcon(KIcon("edit-delete"));
    editCutAction->setIcon(KIcon("edit-cut"));
    editCopyAction->setIcon(KIcon("edit-copy"));
    editPasteAction->setIcon(KIcon("edit-paste"));
    findFindAction->setIcon(KIcon("edit-find"));
    findReplaceAction->setIcon(KIcon("edit-find-replace"));
    editUndoAction->setIcon(KIcon("edit-undo"));
    editRedoAction->setIcon(KIcon("edit-redo"));
    findFunctionsAction->setIcon(KIcon("view-list-tree"));
    editIncreaseIndentAction->setIcon(KIcon("format-indent-more"));
    editDecreaseIndentAction->setIcon(KIcon("format-indent-less"));
  } else {
    setIconSize(QSize(20,20));
    fileSaveAction->setIcon(QIcon(QPixmap(":/images/02.png")));
    fileAddToProjectAction->setIcon(QIcon(QPixmap(":/images/08.png")));
    fileCompileAction->setIcon(QIcon(QPixmap(":/images/09.png")));
    filePrintAction->setIcon(QIcon(QPixmap(":/images/03.png")));
    filePrintQuicklyAction->setIcon(QIcon(QPixmap(":/images/03.png")));
    editClearAction->setIcon(QIcon(QPixmap(":/images/04.png")));
    editCutAction->setIcon(QIcon(QPixmap(":/images/05.png")));
    editCopyAction->setIcon(QIcon(QPixmap(":/images/06.png")));
    editPasteAction->setIcon(QIcon(QPixmap(":/images/07.png")));
    findFindAction->setIcon(QIcon(QPixmap(":/images/13.png")));
    findReplaceAction->setIcon(QIcon(QPixmap(":/images/14.png")));
    editUndoAction->setIcon(QIcon(QPixmap(":/images/16.png")));
    editRedoAction->setIcon(QIcon(QPixmap(":/images/17.png")));
    findFunctionsAction->setIcon(QIcon(QPixmap(":/images/18.png")));
    editIncreaseIndentAction->setIcon(QIcon(QPixmap(":/images/19.png")));
    editDecreaseIndentAction->setIcon(QIcon(QPixmap(":/images/20.png")));
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
  if (kfinddialog)
    ACTIVATE_WINDOW(kfinddialog->winId());
  else {
    // Never set hasSelection because finding in selection doesn't really make
    // sense with my non-modal find dialog setup.
    kfinddialog=new KFindDialog(this,KFind::FromCursor,findHistory);
    kfinddialog->setModal(false);
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
  bool findBackwards=!!(kfinddialog->options()&KFind::FindBackwards);
  int findCurrentCol;
  kfind=new KFind(kfinddialog->pattern(),kfinddialog->options(),this,kfinddialog);
  kfind->closeFindNextDialog(); // don't use this, a non-modal KFindDialog is used instead
  connect(kfind,SIGNAL(highlight(const QString &,int,int)),
          this,SLOT(findFind_highlight(const QString &,int,int)));
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
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  int currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (kfind->needData()) {
      if (findBackwards?!findCurrentLine:(findCurrentLine>=currNumLines)) {
        // Try restarting the search.
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
        not_found_current:
          KMessageBox::error(this,QString("Text \'%1\' not found").arg(kfinddialog->pattern()));
          delete kfind;
          return;
      } else if (findBackwards) findCurrentLine--; else findCurrentLine++;
      kfind->setData(currView->document()->line(findCurrentLine));
    }
    result=kfind->find();
  } while (result==KFind::NoMatch);
  delete kfind;
}

void SourceFileWindow::findFind_highlight(const QString &text __attribute__((unused)), int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(findCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(findCurrentLine,matchingindex,
                                                findCurrentLine,matchingindex+matchedlength));
}

void SourceFileWindow::findFind_stop()
{
  if (kfinddialog) {
    findHistory=kfinddialog->findHistory();
    kfinddialog->deleteLater();
  }
  kfinddialog=static_cast<KFindDialog *>(NULL);
}

void SourceFileWindow::findReplace()
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
  bool findBackwards=!!(kreplace->options()&KFind::FindBackwards);
  int replaceCurrentCol;
  if (CURRENT_VIEW) {
    if (kreplace->options()&KFind::SelectedText) {
      kreplace->setSelection(CURRENT_VIEW->selectionRange());
      if (findBackwards) {
        kreplace->replaceCurrentLine=kreplace->selEndLine();
        replaceCurrentCol=kreplace->selEndCol();
      } else {
        kreplace->replaceCurrentLine=kreplace->selStartLine();
        replaceCurrentCol=kreplace->selStartCol();
      }
      kreplace->setOptions(kreplace->options()&~KFind::FromCursor);
    } else if (kreplace->options()&KFind::FromCursor) {
      if (CURRENT_VIEW->selection()) {
        if (findBackwards) {
          CURRENT_VIEW->selectionRange().start().position(kreplace->replaceCurrentLine,
                                                          replaceCurrentCol);
          if ((--replaceCurrentCol)==-1) {
            if (!kreplace->replaceCurrentLine) goto skip_data;
            kreplace->replaceCurrentLine--;
          }
        } else {
          CURRENT_VIEW->selectionRange().end().position(kreplace->replaceCurrentLine,
                                                        replaceCurrentCol);
        }
      } else {
        CURRENT_VIEW->cursorPosition().position(kreplace->replaceCurrentLine,replaceCurrentCol);
        // Don't prompt for restarting if we actually searched the entire document.
        if (findBackwards?(kreplace->replaceCurrentLine==(CURRENT_VIEW->document()->lines()-1)
                           && replaceCurrentCol==(CURRENT_VIEW->document()->lineLength(kreplace->replaceCurrentLine)))
                         :(!kreplace->replaceCurrentLine&&!replaceCurrentCol))
          kreplace->setOptions(kreplace->options()&~KFind::FromCursor);
      }
    } else {
      kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
      replaceCurrentCol=-1;
    }
    kreplace->setData(CURRENT_VIEW->document()->line(kreplace->replaceCurrentLine),replaceCurrentCol);
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
  bool findBackwards=!!(kreplace->options()&KFind::FindBackwards);

  // Reinitialize.
  if (!firstTime) {
    int replaceCurrentCol;
    // Non-first-time always continues from cursor.
    if (CURRENT_VIEW->selection()) {
      if (findBackwards) {
        CURRENT_VIEW->selectionRange().start().position(kreplace->replaceCurrentLine,
                                                        replaceCurrentCol);
        if ((--replaceCurrentCol)==-1) {
          if (!kreplace->replaceCurrentLine) goto skip_data;
          kreplace->replaceCurrentLine--;
        }
      } else {
        CURRENT_VIEW->selectionRange().end().position(kreplace->replaceCurrentLine,
                                                      replaceCurrentCol);
      }
    } else {
      CURRENT_VIEW->cursorPosition().position(kreplace->replaceCurrentLine,
                                              replaceCurrentCol);
    }
    kreplace->setData(CURRENT_VIEW->document()->line(kreplace->replaceCurrentLine),replaceCurrentCol);
  }
  skip_data:;

  // Now find the next occurrence.
  KFind::Result result;
  KTextEditor::View *currView=CURRENT_VIEW;
  int currNumLines=0;
  if (CURRENT_VIEW) currNumLines=CURRENT_VIEW->document()->lines();
  do {
    if (kreplace->needData()) {
      if (kreplace->haveSelection()
          ?(findBackwards?(kreplace->replaceCurrentLine<=kreplace->selStartLine())
                         :(kreplace->replaceCurrentLine>=kreplace->selEndLine()))
          :(findBackwards?!kreplace->replaceCurrentLine:(kreplace->replaceCurrentLine>=currNumLines))) {
        if (kreplace->shouldRestart()) {
          // Drop "From cursor" and "Selected text" options.
          kreplace->setOptions(kreplace->options()&~(KFind::FromCursor
                                                                 |KFind::SelectedText));
          kreplace->invalidateSelection();
          // Reinitialize.
          kreplace->replaceCurrentLine=findBackwards?(CURRENT_VIEW->document()->lines()-1):0;
          kreplace->setData(CURRENT_VIEW->document()->line(kreplace->replaceCurrentLine));
          // Start again as if it was the first time.
          findReplace_next(TRUE);
          return;
        } else {
          findReplace_stop();
          return;
        }
      } else if (findBackwards) kreplace->replaceCurrentLine--; else kreplace->replaceCurrentLine++;
      kreplace->setData(currView->document()->line(kreplace->replaceCurrentLine));
    }
    result=kreplace->replace();
  } while (result==KFind::NoMatch);
}

void SourceFileWindow::findReplace_highlight(const QString &text __attribute__((unused)), int matchingindex, int matchedlength)
{
  CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(kreplace->replaceCurrentLine,matchingindex+matchedlength));
  CURRENT_VIEW->setSelection(KTextEditor::Range(kreplace->replaceCurrentLine,matchingindex,
                                                kreplace->replaceCurrentLine,matchingindex+matchedlength));
}

void SourceFileWindow::findReplace_replace(const QString &text, int replacementIndex, int replacedLength, int matchedLength)
{
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
  CURRENT_VIEW->document()->insertText(KTextEditor::Cursor(kreplace->replaceCurrentLine,replacementIndex),
                                       text.mid(replacementIndex,replacedLength));
  // We can't put the cursor back now because this breaks editBegin/editEnd.
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  bool updateCursor=(line==kreplace->replaceCurrentLine
                     && col==replacementIndex+replacedLength);
  CURRENT_VIEW->document()->removeText(KTextEditor::Range(kreplace->replaceCurrentLine,replacementIndex+replacedLength,
                                       kreplace->replaceCurrentLine,replacementIndex+replacedLength+matchedLength));
  CURRENT_VIEW->document()->endEditing();
  if (updateCursor)
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(kreplace->replaceCurrentLine,replacementIndex));
  if (update) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(kreplace->replaceCurrentLine,replacementIndex+replacedLength));
    CURRENT_VIEW->setSelection(KTextEditor::Range(kreplace->replaceCurrentLine,replacementIndex,
                                                  kreplace->replaceCurrentLine,replacementIndex+replacedLength));
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
  sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
                                         isASMFile);
  for (SourceFileFunctions::Iterator it=sourceFileFunctions.begin();
       it!=sourceFileFunctions.end(); ++it)
    functionDialog->functionListBox->insertItem((*it).name);
  functionDialog->exec();
  delete functionDialog;
}

void SourceFileWindow::findFunctions_functionListBox_highlighted(int index)
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

void SourceFileWindow::findFunctions_functionListBox_selected(int index)
{
  if (index>=0) {
    int line=sourceFileFunctions[index].implementationLine>=0
             ?sourceFileFunctions[index].implementationLine
             :sourceFileFunctions[index].prototypeLine;
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(line,0));
    functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_prototypeButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].prototypeLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      sourceFileFunctions[index].prototypeLine,0));
    functionDialog->accept();
  }
}

void SourceFileWindow::findFunctions_implementationButton_clicked()
{
  int index=functionDialog->functionListBox->currentItem();
  if (index>=0 && sourceFileFunctions[index].implementationLine>=0) {
    CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(
      sourceFileFunctions[index].implementationLine,0));
    functionDialog->accept();
  }
}

void SourceFileWindow::findFunctionsPopup_aboutToShow()
{
  findFunctionsPopup->clear();
  sourceFileFunctions=getFunctions(CURRENT_VIEW->document()->text(),
                                         isASMFile);
  int idx=0;
  for (SourceFileFunctions::Iterator it=sourceFileFunctions.begin();
       it!=sourceFileFunctions.end(); ++it,++idx)
    findFunctionsPopup->insertItem((*it).name,idx);
}

void SourceFileWindow::findFunctionsPopup_aboutToHide()
{
  QTimer::singleShot(0,this,SLOT(findFunctionsPopup_aboutToHide_async()));
}

void SourceFileWindow::findFunctionsPopup_aboutToHide_async()
{
  findFunctionsPopup->clear();
}

void SourceFileWindow::findFunctionsPopup_activated(int id)
{
  int line=sourceFileFunctions[id].implementationLine>=0
           ?sourceFileFunctions[id].implementationLine
           :sourceFileFunctions[id].prototypeLine;
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
  mainForm->findAndOpenFile(fileName,category);
}

void SourceFileWindow::findFindSymbolDeclaration()
{
  QString fileText=CURRENT_VIEW->document()->text();
  // "Find symbol declaration" only operates on C files.
  if (isCFile) {
    QString fileName=fileName;
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
    if (findSymbolInFile(wordUnderCursor,fileText,fileName,mainForm,
                         symbolFile,symbolLine,systemHeader)
        && !symbolFile.isNull()) {
      if (symbolFile==fileName)
        CURRENT_VIEW->setCursorPosition(KTextEditor::Cursor(symbolLine,0));
      else {
        mainForm->openHeader(symbolFile,systemHeader,symbolLine);
        if (!systemHeader)
          ACTIVATE_WINDOW(mainForm->winId());
      }
    }
  }
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
  rowStatusLabel->setMaximumWidth(30);
  rowStatusLabel->setText(QString("%1").arg(line+1));
  colStatusLabel->setMaximumWidth(30);
  colStatusLabel->setText(QString("%1").arg(col+1));
  charsStatusLabel->setMaximumWidth(100);
  charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
  rightStatusLabel->setMaximumWidth(rightStatusSize-160>0?rightStatusSize-160:0);
  rightStatusLabel->setText(fileName);
}

void SourceFileWindow::current_view_cursorPositionChanged(KTextEditor::View *view, const KTextEditor::Cursor &newPosition)
{
  if (CURRENT_VIEW==view && !disableViewEvents) {
    int line, col;
    newPosition.position(line,col);
    rowStatusLabel->setText(QString("%1").arg(line+1));
    colStatusLabel->setText(QString("%1").arg(col+1));
  }
}

void SourceFileWindow::current_view_textChanged(KTextEditor::Document *document)
{
  if (disableViewEvents) return;
  if (CURRENT_VIEW && CURRENT_VIEW->document()==document) {
    charsStatusLabel->setText(QString("%1 Characters").arg(CURRENT_VIEW->document()->text().length()));
    if (projectCompletion.contains(fileName))
      projectCompletion[fileName].dirty=TRUE;
    if (preferences.deleteOverwrittenErrors) MainForm::deleteOverwrittenErrorsIn(this);
  }
  if (kreplace) kreplace->invalidateSelection();
}

void SourceFileWindow::current_view_undoChanged()
{
  if (CURRENT_VIEW && !disableViewEvents) {
    editUndoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    editRedoAction->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
    shortcuts[0]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Undo))->isEnabled());
    shortcuts[1]->setEnabled(CURRENT_VIEW->action(KStandardAction::name(KStandardAction::Redo))->isEnabled());
  }
}

void SourceFileWindow::current_view_selectionChanged(KTextEditor::View *view)
{
  if (CURRENT_VIEW==view && !disableViewEvents) {
    editClearAction->setEnabled(view->selection());
    editCutAction->setEnabled(view->selection());
    editCopyAction->setEnabled(view->selection());
    shortcuts[2]->setEnabled(view->selection());
    shortcuts[3]->setEnabled(view->selection());
  }
}

void SourceFileWindow::current_view_textInserted(KTextEditor::View *view, const KTextEditor::Cursor &position, const QString &text)
{
  int line,col;
  position.position(line,col);
  if (preferences.autoBlocks && text=="{"
      && col==view->document()->lineLength(line)-1) {
    KTextEditor::Document *doc=view->document();
    // Only for C files.
    if (isCFile) {
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
  // Completion only operates on C files.
  if (text=="(" && isCFile)
    new ArgHintPopup(view,fileName,mainForm);
}

void SourceFileWindow::current_view_newLineHook()
{
  int line,col;
  CURRENT_VIEW->cursorPosition().position(line,col);
  KTextEditor::Document *doc=CURRENT_VIEW->document();
  if (preferences.autoBlocks && line && doc->line(line-1).endsWith("{")) {
    // Only for C files.
    if (isCFile) {
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
    shortcuts[4]->setEnabled(!clipboard->text().isNull());
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
  if (!fileName.compare(fileName)) {
    if (KMessageBox::questionYesNo(this,
          QString("The file \'%1\' has been changed by another program. "
                  "Do you want to reload it?").arg(fileName),"File Changed")
          ==KMessageBox::Yes) {
      CURRENT_VIEW->document()->setModified(FALSE);
      disableViewEvents=TRUE;
      CURRENT_VIEW->document()->documentReload();
      disableViewEvents=FALSE;
      updateRightStatusLabel();
    }
  }
}

void SourceFileWindow::languageChange()
{
  retranslateUi(this);
}

