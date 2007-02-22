#ifndef SOURCEFILEWINDOW_H
#define SOURCEFILEWINDOW_H

#include "ui_srcfilewin.h"

class SourceFileWindow : public Q3MainWindow, public Ui::SourceFileWindow
{
    Q_OBJECT

public:
    SourceFileWindow(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = Qt::WType_TopLevel);
    ~SourceFileWindow();

    virtual void initBase();
    void * createView( const QString & fileName, const QString & fileText, const QString & hlModeName, unsigned tabWidth );
    virtual int savePrompt( void );
    virtual void applyPreferences();

public slots:
    virtual void destroy();
    virtual void te_popup_aboutToShow();
    virtual void te_popup_activated( int index );
    virtual void accel_activated( int index );
    virtual void fileSave();
    virtual void fileSaveAs();
    virtual void fileAddToProject();
    virtual void fileCompile();
    virtual void filePrint();
    virtual void filePrintQuickly();
    virtual void editUndo();
    virtual void editRedo();
    virtual void editClear();
    virtual void editCut();
    virtual void editCopy();
    virtual void editPaste();
    virtual void editSelectAll();
    virtual void editIncreaseIndent();
    virtual void editDecreaseIndent();
    virtual void findFind();
    virtual void findFind_next();
    virtual void findFind_highlight( const QString & unused_text, int matchingindex, int matchedlength );
    virtual void findFind_stop();
    virtual void findReplace();
    virtual void findReplace_next();
    virtual void findReplace_highlight( const QString & unused_text, int matchingindex, int matchedlength );
    virtual void findReplace_replace( const QString & text, int replacementIndex, int replacedLength, int matchedLength );
    virtual void findReplace_stop();
    virtual void findFunctions();
    virtual void findFunctions_functionListBox_highlighted( int index );
    virtual void findFunctions_functionListBox_selected( int index );
    virtual void findFunctions_prototypeButton_clicked();
    virtual void findFunctions_implementationButton_clicked();
    virtual void findFunctionsPopup_aboutToShow();
    virtual void findFunctionsPopup_aboutToHide();
    virtual void findFunctionsPopup_aboutToHide_async();
    virtual void findFunctionsPopup_activated( int id );
    virtual void findOpenFileAtCursor();
    virtual void findFindSymbolDeclaration();
    virtual void resizeEvent( QResizeEvent * event );
    virtual void statusBar_messageChanged( const QString & message );
    virtual void current_view_cursorPositionChanged();
    virtual void current_view_textChanged();
    virtual void current_view_undoChanged();
    virtual void current_view_selectionChanged();
    virtual void current_view_charactersInteractivelyInserted( int line, int col, const QString & characters );
    virtual void clipboard_dataChanged();
    virtual void KDirWatch_dirty( const QString & fileName );
    virtual void completionPopup_closed();

protected:
    virtual void closeEvent( QCloseEvent * e );

protected slots:
    virtual void languageChange();

private:
    void removeTrailingSpacesFromView( void * view );
    void findReplace_next( bool firstTime );
    void updateSizes();
    void updateRightStatusLabel();
    void current_view_newLineHook();

};

#endif // SOURCEFILEWINDOW_H
