#ifndef PREFERENCES_H
#define PREFERENCES_H

#include "ui_preferencesdlg.h"

class Preferences : public QDialog, public Ui::Preferences
{
    Q_OBJECT

public:
    Preferences(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~Preferences();

public slots:
    virtual void init();
    virtual void destroy();
    virtual void linkTarget_toggled( bool unused_on );
    virtual void bgColorChange_clicked();
    virtual void editorFontChange_clicked();
    virtual void syntaxLanguage_activated( int index );
    virtual void syntaxEnabled_toggled( bool on );
    virtual void resetButton_clicked();
    virtual void numberColorButton_clicked();
    virtual void numberStyleButton_clicked();
    virtual void symbolColorButton_clicked();
    virtual void symbolStyleButton_clicked();
    virtual void parenthesisColorsButton_clicked();
    virtual void parenthesisStyleButton_clicked();
    virtual void syntaxListView_selectionChanged();
    virtual void syntaxListView_itemRenamed( Q3ListViewItem * item, const QString & str, int unused_col );
    virtual void syntaxListViewAccel_activated( int id );
    virtual void newStyleButton_clicked();
    virtual void newListButton_clicked();
    virtual void editButton_clicked();
    virtual void editDialog_colorButton_clicked();
    virtual void editDialog_styleButton_clicked();
    virtual void clearSelectionButton_clicked();
    virtual void applyButton_clicked();
    virtual void templateListBox_selectionChanged();
    virtual void templateListBox_currentChanged( Q3ListBoxItem * item );
    virtual void templateIdentifier_textChanged( const QString & text );
    virtual void regenCompletionInfoButton_clicked();

protected slots:
    virtual void languageChange();

};

#endif // PREFERENCES_H
