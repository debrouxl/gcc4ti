#ifndef TOOLSDIALOG_H
#define TOOLSDIALOG_H

#include "ui_toolsdlg.h"

class ToolsDialog : public QDialog, public Ui::ToolsDialog
{
    Q_OBJECT

public:
    ToolsDialog(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ToolsDialog();

public slots:
    virtual void init();
    virtual void addButton_clicked();
    virtual void editButton_clicked();
    virtual void removeButton_clicked();
    virtual void listView_selectionChanged();
    virtual void accept();

protected slots:
    virtual void languageChange();

};

#endif // TOOLSDIALOG_H
