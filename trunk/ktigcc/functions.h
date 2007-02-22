#ifndef FUNCTIONDIALOG_H
#define FUNCTIONDIALOG_H

#include "ui_functions.h"

class FunctionDialog : public QDialog, public Ui::FunctionDialog
{
    Q_OBJECT

public:
    FunctionDialog(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~FunctionDialog();

public slots:
    virtual void accept();

protected slots:
    virtual void languageChange();

};

#endif // FUNCTIONDIALOG_H
