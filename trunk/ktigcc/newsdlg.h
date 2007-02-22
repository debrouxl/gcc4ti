#ifndef NEWSDIALOG_H
#define NEWSDIALOG_H

#include "ui_newsdlg.h"

class NewsDialog : public QDialog, public Ui::NewsDialog
{
    Q_OBJECT

public:
    NewsDialog(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~NewsDialog();

    virtual bool loadNews();

public slots:
    virtual void proxySettingsButton_clicked();
    virtual void refreshButton_clicked();
    virtual void visitButton_clicked();

protected slots:
    virtual void languageChange();

};

#endif // NEWSDIALOG_H
