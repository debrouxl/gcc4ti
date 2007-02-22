#ifndef TOOLPROPERTIES_H
#define TOOLPROPERTIES_H

#include "ui_toolprops.h"

class ToolProperties : public QDialog, public Ui::ToolProperties
{
    Q_OBJECT

public:
    ToolProperties(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ToolProperties();

public slots:
    virtual void init();
    virtual void accept();
    virtual void validate();
    virtual void browseButton_clicked();

protected slots:
    virtual void languageChange();

};

#endif // TOOLPROPERTIES_H
