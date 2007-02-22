#ifndef CUSTOMSTYLE_H
#define CUSTOMSTYLE_H

#include "ui_customstyle.h"

class CustomStyle : public QDialog, public Ui::CustomStyle
{
    Q_OBJECT

public:
    CustomStyle(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~CustomStyle();

protected slots:
    virtual void languageChange();

};

#endif // CUSTOMSTYLE_H
