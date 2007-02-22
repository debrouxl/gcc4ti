#ifndef SELECTSTYLE_H
#define SELECTSTYLE_H

#include "ui_selectstyle.h"

class SelectStyle : public QDialog, public Ui::SelectStyle
{
    Q_OBJECT

public:
    SelectStyle(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~SelectStyle();

public slots:
    virtual void customStyle_toggled( bool on );

protected slots:
    virtual void languageChange();

};

#endif // SELECTSTYLE_H
