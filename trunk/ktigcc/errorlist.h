#ifndef ERRORLIST_H
#define ERRORLIST_H

#include "ui_errorlist.h"

class ErrorList : public QWidget, public Ui::ErrorList
{
    Q_OBJECT

public:
    ErrorList(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = 0);
    ~ErrorList();

protected:
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void focusInEvent(QFocusEvent *);
    virtual void focusOutEvent(QFocusEvent *);

protected slots:
    virtual void languageChange();

};

#endif // ERRORLIST_H
