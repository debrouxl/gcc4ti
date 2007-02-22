#ifndef PROGRAMOUTPUT_H
#define PROGRAMOUTPUT_H

#include "ui_programoutput.h"

class ProgramOutput : public QDialog, public Ui::ProgramOutput
{
    Q_OBJECT

public:
    ProgramOutput(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ProgramOutput();

protected slots:
    virtual void languageChange();

};

#endif // PROGRAMOUTPUT_H
