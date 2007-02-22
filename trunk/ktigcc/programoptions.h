#ifndef PROGRAMOPTIONS_H
#define PROGRAMOPTIONS_H

#include "ui_programoptions.h"

class ProgramOptions : public QDialog, public Ui::ProgramOptions
{
    Q_OBJECT

public:
    ProgramOptions(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ProgramOptions();

    void ImportSettings();
    void ExportSettings();

public slots:
    virtual void CMinimumAMSVersion_toggled( bool on );
    virtual void CalcCheckbox_toggled( bool on_unused );
    virtual void KernelRadiobutton_toggled( bool on_unused );
    virtual void RelocSettings_toggled( bool on_unused );

protected:
    virtual void mousePressEvent( QMouseEvent * e );

protected slots:
    virtual void languageChange();

};

#endif // PROGRAMOPTIONS_H
