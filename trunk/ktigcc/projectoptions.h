#ifndef PROJECTOPTIONS_H
#define PROJECTOPTIONS_H

#include "ui_projectoptions.h"

class ProjectOptions : public QDialog, public Ui::ProjectOptions
{
    Q_OBJECT

public:
    ProjectOptions(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
    ~ProjectOptions();

public slots:
    virtual void RegularProgram_toggled( bool state );
    virtual void ExternalDataVariable_toggled( bool state );
    virtual void CompressProgram_toggled( bool state );
    virtual void CheckOncalcNames();
    virtual void UpdateVisibilities();
    virtual void ProgramOptionsFunc();
    virtual void browseButton_clicked();

protected slots:
    virtual void languageChange();

private:
    void init();
    void destroy();
    void ImportSettings( void );
    void ExportSettings( void );

};

#endif // PROJECTOPTIONS_H
