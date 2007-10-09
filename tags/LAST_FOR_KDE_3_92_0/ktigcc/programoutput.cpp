#include "programoutput.h"

#include <QVariant>
#include <kpushbutton.h> 

ProgramOutput::ProgramOutput(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  buttonClose->setGuiItem(KStandardGuiItem::Close);
}

ProgramOutput::~ProgramOutput()
{
}

void ProgramOutput::languageChange()
{
  retranslateUi(this);
}
