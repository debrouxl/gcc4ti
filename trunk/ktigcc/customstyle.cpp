#include "customstyle.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <kpushbutton.h> 

CustomStyle::CustomStyle(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  okButton->setGuiItem(KStandardGuiItem::Ok);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
}

CustomStyle::~CustomStyle()
{
}

void CustomStyle::languageChange()
{
  retranslateUi(this);
}

