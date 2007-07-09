#include "wordlist.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <kpushbutton.h> 

WordList::WordList(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, name, modal, fl)
{
  setupUi(this);
  okButton->setGuiItem(KStandardGuiItem::Ok);
  cancelButton->setGuiItem(KStandardGuiItem::Cancel);
}

WordList::~WordList()
{
}

void WordList::languageChange()
{
  retranslateUi(this);
}

