/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006-2007 Kevin Kofler

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA.
*/

#include "newsdlg.h"

#include <kcmultidialog.h>
#include <kcmoduleinfo.h>
#include <krun.h>
#include <kurl.h>
#include <kio/netaccess.h>
#include <kmessagebox.h>
#include <QVariant>
#include <QImage>
#include <QPixmap>
#include <QDateTime>
#include <QString>
#include <Q3ListBox>
#include <QColor>
#include <QPainter>
#include <QPen>
#include <cstdio>
#include <cstring>
#include "ktigcc.h"

class ColoredListBoxText : public Q3ListBoxText {
  public:
    ColoredListBoxText(Q3ListBox *listbox, const QString &text,
                       const QColor &textColor)
      : Q3ListBoxText(listbox,text), color(textColor) {}
    virtual ~ColoredListBoxText() {}
  protected:
    virtual void paint(QPainter *painter) {
      QPen oldPen=painter->pen();
      QPen pen=oldPen;
      pen.setColor(color);
      painter->setPen(pen);
      Q3ListBoxText::paint(painter);
      painter->setPen(oldPen);
    }
  private:
    QColor color;
};

bool NewsDialog::loadNews()
{
  QString tmpFile;
  bool result=FALSE;
  pconfig->setGroup("News Headlines");
  QDateTime defaultDateTime;
  QDate latestHeadline=pconfig->readDateTimeEntry("Latest Headline",
                                                  &defaultDateTime).date();
  if(KIO::NetAccess::download(
      KUrl("http://tigcc.ticalc.org/linux/newsheadlines.txt"),tmpFile,this)) {
    #define ERROR(s) do {KMessageBox::error(this,(s)); goto done;} while(0)
    #define ZAP_LF() do {char *p=line+(std::strlen(line)-1); if (*p=='\n') *p=0;} while(0)
    #define ZAP_CR() do {char *p=line+(std::strlen(line)-1); if (*p=='\r') *p=0;} while(0)
    std::FILE *f=std::fopen(tmpFile,"r");
    if (!f) ERROR("Downloading news failed.");
    char line[32768];
    // "TIGCC News Format"
    if (!std::fgets(line,32768,f)) ERROR("Invalid news file.");
    ZAP_LF();ZAP_CR();
    if (std::strcmp(line,"TIGCC News Format")) ERROR("Invalid news file.");
    // Empty line
    if (!std::fgets(line,32768,f)) ERROR("Invalid news file.");
    ZAP_LF();ZAP_CR();
    if (*line) ERROR("Invalid news file.");
    newsListBox->clear();
    while (1) {
      bool itemIsNew=FALSE;
      unsigned y,m,d;
      // Date
      if (!std::fgets(line,32768,f)) goto done;
      ZAP_LF();ZAP_CR();
      if (!*line) goto done;
      if (std::sscanf(line,"%4u%2u%2u",&y,&m,&d)<3) ERROR("Invalid news file.");
      if (latestHeadline.isNull() || QDate(y,m,d)>latestHeadline) {
        if (!result) {
          pconfig->writeEntry("Latest Headline",QDateTime(QDate(y,m,d)));
          pconfig->sync();
        }
        result=itemIsNew=TRUE;
      }
      // Title
      if (!std::fgets(line,32768,f)) ERROR("Invalid news file.");
      ZAP_LF();ZAP_CR();
      new ColoredListBoxText(newsListBox,QString::fromUtf8(line),
                             itemIsNew?Qt::red:Qt::gray);
      // Empty line
      if (!std::fgets(line,32768,f)) goto done;
      ZAP_LF();ZAP_CR();
      if (*line) ERROR("Invalid news file.");
    }
    #undef ERROR
    #undef ZAP_LF
    #undef ZAP_CR
    done: if (f) std::fclose(f);
    KIO::NetAccess::removeTempFile(tmpFile);
  } else {
    KMessageBox::error(this,KIO::NetAccess::lastErrorString());
  }
  return result;
}

void NewsDialog::proxySettingsButton_clicked()
{
  KCModuleInfo proxyModuleInfo("proxy");
  if (proxyModuleInfo.moduleName().isNull())
    KMessageBox::error(this,"This feature requires kdebase.");
  else {
    KCMultiDialog proxySettings(this);
    proxySettings.addModule(proxyModuleInfo);
    proxySettings.exec();
  }
}

void NewsDialog::refreshButton_clicked()
{
  loadNews();
}

void NewsDialog::visitButton_clicked()
{
  KRun::runUrl(KUrl("http://tigcc.ticalc.org/linux/"),"text/html",
               static_cast<QWidget *>(parent()));
}

/*
 *  Constructs a NewsDialog as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
NewsDialog::NewsDialog(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
NewsDialog::~NewsDialog()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void NewsDialog::languageChange()
{
    retranslateUi(this);
}

