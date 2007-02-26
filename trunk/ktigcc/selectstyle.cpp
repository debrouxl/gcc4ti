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

#include "selectstyle.h"

#include <QVariant>
#include <QImage>
#include <QPixmap>

void SelectStyle::customStyle_toggled(bool on)
{
  if (!on) {
    boldChk->setChecked(FALSE);
    italicChk->setChecked(FALSE);
    underlineChk->setChecked(FALSE);
    strikeoutChk->setChecked(FALSE);
  }
  boldChk->setEnabled(on);
  italicChk->setEnabled(on);
  underlineChk->setEnabled(on);
  strikeoutChk->setEnabled(on);
}

/*
 *  Constructs a SelectStyle as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
SelectStyle::SelectStyle(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
SelectStyle::~SelectStyle()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void SelectStyle::languageChange()
{
    retranslateUi(this);
}

