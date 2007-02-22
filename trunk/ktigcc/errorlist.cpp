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

#include "errorlist.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>
#include <qevent.h>
#include <q3listview.h>
#include <qapplication.h>
#include <qclipboard.h>
#include <qstring.h>
#include <QFocusEvent>
#include <QKeyEvent>

void ErrorList::keyPressEvent(QKeyEvent *e)
{
  if ((e->key()==Qt::Key_Insert || e->key()==Qt::Key_C)
      && e->state()==Qt::ControlModifier) {
    // Copy selected errors to the clipboard.
    Q3ListViewItemIterator lvit(errorListView,Q3ListViewItemIterator::Selected);
    Q3ListViewItem *errorItem;
    QString clipboardText;
    for (errorItem=lvit.current();errorItem;errorItem=(++lvit).current()) {
      clipboardText.append(errorItem->text(0));
      clipboardText.append('\n');
    }
    QApplication::clipboard()->setText(clipboardText,QClipboard::Clipboard);
    e->accept();
  } else QWidget::keyPressEvent(e);
}

void ErrorList::focusInEvent(QFocusEvent *e)
{
  QWidget::focusInEvent(e);
  grabKeyboard();
}

void ErrorList::focusOutEvent(QFocusEvent *e)
{
  releaseKeyboard();
  QWidget::focusOutEvent(e);
}

/*
 *  Constructs a ErrorList as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
ErrorList::ErrorList(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : QWidget(parent, name, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
ErrorList::~ErrorList()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ErrorList::languageChange()
{
    retranslateUi(this);
}

