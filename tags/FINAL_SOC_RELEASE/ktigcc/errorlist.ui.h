/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/

/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006 Kevin Kofler

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

#include <qevent.h>
#include <qlistview.h>
#include <qapplication.h>
#include <qclipboard.h>
#include <qstring.h>

void ErrorList::keyPressEvent(QKeyEvent *e)
{
  if ((e->key()==Qt::Key_Insert || e->key()==Qt::Key_C)
      && e->state()==Qt::ControlButton) {
    // Copy selected errors to the clipboard.
    QListViewItemIterator lvit(errorListView,QListViewItemIterator::Selected);
    QListViewItem *errorItem;
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
