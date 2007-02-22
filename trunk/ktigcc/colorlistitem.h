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

#pragma once

#include <q3listbox.h>
#include <qpainter.h>
#include <qstyle.h>

class ColorListItem : public Q3ListBoxItem {
  public:
    ColorListItem(Q3ListBox *listbox, const QColor &color)
      : Q3ListBoxItem(listbox), m_color(color) {}
    virtual ~ColorListItem() {}
    QColor color() {return m_color;}
    void setColor(const QColor &color) {m_color=color; listBox()->update();}
  protected:
    virtual void paint(QPainter *painter) {
      // Find out whether we are painted onto our listbox.
      bool inListBox=listBox() && listBox()->viewport()==painter->device();
  
      QRect r(0,0,width(listBox()),height(listBox()));
      if (inListBox && isSelected())
        painter->eraseRect(r);
      painter->fillRect(2,2,width(listBox())-4,height(listBox())-4,m_color);
      if (inListBox && isCurrent())
        listBox()->style().drawPrimitive(QStyle::PE_FocusRect,painter,r,listBox()->colorGroup());
    }
    virtual int height(const Q3ListBox *) const {return 16;}
    virtual int width(const Q3ListBox *listBox) const {
      return listBox->contentsWidth();
    }
  private:
    QColor m_color;
};
