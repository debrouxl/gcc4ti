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

#pragma once

#include <qstring.h>
#include <qvaluevector.h>
struct SourceFileFunction {
  SourceFileFunction() : name(), prototypeLine(-1), implementationLine(-1) {}
  SourceFileFunction(const QString &n) :
    name(n), prototypeLine(-1), implementationLine(-1) {}
  SourceFileFunction(const QString &n, int p, int i) :
    name(n), prototypeLine(p), implementationLine(i) {}
  bool operator==(const SourceFileFunction &other) const
    {return name==other.name;}
  bool operator!=(const SourceFileFunction &other) const
    {return name!=other.name;}
  QString name;
  int prototypeLine;
  int implementationLine;
};
class SourceFileFunctions : public QValueVector<SourceFileFunction> {
  public:
    iterator find(const SourceFileFunction &item) {
      return qFind(begin(),end(),item);
    }
};
SourceFileFunctions getCFunctions(const QString &text);
SourceFileFunctions getASMFunctions(const QString &text);
#define getFunctions(text,isasm) (((isasm)?getASMFunctions:getCFunctions)((text)))
