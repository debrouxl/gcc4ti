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

#include <qobject.h>
#include <qstring.h>

class QWidget;
class KProcIO;
class KExtendedSocket;
class AssistantClient : public QObject {
  Q_OBJECT

  public:
    AssistantClient(QObject *parent=0, const QString &profile=QString::null);
    ~AssistantClient();
    void openAssistant(const QString &page=QString::null);

  private slots:
    void procIO_processExited();
    void procIO_readReady();

  private:
    QWidget *parentWidget;
    KProcIO *procIO;
    KExtendedSocket *socket;
    QString assistantProfile;
};
