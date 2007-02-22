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

// This class is like QAssistantClient, but simpler (taylored to KTIGCC's use)
// and uses KProcess and KExtendedSocket instead of QProcess and QSocket. This
// is needed because, very annoyingly, QProcess doesn't work together with
// KProcess.

#include "assistant.h"
#include <kprocio.h>
#include <kextsock.h>
#include <kmessagebox.h>
#include <qtextcodec.h>
#include <qwidget.h>
#include <qapplication.h>
#include <qeventloop.h>
#include <q3textstream.h>
#include <unistd.h>

AssistantClient::AssistantClient(QObject *parent, const QString &profile)
  : QObject(parent), parentWidget(0), procIO(0), socket(0),
    assistantProfile(profile)
{
  if (parent->isWidgetType()) parentWidget=static_cast<QWidget *>(parent);
}

AssistantClient::~AssistantClient()
{
  int socketStatus=socket?socket->socketStatus():0;
  if (socketStatus && socketStatus<KExtendedSocket::lookupInProgress) {
    KMessageBox::error(parentWidget,socket->strError(socketStatus,
                                                     socket->systemError()),
                       "Socket Error");
  }
  if (procIO) {
    procIO->kill();
    usleep(100000);
    delete procIO;
  }
  if (socket) delete socket;
}

void AssistantClient::openAssistant(const QString &page)
{
  int socketStatus=socket?socket->socketStatus():0;
  if (socketStatus && socketStatus<KExtendedSocket::lookupInProgress) {
    KMessageBox::error(parentWidget,socket->strError(socketStatus,
                                                     socket->systemError()),
                       "Socket Error");
    procIO->kill();
    usleep(100000);
    delete procIO;
    procIO=static_cast<KProcIO *>(NULL);
    return;
  }
  if (socketStatus>=KExtendedSocket::closing) {
    // The old process is not closed yet, but the connection was already lost.
    // So kill the old process now and run a new one.
    procIO->kill();
    usleep(100000);
    delete procIO;
    procIO=static_cast<KProcIO *>(NULL);
    delete socket;
    socket=static_cast<KExtendedSocket *>(NULL);
  }
  if (!procIO) {
    start_new:
    // The QTextCodec has to be passed explicitly, or it will default to
    // ISO-8859-1 regardless of the locale, which is just broken.
    procIO=new KProcIO(QTextCodec::codecForLocale());
    // Use MergedStderr instead of Stderr so the messages get ordered
    // properly.
    procIO->setComm(static_cast<KProcess::Communication>(
      KProcess::Stdout|KProcess::MergedStderr));
    (*procIO)<<"assistant"<<"-server";
    if (!assistantProfile.isNull())
      (*procIO)<<"-profile"<<assistantProfile;
    if (!page.isNull())
      (*procIO)<<"-file"<<page;
    connect(procIO,SIGNAL(processExited(KProcess*)),
            this,SLOT(procIO_processExited()));
    connect(procIO,SIGNAL(readReady(KProcIO*)),this,SLOT(procIO_readReady()));
    if (!procIO->start()) {
      KMessageBox::error(parentWidget,"Could not run assistant.\n"
                         "This feature requires Qt 3 Assistant.");
      delete procIO;
      procIO=static_cast<KProcIO *>(NULL);
      return;
    }
  } else if (!page.isNull()) {
    // Wait for Qt Assistant to actually open.
    while (procIO && !socket) {
      usleep(10000);
      QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,10);
    }
    if (!procIO) goto start_new;
    Q3TextStream stream(static_cast<QIODevice *>(socket));
    stream<<page<<'\n';
  }
}

void AssistantClient::procIO_processExited()
{
  disconnect(procIO,SIGNAL(processExited(KProcess*)),
             this,SLOT(procIO_processExited()));
  disconnect(procIO,SIGNAL(readReady(KProcIO*)),this,SLOT(procIO_readReady()));
  procIO->deleteLater();
  procIO=static_cast<KProcIO *>(NULL);
  int socketStatus=socket?socket->socketStatus():0;
  if (socketStatus && socketStatus<KExtendedSocket::lookupInProgress) {
    KMessageBox::error(parentWidget,socket->strError(socketStatus,
                                                     socket->systemError()),
                       "Socket Error");
  }
  if (socket) delete socket;
  socket=static_cast<KExtendedSocket *>(NULL);
}

void AssistantClient::procIO_readReady()
{
  QString line;
  while (procIO->readln(line)>=0) {
    if (!socket) {
      bool ok;
      unsigned short port=line.toUShort(&ok);
      if (ok) {
        socket=new KExtendedSocket("localhost",port,KExtendedSocket::inetSocket);
        switch (socket->connect()) {
          case 0:
            break;
          case -1:
            KMessageBox::error(parentWidget,socket->strError(socket->socketStatus(),
                                                             socket->systemError()),
                               "Socket Error");
            goto error;
          case -2:
            KMessageBox::error(parentWidget,"Failed to connect to Qt Assistant.",
                               "Socket Error");
            goto error;
          case -3:
            KMessageBox::error(parentWidget,"Connection to Qt Assistant timed out.",
                               "Socket Error");
            goto error;
          default:
            KMessageBox::error(parentWidget,"Socket Error.");
            error:
            disconnect(procIO,SIGNAL(processExited(KProcess*)),
                       this,SLOT(procIO_processExited()));
            disconnect(procIO,SIGNAL(readReady(KProcIO*)),
                       this,SLOT(procIO_readReady()));
            procIO->kill();
            usleep(100000);
            procIO->deleteLater();
            procIO=static_cast<KProcIO *>(NULL);
            delete socket;
            socket=static_cast<KExtendedSocket *>(NULL);
            return;
        }
        continue;
      }
    }
    KMessageBox::error(parentWidget,line,"Qt Assistant Error");
  }
}
