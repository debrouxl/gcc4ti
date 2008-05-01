/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2006-2008 Kevin Kofler

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

#include "callbacks.h"
#include <cstring>
#include <kprogressdialog.h>
#include <ticalcs.h>
#include <QApplication>
#include <QEventLoop>

static KProgressDialog *sendingProgress=0;

static void callback_ticalcs_refresh(void)
{
  if (sendingProgress) {
    QCoreApplication::processEvents(QEventLoop::AllEvents,100);
    if (sendingProgress->wasCancelled()) ticalcsUpdate.cancel=TRUE;
  }
}

static void callback_ticalcs_start(void)
{
  ticalcsUpdate.max1=ticalcsUpdate.cnt1=
  ticalcsUpdate.max2=ticalcsUpdate.cnt2=
  ticalcsUpdate.max3=ticalcsUpdate.cnt3=0;
}

static void callback_ticalcs_stop(void)
{
  ticalcsUpdate.max1=ticalcsUpdate.cnt1=
  ticalcsUpdate.max2=ticalcsUpdate.cnt2=
  ticalcsUpdate.max3=ticalcsUpdate.cnt3=0;
}

static void callback_ticalcs_pbar(void)
{
  sendingProgress->progressBar()->setRange(0,ticalcsUpdate.max1);
  sendingProgress->progressBar()->setValue(ticalcsUpdate.cnt1);
  callback_ticalcs_refresh();
}

static void callback_ticalcs_label(void)
{
  sendingProgress->setLabelText(QString("Sending \'%1\'").arg(ticalcsUpdate.text));
  callback_ticalcs_refresh();
}

// Using this strange initialization technique here in an attempt to cope with
// changes to the structure. In C, I'd use C99 designated initializers, but g++
// doesn't like them.
struct CalcUpdateInitialized : public CalcUpdate {
  CalcUpdateInitialized() {
    std::memset(static_cast<CalcUpdate *>(this),0,sizeof(CalcUpdate));
    start=callback_ticalcs_start;
    stop=callback_ticalcs_stop;
    refresh=callback_ticalcs_refresh;
    pbar=callback_ticalcs_pbar;
    label=callback_ticalcs_label;
  }
};
CalcUpdate ticalcsUpdate=CalcUpdateInitialized();

void callbacksInit(QWidget *parent)
{
  sendingProgress=new KProgressDialog(parent,"Sending Variable",
                                      QString::null);
  sendingProgress->setModal(true);
  sendingProgress->show();
  callback_ticalcs_refresh();
}

void callbacksCleanup(void)
{
  ticalcsUpdate.cancel=FALSE;
  if (sendingProgress) {
    delete sendingProgress;
    sendingProgress=static_cast<KProgressDialog *>(NULL);
  }
  QCoreApplication::processEvents(QEventLoop::ExcludeUserInput,100);
}
