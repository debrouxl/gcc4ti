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

#include "preferences.h"

#include <qcheckbox.h>
#include <qradiobutton.h>
#include <knuminput.h>
#include <kfontdialog.h>
#include <kcolordialog.h>

void Preferences::init()
{
  // General
  stopAtFirstError->setChecked(preferences.stopAtFirstError);
  jumpToError->setChecked(preferences.jumpToError);
  successMessage->setChecked(preferences.successMessage);
  deleteAsmFiles->setChecked(preferences.deleteAsmFiles);
  deleteObjFiles->setChecked(preferences.deleteObjFiles);
  splitSourceFiles->setChecked(preferences.splitSourceFiles);
  allowImplicitDeclaration->setChecked(preferences.allowImplicitDeclaration);
  autoSave->setChecked(preferences.autoSave);
  downloadHeadlines->setChecked(preferences.downloadHeadlines);
  deleteOverwrittenErrors->setChecked(preferences.deleteOverwrittenErrors);
  useSystemIcons->setChecked(preferences.useSystemIcons);

  // Transfer
  switch (preferences.linkTarget) {
    default: // LT_NONE
      targetNone->setChecked(TRUE);
      break;
    case LT_TIEMU:
      targetTiEmu->setChecked(TRUE);
      break;
    case LT_REALCALC:
      targetRealCalc->setChecked(TRUE);
      break;
  }
  switch (preferences.linkPort) {
    default: // PORT_1
      port1->setChecked(TRUE);
      break;
    case PORT_2:
      port2->setChecked(TRUE);
      break;
    case PORT_3:
      port3->setChecked(TRUE);
      break;
    case PORT_4:
      port4->setChecked(TRUE);
      break;
  }
  switch (preferences.linkCable) {
    default: // CABLE_GRY
      grayLink->setChecked(TRUE);
      break;
    case CABLE_BLK:
      blackLink->setChecked(TRUE);
      break;
    case CABLE_PAR:
      parallelLink->setChecked(TRUE);
      break;
    case CABLE_SLV:
      silverLink->setChecked(TRUE);
      break;
    case CABLE_USB:
      directLink->setChecked(TRUE);
      break;
  }
  
  // Editor
  tabWidthC->setValue(preferences.tabWidthC);
  tabWidthAsm->setValue(preferences.tabWidthAsm);
  useBgColor->setChecked(preferences.useBgColor);
  bgColor->setBackgroundColor(preferences.bgColor);
  editorFont->setFont(preferences.editorFont);
  editorFont->setText(preferences.editorFont.family());
  useCalcCharset->setChecked(preferences.useCalcCharset);
  lazyLoading->setChecked(preferences.lazyLoading);
  autoBlocks->setChecked(preferences.autoBlocks);
  removeTrailingSpaces->setChecked(preferences.removeTrailingSpaces);
}

void Preferences::destroy()
{
  if (result()==Accepted) {
    // General
    preferences.stopAtFirstError=stopAtFirstError->isChecked();
    preferences.jumpToError=jumpToError->isChecked();
    preferences.successMessage=successMessage->isChecked();
    preferences.deleteAsmFiles=deleteAsmFiles->isChecked();
    preferences.deleteObjFiles=deleteObjFiles->isChecked();
    preferences.splitSourceFiles=splitSourceFiles->isChecked();
    preferences.allowImplicitDeclaration=allowImplicitDeclaration->isChecked();
    preferences.autoSave=autoSave->isChecked();
    preferences.downloadHeadlines=downloadHeadlines->isChecked();
    preferences.deleteOverwrittenErrors=deleteOverwrittenErrors->isChecked();
    preferences.useSystemIcons=useSystemIcons->isChecked();
  
    // Transfer
    preferences.linkTarget=targetTiEmu->isChecked()?LT_TIEMU
                           :targetRealCalc->isChecked()?LT_REALCALC
                           :LT_NONE;
    preferences.linkPort=port2->isChecked()?PORT_2
                         :port3->isChecked()?PORT_3
                         :port4->isChecked()?PORT_4
                         :PORT_1;
    preferences.linkCable=blackLink->isChecked()?CABLE_BLK
                          :parallelLink->isChecked()?CABLE_PAR
                          :silverLink->isChecked()?CABLE_SLV
                          :directLink->isChecked()?CABLE_USB
                          :CABLE_GRY;
    
    // Editor
    preferences.tabWidthC=tabWidthC->value();
    preferences.tabWidthAsm=tabWidthAsm->value();
    preferences.useBgColor=useBgColor->isChecked();
    preferences.bgColor=bgColor->backgroundColor();
    preferences.editorFont=editorFont->font();
    preferences.useCalcCharset=useCalcCharset->isChecked();
    preferences.lazyLoading=lazyLoading->isChecked();
    preferences.autoBlocks=autoBlocks->isChecked();
    preferences.removeTrailingSpaces=removeTrailingSpaces->isChecked();
  }
}

#define unused_on on __attribute__((unused))
void Preferences::linkTarget_toggled(bool unused_on)
{
  bool isRealCalc=targetRealCalc->isChecked();
  linkPort->setEnabled(isRealCalc);
  linkCable->setEnabled(isRealCalc);
}

void Preferences::bgColorChange_pressed()
{
  bgColorChange->setDown(FALSE);
  QColor color=bgColor->backgroundColor();
  if (KColorDialog::getColor(color,this) == KColorDialog::Accepted) {
    useBgColor->setChecked(TRUE);
    bgColor->setBackgroundColor(color);
  }
}

void Preferences::editorFontChange_pressed()
{
  editorFontChange->setDown(FALSE);
  QFont font=editorFont->font();
  if (KFontDialog::getFont(font,TRUE,this) == KFontDialog::Accepted) {
    editorFont->setFont(font);
    editorFont->setText(font.family());
  }
}
