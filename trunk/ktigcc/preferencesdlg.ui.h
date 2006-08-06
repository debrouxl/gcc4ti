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
#include <qaccel.h>
#include <qcolor.h>
#include <qvaluelist.h>
#include <qapplication.h>
#include <qeventloop.h>
#include <knuminput.h>
#include <kfontdialog.h>
#include <kcolordialog.h>
#include <kcombobox.h>
#include <klistview.h>
#include <klineedit.h>
#include <keditlistbox.h>
#include "ktigcc.h"
#include "selectstyle.h"
#include "selectcolors.h"
#include "colorlistitem.h"
#include "customstyle.h"
#include "wordlist.h"

class RenamableKListViewItem : public KListViewItem {
  public:
  RenamableKListViewItem(QListViewItem *parent, QString text)
    : KListViewItem(parent, text)
  {
    setRenameEnabled(0,TRUE);
  }
  RenamableKListViewItem(QListViewItem *parent, QListViewItem *after,
                         QString text)
    : KListViewItem(parent, after, text)
  {
    setRenameEnabled(0,TRUE);
  }
  virtual int rtti(void) const {return 0x716CC8;}
  // Work around gratuitous API difference. Why do I have to do this? That's
  // what startRename is a virtual method for. KListViewItem should do this.
  virtual void startRename(int col)
  {
    static_cast<KListView *>(listView())->rename(this,col);
  }
};

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

  // Don't allow selecting a USB cable if libticables2 hasn't been compiled
  // without USB support or if USB support can't be used.
  if (!have_usb) {
    if (silverLink->isChecked() || directLink->isChecked()) {
      grayLink->setChecked(TRUE);
      targetNone->setChecked(TRUE);
    }
    silverLink->setEnabled(FALSE);
    directLink->setEnabled(FALSE);
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

  // Syntax
  if (preferences.haveA68k)
    syntaxLanguage->insertItem("A68k Assembly Files");
  if (preferences.haveQuill)
    syntaxLanguage->insertItem("Quill Files");
  preferences.tempSynC=preferences.synC;
  preferences.tempSynS=preferences.synS;
  preferences.tempSynAsm=preferences.synAsm;
  preferences.tempSynQll=preferences.synQll;
  syntaxLanguage_activated(syntaxLanguage->currentItem());
  syntaxListView->setSorting(-1);
  syntaxListView->setColumnWidthMode(0,QListView::Maximum);
  syntaxListView->header()->hide();
  syntaxListView->setAlternateBackground(QColor());
  QListViewItem *rootListItem=syntaxListView->firstChild();
  QListViewItem *customStylesItem=rootListItem->firstChild();
  customStylesItem->setOpen(TRUE);
  QListViewItem *wordListsItem=customStylesItem->nextSibling();
  wordListsItem->setOpen(TRUE);
  QAccel *syntaxListViewAccel=new QAccel(syntaxListView);
  syntaxListViewAccel->insertItem(Key_Delete,0);
  connect(syntaxListViewAccel,SIGNAL(activated(int)),
          this,SLOT(syntaxListViewAccel_activated(int)));
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

    // Syntax
    preferences.synC=preferences.tempSynC;
    preferences.synS=preferences.tempSynS;
    preferences.synAsm=preferences.tempSynAsm;
    preferences.synQll=preferences.tempSynQll;
  }
}

#define unused_on on __attribute__((unused))
void Preferences::linkTarget_toggled(bool unused_on)
{
  bool isRealCalc=targetRealCalc->isChecked();
  linkPort->setEnabled(isRealCalc);
  linkCable->setEnabled(isRealCalc);
}

void Preferences::bgColorChange_clicked()
{
  QColor color=bgColor->backgroundColor();
  if (KColorDialog::getColor(color,this)==KColorDialog::Accepted) {
    useBgColor->setChecked(TRUE);
    bgColor->setBackgroundColor(color);
  }
}

void Preferences::editorFontChange_clicked()
{
  QFont font=editorFont->font();
  if (KFontDialog::getFont(font,TRUE,this)==KFontDialog::Accepted) {
    editorFont->setFont(font);
    editorFont->setText(font.family());
  }
}

void Preferences::syntaxLanguage_activated(int index)
{
  if (!index) preferences.syn=&preferences.tempSynC;
  else if (!--index) preferences.syn=&preferences.tempSynS;
  else if (preferences.haveA68k && !--index)
    preferences.syn=&preferences.tempSynAsm;
  else if (preferences.haveQuill && !--index)
    preferences.syn=&preferences.tempSynQll;
  else {
    qWarning("Preferences::syntaxLanguage_activated: Invalid index.");
    preferences.syn=&preferences.tempSynC;
  }

  syntaxEnabled->setChecked(preferences.syn->enabled);
  QListViewItem *rootListItem=syntaxListView->firstChild();
  QListViewItem *item, *nextItem;
  QListViewItem *customStylesItem=rootListItem->firstChild();
  for (item=customStylesItem->firstChild(); item; item=nextItem) {
    nextItem=item->nextSibling();
    delete item;
  }
  item=static_cast<QListViewItem *>(NULL);
  for (QValueList<Syn_CustomStyle>::ConstIterator it=
         preferences.syn->customStyles.begin();
       it!=preferences.syn->customStyles.end(); ++it) {
    item=new RenamableKListViewItem(customStylesItem,item,(*it).name);
  }
  QListViewItem *wordListsItem=customStylesItem->nextSibling();
  for (item=wordListsItem->firstChild(); item; item=nextItem) {
    nextItem=item->nextSibling();
    delete item;
  }  
  item=static_cast<QListViewItem *>(NULL);
  for (QValueList<Syn_WordList>::ConstIterator it=
         preferences.syn->wordLists.begin();
       it!=preferences.syn->wordLists.end(); ++it) {
    item=new RenamableKListViewItem(wordListsItem,item,(*it).name);
  }
}

void Preferences::syntaxEnabled_toggled(bool on)
{
  preferences.syn->enabled=on;
  resetButton->setEnabled(on);
  numberColorButton->setEnabled(on);
  numberStyleButton->setEnabled(on);
  symbolColorButton->setEnabled(on);
  symbolStyleButton->setEnabled(on);
  parenthesisColorsButton->setEnabled(on);
  parenthesisStyleButton->setEnabled(on);
  syntaxListView->setEnabled(on);
  newStyleButton->setEnabled(on);
  newListButton->setEnabled(on);
  QListViewItem *selectedItem=syntaxListView->selectedItem();
  editButton->setEnabled(on&&selectedItem&&selectedItem->rtti()==0x716CC8);
}

void Preferences::resetButton_clicked()
{
  resetSyntaxPreference(preferences.syn);
  syntaxLanguage_activated(syntaxLanguage->currentItem());
}

void Preferences::numberColorButton_clicked()
{
  QColor color=preferences.syn->numberColor;
  if (KColorDialog::getColor(color,this)==KColorDialog::Accepted)
    preferences.syn->numberColor=color;
}

void Preferences::numberStyleButton_clicked()
{
  SelectStyle selectStyle(this);
  selectStyle.customStyle->setChecked(!!(preferences.syn->numberStyle&SYNS_CUSTOM));
  if (preferences.syn->numberStyle&SYNS_CUSTOM) {
    selectStyle.boldChk->setChecked(!!(preferences.syn->numberStyle&SYNS_BOLD));
    selectStyle.underlineChk->setChecked(!!(preferences.syn->numberStyle&SYNS_UNDERLINE));
    selectStyle.italicChk->setChecked(!!(preferences.syn->numberStyle&SYNS_ITALIC));
    selectStyle.strikeoutChk->setChecked(!!(preferences.syn->numberStyle&SYNS_STRIKEOUT));
  }
  selectStyle.exec();
  if (selectStyle.result()==QDialog::Accepted) {
    preferences.syn->numberStyle=0;
    if (selectStyle.customStyle->isChecked()) {
      preferences.syn->numberStyle|=SYNS_CUSTOM;
      if (selectStyle.boldChk->isChecked()) preferences.syn->numberStyle|=SYNS_BOLD;
      if (selectStyle.underlineChk->isChecked()) preferences.syn->numberStyle|=SYNS_UNDERLINE;
      if (selectStyle.italicChk->isChecked()) preferences.syn->numberStyle|=SYNS_ITALIC;
      if (selectStyle.strikeoutChk->isChecked()) preferences.syn->numberStyle|=SYNS_STRIKEOUT;
    }
  }
}

void Preferences::symbolColorButton_clicked()
{
  QColor color=preferences.syn->symbolColor;
  if (KColorDialog::getColor(color,this)==KColorDialog::Accepted)
    preferences.syn->symbolColor=color;
}

void Preferences::symbolStyleButton_clicked()
{
  SelectStyle selectStyle(this);
  selectStyle.customStyle->setChecked(!!(preferences.syn->symbolStyle&SYNS_CUSTOM));
  if (preferences.syn->symbolStyle&SYNS_CUSTOM) {
    selectStyle.boldChk->setChecked(!!(preferences.syn->symbolStyle&SYNS_BOLD));
    selectStyle.underlineChk->setChecked(!!(preferences.syn->symbolStyle&SYNS_UNDERLINE));
    selectStyle.italicChk->setChecked(!!(preferences.syn->symbolStyle&SYNS_ITALIC));
    selectStyle.strikeoutChk->setChecked(!!(preferences.syn->symbolStyle&SYNS_STRIKEOUT));
  }
  selectStyle.exec();
  if (selectStyle.result()==QDialog::Accepted) {
    preferences.syn->symbolStyle=0;
    if (selectStyle.customStyle->isChecked()) {
      preferences.syn->symbolStyle|=SYNS_CUSTOM;
      if (selectStyle.boldChk->isChecked()) preferences.syn->symbolStyle|=SYNS_BOLD;
      if (selectStyle.underlineChk->isChecked()) preferences.syn->symbolStyle|=SYNS_UNDERLINE;
      if (selectStyle.italicChk->isChecked()) preferences.syn->symbolStyle|=SYNS_ITALIC;
      if (selectStyle.strikeoutChk->isChecked()) preferences.syn->symbolStyle|=SYNS_STRIKEOUT;
    }
  }
}

void Preferences::parenthesisColorsButton_clicked()
{
  SelectColors selectColors(this);
  selectColors.colorList->clear();
  for (QValueList<QColor>::ConstIterator it=preferences.syn->parenthesisColors.begin();
       it!=preferences.syn->parenthesisColors.end(); ++it)
    new ColorListItem(selectColors.colorList,*it);
  selectColors.exec();
  if (selectColors.result()==QDialog::Accepted) {
    preferences.syn->parenthesisColors.clear();
    for (QListBoxItem *item=selectColors.colorList->firstItem(); item;
         item=item->next())
      preferences.syn->parenthesisColors.append(
        static_cast<ColorListItem *>(item)->color());
  }
}

void Preferences::parenthesisStyleButton_clicked()
{
  SelectStyle selectStyle(this);
  selectStyle.customStyle->setChecked(!!(preferences.syn->parenthesisStyle&SYNS_CUSTOM));
  if (preferences.syn->parenthesisStyle&SYNS_CUSTOM) {
    selectStyle.boldChk->setChecked(!!(preferences.syn->parenthesisStyle&SYNS_BOLD));
    selectStyle.underlineChk->setChecked(!!(preferences.syn->parenthesisStyle&SYNS_UNDERLINE));
    selectStyle.italicChk->setChecked(!!(preferences.syn->parenthesisStyle&SYNS_ITALIC));
    selectStyle.strikeoutChk->setChecked(!!(preferences.syn->parenthesisStyle&SYNS_STRIKEOUT));
  }
  selectStyle.exec();
  if (selectStyle.result()==QDialog::Accepted) {
    preferences.syn->parenthesisStyle=0;
    if (selectStyle.customStyle->isChecked()) {
      preferences.syn->parenthesisStyle|=SYNS_CUSTOM;
      if (selectStyle.boldChk->isChecked()) preferences.syn->parenthesisStyle|=SYNS_BOLD;
      if (selectStyle.underlineChk->isChecked()) preferences.syn->parenthesisStyle|=SYNS_UNDERLINE;
      if (selectStyle.italicChk->isChecked()) preferences.syn->parenthesisStyle|=SYNS_ITALIC;
      if (selectStyle.strikeoutChk->isChecked()) preferences.syn->parenthesisStyle|=SYNS_STRIKEOUT;
    }
  }
}

void Preferences::syntaxListView_selectionChanged()
{
  QListViewItem *selectedItem=syntaxListView->selectedItem();
  editButton->setEnabled(syntaxEnabled->isChecked()
                         && selectedItem && selectedItem->rtti()==0x716CC8);
}

#define unused_col col __attribute__((unused))
void Preferences::syntaxListView_itemRenamed(QListViewItem *item, const QString &str, int unused_col)
{
  QListViewItem *rootListItem=syntaxListView->firstChild();
  QListViewItem *customStylesItem=rootListItem->firstChild();
  QListViewItem *wordListsItem=customStylesItem->nextSibling();
  if (item->parent()==customStylesItem) {
    QListViewItem *i;
    QValueList<Syn_CustomStyle>::Iterator it;
    for (it=preferences.syn->customStyles.begin(), i=customStylesItem->firstChild();
         i!=item && it!=preferences.syn->customStyles.end() && i;
         ++it, i=i->nextSibling());
    if (it==preferences.syn->customStyles.end() || !i)
      qWarning("Preferences::syntaxListView_itemRenamed: Invalid item.");
    else
      (*it).name=str;
  } else if (item->parent()==wordListsItem) {
    QListViewItem *i;
    QValueList<Syn_WordList>::Iterator it;
    for (it=preferences.syn->wordLists.begin(), i=wordListsItem->firstChild();
         i!=item && it!=preferences.syn->wordLists.end() && i;
         ++it, i=i->nextSibling());
    if (it==preferences.syn->wordLists.end() || !i)
      qWarning("Preferences::syntaxListView_itemRenamed: Invalid item.");
    else
      (*it).name=str;
  } else qWarning("Preferences::syntaxListView_itemRenamed: Invalid parent.");
}

void Preferences::syntaxListViewAccel_activated(int id)
{
  if (!id) {
    QListViewItem *currentItem=syntaxListView->currentItem();
    if (currentItem && currentItem->rtti()==0x716CC8) {
      QListViewItem *rootListItem=syntaxListView->firstChild();
      QListViewItem *customStylesItem=rootListItem->firstChild();
      QListViewItem *wordListsItem=customStylesItem->nextSibling();
      if (currentItem->parent()==customStylesItem) {
        QListViewItem *i;
        QValueList<Syn_CustomStyle>::Iterator it;
        for (it=preferences.syn->customStyles.begin(), i=customStylesItem->firstChild();
             i!=currentItem && it!=preferences.syn->customStyles.end() && i;
             ++it, i=i->nextSibling());
        if (it==preferences.syn->customStyles.end() || !i)
          qWarning("Preferences::syntaxListViewAccel_activated: Invalid item.");
        else {
          delete currentItem;
          preferences.syn->customStyles.remove(it);
        }
      } else if (currentItem->parent()==wordListsItem) {
        QListViewItem *i;
        QValueList<Syn_WordList>::Iterator it;
        for (it=preferences.syn->wordLists.begin(), i=wordListsItem->firstChild();
             i!=currentItem && it!=preferences.syn->wordLists.end() && i;
             ++it, i=i->nextSibling());
        if (it==preferences.syn->wordLists.end() || !i)
          qWarning("Preferences::syntaxListViewAccel_activated: Invalid item.");
        else {
          delete currentItem;
          preferences.syn->wordLists.remove(it);
        }
      } else qWarning("Preferences::syntaxListViewAccel_activated: Invalid parent.");
    }
  }
}

void Preferences::newStyleButton_clicked()
{
  Syn_CustomStyle newStyle;
  newStyle.name="New Style";
  preferences.syn->customStyles.append(newStyle);
  QListViewItem *rootListItem=syntaxListView->firstChild();
  QListViewItem *customStylesItem=rootListItem->firstChild();
  QListViewItem *item=customStylesItem->firstChild();
  while (item && item->nextSibling()) item=item->nextSibling();
  item=new RenamableKListViewItem(customStylesItem,item,newStyle.name);
  syntaxListView->setCurrentItem(item);
  syntaxListView->setSelected(item,TRUE);
  // startRename won't work properly if I don't do this. :-/
  QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,1000);
  syntaxListView->ensureItemVisible(item);
  QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,1000);
  item->startRename(0);
}

void Preferences::newListButton_clicked()
{
  Syn_WordList newList;
  newList.name="New List";
  preferences.syn->wordLists.append(newList);
  QListViewItem *rootListItem=syntaxListView->firstChild();
  QListViewItem *customStylesItem=rootListItem->firstChild();
  QListViewItem *wordListsItem=customStylesItem->nextSibling();
  QListViewItem *item=wordListsItem->firstChild();
  while (item && item->nextSibling()) item=item->nextSibling();
  item=new RenamableKListViewItem(wordListsItem,item,newList.name);
  syntaxListView->setCurrentItem(item);
  syntaxListView->setSelected(item,TRUE);
  // startRename won't work properly if I don't do this. :-/
  QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,1000);
  syntaxListView->ensureItemVisible(item);
  QApplication::eventLoop()->processEvents(QEventLoop::ExcludeUserInput,1000);
  item->startRename(0);
}

static QDialog *editDialog;
static Syn_Style tempStyle;
static QColor tempColor;

void Preferences::editButton_clicked()
{
  QListViewItem *currentItem=syntaxListView->currentItem();
  if (currentItem && currentItem->rtti()==0x716CC8) {
    QListViewItem *rootListItem=syntaxListView->firstChild();
    QListViewItem *customStylesItem=rootListItem->firstChild();
    QListViewItem *wordListsItem=customStylesItem->nextSibling();
    if (currentItem->parent()==customStylesItem) {
      QListViewItem *i;
      QValueList<Syn_CustomStyle>::Iterator it;
      for (it=preferences.syn->customStyles.begin(), i=customStylesItem->firstChild();
           i!=currentItem && it!=preferences.syn->customStyles.end() && i;
           ++it, i=i->nextSibling());
      if (it==preferences.syn->customStyles.end() || !i)
        qWarning("Preferences::editButton_clicked: Invalid item.");
      else {
        Syn_CustomStyle &customStyle=*it;
        CustomStyle customStyleDlg(this);
        editDialog=&customStyleDlg;
        customStyleDlg.beginning->setText(customStyle.beginning);
        customStyleDlg.ending->setText(customStyle.ending=="\n"?"\\n"
                                       :customStyle.ending);
        customStyleDlg.ignoreEndingAfter->setText(QString(customStyle.ignoreEndingAfter));
        customStyleDlg.switchable->setChecked(customStyle.switchable);
        customStyleDlg.lineStartOnly->setChecked(customStyle.lineStartOnly);
        tempStyle=customStyle.style;
        tempColor=customStyle.color;
        connect(customStyleDlg.styleButton,SIGNAL(clicked()),
                this,SLOT(editDialog_styleButton_clicked()));
        connect(customStyleDlg.colorButton,SIGNAL(clicked()),
                this,SLOT(editDialog_colorButton_clicked()));
        customStyleDlg.exec();
        if (customStyleDlg.result()==QDialog::Accepted) {
          customStyle.beginning=customStyleDlg.beginning->text();
          customStyle.ending=customStyleDlg.ending->text()=="\\n"?"\n"
                             :customStyleDlg.ending->text();
          customStyle.ignoreEndingAfter=customStyleDlg.ignoreEndingAfter->text()[0];
          customStyle.switchable=customStyleDlg.switchable->isChecked();
          customStyle.lineStartOnly=customStyleDlg.lineStartOnly->isChecked();
          customStyle.style=tempStyle;
          customStyle.color=tempColor;
        }
      }
    } else if (currentItem->parent()==wordListsItem) {
      QListViewItem *i;
      QValueList<Syn_WordList>::Iterator it;
      for (it=preferences.syn->wordLists.begin(), i=wordListsItem->firstChild();
           i!=currentItem && it!=preferences.syn->wordLists.end() && i;
           ++it, i=i->nextSibling());
      if (it==preferences.syn->wordLists.end() || !i)
        qWarning("Preferences::editButton_clicked: Invalid item.");
      else {
        Syn_WordList &wordList=*it;
        WordList wordListDlg(this);
        editDialog=&wordListDlg;
        wordListDlg.wordList->setItems(wordList.list);
        wordListDlg.caseSensitive->setChecked(wordList.caseSensitive);
        tempStyle=wordList.style;
        tempColor=wordList.color;
        connect(wordListDlg.styleButton,SIGNAL(clicked()),
                this,SLOT(editDialog_styleButton_clicked()));
        connect(wordListDlg.colorButton,SIGNAL(clicked()),
                this,SLOT(editDialog_colorButton_clicked()));
        wordListDlg.exec();
        if (wordListDlg.result()==QDialog::Accepted) {
          wordList.list=wordListDlg.wordList->items();
          wordList.caseSensitive=wordListDlg.caseSensitive->isChecked();
          wordList.style=tempStyle;
          wordList.color=tempColor;
        }
      }
    } else qWarning("Preferences::editButton_clicked: Invalid parent.");
  }
}

void Preferences::editDialog_colorButton_clicked()
{
  QColor color=tempColor;
  if (KColorDialog::getColor(color,QColor(),editDialog)==KColorDialog::Accepted)
    tempColor=color;
}

void Preferences::editDialog_styleButton_clicked()
{
  SelectStyle selectStyle(editDialog);
  selectStyle.customStyle->setChecked(!!(tempStyle&SYNS_CUSTOM));
  if (tempStyle&SYNS_CUSTOM) {
    selectStyle.boldChk->setChecked(!!(tempStyle&SYNS_BOLD));
    selectStyle.underlineChk->setChecked(!!(tempStyle&SYNS_UNDERLINE));
    selectStyle.italicChk->setChecked(!!(tempStyle&SYNS_ITALIC));
    selectStyle.strikeoutChk->setChecked(!!(tempStyle&SYNS_STRIKEOUT));
  }
  selectStyle.exec();
  if (selectStyle.result()==QDialog::Accepted) {
    tempStyle=0;
    if (selectStyle.customStyle->isChecked()) {
      tempStyle|=SYNS_CUSTOM;
      if (selectStyle.boldChk->isChecked()) tempStyle|=SYNS_BOLD;
      if (selectStyle.underlineChk->isChecked()) tempStyle|=SYNS_UNDERLINE;
      if (selectStyle.italicChk->isChecked()) tempStyle|=SYNS_ITALIC;
      if (selectStyle.strikeoutChk->isChecked()) tempStyle|=SYNS_STRIKEOUT;
    }
  }
}
