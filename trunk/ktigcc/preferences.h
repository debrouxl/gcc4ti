/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2007 Kevin Kofler
   Copyright (C) 2006 Joey Adams

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

#include <QString>
#include <QStringList>
#include <q3valuelist.h>
#include <QPair>
#include <qcolor.h>
#include <qfont.h>
#include <ticables.h>
class QWidget;
class KConfig;

#define SYNS_CUSTOM    1
#define SYNS_BOLD      2
#define SYNS_UNDERLINE 4
#define SYNS_ITALIC    8
#define SYNS_STRIKEOUT 16

typedef unsigned short Syn_Style;

typedef struct
{
  QString name;
  QString beginning;
  QString ending;
  QChar ignoreEndingAfter;
  bool switchable;
  bool lineStartOnly;
  QColor color;
  Syn_Style style;
} Syn_CustomStyle;

typedef struct
{
  QString name;
  QStringList list;
  QColor color;
  Syn_Style style;
  bool caseSensitive;
} Syn_WordList;

typedef struct
{
  bool enabled;
  QColor numberColor;
  QColor symbolColor;
  Q3ValueList<QColor> parenthesisColors;
  Syn_Style numberStyle;
  Syn_Style symbolStyle;
  Syn_Style parenthesisStyle;
  Q3ValueList<Syn_CustomStyle> customStyles;
  Q3ValueList<Syn_WordList> wordLists;
} Syn_SettingsForDoc;

int SynToXML(Syn_SettingsForDoc &syn,const QString &destFileName);


enum LinkTargets {LT_NONE, LT_TIEMU, LT_REALCALC};

typedef struct
{
  // General
  bool stopAtFirstError;
  bool jumpToError;
  bool successMessage;
  bool deleteAsmFiles;
  bool deleteObjFiles;
  bool splitSourceFiles;
  bool allowImplicitDeclaration;
  bool autoSave;
  bool downloadHeadlines;
  bool deleteOverwrittenErrors;
  bool useSystemIcons;

  // Transfer
  LinkTargets linkTarget;
  CablePort linkPort;
  CableModel linkCable;

  // Editor
  unsigned short tabWidthC;
  unsigned short tabWidthAsm;
  bool useBgColor;
  QColor bgColor;
  QFont editorFont;
  bool useCalcCharset;
  bool lazyLoading;
  bool autoBlocks;
  bool removeTrailingSpaces;

  // Syntax Highlighting
  Syn_SettingsForDoc synC;
  Syn_SettingsForDoc synS;
  Syn_SettingsForDoc synAsm;
  Syn_SettingsForDoc synQll;

  // Coding
  Q3ValueList<QPair<QString,QString> > templates;

  // Used internally by the Preferences dialog
  bool haveA68k;
  bool haveQuill;
  Syn_SettingsForDoc tempSynC;
  Syn_SettingsForDoc tempSynS;
  Syn_SettingsForDoc tempSynAsm;
  Syn_SettingsForDoc tempSynQll;
  Syn_SettingsForDoc *syn;
} TIGCCPrefs;

void resetSyntaxPreference(Syn_SettingsForDoc *syn);
void loadPreferences(void);
void savePreferences(void);
int showPreferencesDialog(QWidget *parent, bool haveA68k, bool haveQuill);

extern TIGCCPrefs preferences;

#define C_ENABLED_HL_MODE ("TIGCC C")
#define S_ENABLED_HL_MODE ("TIGCC GNU As")
#define ASM_ENABLED_HL_MODE ("TIGCC A68k")
#define QLL_ENABLED_HL_MODE ("TIGCC Quill")
#define C_HL_MODE (preferences.synC.enabled?C_ENABLED_HL_MODE:"None")
#define S_HL_MODE (preferences.synS.enabled?S_ENABLED_HL_MODE:"None")
#define ASM_HL_MODE (preferences.synAsm.enabled?ASM_ENABLED_HL_MODE:"None")
#define QLL_HL_MODE (preferences.synQll.enabled?QLL_ENABLED_HL_MODE:"None")
