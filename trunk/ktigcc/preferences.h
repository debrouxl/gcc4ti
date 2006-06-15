/*
   ktigcc - TIGCC IDE for KDE

   Copyright (C) 2004-2006 Kevin Kofler
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

#include <qstring.h>
#include <qvaluelist.h>
#include <qcolor.h>
#include <qfont.h>
#include <ticables.h>
class KConfig;

typedef struct
{
  unsigned char r,g,b;
  unsigned char reserved;
} Syn_Color;

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
	char ignoreEndingAfter;
	char switchable;
	Syn_Color color;
	Syn_Style style;
} Syn_CustomStyle;

typedef struct
{
    QString name;
	QString list; //Items are delimited by '\n' characters; blank lines are ignored.
	Syn_Color color;
	Syn_Style style;
	char caseSensitive;
} Syn_WordList;

typedef struct
{
	char enabled;
	Syn_Color numberColor;
	Syn_Color symbolColor;
	QValueList<Syn_Color> parenthesisColors;
	Syn_Style numberStyle;
	Syn_Style symbolStyle;
	Syn_Style parenthesisStyle;
	QValueList<Syn_CustomStyle> customStyles;
	QValueList<Syn_WordList> wordLists;
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
  Syn_SettingsForDoc synASM;
  Syn_SettingsForDoc synQLL;
} TIGCCPrefs;

void loadPreferences(TIGCCPrefs *prefs,KConfig *cfg);
void savePreferences(TIGCCPrefs *prefs,KConfig *cfg);

extern TIGCCPrefs preferences;
