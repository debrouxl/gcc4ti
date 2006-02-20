
typedef struct
{
  unsigned char r,g,b;
  unsigned char reserved;
} Syn_Color;

typedef struct
{
	unsigned short custom:1,bold:1,underline:1,italic:1,strikeout:1;
} Syn_Style;

typedef struct
{
	QString beginning;
	QString ending;
	char ignoreEndingAfter;
	char switchable;
	Syn_Color color;
	Syn_Style style;
} Syn_CustomStyle;

typedef struct
{
	QStringList list;
	Syn_Color color;
	Syn_Style style;
	char caseSensitive;
} Syn_WordList;

typedef struct
{
	char enabled;
	Syn_Color numberColor;
	Syn_Color symbolColor;
	Syn_Color parenthesisColor;
	Syn_Style numberStyle;
	Syn_Style symbolStyle;
	Syn_Style parenthesisStyle;
	QValueList<Syn_CustomStyle> customStyles;
	QValueList<Syn_WordList> wordLists;
} Syn_SettingsForDoc;

int SynToXML(Syn_SettingsForDoc &syn,const QString &destFileName);


typedef struct
{
  bool lazyLoading;
  unsigned char tabWidthC;
  unsigned char tabWidthAsm;
} TIGCCPrefs;

void loadPreferences(TIGCCPrefs *prefs,KConfig *cfg);
void savePreferences(TIGCCPrefs *prefs,KConfig *cfg);
void defaultPreferences(TIGCCPrefs *prefs);
