
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


typedef struct
{
  bool lazyLoading;
  unsigned short tabWidthC;
  unsigned short tabWidthAsm;
  Syn_SettingsForDoc synC;
  Syn_SettingsForDoc synS;
  Syn_SettingsForDoc synASM;
  Syn_SettingsForDoc synQLL;
} TIGCCPrefs;

void loadPreferences(TIGCCPrefs *prefs,KConfig *cfg);
void savePreferences(TIGCCPrefs *prefs,KConfig *cfg);
void defaultPreferences(TIGCCPrefs *prefs);
