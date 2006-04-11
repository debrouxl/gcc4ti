TEMPLATE	= app
LANGUAGE	= C++

CONFIG	+= qt warn_on debug

unix:LIBS	+= -lktexteditor -lqassistantclient

unix:INCLUDEPATH	+= /usr/include/kde

HEADERS	+= tpr.h \
	ktigcc.h \
	preferences.h

SOURCES	+= ktigcc.cpp \
	tpr.cxx \
	preferences.cxx

FORMS	= mainform.ui \
	projectoptions.ui

IMAGES	= images/00 \
	images/01 \
	images/02 \
	images/03 \
	images/04 \
	images/05 \
	images/06 \
	images/07 \
	images/08 \
	images/09 \
	images/10 \
	images/11 \
	images/12 \
	images/13 \
	images/14 \
	images/15 \
	images/16 \
	images/17 \
	images/18 \
	images/19 \
	images/20 \
	images/21 \
	images/22 \
	images/23 \
	images/24 \
	images/25 \
	images/26 \
	images/27 \
	images/tpr.png \
	images/folder1.png \
	images/folder2.png \
	images/filec.png \
	images/fileh.png \
	images/fileo.png \
	images/files.png \
	images/filet.png \
	images/filex.png

unix {
  UI_DIR = .ui
  MOC_DIR = .moc
  OBJECTS_DIR = .obj
}

syntaxfiles.path = /usr/share/apps/katepart/syntax/
syntaxfiles.files = gnuasm68k.xml masm68k.xml
INSTALLS += syntaxfiles

executable.path = /usr/local/tigcc/bin
executable.files = ktigcc
INSTALLS += executable

QMAKE_CXXFLAGS_DEBUG = -Os -g -Wno-non-virtual-dtor
QMAKE_CXXFLAGS_RELEASE = -Os -s -fomit-frame-pointer -Wno-non-virtual-dtor

QMAKE_LFLAGS_RELEASE = -s
