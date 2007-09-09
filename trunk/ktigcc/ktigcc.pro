TEMPLATE	= app
LANGUAGE	= C++
VERSION = 1.80

CONFIG	-= debug_and_release debug_and_release_target
CONFIG	+= qt warn_on qdbus assistant
!win32:CONFIG += debug

QT += xml qt3support

LIBS	+= -lktexteditor -lkutils -lkdeui -lkdecore -lkio -lkparts -lkde3support

HEADERS	+= tpr.h \
	ktigcc.h \
	preferences.h \
	tiemu.h \
	tiemu_stub.h \
	callbacks.h \
	parsing.h \
	colorlistitem.h \
	completion.h \
	srcfilewin.h \
	projectoptions.h \
	programoptions.h \
	preferencesdlg.h \
	mainform.h \
	errorlist.h \
	programoutput.h \
	functions.h \
	newsdlg.h \
	toolsdlg.h \
	toolprops.h \
	selectstyle.h \
	selectcolors.h \
	customstyle.h \
	wordlist.h

SOURCES	+= ktigcc.cpp \
	preferences.cpp \
	tpr.cpp \
	tiemu_stub.cpp \
	callbacks.cpp \
	parsing.cpp \
	completion.cpp \
	srcfilewin.cpp \
	projectoptions.cpp \
	programoptions.cpp \
	preferencesdlg.cpp \
	mainform.cpp \
	errorlist.cpp \
	programoutput.cpp \
	functions.cpp \
	newsdlg.cpp \
	toolsdlg.cpp \
	toolprops.cpp \
	selectstyle.cpp \
	selectcolors.cpp \
	customstyle.cpp \
	wordlist.cpp

FORMS	= srcfilewin.ui \
	projectoptions.ui \
	programoptions.ui \
	preferencesdlg.ui \
	mainform.ui \
	errorlist.ui \
	programoutput.ui \
	functions.ui \
	newsdlg.ui \
	toolsdlg.ui \
	toolprops.ui \
	selectstyle.ui \
	selectcolors.ui \
	customstyle.ui \
	wordlist.ui

RESOURCES += icons.qrc

# If this is not used, a build from /usr/.... will generate
# Makefiles with relative paths for install.  This totally
# fucks up using INSTALL_ROOT for a staged install.
# See http://bugs.debian.org/180240
# (Thanks to Debian for the fix for this qmake stupidity.)
QMAKE_PROJECT_DEPTH=1

UI_DIR = .ui
MOC_DIR = .moc
OBJECTS_DIR = .obj

win32 {
  KDEPREFIX = $$(KDEPREFIX)
  isEmpty(KDEPREFIX) {
    win32-cross-g++ {
      error(Please source cross-mingw32-ktigcc.sh to set up the cross-build environment.)
    } else {
      # Try running kde4-config, however chances are it's not in the path or it was compiled with a bad prefix.
      KDEPREFIX = $$system(kde4-config --prefix)
      isEmpty(KDEPREFIX):error(KDE 4 kdelibs not found, set KDEPREFIX.)
      !exists($$KDEPREFIX):error(KDE 4 kdelibs not found, set KDEPREFIX.)
    }
  }
  KDEINCDIR = $$KDEPREFIX/include
  # $$KDEINCDIR/mingw contains the kdewin32 headers, defining stuff like mkdtemp.
  INCLUDEPATH += $$KDEINCDIR/mingw $$KDEINCDIR
  LIBS += -lkdewin32
  QMAKE_LIBDIR = $$KDEPREFIX/lib $$QMAKE_LIBDIR
} else {
  KDEPREFIX = $$system(kde4-config --prefix)
  isEmpty(KDEPREFIX):error(KDE 4 kdelibs required.)

  exists($$KDEPREFIX/include/kde4/KDE) {
    KDEINCDIR = $$KDEPREFIX/include/kde4
  } else:exists($$KDEPREFIX/include/kde/KDE) {
    KDEINCDIR = $$KDEPREFIX/include/kde
  } else {
    KDEINCDIR = $$KDEPREFIX/include
  }
  INCLUDEPATH += $$KDEINCDIR

  KDELIBDIR = $$KDEPREFIX/lib$$system(kde4-config --libsuffix)
  KDEDEVELLIBDIR = $$KDELIBDIR
  exists($$KDEDEVELLIBDIR/kde4/devel) {
    KDEDEVELLIBDIR = $$KDEDEVELLIBDIR/kde4/devel
  }

  !equals(KDEDEVELLIBDIR,/usr/lib):!equals(KDEDEVELLIBDIR,/usr/lib64) {
    QMAKE_LIBDIR = $$KDEDEVELLIBDIR $$QMAKE_LIBDIR
  }

  !equals(KDELIBDIR,/usr/lib):!equals(KDELIBDIR,/usr/lib64) {
    !darwin-*:!macx-* {
      LIBS += -Wl,--rpath,"$$KDELIBDIR"
    }
  }
}

GLIB2_MINVERSION = 2.0.0
HAVE_GLIB2 = $$system(pkg-config --atleast-version=$$GLIB2_MINVERSION glib-2.0 && echo yes || echo no)
!equals(HAVE_GLIB2,yes):error(glib2 $$GLIB2_MINVERSION or higher required.)
PKGCONFIG_CFLAGS += $$system(pkg-config --cflags glib-2.0)
LIBS += $$system(pkg-config --libs glib-2.0)

TICONV_MINVERSION = 0.0.1
HAVE_TICONV = $$system(pkg-config --atleast-version=$$TICONV_MINVERSION ticonv && echo yes || echo no)
!equals(HAVE_TICONV,yes):error(libticonv $$TICONV_MINVERSION or higher required.)
PKGCONFIG_CFLAGS += $$system(pkg-config --cflags ticonv)
LIBS += $$system(pkg-config --libs ticonv)

TICABLES_MINVERSION = 0.1.3
HAVE_TICABLES = $$system(pkg-config --atleast-version=$$TICABLES_MINVERSION ticables2 && echo yes || echo no)
!equals(HAVE_TICABLES,yes):error(libticables2 $$TICABLES_MINVERSION or higher required.)
PKGCONFIG_CFLAGS += $$system(pkg-config --cflags ticables2)
LIBS += $$system(pkg-config --libs ticables2)

TIFILES_MINVERSION = 0.0.9
HAVE_TIFILES = $$system(pkg-config --atleast-version=$$TIFILES_MINVERSION tifiles2 && echo yes || echo no)
!equals(HAVE_TIFILES,yes):error(libtifiles2 $$TIFILES_MINVERSION or higher required.)
PKGCONFIG_CFLAGS += $$system(pkg-config --cflags tifiles2)
LIBS += $$system(pkg-config --libs tifiles2)

TICALCS_MINVERSION = 0.2.4
HAVE_TICALCS = $$system(pkg-config --atleast-version=$$TICALCS_MINVERSION ticalcs2 && echo yes || echo no)
!equals(HAVE_TICALCS,yes):error(libticalcs2 $$TICALCS_MINVERSION or higher required.)
PKGCONFIG_CFLAGS += $$system(pkg-config --cflags ticalcs2)
LIBS += $$system(pkg-config --libs ticalcs2)

isEmpty(PREFIX) {
  PREFIX = $$(PREFIX)
}
isEmpty(PREFIX) {
  PREFIX = /usr/local
}
target.path = $$PREFIX/bin
documentation.path = $$PREFIX/share/doc/ktigcc
documentation.files = COPYING NEWS ChangeLog
INSTALLS += target documentation

isEmpty(CXXFLAGS) {
  CXXFLAGS = $$(CXXFLAGS)
}
isEmpty(CXXFLAGS) {
  debug {
    CXXFLAGS = -Os -g
  } else {
    CXXFLAGS = -Os -s -fomit-frame-pointer
  }
}
*-g++*:CXXFLAGS += -DHAVE_STDINT_H
QMAKE_CXXFLAGS_DEBUG = $$CXXFLAGS -Wno-non-virtual-dtor $$PKGCONFIG_CFLAGS
QMAKE_CXXFLAGS_RELEASE = $$CXXFLAGS -Wno-non-virtual-dtor $$PKGCONFIG_CFLAGS

QMAKE_LFLAGS_RELEASE = -s

DISTFILES += $${documentation.files} INSTALL configure KTIGCC.anjuta fedora/ktigcc.spec images/*.png images/copyright.txt mingw/cross-mingw32-ktigcc.sh mingw/mkspecs/win32-cross-g++/qmake.conf mingw/mkspecs/win32-cross-g++/qplatformdefs.h

distbz2.target = dist-bzip2
distbz2.commands = $(MKDIR) .obj/ktigcc-$$VERSION && $(COPY_FILE) -p --parents ktigcc.pro $$SOURCES $$HEADERS $$FORMS $$RESOURCES $$DISTFILES $$OBJECTS_DIR/ktigcc-$$VERSION/ && (cd $$OBJECTS_DIR && $(TAR) ktigcc-$${VERSION}.tar.bz2 -j ktigcc-$$VERSION) && $(MOVE) $$OBJECTS_DIR/ktigcc-$${VERSION}.tar.bz2 . && $(DEL_FILE) -r $$OBJECTS_DIR/ktigcc-$$VERSION
rpm.target = rpm
rpm.commands = rpmbuild -ta ktigcc-$${VERSION}.tar.bz2
rpm.depends = distbz2
QMAKE_EXTRA_UNIX_TARGETS += distbz2 rpm
