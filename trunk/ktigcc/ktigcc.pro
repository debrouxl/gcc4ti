TEMPLATE	= app
LANGUAGE	= C++

CONFIG	-= debug_and_release debug_and_release_target
CONFIG	+= qt warn_on qdbus assistant
!win32-cross-g++:CONFIG += debug

QT += xml qt3support

LIBS	+= -lktexteditor -lkutils -lkdeui -lkdecore -lkio -lkparts -lkde3support

HEADERS	+= tpr.h \
	ktigcc.h \
	preferences.h \
	srcfile.h \
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

win32-cross-g++ {
  KDEPREFIX = $$(KDEPREFIX)
  isEmpty(KDEPREFIX):error(Please source mingw32-ktigcc.sh to set up the cross-build environment.)
  INCLUDEPATH += $$KDEPREFIX/include
  QMAKE_LIBDIR = $$KDEPREFIX/lib $$QMAKE_LIBDIR
} else {
  KDEPREFIX = $$system(kde4-config --prefix)
  isEmpty(KDEPREFIX):error(KDE 4 kdelibs required.)

  exists($$KDEPREFIX/include/kde4/KDE) {
    INCLUDEPATH += $$KDEPREFIX/include/kde4
  } else:exists($$KDEPREFIX/include/kde/KDE) {
    INCLUDEPATH += $$KDEPREFIX/include/kde
  } else {
    INCLUDEPATH += $$KDEPREFIX/include
  }

  KDELIBDIR = $$KDEPREFIX/lib$$system(kde4-config --libsuffix)

  !equals(KDELIBDIR,/usr/lib):!equals(KDELIBDIR,/usr/lib64) {
    QMAKE_LIBDIR = $$KDELIBDIR $$QMAKE_LIBDIR
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

win32-cross-g++ {
  # Hack out hardcoded prefix from the glib2 binary package's pkg-config file.
  HARDCODED_GLIB_PREFIX = $$system(pkg-config --variable=prefix glib-2.0)
  PKGCONFIG_CFLAGS ~= s!$$HARDCODED_GLIB_PREFIX!$$KDEPREFIX!g
  LIBS ~= s!$$HARDCODED_GLIB_PREFIX!$$KDEPREFIX!g
}

TIGCC = $$(TIGCC)
isEmpty(TIGCC) {
  TIGCC = /usr/local/tigcc
}
target.path = $$TIGCC/bin
documentation.path = $$TIGCC/doc/ktigcc
documentation.files = COPYING NEWS ChangeLog
INSTALLS += target documentation

CXXFLAGS = $$(CXXFLAGS)
isEmpty(CXXFLAGS) {
  debug {
    CXXFLAGS = -Os -g
  } else {
    CXXFLAGS = -Os -s -fomit-frame-pointer
  }
}
QMAKE_CXXFLAGS_DEBUG = $$CXXFLAGS -Wno-non-virtual-dtor $$PKGCONFIG_CFLAGS
QMAKE_CXXFLAGS_RELEASE = $$CXXFLAGS -Wno-non-virtual-dtor $$PKGCONFIG_CFLAGS

QMAKE_LFLAGS_RELEASE = -s

DISTFILES += $${documentation.files} INSTALL configure KTIGCC.anjuta fedora/ktigcc.spec images/copyright.txt

distbz2.target = dist-bzip2
distbz2.commands = zcat ktigcc.tar.gz | bzip2 --best -c > ktigcc.tar.bz2
distbz2.depends = dist
rpm.target = rpm
# The TAR_OPTIONS=--wildcards is a workaround for rpmbuild 4.4.2 being
# incompatible with tar 1.15.91 (Fedora bug #206841).
rpm.commands = TAR_OPTIONS=--wildcards rpmbuild -ta ktigcc.tar.bz2
rpm.depends = distbz2
QMAKE_EXTRA_UNIX_TARGETS += distbz2 rpm
