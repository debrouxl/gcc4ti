# This script sets up variables for cross-compilation of KDE 4 + tilibs programs to MinGW.
# It assumes a KDE 4+ development environment installation and the tilibs from TiEmu 3.
export CC=i386-mingw32-gcc
export CXX=i386-mingw32-g++
export RC=i386-mingw32-windres
export KDEPREFIX=~/.wine/c/kde4
TIEMUPREFIX=~/.wine/c/tiemu
#export C_INCLUDE_PATH=$KDEPREFIX/include:$TIEMUPREFIX/include:/usr/local/i386-mingw32/include
#export LIBRARY_PATH=$KDEPREFIX/lib:$TIEMUPREFIX/lib:/usr/local/i386-mingw32/lib/w32api:/usr/local/i386-mingw32/lib
export PKG_CONFIG_PATH=/usr/local/i386-mingw32/lib/pkgconfig:$KDEPREFIX/lib/pkgconfig:$TIEMUPREFIX/lib/pkgconfig:/target/lib/pkgconfig
export QMAKESPEC=win32-cross-g++
