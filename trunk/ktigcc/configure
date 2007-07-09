#!/bin/sh
QMAKE_ARGS=""
if test ! -z "$1"
then case "$1" in
 --prefix=*) QMAKE_ARGS=`echo $1 | sed 's/^--prefix/PREFIX/'` ;;
esac
fi
qmake-qt4 $QMAKE_ARGS
if [ $? == 127 ]; then echo "error: Qt 4 required"; fi
