TEMPLATE = app
LANGUAGE = C++

CONFIG -= debug_and_release debug_and_release_target
CONFIG += qt warn_on

QT -= gui

SOURCES = converttexi.cpp

QMAKE_CXXFLAGS_DEBUG = -Os -g
QMAKE_CXXFLAGS_RELEASE = -Os -g
