#TPRbuilder makefile
#Copyright (C) 2003, Kevin Kofler

ifeq ($(OSTYPE), msys)
EXEEXT = .exe
else
EXEEXT =
endif

all: tprbuilder$(EXEEXT)

CC = gcc
CFLAGS = -Os -s -fno-exceptions
WARN_CFLAGS = -Wall -W -Wwrite-strings

tprbuilder$(EXEEXT): tprbuilder.c tprbuilder.h
	$(CC) $(CFLAGS) $(WARN_CFLAGS) tprbuilder.c -o tprbuilder$(EXEEXT)

.PHONY: clean

clean:
	rm -f tprbuilder$(EXEEXT)
