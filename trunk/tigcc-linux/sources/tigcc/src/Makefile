# TIGCC frontend makefile
# Copyright (C) 2003, Kevin Kofler
# Copyright (C) 2011, Lionel Debroux

ifeq ($(OSTYPE), msys)
EXEEXT = .exe
else
EXEEXT =
endif

all: tigcc$(EXEEXT)

CC ?= gcc
CFLAGS = -Os -s -fno-exceptions
WARN_CFLAGS = -Wall -W -Wwrite-strings

tigcc$(EXEEXT): tigcc.c tigcc.h
	$(CC) $(CFLAGS) $(WARN_CFLAGS) tigcc.c -o tigcc$(EXEEXT)

.PHONY: clean

clean:
	rm -f tigcc$(EXEEXT)
