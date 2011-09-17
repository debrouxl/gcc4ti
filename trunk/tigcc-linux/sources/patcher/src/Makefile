#Patcher makefile
#Copyright (C) 2003, Kevin Kofler
#Copyright (C) 2011, Lionel Debroux

all: tigcc-patcher

CC ?= gcc
CFLAGS = -Os -s
WARN_CFLAGS = -Wall -W -Wwrite-strings

tigcc-patcher: patcher.c patcher.h
	$(CC) $(CFLAGS) $(WARN_CFLAGS) patcher.c -o tigcc-patcher

.PHONY: clean

clean:
	rm -f tigcc-patcher
