#Patcher makefile
#Copyright (C) 2003, Kevin Kofler

all: patcher

CC = gcc
CFLAGS = -Os -s
WARN_CFLAGS = -Wall -W -Wwrite-strings

patcher: patcher.c patcher.h
	$(CC) $(CFLAGS) $(WARN_CFLAGS) patcher.c -o patcher

.PHONY: clean

clean:
	rm -f patcher
