#TIGCC frontend makefile
#Copyright (C) 2003, Kevin Kofler

all: tigcc

CC = gcc
CFLAGS = -Os -s
WARN_CFLAGS = -Wall -W

tigcc: tigcc.c tigcc.h
	$(CC) $(CFLAGS) $(WARN_CFLAGS) tigcc.c -o tigcc

.PHONY: clean

clean:
	rm -f tigcc
