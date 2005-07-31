#A68k compatibility wrapper makefile
#Copyright (C) 2005, Kevin Kofler

all: a68k

CC = gcc
CFLAGS = -Os -s -fno-exceptions
WARN_CFLAGS = -Wall -W -Wwrite-strings

a68k: a68k.c
	$(CC) $(CFLAGS) $(WARN_CFLAGS) a68k.c -o a68k

.PHONY: clean

clean:
	rm -f a68k a68k.exe
