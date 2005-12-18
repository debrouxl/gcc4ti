#Envreg makefile
#Copyright (C) 2004, Kevin Kofler

all: envreg

CC = gcc
CFLAGS = -Os -s
WARN_CFLAGS = -Wall -W -Wwrite-strings

envreg: envreg.c
	$(CC) $(CFLAGS) $(WARN_CFLAGS) envreg.c -o envreg

.PHONY: clean

clean:
	rm -f envreg
