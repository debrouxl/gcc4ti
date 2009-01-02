# ttpack makefile
# Copyright (C) 2009, Lionel Debroux

ifeq ($(OSTYPE), msys)
EXEEXT = .exe
else
EXEEXT =
endif

all: ttpack$(EXEEXT)

CC ?= gcc
CFLAGS = -Os -s -fno-exceptions
WARN_CFLAGS = -Wall -W -Wwrite-strings

ttpack$(EXEEXT): packhead.h revtools.h strhead.h tt.h ttversion.h ttstrip.c bin2oth.c ttbin2oth.c ttpack.c
	$(CC) $(CFLAGS) $(WARN_CFLAGS) ttpack.c -o ttpack$(EXEEXT)

.PHONY: clean

clean:
	rm -f ttpack$(EXEEXT)

