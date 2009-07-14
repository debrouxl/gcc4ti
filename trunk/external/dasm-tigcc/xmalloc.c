 /*
  * UAE - The Un*x Amiga Emulator
  *
  * Various stuff missing in some OSes.
  *
  * Copyright 1997 Bernd Schmidt
  */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "sysdep.h"

void *xmalloc(size_t n)
{
    void *a = malloc (n);
    if (a == NULL) {
	fprintf (stderr, "virtual memory exhausted\n");
	abort ();
    }
    return a;
}

void *xcalloc (size_t n, size_t size)
{
    void *a = calloc (n, size);
    if (a == NULL) {
	fprintf (stderr, "virtual memory exhausted\n");
	abort ();
    }
    return a;
}
