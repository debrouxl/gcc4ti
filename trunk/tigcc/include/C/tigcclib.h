#include <alloc.h>
#include <args.h>
#include <assert.h>
#include <asmtypes.h>
#include <bascmd.h>
#include <basfunc.h>
#include <basop.h>
#include <cert.h>
#include <compat.h>
#include <ctype.h>
#include <dialogs.h>
#include <dll.h>
#include <error.h>
#include <estack.h>
#include <events.h>
#include <files.h>
#include <flash.h>
#include <float.h>
#include <gdraw.h>
#include <graph.h>
#include <graphing.h>
#include <gray.h>
#include <homescr.h>
#include <intr.h>
#include <kbd.h>
#include <limits.h>
#include <link.h>
#include <mem.h>
#include <menus.h>
#include <peekpoke.h>
#include <printf.h>
#include <rsa.h>
#include <setjmp.h>
#include <sprites.h>
#include <statline.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <system.h>
#include <textedit.h>
#include <timath.h>
#include <unknown.h>
#include <values.h>
#include <vat.h>
#include <version.h>
#include <wingraph.h>

/* The following macros are for debugging purposes; don't use them... */

#define __MARK(s) asm(".ascii \"" #s "\"");
#define __HALT asm volatile("0:bra.s 0b")

#define __WHERE_AM_I ({register void *__p; asm("bsr 0f; 0:move.l (%%sp)+,%0":"=g"(__p)); __p;})
