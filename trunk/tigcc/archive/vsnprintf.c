#include <setjmp.h>
#include <stdarg.h>
#include <stdio.h>

static unsigned long vsnprintf_maxlen = 0;
static unsigned short vsnprintf_written = 0;
static JMP_BUF vsnprintf_jmpbuf = {};


CALLBACK void vsnprintf_callback(char c, void **ptr) {
    if (vsnprintf_written >= vsnprintf_maxlen) {
        longjmp(&vsnprintf_jmpbuf, 1);
    }
    *((*(char**)ptr)++) = c;

    vsnprintf_written++;
}

__ATTR_LIB_C__ short vsnprintf (char *buffer, long maxlen, const char *format, va_list ap) {
    void *p = buffer;

    vsnprintf_maxlen = maxlen;
    vsnprintf_written = 0;

    if (setjmp (&vsnprintf_jmpbuf) == 0) {
        vcbprintf(vsnprintf_callback, &p, format, ap);
    }
    // Fall through.

    if (maxlen != 0) {
        *((char*)buffer + maxlen - 1) = 0;
    }

    return (char *)p - (char *)buffer;
}
