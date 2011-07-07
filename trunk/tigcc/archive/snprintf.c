#include <stdarg.h>
#include <stdio.h>

__ATTR_LIB_C__ __attribute__((__format__(__printf__,3,4))) short snprintf(char *buffer, long maxlen, const char *format, ...) {
    short result;
    va_list ap;
    va_start(ap, format);
    result = vsnprintf(buffer, maxlen, format, ap);
    va_end(ap);
    return result;
}
