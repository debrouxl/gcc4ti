#ifndef __STDARG
#define __STDARG

#include <default.h>

/* Begin Auto-Generated Part */
#ifndef __HAVE_va_list
#define __HAVE_va_list
typedef void*va_list;
#endif
#define va_arg(ap,type) (*(type*)((((char*)(ap))+=((sizeof(type)+1)&0xFFFE))-(((sizeof(type)+1)&0xFFFE))))
#define va_end(ap) ((void)0)
#define va_start(ap,parmN) ((void)((ap)=(va_list)((char*)(&parmN)+((sizeof(parmN)+1)&0xFFFE))))
/* End Auto-Generated Part */

#endif
