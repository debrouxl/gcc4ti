#ifndef __STDDEF
#define __STDDEF

#ifndef KERNEL_NEW
#include <default.h>
#endif

/* Begin Auto-Generated Part */
#define NULL ((void*)0)
#ifndef __HAVE_size_t
#define __HAVE_size_t
typedef unsigned long size_t;
#endif
#define offsetof(type,member) ((unsigned long)&(((type*)0)->member))
#define OFFSETOF offsetof
/* End Auto-Generated Part */

#endif
