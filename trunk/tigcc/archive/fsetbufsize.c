#include <stdio.h>

__ATTR_LIB_C__ void fsetbufsize(short newsize, FILE *f)
{
  if(newsize&&f)
    f->buffincrement=newsize;
}
