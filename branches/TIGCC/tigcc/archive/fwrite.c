#include <stdio.h>

__ATTR_LIB_C__ unsigned short fwrite(const void *ptr, short size, short n, FILE *f)
{
  unsigned short i,j;
  short binmode=f->flags&_F_BIN;
  f->flags|=_F_BIN;
  for(i=0;i<(unsigned short)n;i++)
    for(j=0;j<(unsigned short)size;j++)
      if(fputc(*(unsigned char*)ptr++,f)<0) goto fwrite_error;
fwrite_error:
  f->flags&=binmode|(~_F_BIN);
  return i;
}
