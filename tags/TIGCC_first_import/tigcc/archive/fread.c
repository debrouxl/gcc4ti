#include <stdio.h>

__ATTR_LIB_C__ unsigned short fread(void *ptr, short size, short n, FILE *f)
{
  unsigned short i,j;
  short c,binmode=f->flags&_F_BIN;
  f->flags|=_F_BIN;
  for(i=0;i<(unsigned short)n;i++)
    for(j=0;j<(unsigned short)size;j++)
      {
        if((c=fgetc(f))<0) goto fread_error;
        *(unsigned char*)ptr++=c;
      }
fread_error:
  f->flags&=binmode|(~_F_BIN);
  return i;
}
