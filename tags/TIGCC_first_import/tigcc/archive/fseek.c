#include <stdio.h>
#include <peekpoke.h>

__ATTR_LIB_C__ short fseek(FILE *f, long offset, short wh)
{
  short bmode=f->flags&_F_BIN;
  char *start=(char*)f->base+(bmode?2:5);
  char *end=(char*)f->base+peek_w(f->base)+(bmode?2:0);
  char *pos=((wh==SEEK_SET)?start:((wh==SEEK_CUR)?(f->fpos):end))+offset;
  if(f->flags&_F_ERR) return EOF;
  if(pos<start||pos>end||(unsigned short)wh>SEEK_END) __FERROR(f);
  f->fpos=pos;
  f->unget=0;
  if(pos==end) f->flags|=_F_EOF;
  else f->flags&=~_F_EOF;
  return 0;
}
