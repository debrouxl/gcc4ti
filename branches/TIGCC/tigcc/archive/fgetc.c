#include <stdio.h>
#include <peekpoke.h>

__ATTR_LIB_C__ short fgetc(FILE *f)
{
  short c,tmode=!(f->flags&_F_BIN);
  if(f->flags&_F_ERR) return EOF;
  if(!(f->flags&_F_READ)) __FERROR(f);
  if((c=f->unget)<0)
    {
      f->unget=0; return c&0xFF;
    }
  if(feof(f)) return EOF;
  c=peek(f->fpos++);
  if(c=='\r'&&tmode) fgetc(f);
  if(f->base+peek_w(f->base)+(tmode?0:2)==f->fpos) f->flags|=_F_EOF;
  return c;
}
