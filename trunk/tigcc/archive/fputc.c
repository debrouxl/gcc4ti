#include <stdio.h>
#include <peekpoke.h>
#include <alloc.h>

__ATTR_TIOS_CALLBACK__ short fputc(short c, FILE *f)
{
  short tmode=!(f->flags&_F_BIN);
  unsigned short minalloc;
  char *base=f->base,*oldbase=base;
  if(f->flags&_F_ERR) return EOF;
  if(!(f->flags&_F_WRIT)) __FERROR(f);
  minalloc=peek_w(base)+3;
  if(minalloc>65520u) __FERROR(f);
  if(minalloc>f->alloc)
    {
      HeapUnlock(f->handle);
      if(f->alloc<=65520u-f->buffincrement) f->alloc+=f->buffincrement;
      else f->alloc=65520u;
      if(!HeapRealloc(f->handle,f->alloc)) __FERROR(f);
      base=f->base=HLock(f->handle);
      f->fpos+=base-oldbase;
      oldbase=base;
    }
  if(feof(f)) (*(short*)base)++;
  if(c=='\n'&&tmode) c='\r';
  poke(f->fpos++,c);
  if(c=='\r'&&tmode) fputc(' ',f);
  if(base+peek_w(base)+(tmode?0:2)==f->fpos)
    {
      f->flags|=_F_EOF;
      if(tmode)
        {
          poke(f->fpos,0);
          poke(f->fpos+1,0xE0);
        }
    }
  return c;
}
