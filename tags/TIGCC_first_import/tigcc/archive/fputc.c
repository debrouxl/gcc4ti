#include <stdio.h>
#include <peekpoke.h>
#include <alloc.h>

__ATTR_TIOS_CALLBACK__ short fputc(short c, FILE *f)
{
  short tmode=!(f->flags&_F_BIN);
  char *base=f->base,*oldbase=base;
  if(f->flags&_F_ERR) return EOF;
  if(!(f->flags&_F_WRIT)) __FERROR(f);
  if(peek_w(base)+10>f->alloc)
    {
      HeapUnlock(f->handle);
      if(!HeapRealloc(f->handle,f->alloc+=f->buffincrement)) __FERROR(f);
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
