#include <stdio.h>

__ATTR_LIB_C__ long ftell(const FILE *f)
{
  if(f->flags&_F_ERR) return EOF;
  return f->fpos-(char*)f->base-((f->flags&_F_BIN)?2:5);
}
