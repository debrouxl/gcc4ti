#include <stdio.h>
#include <alloc.h>

__ATTR_LIB_C__ short fclose(FILE *f)
{
  short s;
  
  if(!f) return EOF;
  s=(f->flags&_F_ERR)?EOF:0;
  
  if(f->flags&_F_WRIT) HeapRealloc(f->handle,(*(unsigned short*)(f->base))+2);
  HeapUnlock(f->handle);
  free(f);
  
  return s;
}
