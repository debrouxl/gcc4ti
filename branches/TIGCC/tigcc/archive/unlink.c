#include <stdio.h>
#include <vat.h>

__ATTR_LIB_C__ short unlink(const char *fname)
{
  char sym[50],*sptr=sym;
  *sptr=0; while((*++sptr=*fname++));
  return SymDel(sptr)-1;
}
