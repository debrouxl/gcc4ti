#include <stdio.h>
#include <vat.h>

__ATTR_LIB_C__ short rename(const char *old, const char *new)
{
  char sym[100],*sptrold=sym,*sptrnew;
  *sptrold=0; while((*++sptrold=*old++));
  sptrnew=sptrold; while((*++sptrnew=*new++));
  return SymMove(sptrold,sptrnew)-1;
}
