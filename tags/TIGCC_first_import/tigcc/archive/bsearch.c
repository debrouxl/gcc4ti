#include <stdlib.h>

// Do not use register a5; callback function might need it.
register long __tbl asm ("a5");

__ATTR_LIB_C__ void *bsearch(const void *key, const void *bptr, short n, short w, compare_t cmp_func)
{
  unsigned short left=0,right=n-1,index;
  short rcmp;
  void *rptr;
  do
    {
      index=(left+right)>>1;
      if((rcmp=cmp_func(key,rptr=(char*)bptr+(long)index*(unsigned short)w))>0) left=index+1;
      else if(rcmp<0) right=index-1;
      else return rptr;
    } while(left<=right);
  return 0;
}
