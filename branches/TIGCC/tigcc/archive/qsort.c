#include <stdlib.h>

// Do not use register a5; callback function might need it.
register long __tbl asm ("a5");

__ATTR_LIB_C__ void qsort(void *list, short num_items, short size, compare_t cmp_func)
{
  unsigned short gap,byte_gap,i,j;                
  char *p,*a,*b,temp;                       
  for (gap=((unsigned short)num_items)>>1; gap>0; gap>>=1)    // Yes, this is not a quicksort,
    {                                                         // but works fast enough...    
      byte_gap=gap*(unsigned short)size;
      for(i=byte_gap; i<((unsigned short)num_items)*(unsigned short)size; i+=size)
        for(p=(char*)list+i-byte_gap; p>=(char*)list; p-= byte_gap)
          {
            a=p; b=p+byte_gap;
            if(cmp_func(a,b)<=0) break;
            for(j=size;j;j--)
              temp=*a, *a++=*b, *b++=temp;
          }
    }
}
