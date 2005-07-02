#include <sprites.h>

__ATTR_LIB_C__ void Sprite32(short x, short y, short h, const unsigned long *sprite, void *buff, short mode)
{
  long addr=(long)buff+30*y+((x>>3)&0xfffe);
  unsigned long data,d1,d2;
  short cnt=x&15,ccnt=32-cnt;
  for(;h--;addr+=30)
    {
      data=*sprite++;
      if(mode==SPRT_AND)
        {
          data=~data;
          *(long*)addr&=~(data>>cnt),*(long*)(addr+4)&=~(data<<ccnt);
        }
      else
        {
          d1=data>>cnt; d2=data<<ccnt;
          if(mode==SPRT_XOR) *(long*)addr^=d1,*(long*)(addr+4)^=d2;
          else *(long*)addr|=d1,*(long*)(addr+4)|=d2;
        }
    }
}
