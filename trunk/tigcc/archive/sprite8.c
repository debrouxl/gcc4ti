#include <sprites.h>

__ATTR_LIB_C__ void Sprite8(short x, short y, short h, const unsigned char *sprite, void *buff, short mode)
{
  long addr=(long)buff+30*y+((x>>3)&0xfffe),d1;
  unsigned short cnt=24-(x&15),data;
  for(;h--;addr+=30)
    {
      data=*sprite++;
      if(mode==SPRT_AND) *(long*)addr&=~((long)(~data&0xff)<<cnt);
      else
        {
          d1=(long)data<<cnt;
          if(mode==SPRT_XOR) *(long*)addr^=d1;
          else *(long*)addr|=d1;
        }
    }
}
