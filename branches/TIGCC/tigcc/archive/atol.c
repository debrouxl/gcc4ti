#include <stdlib.h>
#include <ctype.h>

__ATTR_LIB_C__ long atol(const char *s)
{
  unsigned short c,neg=0;
  long value=0;
  while(*s==' ') ++s;
  if(*s=='-'||*(const unsigned char*)s==0xAD) ++s,neg=-1;
  else if(*s=='+') ++s;
  while(isdigit(c=*(const unsigned char*)s++)) value=(value<<3)+(value<<1)+c-'0';
  return neg?-value:value;
}
