#include <stdlib.h>
#include <ctype.h>

__ATTR_LIB_C__ long strtol(const char *nptr, char **endptr, short base)
{
  const unsigned char *s=nptr;
  unsigned long acc,cutoff,cb;
  unsigned short c,cutlim,neg=0;
  short any;
  do {c=*s++;} while (c==' ');
  if(c=='-'||c==0xAD) neg=1, c=*s++;
  else if (c=='+') c=*s++;
  if((base==0||base==16)&&c=='0'&&(*s=='x'||*s=='X'))
    {
      c=s[1]; s+=2; base=16;
    }
  if(base==0) base=c=='0'?8:10;
  cb=neg?0x80000000:0x7FFFFFFF;
  asm volatile("\n"
    "	move.l %3,%%d1\n"
    "	move.l %2,%%d0\n"
    "	move.l 0xC8,%%a0\n"
    "	move.l (%%a0,0x2AA*4),%%a0\n" /* _du32u32 */
    "	jsr (%%a0)\n"
    "	move.l %%d1,%0\n"
    "	move.l %3,%%d1\n"
    "	move.l %2,%%d0\n"
    "	move.l 0xC8,%%a0\n"
    "	move.l (%%a0,0x2AB*4),%%a0\n" /* _mu32u32 */
    "	jsr (%%a0)\n"
    "	move.w %%d1,%1":"=g"(cutoff),"=g"(cutlim):"g"((unsigned long)base),
    "g"(cb):"a0","a1","d0","d1","d2");
  for(acc=0,any=0;;c=*s++)
    {
      if(isdigit(c)) c-='0';
      else if(isalpha(c)) c-=isupper(c)?'A'-10:'a'-10;
      else break;
      if (c>=(unsigned short)base) break;
      if (any<0||acc>cutoff||(acc==cutoff&&c>cutlim)) any=-1;
      else
        {
          any=1;
          asm volatile("\n"
            "	move.l %1,%%d0\n"
            "	mulu %2,%%d0\n"
            "	move.l %1,%%d1\n"
            "	swap %%d1\n"
            "	mulu %2,%%d1\n"
            "	swap %%d1\n"
            "	clr.w %%d1\n"
            "	add.l %%d1,%%d0\n"
            "	move.l %%d0,%0":"=g"(acc):"g"(acc),"g"(base):"d0","d1","d2");
          acc+=c;
        }
    }
  if (any<0) acc=neg?0x80000000:0x7FFFFFFF;
  else if(neg) acc=-acc;
  if(endptr!=0) *endptr=(char*)(any?(char*)s-1:nptr);
  return (acc);
}
