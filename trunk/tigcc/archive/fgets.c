#include <stdio.h>

__ATTR_LIB_C__ char *fgets(char *s, short n, FILE *fp)
{
  short c=EOF;
  char *cs=s;
  while(--n>0&&(c=fgetc(fp))!=EOF)
    {
      if(c=='\r'&&!(fp->flags&_F_BIN)) c='\n';
      if ((*cs++=c)=='\n') break;
    }
  *cs=0;
  return ((c==EOF&&cs==s)?NULL:s);
}
