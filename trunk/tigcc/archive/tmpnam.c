#include <stdio.h>
#include <vat.h>
#include <stdlib.h>
#include <string.h>

//tmpnam implementation by Greg Dietsche
//email: gforce@calc.org
//webpage: http://gforce.calc.org/

__ATTR_LIB_C__ char *tmpnam(char *s)
{
  static char buff[10]={0}; //"\0        ";
  register char *bptr=buff;
  register short i;
  
  do {
    for(i=1;i<9;i++)
      bptr[i]=((rand()%25)+97);
  } while(SymFind(bptr+9).offset);
  
  if(s) return strcpy(s,bptr+1);
  else  return bptr+1;
}
