// Custom DLL example; must be named "mydll"
// Note: You should not use DLLs under normal circumstances; see section
// "How to break the 64 KB limit using a DLL" for more information.

#define USE_TI89

#include <tigcclib.h>

DLL_INTERFACE

char MessageInDLL[]="Hello!\n";
long GlobalVarInDLL;
void HelloFromDLL(void);
int SumFromDLL(int,int);

DLL_ID 372377271
DLL_VERSION 2,12
DLL_EXPORTS HelloFromDLL,SumFromDLL,MessageInDLL,&GlobalVarInDLL

DLL_IMPLEMENTATION

void HelloFromDLL(void)
{
  printf ("Hello from DLL!\n");
  printf ("Global variable is %ld\n", GlobalVarInDLL);
}

int SumFromDLL(int a, int b)
{
  return (a + b);
}
