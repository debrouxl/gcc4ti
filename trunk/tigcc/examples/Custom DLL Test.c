#define USE_TI89

#define SAVE_SCREEN

#include <tigcclib.h>

#define HelloFromDLL _DLL_call(void,(void),0)
#define SumFromDLL _DLL_call_attr(int,(int,int),__attribute__((stkparm)),1)
#define MessageInDLL _DLL_reference(const char,2)
#define GlobalVarInDLL _DLL_glbvar(long,3)

void _main(void)
{
  if (LoadDLL ("mydll", 372377271, 2, 11) != DLL_OK) 
    {
      DlgMessage ("ERROR", "Error loading DLL!", BT_OK, BT_NONE);
      return;
    }
  clrscr ();
  GlobalVarInDLL = 1234567;
  HelloFromDLL ();
  printf ("Sum from DLL: 2+3=%d\n", SumFromDLL (2, 3));
  printf ("Message from DLL: %s\n", MessageInDLL);
  ngetchx ();  
  UnloadDLL ();
}
