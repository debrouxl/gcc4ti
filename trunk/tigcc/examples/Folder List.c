// Write the variables in a folder into a list variable

#define RETURN_VALUE dirlist

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#include <args.h>
#include <estack.h>
#include <vat.h>

void _main(void)
{
  ESI argptr = top_estack;
  SYM_ENTRY *SymPtr = SymFindFirst (GetSymstrArg (argptr), 1);
  push_END_TAG ();
  while (SymPtr)
    {
      push_ANSI_string (SymPtr->name);
      SymPtr = SymFindNext ();
    }
  push_LIST_TAG ();
}
