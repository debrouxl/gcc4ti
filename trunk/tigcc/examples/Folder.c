// Get the variables in a folder as a list

#define RETURN_VALUE

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 101

#include <args.h>
#include <estack.h>
#include <vat.h>

void _main(void)
{
  ESI argptr = top_estack;
  SYM_ENTRY *SymPtr = SymFindFirst (GetSymstrArg (argptr), 1);
  while (GetArgType (top_estack) != END_TAG)  // Clean up arguments
    top_estack = next_expression_index (top_estack);
  top_estack--;
  push_END_TAG ();
  while (SymPtr)
    {
      push_ANSI_string (SymPtr->name);
      SymPtr = SymFindNext ();
    }
  push_LIST_TAG ();
}
