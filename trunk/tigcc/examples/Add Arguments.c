// Add the first two integers passed to the program

#define RETURN_VALUE

#define USE_TI89
#define USE_TI92PLUS
#define USE_V200

#define MIN_AMS 101

#include <args.h>
#include <estack.h>

void _main(void)
{
  ESI argptr = top_estack;
  short a = GetIntArg (argptr);
  short b = GetIntArg (argptr);
  while (GetArgType (top_estack) != END_TAG)  // Clean up arguments
    top_estack = next_expression_index (top_estack);
  top_estack--;
  push_longint (a + b);
}
